// Process the files with the compiled scripts
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{
    Address, AddressType, AddressValue, Command, CommandData, ProcessingContext, Substitution,
    Transliteration,
};
use crate::fast_io::{IOChunk, LineReader, OutputBuffer};
use crate::in_place::InPlace;
use crate::named_writer;
use atty::Stream;
use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use uucore::error::{UResult, USimpleError};

/// Return true if the passed address matches the current I/O context.
fn match_address(
    addr: &Address,
    pattern: &mut IOChunk,
    context: &ProcessingContext,
) -> UResult<bool> {
    match addr.atype {
        AddressType::Re => {
            if let AddressValue::Regex(ref re) = addr.value {
                Ok(re.is_match(pattern.try_as_str()?))
            } else {
                Ok(false)
            }
        }

        AddressType::Line => {
            if let AddressValue::LineNumber(lineno) = addr.value {
                Ok(context.line_number == lineno)
            } else {
                Ok(false)
            }
        }

        // Recognize "$" as the last line of last file. This is consistent
        // with the original 7th Research Edition implementation:
        // https://github.com/dspinellis/unix-history-repo/blob/Research-V7/usr/src/cmd/sed/sed1.c#L665
        // The FreeBSD version checked for subsequent empty files, but this
        // can lead to destructive reads (e.g. from named pipes),
        // and is probably an overkill.
        AddressType::Last => Ok(context.last_line && (context.last_file || context.separate)),

        _ => panic!("invalid address type in match_address"),
    }
}

#[allow(dead_code)]
/// Return true if the command applies to the given pattern.
fn applies(
    command: &mut Command,
    pattern: &mut IOChunk,
    context: &mut ProcessingContext,
) -> UResult<bool> {
    let linenum = context.line_number;

    let result = if command.addr1.is_none() && command.addr2.is_none() {
        Ok(true)
    } else if let Some(addr2) = &command.addr2 {
        if let Some(start) = command.start_line {
            match addr2.atype {
                AddressType::RelLine => {
                    if let AddressValue::LineNumber(n) = addr2.value {
                        if linenum - start <= n {
                            Ok(true)
                        } else {
                            command.start_line = None;
                            Ok(false)
                        }
                    } else {
                        Ok(false)
                    }
                }
                _ => {
                    if match_address(addr2, pattern, context)? {
                        command.start_line = None;
                        context.last_address = true;
                        Ok(true)
                    } else if addr2.atype == AddressType::Line {
                        if let AddressValue::LineNumber(n) = addr2.value {
                            if linenum > n {
                                command.start_line = None;
                                Ok(false)
                            } else {
                                Ok(true)
                            }
                        } else {
                            Ok(true)
                        }
                    } else {
                        Ok(true)
                    }
                }
            }
        } else if let Some(addr1) = &command.addr1 {
            if match_address(addr1, pattern, context)? {
                match addr2.atype {
                    AddressType::Line => {
                        if let AddressValue::LineNumber(n) = addr2.value {
                            if linenum >= n {
                                context.last_address = true;
                            } else {
                                command.start_line = Some(linenum);
                            }
                        }
                    }
                    AddressType::RelLine => {
                        if let AddressValue::LineNumber(0) = addr2.value {
                            context.last_address = true;
                        } else {
                            command.start_line = Some(linenum);
                        }
                    }
                    _ => {
                        command.start_line = Some(linenum);
                    }
                }
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    } else if let Some(addr1) = &command.addr1 {
        Ok(match_address(addr1, pattern, context)?)
    } else {
        Ok(false)
    };

    if command.non_select {
        result.map(|v| !v)
    } else {
        result
    }
}

/// Write the specified chunk to the output for a given processing context.
fn write_chunk(
    output: &mut OutputBuffer,
    context: &ProcessingContext,
    chunk: &IOChunk,
) -> std::io::Result<()> {
    output.write_chunk(chunk)?;

    if context.unbuffered {
        output.flush()?;
    }

    Ok(())
}

/// Perform the specified RE replacement in the provided pattern space.
fn substitute(
    pattern: &mut IOChunk,
    sub: &mut Substitution,
    context: &ProcessingContext,
    output: &mut OutputBuffer,
) -> UResult<()> {
    let mut count = 0;
    let mut last_end = 0;
    let mut result = String::new();
    let mut replaced = false;

    let text = pattern.try_as_str()?;

    for caps in sub.regex.captures_iter(text) {
        count += 1;
        let m = caps.get(0).unwrap();

        // Always write the unmatched text before this match.
        result.push_str(&text[last_end..m.start()]);

        if sub.occurrence == 0 || count == sub.occurrence {
            let replacement = sub.replacement.apply(&caps)?;
            result.push_str(&replacement);
            replaced = true;
        } else {
            // Not the target match — leave the match unchanged.
            result.push_str(m.as_str());
        }

        last_end = m.end();
    }

    // Handle substitution success.
    if replaced {
        result.push_str(&text[last_end..]);

        pattern.set_to_string(result, pattern.is_newline_terminated());

        if sub.print_flag {
            write_chunk(output, context, pattern)?;
        }

        // Write to file if needed.
        if let Some(ref writer) = sub.write_file {
            writer.borrow_mut().write_line(pattern.try_as_str()?)?;
        }
    }

    Ok(())
}

/// Apply the specified transliteration in the provided pattern space.
fn transliterate(pattern: &mut IOChunk, trans: &Transliteration) -> UResult<()> {
    let text = pattern.try_as_str()?;
    let mut result = String::with_capacity(text.len());
    let mut replaced = false;

    // Perform the transliteration.
    for ch in text.chars() {
        let mapped = trans.lookup(ch);
        if mapped != ch {
            replaced = true;
        }
        result.push(mapped);
    }

    // Lazy replace.
    if replaced {
        pattern.set_to_string(result, pattern.is_newline_terminated());
    }

    Ok(())
}

/// Process a single input file
fn process_file(
    commands: &Option<Rc<RefCell<Command>>>,
    reader: &mut LineReader,
    output: &mut OutputBuffer,
    context: &mut ProcessingContext,
) -> UResult<()> {
    while let Some((mut pattern, last_line)) = reader.get_line()? {
        context.last_line = last_line;
        context.line_number += 1;
        let mut current: Option<Rc<RefCell<Command>>> = commands.clone();
        while let Some(command_rc) = current {
            let mut command = command_rc.borrow_mut();

            if !applies(&mut command, &mut pattern, context)? {
                // Advance to next command
                current = command.next.clone();
                continue;
            }

            // TODO: continue if command doesn't apply
            match command.code {
                '{' => {
                    current = Some(command.data.get_subcommand());
                    continue;
                }
                'a' => {
                    // TODO
                }
                'b' => {
                    current = Some(command.data.get_subcommand());
                    continue;
                }
                'c' => {
                    // TODO
                }
                'd' => {
                    // Delete the pattern space and start the next cycle.
                    pattern.clear();
                    break;
                }
                'D' => {
                    // TODO
                }
                'g' => {
                    // TODO
                }
                'G' => {
                    // TODO
                }
                'h' => {
                    // TODO
                }
                'H' => {
                    // TODO
                }
                'i' => {
                    // TODO
                }
                'l' => {
                    // TODO
                }
                'n' => {
                    // TODO
                }
                'N' => {
                    // TODO
                }
                'p' => {
                    // Write the pattern space to standard output.
                    write_chunk(output, context, &pattern)?;
                }
                'P' => {
                    // TODO
                }
                'q' => {
                    // TODO
                }
                'r' => {
                    // TODO
                }
                's' => {
                    let subst = match &mut command.data {
                        CommandData::Substitution(subst) => subst,
                        _ => panic!("Expected Substitution command data"),
                    };

                    substitute(&mut pattern, &mut *subst, context, output)?;
                }
                't' => {
                    // TODO
                }
                'w' => {
                    // TODO
                }
                'x' => {
                    // TODO
                }
                'y' => {
                    let trans = match &mut command.data {
                        CommandData::Transliteration(trans) => trans,
                        _ => panic!("Expected Transliteration command data"),
                    };

                    transliterate(&mut pattern, trans)?;
                }
                ':' => {
                    // TODO
                }
                '}' => {
                    // TODO
                }
                '=' => {
                    // TODO
                }
                // The compilation should supply only valid codes.
                _ => panic!("invalid command code"),
            } // match
            // Advance to next command.
            current = command.next.clone();
        }

        if !context.quiet {
            write_chunk(output, context, &pattern)?;
        }
    }
    Ok(())
}

/// Process all input files
pub fn process_all_files(
    commands: Option<Rc<RefCell<Command>>>,
    files: Vec<PathBuf>,
    mut context: ProcessingContext,
) -> UResult<()> {
    context.unbuffered = context.unbuffered || atty::is(Stream::Stdout);

    let mut in_place = InPlace::new(context.clone())?;
    let last_file_index = files.len() - 1;

    for (index, path) in files.iter().enumerate() {
        context.last_file = index == last_file_index;
        let mut reader = LineReader::open(path).map_err(|e| {
            USimpleError::new(
                2,
                format!("Error opening input file {}: {}", path.display(), e),
            )
        })?;
        let output = in_place.begin(path)?;

        if context.separate {
            context.line_number = 0;
        }
        process_file(&commands, &mut reader, output, &mut context)?;

        in_place.end()?;
    }

    // Flush all output files
    named_writer::flush_all()?;

    Ok(())
}
