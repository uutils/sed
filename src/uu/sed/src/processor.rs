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
    Address, AddressType, AddressValue, AppendElement, Command, CommandData, InputAction,
    ProcessingContext, Substitution, Transliteration,
};
use crate::fast_io::{IOChunk, LineReader, OutputBuffer};
use crate::in_place::InPlace;
use crate::named_writer;
use regex::Regex;
use std::cell::RefCell;
use std::io::{self, IsTerminal};
use std::path::PathBuf;
use std::rc::Rc;
use uucore::error::{UResult, USimpleError};

/// Return true if the passed address matches the current I/O context.
fn match_address(
    addr: &Address,
    pattern: &mut IOChunk,
    context: &mut ProcessingContext,
) -> UResult<bool> {
    match addr.atype {
        AddressType::Re => {
            if let AddressValue::Regex(ref re) = addr.value {
                let regex = re_or_saved_re(re, context)?;
                Ok(regex.is_match(pattern.as_str()?))
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

/// Return the RE or the saved RE if the RE is None.
/// Update the saved RE to RE.
fn re_or_saved_re(regex: &Option<Regex>, context: &mut ProcessingContext) -> UResult<Regex> {
    match regex {
        Some(re) => {
            *context.saved_regex.borrow_mut() = Some(re.clone());
            Ok(re.clone())
        }
        None => match &*context.saved_regex.borrow() {
            Some(saved_re) => Ok(saved_re.clone()),
            None => Err(USimpleError::new(2, "no previous regular expression")),
        },
    }
}

/// Perform the specified RE replacement in the provided pattern space.
fn substitute(
    pattern: &mut IOChunk,
    sub: &mut Substitution,
    context: &mut ProcessingContext,
    output: &mut OutputBuffer,
) -> UResult<()> {
    let mut count = 0;
    let mut last_end = 0;
    let mut result = String::new();
    let mut replaced = false;

    let text = pattern.as_str()?;

    let regex = re_or_saved_re(&sub.regex, context)?;

    for caps in regex.captures_iter(text) {
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
            writer.borrow_mut().write_line(pattern.as_str()?)?;
        }
        context.substitution_made = true;
    }

    Ok(())
}

/// Apply the specified transliteration in the provided pattern space.
fn transliterate(pattern: &mut IOChunk, trans: &Transliteration) -> UResult<()> {
    let text = pattern.as_str()?;
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

/// Output any data queued for output at the end of the cycle.
fn flush_appends(output: &mut OutputBuffer, context: &mut ProcessingContext) -> UResult<()> {
    for elem in &context.append_elements {
        match elem {
            AppendElement::Text(text) => {
                output.write_str(text.clone())?;
            }
            AppendElement::File(path) => {
                output.copy_file(path)?;
            }
        }
    }
    context.append_elements.clear();
    Ok(())
}

/// Process a single input file
fn process_file(
    commands: &Option<Rc<RefCell<Command>>>,
    reader: &mut LineReader,
    output: &mut OutputBuffer,
    context: &mut ProcessingContext,
) -> UResult<()> {
    // Loop over the input lines as pattern space.
    'lines: while let Some((mut pattern, last_line)) = reader.get_line()? {
        context.last_line = last_line;
        context.line_number += 1;
        context.substitution_made = false;
        // Set the script command from which to start.
        let mut current: Option<Rc<RefCell<Command>>> =
            if let Some(action) = context.input_action.take() {
                // Continue processing the `N` command.
                let current_line = pattern.as_str()?;
                let mut combined_lines = action.prepend;
                combined_lines.push('\n');
                combined_lines.push_str(current_line);

                pattern.set_to_string(combined_lines, pattern.is_newline_terminated());
                action.next_command
            } else {
                // Start from the script top.
                commands.clone()
            };

        // Loop over script commands.
        while let Some(command_rc) = current.clone() {
            let mut command = command_rc.borrow_mut();

            if !applies(&mut command, &mut pattern, context)? {
                // Advance to next command
                current = command.next.clone();
                continue;
            }

            match command.code {
                '{' => {
                    // Block begin; start processing the enclosed ones.
                    let CommandData::Block(body) = &command.data else {
                        panic!("Expected Block command data");
                    };
                    current = body.clone();
                    continue;
                }
                '}' => {
                    // Block end: continue with the block's patched next.
                }
                'a' => {
                    // Write the text to standard output at a later point.
                    let text = match &command.data {
                        CommandData::Text(text) => text,
                        _ => panic!("Expected Text command data"),
                    };
                    context
                        .append_elements
                        .push(AppendElement::Text(text.clone().into_owned()));
                }
                'b' => {
                    // Branch to the specified label or end if none is given.
                    let CommandData::BranchTarget(target) = &command.data else {
                        panic!("Expected BranchTarget command data");
                    };
                    if target.is_some() {
                        // New command to execute
                        current = target.clone();
                        continue;
                    } else {
                        // Branch to the end of the script.
                        break;
                    }
                }
                'c' => {
                    // At range end replace pattern space with text and
                    // start the next cycle.
                    pattern.clear();
                    if command.addr2.is_none() || context.last_address || context.last_line {
                        let text = match &command.data {
                            CommandData::Text(text) => text,
                            _ => panic!("Expected Text command data"),
                        };
                        output.write_str(text.as_ref())?;
                    }
                    break;
                }
                'd' => {
                    // Delete the pattern space and start the next cycle.
                    pattern.clear();
                    break;
                }
                'D' => {
                    // Delete up to \n and start a new cycle without new input.
                    if let Some(pos) = pattern.as_str()?.find('\n') {
                        let (s, _) = pattern.fields_mut()?;
                        s.drain(..=pos);
                        current = commands.clone();
                        continue;
                    } else {
                        // Same as d
                        pattern.clear();
                        break;
                    }
                }
                'g' => {
                    // Replace pattern with the contents of the hold space.
                    pattern.set_to_string(context.hold.content.clone(), context.hold.has_newline);
                }
                'G' => {
                    // Append to pattern \n followed by hold space contents.
                    let (pat_content, pat_has_newline) = pattern.fields_mut()?;
                    pat_content.push('\n');
                    pat_content.push_str(&context.hold.content);
                    *pat_has_newline = context.hold.has_newline;
                }
                'h' => {
                    // Replace hold with the contents of the pattern space.
                    context.hold.content = pattern.as_str()?.to_string();
                    context.hold.has_newline = pattern.is_newline_terminated();
                }
                'H' => {
                    // Append to hold \n followed by pattern space contents.
                    context.hold.content.push('\n');
                    context.hold.content.push_str(pattern.as_str()?);
                    context.hold.has_newline = pattern.is_newline_terminated();
                }
                'i' => {
                    // Write text to standard output.
                    let text = match &command.data {
                        CommandData::Text(text) => text,
                        _ => panic!("Expected Text command data"),
                    };
                    output.write_str(text.as_ref())?;
                }
                'l' => {
                    // TODO
                }
                'n' => {
                    break;
                }
                'N' => {
                    flush_appends(output, context)?;
                    // Append to pattern `\n` and the next line
                    // Rather than reading input here, which would result
                    // in a double borrow on reader, modify the action
                    // to perform when the next line is read.
                    context.input_action = Some(InputAction {
                        next_command: command.next.clone(),
                        prepend: pattern.as_str()?.to_string(),
                    });
                    continue 'lines;
                }
                'p' => {
                    // Write the pattern space to standard output.
                    write_chunk(output, context, &pattern)?;
                }
                'P' => {
                    // Output pattern space, up to the first \n.
                    let line = pattern.as_str()?;
                    match line.find('\n') {
                        Some(pos) => {
                            output.write_str(&line[..=pos])?;
                        }
                        None => {
                            output.write_str(line)?;
                            output.write_str("\n")?;
                        }
                    }
                }
                'q' => {
                    context.stop_processing = true;
                    break;
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
                't' if !context.substitution_made => { /* Do nothing. */ }
                't' => {
                    // Branch to the specified label or end if none is given
                    // if a substitution was made since last cycle or t.
                    let CommandData::BranchTarget(target) = &command.data else {
                        panic!("Expected BranchTarget command data");
                    };
                    context.substitution_made = false;
                    if target.is_some() {
                        // New command to execute
                        current = target.clone();
                        continue;
                    } else {
                        // Branch to the end of the script.
                        break;
                    }
                }
                'w' => {
                    // TODO
                }
                'x' => {
                    // Exchange the contents of the pattern and hold spaces.
                    let (pat_content, pat_has_newline) = pattern.fields_mut()?;
                    std::mem::swap(pat_content, &mut context.hold.content);
                    std::mem::swap(pat_has_newline, &mut context.hold.has_newline);
                }
                'y' => {
                    let trans = match &mut command.data {
                        CommandData::Transliteration(trans) => trans,
                        _ => panic!("Expected Transliteration command data"),
                    };

                    transliterate(&mut pattern, trans)?;
                }
                ':' => {
                    // Branch target; do nothing.
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

        flush_appends(output, context)?;

        if context.stop_processing {
            break;
        }
    }

    // Handle any N command remains.
    if context.separate && !context.quiet {
        if let Some(action) = context.input_action.take() {
            let mut pending = action.prepend;
            pending.push('\n');
            output.write_str(pending)?;
            if context.unbuffered {
                output.flush()?;
            }
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
    context.unbuffered = context.unbuffered || io::stdout().is_terminal();

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

        // Handle any N command remains.
        if context.last_file && !context.separate && !context.quiet {
            if let Some(action) = context.input_action.take() {
                let mut pending = action.prepend;
                pending.push('\n');
                output.write_str(pending)?;
            }
        }

        in_place.end()?;

        if context.stop_processing {
            break;
        }
    }

    // Flush all output files
    named_writer::flush_all()?;

    Ok(())
}
