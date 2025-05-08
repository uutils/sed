// Process the files with the compiled scripts
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{Address, AddressType, AddressValue, Command, ProcessingContext};
use crate::fast_io::{IOChunk, LineReader, OutputBuffer};
use crate::in_place::InPlace;
use atty::Stream;
use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use uucore::error::UResult;

#[allow(dead_code)]
/// Return true if the passed address matches the current I/O context.
fn match_address(
    addr: &Address,
    reader: &mut LineReader,
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
        AddressType::Last => Ok(reader.is_last_line()?),
        _ => panic!("invalid address type in match_address"),
    }
}

#[allow(dead_code)]
/// Return true if the command applies to the given pattern.
fn applies(
    command: &mut Command,
    reader: &mut LineReader,
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
                    if match_address(addr2, reader, pattern, context)? {
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
            if match_address(addr1, reader, pattern, context)? {
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
        Ok(match_address(addr1, reader, pattern, context)?)
    } else {
        Ok(false)
    };

    if command.non_select {
        result.map(|v| !v)
    } else {
        result
    }
}

/// Process a single input file
fn process_file(
    commands: &Option<Rc<RefCell<Command>>>,
    reader: &mut LineReader,
    output: &mut OutputBuffer,
    context: &mut ProcessingContext,
) -> UResult<()> {
    while let Some(p) = reader.get_line()? {
        let mut pattern = p;
        context.line_number += 1;
        let mut current: Option<Rc<RefCell<Command>>> = commands.clone();
        while let Some(command_rc) = current {
            let command = command_rc.borrow();

            // Not compiled until the double-borrow of reader is resolved.
            #[cfg(any())]
            if !applies(&mut command, reader, &mut pattern, context)? {
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
                    // TODO
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
                    // TODO
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
                    // TODO
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

        output.write_chunk(&pattern)?;
        if context.unbuffered {
            output.flush()?;
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
    for path in files {
        let mut reader = LineReader::open(&path)?;
        let output = in_place.begin(&path)?;

        if context.separate {
            context.line_number = 0;
        }
        process_file(&commands, &mut reader, output, &mut context)?;

        in_place.end()?;
    }

    Ok(())
}
