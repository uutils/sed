// Process the files with the compiled scripts
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{Command, ProcessingContext};
use crate::fast_io::{LineReader, OutputBuffer};
use crate::in_place::InPlace;
use atty::Stream;
use std::path::PathBuf;
use uucore::error::UResult;

/// Process a single input file
fn process_file(
    commands: &Option<Box<Command>>,
    reader: &mut LineReader,
    output: &mut OutputBuffer,
    processing_context: &mut ProcessingContext,
) -> UResult<()> {
    while let Some(pattern_space) = reader.get_line()? {
        processing_context.line_number += 1;
        let mut current = commands.as_deref();
        while let Some(command) = current {
            // TODO: continue if command doesn't apply
            match command.code {
                '{' => {
                    // TODO
                }
                'a' => {
                    // TODO
                }
                'b' => {
                    // TODO
                }
                'c' => {
                    // TODO
                }
                'd' => {
                    // TODO
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
            current = command.next.as_deref();
        }

        output.write_chunk(&pattern_space)?;
        if processing_context.unbuffered {
            output.flush()?;
        }
    }
    Ok(())
}

/// Process all input files
pub fn process_all_files(
    commands: Option<Box<Command>>,
    files: Vec<PathBuf>,
    mut processing_context: ProcessingContext,
) -> UResult<()> {
    processing_context.unbuffered = processing_context.unbuffered || atty::is(Stream::Stdout);

    let mut in_place = InPlace::new(processing_context.clone())?;
    for path in files {
        let mut reader = LineReader::open(&path)?;
        let output = in_place.begin(&path)?;

        if processing_context.separate {
            processing_context.line_number = 0;
        }
        process_file(&commands, &mut reader, output, &mut processing_context)?;

        in_place.end()?;
    }

    Ok(())
}
