// Process the files with the compiled scripts
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{Command, ProcessingOptions};
use crate::fast_io::{LineReader, OutputBuffer};
use crate::in_place::InPlace;
use atty::Stream;
use std::path::PathBuf;
use uucore::error::UResult;

/// Process a single input file
fn process_file(
    _commands: &Option<Box<Command>>,
    reader: &mut LineReader,
    output: &mut OutputBuffer,
    processing_options: &mut ProcessingOptions,
) -> UResult<()> {
    while let Some(pattern_space) = reader.get_line()? {
        // TODO: process commands
        output.write_chunk(&pattern_space)?;
        if processing_options.unbuffered {
            output.flush()?;
        }
    }
    Ok(())
}

/// Process all input files
pub fn process_all_files(
    commands: Option<Box<Command>>,
    files: Vec<PathBuf>,
    mut processing_options: ProcessingOptions,
) -> UResult<()> {
    processing_options.unbuffered = processing_options.unbuffered || atty::is(Stream::Stdout);
    let mut in_place = InPlace::new(processing_options.clone())?;

    for path in files {
        let mut reader = LineReader::open(&path)?;
        let output = in_place.begin(&path)?;

        process_file(&commands, &mut reader, output, &mut processing_options)?;

        in_place.end()?;
    }

    Ok(())
}
