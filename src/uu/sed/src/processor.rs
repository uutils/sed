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
use crate::fast_io::LineReader;
use crate::in_place::InPlace;
use atty::Stream;
use std::path::PathBuf;
use uucore::error::UResult;

pub fn process(
    _commands: Option<Box<Command>>,
    files: Vec<PathBuf>,
    processing_options: ProcessingOptions,
) -> UResult<()> {
    let mut in_place = InPlace::new(&processing_options)?;
    let line_flush = processing_options.unbuffered || atty::is(Stream::Stdout);

    for path in files {
        let mut reader = LineReader::open(&path)?;
        let output = in_place.begin(&path)?;

        while let Some(pattern_space) = reader.get_line()? {
            // TODO: process commands
            output.write_chunk(&pattern_space)?;
            if line_flush {
                output.flush()?;
            }
        }

        in_place.end()?;
    }

    Ok(())
}
