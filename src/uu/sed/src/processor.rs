// Process the files with the compiled scripts
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{CliOptions, Command, ProcessingContext};
use std::path::PathBuf;
use uucore::error::UResult;

pub fn process(
    _commands: Option<Box<Command>>,
    files: Vec<PathBuf>,
    cli_options: CliOptions,
) -> UResult<()> {
    let mut context = ProcessingContext::new(files, cli_options)?;

    while let Some(chunk) = context.get_line()? {
        // TODO: process commands
        context.write_chunk(&chunk)?;
    }
    context.flush()?;

    Ok(())
}
