// Process the files with the compiled scripts
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::Command;
use crate::command::Context;
use std::path::PathBuf;
use uucore::error::UResult;

pub fn process(
    _code: Option<Command>,
    _files: Vec<PathBuf>,
    _context: &mut Context,
) -> UResult<()> {
    // TODO
    Ok(())
}
