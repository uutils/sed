// Compile the scripts into the internal representation of commands
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{Command, Context, ScriptValue};
use crate::script_line_provider::ScriptLineProvider;
use uucore::error::UResult;

pub fn compile(scripts: Vec<ScriptValue>, _context: &mut Context) -> UResult<Option<Command>> {
    let mut _line_provider = ScriptLineProvider::new(scripts);

    // TODO
    Ok(None)
}
