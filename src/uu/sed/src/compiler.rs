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
use uucore::error::UResult;

pub fn compile(_scripts: Vec<ScriptValue>, _context: &mut Context) -> UResult<Option<Command>> {
    // TODO
    Ok(None)
}
