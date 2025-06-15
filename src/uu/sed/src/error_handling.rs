// Parse delimited character sequences
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::Command;
use crate::script_char_provider::ScriptCharProvider;
use crate::script_line_provider::ScriptLineProvider;

use uucore::error::{UResult, USimpleError};

/// Fail with msg as a compile error at the provider location.
/// The error's exit code is 1 (compilation phase).
pub fn compilation_error<T>(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    msg: impl ToString,
) -> UResult<T> {
    Err(USimpleError::new(
        1,
        format!(
            "{}:{}:{}: error: {}",
            lines.get_input_name(),
            lines.get_line_number(),
            line.get_pos() + 1,
            msg.to_string()
        ),
    ))
}

/// Fail with msg as a compilation error at the command's location.
/// The error's exit code is as specified.
fn command_error<T>(command: &Command, msg: impl ToString, exit_code: i32) -> UResult<T> {
    Err(USimpleError::new(
        exit_code,
        format!(
            "{}:{}:{}: command `{}': error: {}",
            command.input_name,
            command.line_number,
            command.column_number,
            command.code,
            msg.to_string()
        ),
    ))
}

/// Fail with msg as a compilation error at the command's location.
/// The error's exit code is 1 (compilation phase).
pub fn semantic_error<T>(command: &Command, msg: impl ToString) -> UResult<T> {
    command_error(command, msg, 1)
}

/// Fail with msg as a runtime error at the command's location.
/// The error's exit code is 2 (processing phase).
pub fn runtime_error<T>(command: &Command, msg: impl ToString) -> UResult<T> {
    command_error(command, msg, 2)
}
