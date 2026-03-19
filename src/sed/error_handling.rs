// Parse delimited character sequences
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::sed::command::ProcessingContext;
use crate::sed::script_char_provider::ScriptCharProvider;
use crate::sed::script_line_provider::ScriptLineProvider;

use std::rc::Rc;

use uucore::error::{UResult, USimpleError};

#[derive(Clone, Debug)]
/// The location in a script where a command is defined
pub struct ScriptLocation {
    pub input_name: Rc<str>,  // Shared input name
    pub line_number: usize,   // 1-based line number
    pub column_number: usize, // 1-based column number
}

impl Default for ScriptLocation {
    fn default() -> Self {
        ScriptLocation {
            input_name: Rc::from("<unknown>"),
            line_number: 1,
            column_number: 1,
        }
    }
}

impl ScriptLocation {
    /// Construct with position information from the given providers.
    pub fn at_position(lines: &ScriptLineProvider, line: &ScriptCharProvider) -> Self {
        ScriptLocation {
            line_number: lines.get_line_number(),
            column_number: line.get_pos() + 1,
            input_name: Rc::from(lines.get_input_name()),
        }
    }
}

/// Fail with msg as a compile error at the provider location.
/// The error's exit code is 1 (compilation phase).
/// Format matches GNU sed: `-e expression #N, char C: message` for string
/// scripts, `file:line: message` for file scripts.
pub fn compilation_error<T>(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    msg: impl ToString,
) -> UResult<T> {
    let input_name = lines.get_input_name();
    let message = if input_name.starts_with("-e expression") {
        // GNU format: char position is 1-based within the current line
        format!(
            "{}, char {}: {}",
            input_name,
            line.get_pos() + 1,
            msg.to_string()
        )
    } else {
        format!(
            "{}:{}: {}",
            input_name,
            lines.get_line_number(),
            msg.to_string()
        )
    };
    Err(USimpleError::new(1, message))
}

/// Fail with msg as a compilation error at the command's location.
/// The error's exit code is as specified.
/// Format matches GNU sed conventions.
fn location_error<T>(location: &ScriptLocation, msg: impl ToString, exit_code: i32) -> UResult<T> {
    let message = if location.input_name.starts_with("-e expression") {
        format!(
            "{}, char {}: {}",
            location.input_name,
            location.column_number,
            msg.to_string()
        )
    } else {
        format!(
            "{}:{}: {}",
            location.input_name,
            location.line_number,
            msg.to_string()
        )
    };
    Err(USimpleError::new(exit_code, message))
}

/// Fail with msg as a compilation error at the command's location.
/// The error's exit code is 1 (compilation phase).
pub fn semantic_error<T>(location: &ScriptLocation, msg: impl ToString) -> UResult<T> {
    location_error(location, msg, 1)
}

/// Fail with msg as a runtime error at the command's location.
/// The error's exit code is 2 (processing phase).
pub fn runtime_error<T>(location: &ScriptLocation, msg: impl ToString) -> UResult<T> {
    location_error(location, msg, 2)
}

/// Fail with msg as a runtime error at the command's and input's location.
/// This is to be used in cases where the error depends on both, for example,
/// a fancy regular expression applied on invalid UTF-8 input.
/// (A fixed string match will not err in this case.)
/// The error's exit code is 2 (processing phase).
pub fn input_runtime_error<T>(
    location: &ScriptLocation,
    context: &ProcessingContext,
    msg: impl ToString,
) -> UResult<T> {
    let loc_str = if location.input_name.starts_with("-e expression") {
        format!("{}, char {}", location.input_name, location.column_number)
    } else {
        format!("{}:{}", location.input_name, location.line_number)
    };
    Err(USimpleError::new(
        2,
        format!(
            "{}: {}:{} error: {}",
            loc_str,
            context.input_name,
            context.line_number,
            msg.to_string()
        ),
    ))
}
