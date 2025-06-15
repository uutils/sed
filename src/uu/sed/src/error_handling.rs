// Parse delimited character sequences
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::script_char_provider::ScriptCharProvider;
use crate::script_line_provider::ScriptLineProvider;
use uucore::error::{UResult, USimpleError};

/// Fail with msg as a compile error at the current location
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
