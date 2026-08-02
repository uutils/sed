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

use std::env;
use std::io::{IsTerminal, stderr};
use std::ops::Range;
use std::rc::Rc;

use ariadne::{CharSet, Color, Config, IndexType, Label, Report, ReportKind, Source};
use uucore::diagnostics::enabled;
use uucore::display::Quotable;
use uucore::error::{UResult, USimpleError};

#[derive(Clone, Debug)]
/// The location in a script where a command is defined
pub struct ScriptLocation {
    pub input_name: Rc<str>,  // Shared input name
    pub line_number: usize,   // 1-based line number
    pub column_number: usize, // 1-based column number
    /// The script line, for errors raised after it was consumed.
    /// `None` when diagnostics are off.
    pub line_text: Option<Rc<str>>,
}

impl Default for ScriptLocation {
    fn default() -> Self {
        ScriptLocation {
            input_name: Rc::from("<unknown>"),
            line_number: 1,
            column_number: 1,
            line_text: None,
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
            line_text: if enabled() { line.line_text() } else { None },
        }
    }
}

/// Append the offending script line, underlined, to `message` when
/// diagnostics are enabled. Returns `message` unchanged if nothing can be drawn.
fn with_snippet(
    message: String,
    input_name: &str,
    line_number: usize,
    column_number: usize,
    line_text: Option<&str>,
) -> String {
    // Line 0: the script was exhausted, so there is no line to show.
    if !enabled() || line_number == 0 {
        return message;
    }
    let Some(text) = line_text else {
        return message;
    };
    if text.is_empty() {
        return message;
    }

    // Errors at end of line point one past the last character; add a space
    // for the caret.
    let text = if column_number > text.len() {
        format!("{text} ")
    } else {
        text.to_string()
    };
    // `column_number` is a 1-based byte column.
    let start = column_number - 1;
    let Some(offending) = text.get(start..).and_then(|rest| rest.chars().next()) else {
        return message;
    };

    // Pad with newlines so the gutter shows the real line number (ariadne's
    // display_line_offset only affects the header).
    let padding = "\n".repeat(line_number - 1);
    let start = start + padding.len();
    let source = padding + &text;

    let span = start..start + offending.len_utf8();
    match render_snippet(input_name, &source, span, line_number, column_number) {
        Some(drawn) => format!("{message}\n{drawn}"),
        None => message,
    }
}

/// Draw `source` with `span` underlined, or `None` if ariadne fails.
fn render_snippet(
    input_name: &str,
    source: &str,
    span: Range<usize>,
    line_number: usize,
    column_number: usize,
) -> Option<String> {
    let color = env::var_os("NO_COLOR").is_none() && stderr().is_terminal();
    let config = Config::default()
        // Byte columns, as in the message.
        .with_index_type(IndexType::Byte)
        .with_color(color)
        .with_char_set(CharSet::Unicode);
    let label = Label::new((input_name, span.clone()))
        .with_color(Color::Red)
        .with_message("here");

    let mut rendered = Vec::new();
    Report::build(ReportKind::Error, (input_name, span))
        .with_config(config)
        .with_label(label)
        .finish()
        .write((input_name, Source::from(source)), &mut rendered)
        .ok()?;

    // Drop ariadne's "Error:" line; sed's message replaces it.
    let rendered = String::from_utf8_lossy(&rendered);
    let (_, body) = rendered.split_once('\n')?;
    let (header, body) = body.split_once('\n')?;

    // ariadne's header counts characters; use the message's byte position.
    let prefix = format!("{input_name}:");
    let from = header.find(&prefix)?;
    let position = from + prefix.len();
    let to = position + header[position..].find(' ')?;
    Some(format!(
        "{}{prefix}{line_number}:{column_number}{}\n{}",
        &header[..from],
        &header[to..],
        body.trim_end_matches('\n')
    ))
}

/// Fail with msg as a compile error at the provider location.
/// The error's exit code is 1 (compilation phase).
pub fn compilation_error<T>(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    msg: impl ToString,
) -> UResult<T> {
    let input_name = lines.get_input_name();
    let line_number = lines.get_line_number();
    let column_number = line.get_pos() + 1;
    let message = format!(
        "{}:{}:{}: error: {}",
        input_name,
        line_number,
        column_number,
        msg.to_string()
    );
    Err(USimpleError::new(
        1,
        with_snippet(
            message,
            input_name,
            line_number,
            column_number,
            str::from_utf8(line.get_line()).ok(),
        ),
    ))
}

/// Fail with msg as a compilation error at the command's location.
/// The error's exit code is as specified.
fn location_error<T>(location: &ScriptLocation, msg: impl ToString, exit_code: i32) -> UResult<T> {
    let message = format!(
        "{}:{}:{}: error: {}",
        location.input_name,
        location.line_number,
        location.column_number,
        msg.to_string()
    );
    Err(USimpleError::new(
        exit_code,
        with_snippet(
            message,
            &location.input_name,
            location.line_number,
            location.column_number,
            location.line_text.as_deref(),
        ),
    ))
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
    Err(USimpleError::new(
        2,
        format!(
            "{}:{}:{}: {}:{} error: {}",
            location.input_name,
            location.line_number,
            location.column_number,
            context.input_name.quote(),
            context.line_number,
            msg.to_string()
        ),
    ))
}
