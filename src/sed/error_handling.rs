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
    /// The script line itself, kept so that an error raised long after the
    /// line was consumed can still underline it. `None` when diagnostics are
    /// off -- down a pipe, say -- since the line provider streams and every
    /// compiled command would otherwise pay for a copy nobody reads.
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

/// Append an underlined view of the offending script line to `message`, when
/// the error is going to a terminal (or `UUTILS_DIAG` insists).
///
/// The first line of the result is always the message the caller built, so
/// existing output is only ever added to -- and down a pipe, where anything
/// parsing sed's output lives, it is the whole of it. Anything that cannot be
/// drawn -- a script that is not valid UTF-8, an empty line, a location with no
/// line recorded, an error hit once the whole script has been read -- yields the message unchanged.
fn with_snippet(
    message: String,
    input_name: &str,
    line_number: usize,
    column_number: usize,
    line_text: Option<&str>,
) -> String {
    // Line 0 means the script ran out before the error was noticed: the
    // message names no line, so there is none to point at.
    if !enabled() || line_number == 0 {
        return message;
    }
    let Some(text) = line_text else {
        return message;
    };
    if text.is_empty() {
        return message;
    }

    // Errors that run off the end of the line -- an unterminated regular
    // expression, a missing argument -- report the column just past the last
    // character. Give the caret a space to sit on there, so that the column
    // drawn is the one the message names rather than one short of it.
    let text = if column_number > text.len() {
        format!("{text} ")
    } else {
        text.to_string()
    };
    // `column_number` is a 1-based *byte* column, and anything further out than
    // the space just added is not a position in this line at all.
    let start = column_number - 1;
    let Some(offending) = text.get(start..).and_then(|rest| rest.chars().next()) else {
        return message;
    };

    // Only the offending line is still in hand -- the provider streams and
    // keeps no history -- but the report should show the line number the
    // message quotes. Pad with the missing newlines so that the line lands
    // where it belongs, and move the span along with it. (ariadne's
    // display_line_offset renumbers the header but not the gutter.)
    let padding = "\n".repeat(line_number - 1);
    let start = start + padding.len();
    let source = padding + &text;

    let span = start..start + offending.len_utf8();
    match render_snippet(input_name, &source, span, line_number, column_number) {
        Some(drawn) => format!("{message}\n{drawn}"),
        None => message,
    }
}

/// Draw `source` with the bytes in `span` underlined, headed by the same
/// `input_name:line_number:column_number` the message names.
///
/// Returns `None` when ariadne cannot draw it, so that the caller keeps the
/// plain message.
fn render_snippet(
    input_name: &str,
    source: &str,
    span: Range<usize>,
    line_number: usize,
    column_number: usize,
) -> Option<String> {
    let color = env::var_os("NO_COLOR").is_none() && stderr().is_terminal();
    let config = Config::default()
        // Columns are byte columns, as in the message; ariadne would count
        // characters otherwise and drift on multi-byte scripts.
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

    // ariadne heads every report with its own, untranslated "Error:" line; the
    // message sed already printed takes its place.
    let rendered = String::from_utf8_lossy(&rendered);
    let (_, body) = rendered.split_once('\n')?;
    let (header, body) = body.split_once('\n')?;

    // ariadne's header counts characters even when told to index bytes, while
    // sed's message counts bytes, as GNU sed does. Swap in the message's own
    // position, so that the two agree on a multi-byte line.
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
