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
use std::rc::Rc;
use std::sync::OnceLock;

use unicode_width::UnicodeWidthChar;
use uucore::diagnostics;
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
            line_text: if diagnostics_enabled() {
                line.line_text()
            } else {
                None
            },
        }
    }
}

/// Whether script errors get a snippet. Evaluated once: a location is
/// recorded for every compiled command, and the terminal check is a syscall.
fn diagnostics_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(diagnostics::enabled)
}

/// Format `msg` as an error at the given script position, followed by the
/// offending script line, underlined, when diagnostics are enabled.
fn script_error(
    input_name: &str,
    line_number: usize,
    column_number: usize,
    line_text: Option<&str>,
    msg: impl ToString,
) -> String {
    let message = format!(
        "{input_name}:{line_number}:{column_number}: error: {}",
        msg.to_string()
    );
    // Line 0: the script was exhausted, so there is no line to show.
    if !diagnostics_enabled() || line_number == 0 {
        return message;
    }
    // A script line that is not valid UTF-8 cannot be drawn.
    let Some(text) = line_text.filter(|text| !text.is_empty()) else {
        return message;
    };

    // `column_number` is a 1-based byte column; errors at end of line point
    // one past the last character.
    let Some(prefix) = text.get(..(column_number - 1).min(text.len())) else {
        return message;
    };
    let drawn = render_snippet(
        input_name,
        line_number,
        column_number,
        text,
        prefix,
        use_color(),
    );
    format!("{message}\n{drawn}")
}

/// Whether the underline is colored. uucore's diagnostics make the same
/// check but keep it private.
fn use_color() -> bool {
    env::var_os("NO_COLOR").is_none() && stderr().is_terminal()
}

/// Draw `text` as line `line_number` of `input_name`, underlining the
/// character that follows `prefix`, in the layout uucore's diagnostics use:
///
/// ```text
///    ╭─[ <script argument 1>:1:7 ]
///    │
///  1 │ s/a/b/q
///    │       ─
/// ───╯
/// ```
fn render_snippet(
    input_name: &str,
    line_number: usize,
    column_number: usize,
    text: &str,
    prefix: &str,
    color: bool,
) -> String {
    let number = line_number.to_string();
    let margin = " ".repeat(number.len() + 2);
    // Pad by display width so the underline lines up with the character
    // above it; tabs are kept as they are, since their width depends on the
    // terminal.
    let indent: String = prefix
        .chars()
        .flat_map(|c| {
            let (fill, width) = if c == '\t' {
                ('\t', 1)
            } else {
                (' ', c.width().unwrap_or(0))
            };
            std::iter::repeat_n(fill, width)
        })
        .collect();
    // As wide as the offending character; one column past the end of line.
    let width = text[prefix.len()..]
        .chars()
        .next()
        .and_then(UnicodeWidthChar::width)
        .unwrap_or(1)
        .max(1);
    let underline = "─".repeat(width);
    let underline = if color {
        format!("\x1b[31m{underline}\x1b[0m")
    } else {
        underline
    };
    format!(
        "{margin}╭─[ {input_name}:{line_number}:{column_number} ]\n\
         {margin}│\n\
         \x20{number} │ {text}\n\
         {margin}│ {indent}{underline}\n\
         {rule}╯",
        rule = "─".repeat(margin.len()),
    )
}

/// Fail with msg as a compile error at the provider location.
/// The error's exit code is 1 (compilation phase).
pub fn compilation_error<T>(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    msg: impl ToString,
) -> UResult<T> {
    Err(USimpleError::new(
        1,
        script_error(
            lines.get_input_name(),
            lines.get_line_number(),
            line.get_pos() + 1,
            str::from_utf8(line.get_line()).ok(),
            msg,
        ),
    ))
}

/// Fail with msg as a compilation error at the command's location.
/// The error's exit code is as specified.
fn location_error<T>(location: &ScriptLocation, msg: impl ToString, exit_code: i32) -> UResult<T> {
    Err(USimpleError::new(
        exit_code,
        script_error(
            &location.input_name,
            location.line_number,
            location.column_number,
            location.line_text.as_deref(),
            msg,
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

#[cfg(test)]
mod tests {
    use super::*;

    // The integration tests check the layout; they cannot get color, as
    // their stderr is not a terminal.
    #[test]
    fn the_underline_is_red_when_colored() {
        let drawn = render_snippet("f.sed", 1, 7, "s/a/b/q", "s/a/b/", true);
        assert_eq!(
            drawn,
            concat!(
                "   ╭─[ f.sed:1:7 ]\n",
                "   │\n",
                " 1 │ s/a/b/q\n",
                "   │       \x1b[31m─\x1b[0m\n",
                "───╯",
            )
        );
    }
}
