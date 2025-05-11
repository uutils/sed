// Definitions for the compiled code data structures
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

// TODO: remove when compile is implemented
#![allow(dead_code)]

use crate::named_writer::NamedWriter;

use regex::Captures;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf; // For file descriptors and equivalent
use std::rc::Rc;
use uucore::error::{UResult, USimpleError};

// Compilation and processing options provided mostly through the
// command-line interface
#[derive(Debug, Default, Clone)]
pub struct ProcessingContext {
    // Command-line flags with corresponding names
    pub all_output_files: bool,
    pub debug: bool,
    pub regex_extended: bool,
    pub follow_symlinks: bool,
    pub in_place: bool,
    pub in_place_suffix: Option<String>,
    pub length: usize,
    pub quiet: bool,
    pub posix: bool,
    pub separate: bool,
    pub sandbox: bool,
    pub unbuffered: bool,
    pub null_data: bool,

    // Other context
    /// Current input line number
    pub line_number: usize,
    /// True if this is the last address of a range
    pub last_address: bool,
    /// True if the line read is the last line
    pub last_line: bool,
    /// True if the file is the last file of the ones specified
    pub last_file: bool,
}

#[derive(Debug, PartialEq)]
/// The specification of a script: through a string or a file
pub enum ScriptValue {
    StringVal(String),
    PathVal(PathBuf),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Types of address specifications that precede commands
pub enum AddressType {
    Re,      // Line that matches regex
    Line,    // Specific line
    RelLine, // Relative line
    Last,    // Last line
}

#[derive(Debug)]
/// Format of an address
pub struct Address {
    pub atype: AddressType,  // Address type
    pub value: AddressValue, // Line number or regex
}

#[derive(Debug)]
pub enum AddressValue {
    LineNumber(usize),
    Regex(Regex),
}

#[derive(Debug)]
/// A single part of an RE replacement
pub enum ReplacementPart {
    Literal(String), // Normal text
    WholeMatch,      // &
    Group(u32),      // \1 to \9
}

#[derive(Debug)]
/// All specified replacements for an RE
pub struct ReplacementTemplate {
    pub parts: Vec<ReplacementPart>,
}

impl Default for ReplacementTemplate {
    /// Create an empty template.
    fn default() -> Self {
        ReplacementTemplate { parts: Vec::new() }
    }
}

impl ReplacementTemplate {
    /// Apply the template to the given RE captures.
    /// Example:
    /// let result = regex.replace_all(input, |caps: &regex::Captures| {
    ///    template.apply(caps) });
    /// Returns an error if a backreference in the template was not matched by the RE.
    pub fn apply(&self, caps: &Captures) -> UResult<String> {
        let mut result = String::new();

        for part in &self.parts {
            match part {
                ReplacementPart::Literal(s) => result.push_str(s),

                ReplacementPart::WholeMatch => {
                    result.push_str(caps.get(0).map_or("", |m| m.as_str()));
                }

                ReplacementPart::Group(n) => {
                    let group_index = *n as usize;
                    if group_index >= caps.len() {
                        return Err(USimpleError::new(
                            2,
                            // TODO: Provide code location info
                            format!("\\{} not defined in the regular expression", n),
                        ));
                    }

                    result.push_str(caps.get(group_index).map_or("", |m| m.as_str()));
                }
            }
        }

        Ok(result)
    }
}

#[derive(Debug)]
/// Substitution command
pub struct Substitution {
    pub occurrence: usize, // Which occurrence to substitute
    pub print_flag: bool,  // True if 'p' flag
    pub ignore_case: bool, // True if 'I' flag
    pub write_file: Option<Rc<RefCell<NamedWriter>>>, // Writer to file if 'w' flag is used
    pub regex: Regex,      // Regular expression
    pub line_number: usize, // Line number
    pub replacement: ReplacementTemplate, // Specified broken-down replacement
}

impl Default for Substitution {
    fn default() -> Self {
        Substitution {
            occurrence: 1,
            print_flag: false,
            ignore_case: false,
            write_file: None,
            regex: Regex::new("").unwrap(), // safe dummy regex
            line_number: 0,
            replacement: ReplacementTemplate::default(),
        }
    }
}

#[derive(Debug)]
/// Transliteration command (y)
pub struct Transliteration {
    pub byte_table: [u8; 256],          // Byte translation table
    pub multi_map: HashMap<char, char>, // Direct mapping from one char to another
}

#[derive(Debug)]
/// An internally compiled command.
pub struct Command {
    pub code: char,                         // Command code
    pub addr1: Option<Address>,             // Start address
    pub addr2: Option<Address>,             // End address
    pub non_select: bool,                   // True if '!'
    pub start_line: Option<usize>,          // Start line number (or None)
    pub text: Option<String>,               // Text for ':', 'a', 'c', 'i', 'r', 'w'
    pub data: CommandData,                  // Command-specific data
    pub next: Option<Rc<RefCell<Command>>>, // Pointer to next command
}

impl Default for Command {
    fn default() -> Self {
        Command {
            code: '_',
            addr1: None,
            addr2: None,
            non_select: false,
            start_line: None,
            text: None,
            data: CommandData::None,
            next: None,
        }
    }
}

#[derive(Debug)]
/// Command-specific data
pub enum CommandData {
    None,
    Subcommand(Rc<RefCell<Command>>), // Commands for 'b', 't', '{'
    Substitution(Box<Substitution>),  // Substitute command 's'
    Transliteration(Box<Transliteration>), // Transliteration command 'y'
    NamedWriter(Box<NamedWriter>),    // File descriptor for 'w'
}

impl CommandData {
    pub fn get_subcommand(&self) -> Rc<RefCell<Command>> {
        match self {
            CommandData::Subcommand(rc) => Rc::clone(rc),
            _ => panic!("Called get on non-Subcommand variant"),
        }
    }
}

/*
 * Structure containing things to append before a line is read
 */
#[derive(Debug)]
pub struct AppendBuffer {
    append_type: AppendType,
    content: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppendType {
    String,
    File,
}

/// Flag for space modifications
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceFlag {
    Append,  // Append to contents
    Replace, // Replace contents
}

/// Structure for a processing space (process, hold, otherwise).
#[derive(Debug)]
pub struct Space {
    pub current: String,      // Current space content
    pub deleted: bool,        // Whether content was deleted
    pub append_newline: bool, // Whether originally terminated by \n
    pub backup: String,       // Backing memory
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;

    // Return the captures for the RE applied to the specified string
    fn caps_for<'a>(re: &str, input: &'a str) -> regex::Captures<'a> {
        Regex::new(re).unwrap().captures(input).unwrap()
    }

    #[test]
    // s/foo//
    fn test_empty_template() {
        let template = ReplacementTemplate::default();
        let caps = caps_for("foo", "foo");

        let result = template.apply(&caps).unwrap();
        assert_eq!(result, "");
    }

    #[test]
    // s/abc/hello/
    fn test_literal_only() {
        let template = ReplacementTemplate {
            parts: vec![ReplacementPart::Literal("hello".into())],
        };
        let caps = caps_for("abc", "abc");

        let result = template.apply(&caps).unwrap();
        assert_eq!(result, "hello");
    }

    #[test]
    // s/foo\d+/got: &/
    fn test_whole_match() {
        let template = ReplacementTemplate {
            parts: vec![
                ReplacementPart::Literal("got: ".into()),
                ReplacementPart::WholeMatch,
            ],
        };
        let caps = caps_for(r"foo\d+", "foo42");

        let result = template.apply(&caps).unwrap();
        assert_eq!(result, "got: foo42");
    }

    #[test]
    // s/foo(\d+)/number: \1/
    fn test_backreference() {
        let template = ReplacementTemplate {
            parts: vec![
                ReplacementPart::Literal("number: ".into()),
                ReplacementPart::Group(1),
            ],
        };
        let caps = caps_for(r"foo(\d+)", "foo42");

        let result = template.apply(&caps).unwrap();
        assert_eq!(result, "number: 42");
    }

    #[test]
    // s/(\w+):(\d+)/key: \1, value: \2/
    fn test_multiple_parts() {
        let template = ReplacementTemplate {
            parts: vec![
                ReplacementPart::Literal("key: ".into()),
                ReplacementPart::Group(1),
                ReplacementPart::Literal(", value: ".into()),
                ReplacementPart::Group(2),
            ],
        };
        let caps = caps_for(r"(\w+):(\d+)", "x:123");

        let result = template.apply(&caps).unwrap();
        assert_eq!(result, "key: x, value: 123");
    }

    #[test]
    // s/(a)(b)/\3/
    fn test_invalid_backreference() {
        let template = ReplacementTemplate {
            parts: vec![ReplacementPart::Group(3)],
        };
        let caps = caps_for(r"(a)(b)", "ab"); // only groups 1 and 2 exist

        let err = template.apply(&caps).unwrap_err();
        assert!(err.to_string().contains(r"\3 not defined"));
    }
}
