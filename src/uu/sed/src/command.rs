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
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf; // For file descriptors and equivalent
use std::rc::Rc;
use uucore::error::UResult;

#[derive(Debug, Default, Clone)]
/// Compilation and processing options provided mostly through the
/// command-line interface
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
    /// Stop processing further input.
    pub stop_processing: bool,
    /// Previously compiled RE, saved for reuse when specifying an empty RE
    pub saved_regex: RefCell<Option<Regex>>,
    /// Modification of input processing action
    // This is required to avoid doubly borrowing the reader in the 'N'
    // command.
    pub input_action: Option<InputAction>,
    /// Hold space
    pub hold: StringSpace,
    /// Nesting of { } at compile time
    pub parsed_block_nesting: usize,
    /// Command associated with each label
    pub label_to_command_map: HashMap<String, Rc<RefCell<Command>>>,
    /// True if a substitution was made as specified in the t command
    pub substitution_made: bool,
    /// Elements to append at the end of each command processing cycle
    pub append_elements: Vec<AppendElement>,
}

#[derive(Clone, Debug)]
/// Elements that shall be appended at the end of each command processing cycle
pub enum AppendElement {
    Text(String),  // The specified text string
    File(PathBuf), // The contents of the specified file
}

#[derive(Clone, Debug, Default, PartialEq)]
/// A space mirroring IOChunk, but only with a String
pub struct StringSpace {
    pub content: String,   // Line content without newline
    pub has_newline: bool, // True if \n-terminated
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
    Regex(Option<Regex>),
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
                    // Compilation guarantees we only get valid group numbers
                    result.push_str(
                        caps.get((*n).try_into().unwrap())
                            .map_or("", |m| m.as_str()),
                    );
                }
            }
        }

        Ok(result)
    }

    /// Returns the highest capture group number referenced in this template.
    pub fn max_group_number(&self) -> u32 {
        self.parts
            .iter()
            .filter_map(|part| {
                if let ReplacementPart::Group(n) = part {
                    Some(*n)
                } else {
                    None
                }
            })
            .max()
            .unwrap_or(0)
    }
}

#[derive(Debug, Default)]
/// Substitution command
pub struct Substitution {
    pub occurrence: usize, // Which occurrence to substitute
    pub print_flag: bool,  // True if 'p' flag
    pub ignore_case: bool, // True if 'I' flag
    pub write_file: Option<Rc<RefCell<NamedWriter>>>, // Writer to file if 'w' flag is used
    pub regex: Option<Regex>, // Regular expression
    pub line_number: usize, // Line number
    pub replacement: ReplacementTemplate, // Specified broken-down replacement
}

/// The block of the first and most common Unicode characters:
/// ASCII, Latin Extended, Greek, Curillic, Coptic, Arabic, etc.
/// It comprises all UCS-2 characters.  We use a fast lookup array for these.
const COMMON_UNICODE: usize = 2048;

#[derive(Debug)]
/// Transliteration command (y)
pub struct Transliteration {
    fast: [char; COMMON_UNICODE],
    slow: HashMap<char, char>,
}

impl Default for Transliteration {
    /// Create a new Transliteration with identity mapping for the fast-path.
    fn default() -> Self {
        let mut fast = ['\0'; COMMON_UNICODE];
        for (i, slot) in fast.iter_mut().enumerate() {
            *slot = char::from_u32(i as u32).unwrap_or('\0');
        }
        Self {
            fast,
            slow: HashMap::new(),
        }
    }
}

impl Transliteration {
    /// Create through character mappings from `source` to `target`.
    pub fn from_strings(source: &str, target: &str) -> Self {
        let mut result = Self::default();
        for (from, to) in source.chars().zip(target.chars()) {
            result.insert(from, to);
        }
        result
    }

    /// Set a transliteration mapping from one character to another.
    fn insert(&mut self, from: char, to: char) {
        let cp = from as usize;
        if cp < COMMON_UNICODE {
            self.fast[cp] = to;
        } else {
            self.slow.insert(from, to);
        }
    }

    /// Look up a character transliteration.
    pub fn lookup(&self, ch: char) -> char {
        let cp = ch as usize;
        if cp < COMMON_UNICODE {
            self.fast[cp]
        } else {
            self.slow.get(&ch).copied().unwrap_or(ch)
        }
    }
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
/// After parsing, t, b Label elements are converted into BranchTarget ones.
pub enum CommandData {
    None,
    Block(Option<Rc<RefCell<Command>>>), // Commands for '{'
    BranchTarget(Option<Rc<RefCell<Command>>>), // Commands for 'b', 't'
    Label(Option<String>),               // Label name for 'b', 't', ':'
    NamedWriter(Box<NamedWriter>),       // File descriptor for 'w'
    Substitution(Box<Substitution>),     // Substitute command 's'
    Text(Cow<'static, str>),             // Text for 'a', 'c', 'i'
    Transliteration(Box<Transliteration>), // Transliteration command 'y'
}

#[derive(Debug)]
/// Text to append before a line is read
pub struct AppendBuffer {
    append_type: AppendType,
    content: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppendType {
    String,
    File,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Flag for space modifications
pub enum SpaceFlag {
    Append,  // Append to contents
    Replace, // Replace contents
}

#[derive(Debug, Clone)]
/// Action to execute after reading a new input line
pub struct InputAction {
    /// Next command to execute (rather than commands from start)
    pub next_command: Option<Rc<RefCell<Command>>>,
    /// Data to prepend to the read contents
    pub prepend: String,
}

#[cfg(test)]
mod tests {
    use super::*;

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

    // max_group_number
    #[test]
    fn test_max_group_number_with_groups() {
        let template = ReplacementTemplate {
            parts: vec![
                ReplacementPart::Literal("a".into()),
                ReplacementPart::Group(2),
                ReplacementPart::WholeMatch,
                ReplacementPart::Group(5),
                ReplacementPart::Literal("z".into()),
            ],
        };
        assert_eq!(template.max_group_number(), 5);
    }

    #[test]
    fn test_max_group_number_without_groups() {
        let template = ReplacementTemplate {
            parts: vec![
                ReplacementPart::Literal("no".into()),
                ReplacementPart::WholeMatch,
                ReplacementPart::Literal("groups".into()),
            ],
        };
        assert_eq!(template.max_group_number(), 0);
    }

    // Transliteration
    // Creation and internal functions
    #[test]
    fn test_identity_lookup_fast_path() {
        let t = Transliteration::default();
        assert_eq!(t.lookup('A'), 'A');
        assert_eq!(t.lookup('z'), 'z');
        assert_eq!(t.lookup('\u{07FF}'), '\u{07FF}'); // highest 2-byte UTF-8 char
    }

    #[test]
    fn test_identity_lookup_slow_path() {
        let t = Transliteration::default();
        assert_eq!(t.lookup('\u{0800}'), '\u{0800}'); // just outside fast path
        assert_eq!(t.lookup('\u{1F600}'), '\u{1F600}'); // 😀
    }

    #[test]
    fn test_insert_and_lookup_fast_path() {
        let mut t = Transliteration::default();
        t.insert('a', 'α');
        t.insert('b', 'β');
        assert_eq!(t.lookup('a'), 'α');
        assert_eq!(t.lookup('b'), 'β');
        assert_eq!(t.lookup('c'), 'c'); // unchanged
    }

    #[test]
    fn test_insert_and_lookup_slow_path() {
        let mut t = Transliteration::default();
        t.insert('🦀', 'c'); // U+1F980 Crab emoji -> 'c'
        assert_eq!(t.lookup('🦀'), 'c');
        assert_eq!(t.lookup('🦁'), '🦁'); // unchanged
    }

    #[test]
    fn test_overwrite_mapping() {
        let mut t = Transliteration::default();
        t.insert('x', '1');
        assert_eq!(t.lookup('x'), '1');
        t.insert('x', '2');
        assert_eq!(t.lookup('x'), '2');
    }

    #[test]
    fn test_all_fast_path_mapped_to_space() {
        let mut t = Transliteration::default();
        for cp in 0..COMMON_UNICODE {
            if let Some(ch) = char::from_u32(cp as u32) {
                t.insert(ch, ' ');
            }
        }
        assert_eq!(t.lookup('A'), ' ');
        assert_eq!(t.lookup('\u{07FF}'), ' ');
    }

    // from_strings
    fn test_basic_transliteration() {
        let t = Transliteration::from_strings("abcδ", "1234");

        assert_eq!(t.lookup('a'), '1');
        assert_eq!(t.lookup('b'), '2');
        assert_eq!(t.lookup('c'), '3');
        assert_eq!(t.lookup('δ'), '4');
        assert_eq!(t.lookup('e'), 'e'); // not mapped, fallback
    }

    #[test]
    fn test_unicode_slow_path() {
        let source = "é漢🦀";
        let target = "e文c";
        let t = Transliteration::from_strings(source, target);

        assert_eq!(t.lookup('é'), 'e');
        assert_eq!(t.lookup('漢'), '文');
        assert_eq!(t.lookup('🦀'), 'c');
        assert_eq!(t.lookup('x'), 'x'); // fast fallback
        assert_eq!(t.lookup('文'), '文'); // slow fallback
    }

    #[test]
    fn test_overwrite_fast_path() {
        let t = Transliteration::from_strings("aa", "12");
        assert_eq!(t.lookup('a'), '2'); // last mapping wins
    }
}
