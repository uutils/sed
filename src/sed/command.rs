// Definitions for the compiled code data structures
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::sed::error_handling::{ScriptLocation, runtime_error};
use crate::sed::fast_regex::{Captures, Match, Regex};
use crate::sed::named_writer::NamedWriter;
use crate::sed::script_char_provider::ScriptCharProvider;
use crate::sed::script_line_provider::ScriptLineProvider;

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
    /// Currently processed input file name (not script) in quoted form
    pub input_name: String,
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
    /// Whether sed operates on bytes or UTF-8 characters
    pub character_mode: CharacterMode,
    /// Previously compiled RE, saved for reuse when specifying an empty RE
    pub saved_regex: Option<Regex>,
    /// Modification of input processing action
    // This is required to avoid doubly borrowing the reader in the 'N'
    // command.
    pub input_action: Option<InputAction>,
    /// Hold space
    pub hold: ByteSpace,
    /// Nesting of { } at compile time
    pub parsed_block_nesting: usize,
    /// Command associated with each label
    pub label_to_command_map: HashMap<String, Rc<RefCell<Command>>>,
    /// Commands with a (latchable and resetable) address range
    pub range_commands: Vec<Rc<RefCell<Command>>>,
    /// True if a substitution was made as specified in the t command
    pub substitution_made: bool,
    /// Elements to append at the end of each command processing cycle
    pub append_elements: Vec<AppendElement>,
}

#[derive(Clone, Debug)]
/// Elements that shall be appended at the end of each command processing cycle
pub enum AppendElement {
    Text(Rc<[u8]>), // The specified text bytes
    Path(PathBuf),  // The contents of the specified file path
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
/// Whether sed operates on bytes or UTF-8 characters
pub enum CharacterMode {
    Byte, // Interpret data as arbitrary bytes (C/POSIX locale).
    #[default]
    Utf8, // Interpret data as UTF-8 characters (UTF-8 locale).
}

#[derive(Clone, Debug, Default, PartialEq)]
/// A space mirroring IOChunk without mmap-backed storage.
pub struct ByteSpace {
    pub content: Vec<u8>,  // Line content without newline
    pub has_newline: bool, // True if \n-terminated
}

#[derive(Debug)]
/// Types of address specifications that precede commands
pub enum Address {
    Re(Option<Regex>), // Line that matches (optional) regex
    Line(usize),       // Specific line
    RelLine(usize),    // Relative line
    Last,              // Last line
    StepMatch(usize),  // Lines matching specified step from first
    StepEnd(usize),    // Range ending at specified step from first
}

#[derive(Debug)]
/// A single part of an RE replacement
pub enum ReplacementPart {
    Literal(Vec<u8>), // Normal text
    WholeMatch,       // &
    Group(u32),       // \1 to \9
}

// The maximum value allowed in regex quantifier
pub const RE_DUP_MAX: usize = 32767;

/// Regex modes (BRE or ERE)
#[derive(Copy, Clone, Debug)]
pub enum RegexMode {
    Basic,
    Extended,
}

#[derive(Debug)]
/// All specified replacements for an RE
pub struct ReplacementTemplate {
    pub parts: Vec<ReplacementPart>,
    pub max_group_number: usize, // Highest used group number (e.g. 8 for \8)
}

impl Default for ReplacementTemplate {
    /// Create an empty template.
    fn default() -> Self {
        ReplacementTemplate::new(Vec::new())
    }
}

impl ReplacementTemplate {
    /// Construct from the parts
    pub fn new(parts: Vec<ReplacementPart>) -> Self {
        let max_group_number = parts
            .iter()
            .filter_map(|part| match part {
                ReplacementPart::Group(n) => Some(*n),
                _ => None,
            })
            .max()
            .unwrap_or(0);

        Self {
            parts,
            max_group_number: max_group_number.try_into().unwrap(),
        }
    }

    /// Apply the template to the given RE captures.
    /// Example:
    /// let result = regex.replace_all(input, |caps: &Captures| {
    ///    template.apply_captures(&command, caps) });
    /// Returns an error if a backreference in the template was not matched by the RE.
    pub fn apply_captures(&self, command: &Command, caps: &Captures) -> UResult<Vec<u8>> {
        let mut result = Vec::new();

        // Invalid group numbers may end here through (unkown at compile time)
        // reused REs.
        if self.max_group_number > caps.len() - 1 {
            return runtime_error(
                &command.location,
                format!(
                    "invalid reference \\{} on command's RHS",
                    self.max_group_number
                ),
            );
        }

        for part in &self.parts {
            match part {
                ReplacementPart::Literal(s) => result.extend_from_slice(s),

                ReplacementPart::WholeMatch => {
                    result
                        .extend_from_slice(caps.get(0)?.map(|m| m.as_bytes()).unwrap_or_default());
                }

                ReplacementPart::Group(n) => {
                    let i: usize = (*n).try_into().unwrap();
                    result
                        .extend_from_slice(caps.get(i)?.map(|m| m.as_bytes()).unwrap_or_default());
                }
            }
        }

        Ok(result)
    }

    /// Apply the template to the given RE single match.
    pub fn apply_match(&self, m: &Match) -> Vec<u8> {
        let mut result = Vec::new();

        for part in &self.parts {
            match part {
                ReplacementPart::Literal(s) => result.extend_from_slice(s),

                ReplacementPart::WholeMatch => result.extend_from_slice(m.as_bytes()),

                ReplacementPart::Group(_) => {
                    panic!("unexpected Regex group replacement")
                }
            }
        }
        result
    }
}

#[derive(Debug, Default)]
/// Substitution command
pub struct Substitution {
    pub regex: Option<Regex>,                         // Regular expression
    pub replacement: ReplacementTemplate,             // Specified broken-down replacement
    pub occurrence: usize,                            // Which occurrence to substitute
    pub print_flag: bool,                             // True if 'p' flag
    pub ignore_case: bool,                            // True if 'I' flag
    pub execute: bool,                                // True if 'e' flag (GNU extension)
    pub multiline: bool,                              // True if 'm' or 'M' flag (GNU extension)
    pub write_file: Option<Rc<RefCell<NamedWriter>>>, // Writer to file if 'w' flag is used
}

#[derive(Debug, PartialEq, Eq)]
/// Result of parsing a transliteration string in byte or character mode.
pub enum ParsedTransliteration {
    Bytes(Vec<u8>),
    Text(String),
}

/// The block of the first and most common Unicode characters:
/// ASCII, Latin Extended, Greek, Curillic, Coptic, Arabic, etc.
/// It comprises all UCS-2 characters.  We use a fast lookup array for these.
const COMMON_UNICODE: usize = 2048;

#[derive(Debug)]
/// Transliteration command (y)
pub struct Transliteration {
    byte_fast: [u8; 256],
    unicode_fast: [char; COMMON_UNICODE],
    unicode_slow: HashMap<char, char>,
    pub(crate) is_byte_identity: bool,
}

impl Default for Transliteration {
    /// Create a new Transliteration with identity mapping for the fast-path.
    fn default() -> Self {
        let mut fast = [0u8; 256];
        for (i, slot) in fast.iter_mut().enumerate() {
            *slot = i as u8;
        }
        let mut unicode_fast = ['\0'; COMMON_UNICODE];
        for (i, slot) in unicode_fast.iter_mut().enumerate() {
            *slot = char::from_u32(i as u32).unwrap_or('\0');
        }
        Self {
            byte_fast: fast,
            unicode_fast,
            unicode_slow: HashMap::new(),
            is_byte_identity: true,
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

    /// Create through byte mappings from `source` to `target`.
    pub fn from_bytes(source: &[u8], target: &[u8]) -> Self {
        let mut result = Self::default();
        for (&from, &to) in source.iter().zip(target) {
            result.byte_fast[from as usize] = to;
        }
        result
    }

    /// Set a transliteration mapping from one character to another.
    fn insert(&mut self, from: char, to: char) {
        let cp = from as usize;
        if cp < COMMON_UNICODE {
            self.unicode_fast[cp] = to;
        } else {
            self.unicode_slow.insert(from, to);
        }
        if from.is_ascii() && to.is_ascii() {
            self.byte_fast[from as usize] = to as u8;
        } else {
            self.is_byte_identity = false;
        }
    }

    /// Look up a character transliteration.
    pub fn lookup_char(&self, ch: char) -> char {
        let cp = ch as usize;
        if cp < COMMON_UNICODE {
            self.unicode_fast[cp]
        } else {
            self.unicode_slow.get(&ch).copied().unwrap_or(ch)
        }
    }

    /// Fast byte lookup for pure ASCII transliterations.
    pub fn lookup_byte(&self, byte: u8) -> u8 {
        self.byte_fast[byte as usize]
    }
}

#[derive(Debug)]
/// An internally compiled command.
pub struct Command {
    pub code: char,                         // Command code
    pub addr1: Option<Address>,             // Start address
    pub addr2: Option<Address>,             // End address
    pub non_select: bool,                   // True if '!'
    pub start_line: Option<usize>,          // Start line number (or None if unlatched)
    pub data: CommandData,                  // Command-specific data
    pub next: Option<Rc<RefCell<Command>>>, // Pointer to next command
    pub location: ScriptLocation,           // Command's definition location
}

impl Default for Command {
    fn default() -> Self {
        Command {
            code: '_',
            addr1: None,
            addr2: None,
            non_select: false,
            start_line: None,
            data: CommandData::None,
            next: None,
            location: ScriptLocation::default(),
        }
    }
}

impl Command {
    /// Construct with position information from the given providers.
    pub fn at_position(lines: &ScriptLineProvider, line: &ScriptCharProvider) -> Self {
        Command {
            location: ScriptLocation::at_position(lines, line),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
/// Command-specific data
/// After parsing, t, b Label elements are converted into BranchTarget ones.
pub enum CommandData {
    None,
    BranchTarget(Option<Rc<RefCell<Command>>>), // Commands for 'b', 't', '{'
    Label(Option<String>),                      // Label name for 'b', 't', ':'
    Path(PathBuf),                              // File path for 'r'
    NamedWriter(Rc<RefCell<NamedWriter>>),      // File output for 'w'
    Number(usize),                              // Number for 'l', 'q', 'Q' (GNU)
    Substitution(Box<Substitution>),            // Substitute command 's'
    Text(Rc<[u8]>),                             // Text for 'a', 'c', 'i'
    Transliteration(Box<Transliteration>),      // Transliteration command 'y'
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
    pub prepend: Vec<u8>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sed::fast_io::IOChunk;

    // Return the captures for the RE applied to the specified string
    fn caps_for<'a>(re: &str, chunk: &'a mut IOChunk) -> Captures<'a> {
        Regex::new(re, CharacterMode::Utf8)
            .unwrap()
            .captures(chunk)
            .unwrap()
            .expect("captures")
    }

    #[test]
    // s/foo//
    fn test_empty_template() {
        let template = ReplacementTemplate::default();
        let input = &mut IOChunk::new_from_str("foo");
        let caps = caps_for("foo", input);
        let cmd = Command::default();

        let result = template.apply_captures(&cmd, &caps).unwrap();
        assert_eq!(result, b"");
    }

    #[test]
    // s/abc/hello/
    fn test_literal_only() {
        let template = ReplacementTemplate::new(vec![ReplacementPart::Literal(b"hello".to_vec())]);
        let input = &mut IOChunk::new_from_str("abc");
        let caps = caps_for("abc", input);
        let cmd = Command::default();

        let result = template.apply_captures(&cmd, &caps).unwrap();
        assert_eq!(result, b"hello");
    }

    #[test]
    // s/foo\d+/got: &/
    fn test_whole_match() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Literal(b"got: ".to_vec()),
            ReplacementPart::WholeMatch,
        ]);
        let input = &mut IOChunk::new_from_str("foo42");
        let caps = caps_for(r"foo\d+", input);
        let cmd = Command::default();

        let result = template.apply_captures(&cmd, &caps).unwrap();
        assert_eq!(result, b"got: foo42");
    }

    #[test]
    fn test_apply_match_uses_matched_bytes() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Literal(b"<".to_vec()),
            ReplacementPart::WholeMatch,
            ReplacementPart::Literal(b">".to_vec()),
        ]);
        let m = Match::from_bytes(1, 3, b"\xE9x");

        assert_eq!(template.apply_match(&m), b"<\xE9x>");
    }

    #[test]
    // s/foo(\d+)/number: \1/
    fn test_backreference() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Literal(b"number: ".to_vec()),
            ReplacementPart::Group(1),
        ]);
        let input = &mut IOChunk::new_from_str("foo42");
        let caps = caps_for(r"foo(\d+)", input);
        let cmd = Command::default();

        let result = template.apply_captures(&cmd, &caps).unwrap();
        assert_eq!(result, b"number: 42");
    }

    #[test]
    // s/(\w+):(\d+)/key: \1, value: \2/
    fn test_multiple_parts() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Literal(b"key: ".to_vec()),
            ReplacementPart::Group(1),
            ReplacementPart::Literal(b", value: ".to_vec()),
            ReplacementPart::Group(2),
        ]);
        let input = &mut IOChunk::new_from_str("x:123");
        let caps = caps_for(r"(\w+):(\d+)", input);
        let cmd = Command::default();

        let result = template.apply_captures(&cmd, &caps).unwrap();
        assert_eq!(result, b"key: x, value: 123");
    }

    #[test]
    // s/(\w+):(\d+)/key: \1, value: \3/
    fn test_invalid_group() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Literal(b"key: ".to_vec()),
            ReplacementPart::Group(1),
            ReplacementPart::Literal(b", value: ".to_vec()),
            ReplacementPart::Group(3),
        ]);
        let input = &mut IOChunk::new_from_str("x:123");
        let caps = caps_for(r"(\w+):(\d+)", input);
        let cmd = Command::default();

        let result = template.apply_captures(&cmd, &caps);
        assert!(result.is_err());

        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("invalid reference \\3"));
    }

    // max_group_number
    #[test]
    fn test_max_group_number_with_groups() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Literal(b"a".to_vec()),
            ReplacementPart::Group(2),
            ReplacementPart::WholeMatch,
            ReplacementPart::Group(5),
            ReplacementPart::Literal(b"z".to_vec()),
        ]);
        assert_eq!(template.max_group_number, 5);
    }

    #[test]
    fn test_max_group_number_without_groups() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Literal(b"no".to_vec()),
            ReplacementPart::WholeMatch,
            ReplacementPart::Literal(b"groups".to_vec()),
        ]);
        assert_eq!(template.max_group_number, 0);
    }

    // Transliteration
    // Creation and internal functions
    #[test]
    fn test_identity_lookup_fast_path() {
        let t = Transliteration::default();
        assert_eq!(t.lookup_char('A'), 'A');
        assert_eq!(t.lookup_char('z'), 'z');
        assert_eq!(t.lookup_char('\u{07FF}'), '\u{07FF}'); // highest 2-byte UTF-8 char
    }

    #[test]
    fn test_identity_lookup_slow_path() {
        let t = Transliteration::default();
        assert_eq!(t.lookup_char('\u{0800}'), '\u{0800}'); // just outside fast path
        assert_eq!(t.lookup_char('\u{1F600}'), '\u{1F600}'); // 😀
    }

    #[test]
    fn test_from_bytes_and_lookup_byte() {
        let t = Transliteration::from_bytes(b"a\xE9", b"Z!");
        assert_eq!(t.lookup_byte(b'a'), b'Z');
        assert_eq!(t.lookup_byte(0xE9), b'!');
        assert_eq!(t.lookup_byte(b'b'), b'b');
    }

    #[test]
    fn test_is_byte_identity_tracks_non_ascii_character_mappings() {
        assert!(Transliteration::from_strings("ab", "xy").is_byte_identity);
        assert!(!Transliteration::from_strings("aé", "xy").is_byte_identity);
        assert!(!Transliteration::from_strings("ab", "xé").is_byte_identity);
    }

    #[test]
    fn test_insert_and_lookup_fast_path() {
        let mut t = Transliteration::default();
        t.insert('a', 'α');
        t.insert('b', 'β');
        assert_eq!(t.lookup_char('a'), 'α');
        assert_eq!(t.lookup_char('b'), 'β');
        assert_eq!(t.lookup_char('c'), 'c'); // unchanged
    }

    #[test]
    fn test_insert_and_lookup_slow_path() {
        let mut t = Transliteration::default();
        t.insert('🦀', 'c'); // U+1F980 Crab emoji -> 'c'
        assert_eq!(t.lookup_char('🦀'), 'c');
        assert_eq!(t.lookup_char('🦁'), '🦁'); // unchanged
    }

    #[test]
    fn test_overwrite_mapping() {
        let mut t = Transliteration::default();
        t.insert('x', '1');
        assert_eq!(t.lookup_char('x'), '1');
        t.insert('x', '2');
        assert_eq!(t.lookup_char('x'), '2');
    }

    #[test]
    fn test_all_fast_path_mapped_to_space() {
        let mut t = Transliteration::default();
        for cp in 0..COMMON_UNICODE {
            if let Some(ch) = char::from_u32(cp as u32) {
                t.insert(ch, ' ');
            }
        }
        assert_eq!(t.lookup_char('A'), ' ');
        assert_eq!(t.lookup_char('\u{07FF}'), ' ');
    }

    // from_strings
    #[test]
    fn test_basic_transliteration() {
        let t = Transliteration::from_strings("abcδ", "1234");

        assert_eq!(t.lookup_char('a'), '1');
        assert_eq!(t.lookup_char('b'), '2');
        assert_eq!(t.lookup_char('c'), '3');
        assert_eq!(t.lookup_char('δ'), '4');
        assert_eq!(t.lookup_char('e'), 'e'); // not mapped, fallback
    }

    #[test]
    fn test_unicode_slow_path() {
        let source = "é漢🦀";
        let target = "e文c";
        let t = Transliteration::from_strings(source, target);

        assert_eq!(t.lookup_char('é'), 'e');
        assert_eq!(t.lookup_char('漢'), '文');
        assert_eq!(t.lookup_char('🦀'), 'c');
        assert_eq!(t.lookup_char('x'), 'x'); // fast fallback
        assert_eq!(t.lookup_char('文'), '文'); // slow fallback
    }

    #[test]
    fn test_overwrite_fast_path() {
        let t = Transliteration::from_strings("aa", "12");
        assert_eq!(t.lookup_char('a'), '2'); // last mapping wins
    }
}
