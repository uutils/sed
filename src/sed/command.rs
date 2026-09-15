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
    pub uutil_extensions: bool,

    // Other context
    /// Currently processed input file name (not script)
    pub input_name: PathBuf,
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
    Upper,            // \U: uppercase until \L or \E (GNU extension)
    Lower,            // \L: lowercase until \U or \E (GNU extension)
    UpperFirst,       // \u: uppercase next character (GNU extension)
    LowerFirst,       // \l: lowercase next character (GNU extension)
    End,              // \E: end \U/\L conversion (GNU extension)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PersistentCase {
    None,
    Upper,
    Lower,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SingleCase {
    None,
    Upper,
    Lower,
}

/// Decode a single UTF-8 character (or invalid byte) at the start of `bytes`.
/// Returns the decoded char (None for invalid) and its byte length (1 for invalid).
fn decode_one_utf8(bytes: &[u8]) -> (Option<char>, usize) {
    if bytes.is_empty() {
        return (None, 0);
    }
    if bytes[0] < 0x80 {
        return (Some(bytes[0] as char), 1);
    }
    let max_len = bytes.len().min(4);
    for len in 2..=max_len {
        if let Ok(s) = std::str::from_utf8(&bytes[..len])
            && let Some(ch) = s.chars().next()
            && ch.len_utf8() == len
        {
            return (Some(ch), len);
        }
    }
    // Invalid or incomplete sequence: consume a single byte like GNU mcel_scan.
    (None, 1)
}

fn push_case_converted_char(
    result: &mut Vec<u8>,
    ch: char,
    orig_bytes: &[u8],
    upper: bool,
) {
    if upper {
        for c in ch.to_uppercase() {
            let mut buf = [0u8; 4];
            result.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
        }
    } else {
        // lower
        let mut wrote = false;
        for c in ch.to_lowercase() {
            let mut buf = [0u8; 4];
            result.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
            wrote = true;
        }
        if !wrote {
            // to_lowercase() can yield nothing for some chars; preserve original.
            result.extend_from_slice(orig_bytes);
        }
    }
}

/// Append `input` to `result` applying GNU `s///` case conversion.
///
/// `persistent` selects the active `\U`/`\L` conversion, `single` holds a
/// pending `\u`/`\l` one-shot conversion for the next produced character.
/// An empty `input` (e.g. an unmatched group) leaves `single` untouched so
/// it carries over to the following text, matching GNU sed. A present
/// character — even one without case (digit, punctuation, space) — consumes
/// `single`. Invalid UTF-8 bytes are passed through and consume `single`.
fn append_with_case(
    result: &mut Vec<u8>,
    input: &[u8],
    persistent: PersistentCase,
    single: &mut SingleCase,
    character_mode: CharacterMode,
) {
    if input.is_empty() {
        return;
    }
    if character_mode == CharacterMode::Byte {
        let mut first = true;
        for &b in input {
            let use_single = if first {
                first = false;
                std::mem::replace(single, SingleCase::None)
            } else {
                SingleCase::None
            };
            let upper = match use_single {
                SingleCase::Upper => Some(true),
                SingleCase::Lower => Some(false),
                SingleCase::None => match persistent {
                    PersistentCase::Upper => Some(true),
                    PersistentCase::Lower => Some(false),
                    PersistentCase::None => None,
                },
            };
            match upper {
                Some(true) => result.push(b.to_ascii_uppercase()),
                Some(false) => result.push(b.to_ascii_lowercase()),
                None => result.push(b),
            }
        }
        return;
    }
    // UTF-8 mode: Unicode-aware conversion with invalid-byte passthrough.
    let mut idx = 0;
    let mut first = true;
    while idx < input.len() {
        let (ch_opt, char_len) = decode_one_utf8(&input[idx..]);
        let use_single = if first {
            first = false;
            std::mem::replace(single, SingleCase::None)
        } else {
            SingleCase::None
        };
        let target_upper: Option<bool> = match use_single {
            SingleCase::Upper => Some(true),
            SingleCase::Lower => Some(false),
            SingleCase::None => match persistent {
                PersistentCase::Upper => Some(true),
                PersistentCase::Lower => Some(false),
                PersistentCase::None => None,
            },
        };
        if let Some(ch) = ch_opt {
            match target_upper {
                Some(upper) => push_case_converted_char(
                    result,
                    ch,
                    &input[idx..idx + char_len],
                    upper,
                ),
                None => result.extend_from_slice(&input[idx..idx + char_len]),
            }
            idx += char_len;
        } else {
            // Invalid byte: pass through (single already consumed above).
            result.push(input[idx]);
            idx += 1;
        }
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
    ///    template.apply_captures(&command, caps, character_mode) });
    /// Returns an error if a backreference in the template was not matched by the RE.
    pub fn apply_captures(
        &self,
        command: &Command,
        caps: &Captures,
        character_mode: CharacterMode,
    ) -> UResult<Vec<u8>> {
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

        // GNU resets case conversion for every substituted occurrence; since
        // this function is invoked once per occurrence, fresh state here
        // provides the required isolation for the `g` flag.
        let mut persistent = PersistentCase::None;
        let mut single = SingleCase::None;

        for part in &self.parts {
            match part {
                ReplacementPart::Literal(s) => {
                    append_with_case(&mut result, s, persistent, &mut single, character_mode);
                }

                ReplacementPart::WholeMatch => {
                    let bytes = caps.get(0)?.map(|m| m.as_bytes()).unwrap_or_default();
                    append_with_case(&mut result, bytes, persistent, &mut single, character_mode);
                }

                ReplacementPart::Group(n) => {
                    let i: usize = (*n).try_into().unwrap();
                    let bytes = caps.get(i)?.map(|m| m.as_bytes()).unwrap_or_default();
                    append_with_case(&mut result, bytes, persistent, &mut single, character_mode);
                }

                ReplacementPart::Upper => {
                    persistent = PersistentCase::Upper;
                    single = SingleCase::None;
                }
                ReplacementPart::Lower => {
                    persistent = PersistentCase::Lower;
                    single = SingleCase::None;
                }
                ReplacementPart::End => {
                    persistent = PersistentCase::None;
                    single = SingleCase::None;
                }
                ReplacementPart::UpperFirst => {
                    single = SingleCase::Upper;
                }
                ReplacementPart::LowerFirst => {
                    single = SingleCase::Lower;
                }
            }
        }

        Ok(result)
    }

    /// Apply the template to the given RE single match.
    pub fn apply_match(&self, m: &Match, character_mode: CharacterMode) -> Vec<u8> {
        let mut result = Vec::new();

        let mut persistent = PersistentCase::None;
        let mut single = SingleCase::None;

        for part in &self.parts {
            match part {
                ReplacementPart::Literal(s) => {
                    append_with_case(&mut result, s, persistent, &mut single, character_mode);
                }

                ReplacementPart::WholeMatch => {
                    append_with_case(&mut result, m.as_bytes(), persistent, &mut single, character_mode);
                }

                ReplacementPart::Group(_) => {
                    panic!("unexpected Regex group replacement")
                }

                ReplacementPart::Upper => {
                    persistent = PersistentCase::Upper;
                    single = SingleCase::None;
                }
                ReplacementPart::Lower => {
                    persistent = PersistentCase::Lower;
                    single = SingleCase::None;
                }
                ReplacementPart::End => {
                    persistent = PersistentCase::None;
                    single = SingleCase::None;
                }
                ReplacementPart::UpperFirst => {
                    single = SingleCase::Upper;
                }
                ReplacementPart::LowerFirst => {
                    single = SingleCase::Lower;
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
    pub p_before_e: bool,                             // True if 'p' appears before 'e'
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
        for (slot, value) in fast.iter_mut().zip(0..=u8::MAX) {
            *slot = value;
        }
        let mut unicode_fast = ['\0'; COMMON_UNICODE];
        for (slot, cp) in unicode_fast.iter_mut().zip(0u32..) {
            *slot = char::from_u32(cp).unwrap_or('\0');
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
/// After parsing, t, T, b Label elements are converted into BranchTarget ones.
pub enum CommandData {
    None,
    BranchTarget(Option<Rc<RefCell<Command>>>), // Commands for 'b', 't', 'T', '{'
    Label(Option<String>),                      // Label name for 'b', 't', 'T', ':'
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

        let result = template.apply_captures(&cmd, &caps, CharacterMode::Utf8).unwrap();
        assert_eq!(result, b"");
    }

    #[test]
    // s/abc/hello/
    fn test_literal_only() {
        let template = ReplacementTemplate::new(vec![ReplacementPart::Literal(b"hello".to_vec())]);
        let input = &mut IOChunk::new_from_str("abc");
        let caps = caps_for("abc", input);
        let cmd = Command::default();

        let result = template.apply_captures(&cmd, &caps, CharacterMode::Utf8).unwrap();
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

        let result = template.apply_captures(&cmd, &caps, CharacterMode::Utf8).unwrap();
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

        assert_eq!(template.apply_match(&m, CharacterMode::Utf8), b"<\xE9x>");
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

        let result = template.apply_captures(&cmd, &caps, CharacterMode::Utf8).unwrap();
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

        let result = template.apply_captures(&cmd, &caps, CharacterMode::Utf8).unwrap();
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

        let result = template.apply_captures(&cmd, &caps, CharacterMode::Utf8);
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

    #[test]
    fn test_max_group_number_ignores_case_directives() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Upper,
            ReplacementPart::Literal(b"a".to_vec()),
            ReplacementPart::LowerFirst,
            ReplacementPart::Group(2),
            ReplacementPart::End,
        ]);
        assert_eq!(template.max_group_number, 2);
    }

    fn apply_literal(parts: Vec<ReplacementPart>, mode: CharacterMode) -> Vec<u8> {
        let template = ReplacementTemplate::new(parts);
        let input = &mut IOChunk::new_from_str("x");
        let caps = caps_for("x", input);
        let cmd = Command::default();
        template
            .apply_captures(&cmd, &caps, mode)
            .unwrap()
    }

    #[test]
    fn test_case_upper_and_lower_persistent() {
        // \Uabc -> ABC, \LABC -> abc
        let out = apply_literal(
            vec![
                ReplacementPart::Upper,
                ReplacementPart::Literal(b"abc".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"ABC");
        let out = apply_literal(
            vec![
                ReplacementPart::Lower,
                ReplacementPart::Literal(b"ABC".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"abc");
    }

    #[test]
    fn test_case_single_shot() {
        // \uabc -> Abc, \lABC -> aBC; non-letters consume the directive.
        let out = apply_literal(
            vec![
                ReplacementPart::UpperFirst,
                ReplacementPart::Literal(b"abc".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"Abc");
        let out = apply_literal(
            vec![
                ReplacementPart::LowerFirst,
                ReplacementPart::Literal(b"ABC".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"aBC");
        let out = apply_literal(
            vec![
                ReplacementPart::UpperFirst,
                ReplacementPart::Literal(b"123abc".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"123abc");
    }

    #[test]
    fn test_case_end_and_switch() {
        // \Uabc\Edef -> ABCdef, \Uabc\Ldef -> ABCdef, \L\uABC -> Abc
        let out = apply_literal(
            vec![
                ReplacementPart::Upper,
                ReplacementPart::Literal(b"abc".to_vec()),
                ReplacementPart::End,
                ReplacementPart::Literal(b"def".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"ABCdef");
        let out = apply_literal(
            vec![
                ReplacementPart::Lower,
                ReplacementPart::UpperFirst,
                ReplacementPart::Literal(b"ABC".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"Abc");
        let out = apply_literal(
            vec![
                ReplacementPart::UpperFirst,
                ReplacementPart::End,
                ReplacementPart::Literal(b"abc".to_vec()),
            ],
            CharacterMode::Utf8,
        );
        assert_eq!(out, b"abc");
    }

    #[test]
    fn test_case_applies_to_whole_match() {
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::Upper,
            ReplacementPart::WholeMatch,
        ]);
        let input = &mut IOChunk::new_from_str("aBc DeF");
        let caps = caps_for(".*", input);
        let cmd = Command::default();
        let out = template
            .apply_captures(&cmd, &caps, CharacterMode::Utf8)
            .unwrap();
        assert_eq!(out, b"ABC DEF");
    }

    #[test]
    fn test_case_byte_mode_is_ascii_only() {
        let out = apply_literal(
            vec![
                ReplacementPart::Upper,
                ReplacementPart::Literal(b"abc".to_vec()),
            ],
            CharacterMode::Byte,
        );
        assert_eq!(out, b"ABC");
        // Invalid UTF-8 bytes pass through while consuming a pending \u.
        let template = ReplacementTemplate::new(vec![
            ReplacementPart::UpperFirst,
            ReplacementPart::Literal(b"\xFFabc".to_vec()),
        ]);
        let input = &mut IOChunk::new_from_str("x");
        let caps = caps_for("x", input);
        let cmd = Command::default();
        let out = template
            .apply_captures(&cmd, &caps, CharacterMode::Utf8)
            .unwrap();
        assert_eq!(out, b"\xFFabc");
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
            if let Some(ch) = u32::try_from(cp).ok().and_then(char::from_u32) {
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
