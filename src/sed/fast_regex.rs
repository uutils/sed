// A unified interface to byte and fancy Regex
//
// This allows using byte Regex when possible, resorting to the
// slower fancy_regex crate when needed.
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use fancy_regex::{
    CaptureMatches as FancyCaptureMatches, Captures as FancyCaptures, Regex as FancyRegex,
};
use memchr::memmem;
use regex::bytes::{
    CaptureMatches as ByteCaptureMatches, Captures as ByteCaptures, Regex as ByteRegex,
};
use std::sync::LazyLock;
use uucore::error::{UResult, USimpleError};

use crate::sed::command::CharacterMode;
use crate::sed::fast_io::IOChunk;

/// Convert raw pattern bytes into a valid String pattern for regex::bytes.
fn byte_regex_pattern(pattern: &[u8]) -> String {
    let mut out = String::with_capacity(pattern.len());
    for &byte in pattern {
        if byte > 0x7F {
            // This might result in an invalid Unicode character
            // so encode it as a hex escape.
            out.push_str(&format!(r"\x{byte:02X}"));
        } else {
            out.push(byte as char);
        }
    }
    out
}

/// Map each byte to the Unicode scalar with the same value.
///
/// `fancy_regex` only accepts UTF-8 strings. Using a one-scalar-per-byte
/// representation lets its back-reference engine operate in byte mode while
/// keeping byte identity intact, including for invalid UTF-8 input.
fn byte_fancy_text(bytes: &[u8]) -> (String, Vec<usize>) {
    let mut text = String::with_capacity(bytes.len());
    let mut byte_offsets = Vec::with_capacity(bytes.len() + 1);
    byte_offsets.push(0);

    for (index, &byte) in bytes.iter().enumerate() {
        let character = char::from(byte);
        text.push(character);
        for _ in 1..character.len_utf8() {
            byte_offsets.push(index);
        }
        byte_offsets.push(index + 1);
    }

    (text, byte_offsets)
}

fn mapped_fancy_match<'t>(
    matched: fancy_regex::Match<'_>,
    haystack: &'t [u8],
    byte_offsets: &[usize],
) -> Match<'t> {
    let start = byte_offsets[matched.start()];
    let end = byte_offsets[matched.end()];
    Match::from_bytes(start, end, &haystack[start..end])
}

fn mapped_fancy_captures<'t>(
    captures: &FancyCaptures<'_>,
    haystack: &'t [u8],
    byte_offsets: &[usize],
) -> Captures<'t> {
    Captures::MappedByte(
        (0..captures.len())
            .map(|index| {
                captures
                    .get(index)
                    .map(|matched| mapped_fancy_match(matched, haystack, byte_offsets))
            })
            .collect(),
    )
}

/// REs requiring the fancy_regex capabilities rather than the
/// faster regex::bytes engine.
// False positives only result in a small performance pessimization,
// so this is just a maximally sensitive, good-enough approximation.
// For example, r"\\1" and r"[\1]" will match, whereas only a number
// after an odd number of backslashes and outside a character class
// should match.
static NEEDS_FANCY_RE: LazyLock<ByteRegex> = LazyLock::new(|| ByteRegex::new(r"\\[1-9]").unwrap());

/// All characters signifying that the match must be handled by an RE
/// rather than by plain string pattern matching.
// These do not include the ^$ metacharacters, which we can easily handle.
// Plain string fixed-string matching is currently faster than Regex
// matching, because Regex always constructs an automaton and needs
// to handle state transitions, whereas plain string matching can
// use tailored CPU string or vectored instructions.
pub(crate) static NEEDS_RE: LazyLock<ByteRegex> = LazyLock::new(|| {
    ByteRegex::new(
        r"(?x) # Turn on verbose mode
          ( ^                   # Non-escaped: i.e. at BOL
             | ^[^\\]            # or after a BOL non \
             | [^\\] {2}         # or after two non \ characters
             | \\.               # or after a consumed or escaped \
           )
           (                     # A potentially incompatible match
              [.?|+(\[{*]        # Any magic RE character
                                 # Some are operators so illegal at
                                 # BOL but they should error there,
                                 # not use them as literals.
             | \\[WwDdSsPp]      # Unicode classes
             | \\[AzBb]          # Empty matches
             | \\[0-9]           # Back-references
           )
        ",
    )
    .unwrap()
});

#[derive(Clone, Debug)]
/// Types of literal string anchored matches
enum AnchoredMatch {
    Begin, // ^...
    End,   // ...$
    Both,  // ^...$
    Free,  // ...
}

#[derive(Clone, Debug)]
/// A fast Regex-like matcher for literal strings using memchr:memmem
pub struct LiteralMatcher {
    needle: Vec<u8>,           // Bytes without any anchors
    match_type: AnchoredMatch, // Type of anchoring specified
}

impl LiteralMatcher {
    /// Construct a new matcher based on a needle possible with anchors.
    pub fn new(needle: impl AsRef<[u8]>) -> Self {
        let needle_bytes = needle.as_ref();
        if needle_bytes[0] == b'^' && needle_bytes[needle_bytes.len() - 1] == b'$' {
            LiteralMatcher {
                match_type: AnchoredMatch::Both,
                needle: needle_bytes[1..needle_bytes.len() - 1].to_vec(),
            }
        } else if needle_bytes[0] == b'^' {
            LiteralMatcher {
                match_type: AnchoredMatch::Begin,
                needle: needle_bytes[1..needle_bytes.len()].to_vec(),
            }
        } else if needle_bytes[needle_bytes.len() - 1] == b'$' {
            LiteralMatcher {
                match_type: AnchoredMatch::End,
                needle: needle_bytes[0..needle_bytes.len() - 1].to_vec(),
            }
        } else {
            LiteralMatcher {
                match_type: AnchoredMatch::Free,
                needle: needle_bytes.to_vec(),
            }
        }
    }

    /// Returns the start index of a match, if any
    fn anchored_find(&self, haystack: &[u8]) -> Option<usize> {
        let nlen = self.needle.len();
        let hlen = haystack.len();

        match self.match_type {
            AnchoredMatch::Both => {
                if hlen == nlen && haystack == self.needle.as_slice() {
                    Some(0)
                } else {
                    None
                }
            }
            AnchoredMatch::Begin => {
                if hlen >= nlen && &haystack[..nlen] == self.needle.as_slice() {
                    Some(0)
                } else {
                    None
                }
            }
            AnchoredMatch::End => {
                if hlen >= nlen && &haystack[hlen - nlen..] == self.needle.as_slice() {
                    Some(hlen - nlen)
                } else {
                    None
                }
            }
            AnchoredMatch::Free => memmem::find(haystack, &self.needle),
        }
    }

    /// Return true if the needle occurs in the haystack.
    pub fn is_match(&self, haystack: &[u8]) -> bool {
        self.anchored_find(haystack).is_some()
    }

    /// Return the position and contents of the matched needle.
    pub fn find<'t>(&self, haystack: &'t [u8]) -> Option<(usize, usize, &'t [u8])> {
        self.anchored_find(haystack).map(|start| {
            let end = start + self.needle.len();
            (start, end, &haystack[start..end])
        })
    }

    /// Return all positions and contents of the matched needle.
    pub fn iter<'t>(
        &'t self,
        haystack: &'t [u8],
    ) -> Box<dyn Iterator<Item = (usize, usize, &'t [u8])> + 't> {
        let needle = &self.needle;
        let nlen = needle.len();

        match self.match_type {
            AnchoredMatch::Both | AnchoredMatch::Begin | AnchoredMatch::End => {
                // At most one match; yield it if present
                Box::new(self.find(haystack).into_iter())
            }
            AnchoredMatch::Free => {
                // Multiple potential matches
                Box::new(memmem::find_iter(haystack, needle).map(move |start| {
                    let end = start + nlen;
                    (start, end, &haystack[start..end])
                }))
            }
        }
    }
}

/// Return the passed pattern without any backslash escapes.
pub fn remove_escapes(pattern: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(pattern.len());
    let mut pos = 0;

    while pos < pattern.len() {
        let byte = pattern[pos];
        pos += 1;
        if byte == b'\\' {
            if let Some(&next) = pattern.get(pos) {
                result.push(next);
                pos += 1;
            }
        } else {
            result.push(byte);
        }
    }

    result
}

#[derive(Clone, Debug)]
/// A regular expression that can be implemented in diverse efficient ways
pub enum Regex {
    Literal(LiteralMatcher), // Fastest: literal bytes
    Byte(ByteRegex),         // Slower: byte-based RE
    FancyByte(FancyRegex),   // Byte-mapped RE supporting back-references
    Fancy(FancyRegex),       // Slowest: RE supporting UTF-8 and back-references
}

/// Ensure that a regex matches GNU sed's default semantics for `.`
/// through the appropriate use of the s flag.
pub fn ensure_dotall(pattern: &str) -> String {
    // Add (?s) if no flags present.
    if !pattern.starts_with("(?") {
        return format!("(?s){pattern}");
    }

    let Some(close) = pattern.find(')') else {
        // Malformed inline flag group.
        return pattern.to_owned();
    };

    // Add s flag to ?(...) unless 's' or its complement 'm' is there.
    let flags = &pattern[2..close];

    if flags.contains('m') || flags.contains('s') {
        pattern.to_owned()
    } else {
        format!("(?{flags}s){}", &pattern[close + 1..])
    }
}

impl Regex {
    /// Construct the most efficient RE-like matching engine possible.
    pub fn new(pattern: impl AsRef<[u8]>, character_mode: CharacterMode) -> UResult<Self> {
        let pattern = pattern.as_ref();
        if NEEDS_FANCY_RE.is_match(pattern) {
            if character_mode == CharacterMode::Byte {
                let (pattern, _) = byte_fancy_text(pattern);
                let pattern = &ensure_dotall(&pattern);
                return Ok(Self::FancyByte(
                    FancyRegex::new(pattern).map_err(|e| USimpleError::new(2, e.to_string()))?,
                ));
            }
            let pattern =
                std::str::from_utf8(pattern).map_err(|e| USimpleError::new(2, e.to_string()))?;
            let pattern = &ensure_dotall(pattern);
            Ok(Self::Fancy(
                FancyRegex::new(pattern).map_err(|e| USimpleError::new(2, e.to_string()))?,
            ))
        } else if NEEDS_RE.is_match(pattern) {
            if character_mode == CharacterMode::Byte {
                let pattern = byte_regex_pattern(pattern);
                let pattern = &ensure_dotall(&pattern);
                Ok(Self::Byte(
                    regex::bytes::RegexBuilder::new(pattern)
                        .unicode(false)
                        .build()
                        .map_err(|e| USimpleError::new(2, e.to_string()))?,
                ))
            } else {
                let pattern = std::str::from_utf8(pattern)
                    .map_err(|e| USimpleError::new(2, e.to_string()))?;
                let pattern = &ensure_dotall(pattern);
                Ok(Self::Byte(
                    ByteRegex::new(pattern).map_err(|e| USimpleError::new(2, e.to_string()))?,
                ))
            }
        } else {
            let pattern = remove_escapes(pattern);
            Ok(Self::Literal(LiteralMatcher::new(pattern)))
        }
    }

    /// Check if the regex matches the content of the IOChunk.
    pub fn is_match(&self, chunk: &mut IOChunk) -> UResult<bool> {
        match self {
            Regex::Literal(m) => Ok(m.is_match(chunk.as_bytes())),
            Regex::Byte(re) => Ok(re.is_match(chunk.as_bytes())),
            Regex::FancyByte(re) => {
                let (text, _) = byte_fancy_text(chunk.as_bytes());
                re.is_match(&text)
                    .map_err(|e| USimpleError::new(2, e.to_string()))
            }
            Regex::Fancy(re) => {
                let text = chunk.as_str()?;
                re.is_match(text)
                    .map_err(|e| USimpleError::new(2, e.to_string()))
            }
        }
    }

    /// Return an iterator over capture groups.
    pub fn captures_iter<'t>(&'t self, chunk: &'t IOChunk) -> UResult<CaptureMatches<'t>> {
        match self {
            Regex::Literal(m) => {
                let haystack = chunk.as_bytes();
                Ok(CaptureMatches::Literal(Box::new(m.iter(haystack).map(
                    |(start, end, bytes)| {
                        Ok(Captures::Literal(Match::from_bytes(start, end, bytes)))
                    },
                ))))
            }

            Regex::Byte(re) => Ok(CaptureMatches::Byte(re.captures_iter(chunk.as_bytes()))),

            Regex::FancyByte(re) => {
                let haystack = chunk.as_bytes();
                Ok(CaptureMatches::MappedByte(Box::new(
                    MappedByteCaptureMatches::new(re, haystack),
                )))
            }

            Regex::Fancy(re) => {
                let text = chunk.as_str()?;
                Ok(CaptureMatches::Fancy(re.captures_iter(text)))
            }
        }
    }

    /// Return the number of capture groups, including group 0.
    pub fn captures_len(&self) -> usize {
        match self {
            Regex::Literal(_) => 1, // Only group 0
            Regex::Byte(re) => re.captures_len(),
            Regex::FancyByte(re) => re.captures_len(),
            Regex::Fancy(re) => re.captures_len(),
        }
    }

    /// Return the elements of the first capture.
    pub fn captures<'t>(&self, chunk: &'t IOChunk) -> UResult<Option<Captures<'t>>> {
        match self {
            Regex::Literal(m) => {
                let haystack = chunk.as_bytes();
                match m.find(haystack) {
                    Some((start, end, text)) => {
                        Ok(Some(Captures::Literal(Match::from_bytes(start, end, text))))
                    }
                    None => Ok(None),
                }
            }

            Regex::Byte(re) => {
                let bytes = chunk.as_bytes();
                Ok(re.captures(bytes).map(Captures::Byte))
            }

            Regex::FancyByte(re) => {
                let haystack = chunk.as_bytes();
                let (text, byte_offsets) = byte_fancy_text(haystack);
                match re.captures(&text) {
                    Ok(Some(captures)) => Ok(Some(mapped_fancy_captures(
                        &captures,
                        haystack,
                        &byte_offsets,
                    ))),
                    Ok(None) => Ok(None),
                    Err(e) => Err(USimpleError::new(2, e.to_string())),
                }
            }

            Regex::Fancy(re) => {
                let text = chunk.as_str()?;
                match re.captures(text) {
                    Ok(Some(caps)) => Ok(Some(Captures::Fancy(caps))),
                    Ok(None) => Ok(None),
                    Err(e) => Err(USimpleError::new(2, e.to_string())),
                }
            }
        }
    }

    /// Return a non-capturing result for a single match.
    pub fn find<'t>(&self, chunk: &'t IOChunk) -> UResult<Option<Match<'t>>> {
        match self {
            Regex::Literal(m) => {
                let haystack = chunk.as_bytes();
                match m.find(haystack) {
                    Some((start, end, bytes)) => Ok(Some(Match::from_bytes(start, end, bytes))),
                    None => Ok(None),
                }
            }

            Regex::Byte(re) => {
                let haystack = chunk.as_bytes();
                if let Some(m) = re.find(haystack) {
                    Ok(Some(Match::from_bytes(
                        m.start(),
                        m.end(),
                        &haystack[m.start()..m.end()],
                    )))
                } else {
                    Ok(None)
                }
            }

            Regex::FancyByte(re) => {
                let haystack = chunk.as_bytes();
                let (text, byte_offsets) = byte_fancy_text(haystack);
                match re.find(&text) {
                    Ok(Some(matched)) => {
                        Ok(Some(mapped_fancy_match(matched, haystack, &byte_offsets)))
                    }
                    Ok(None) => Ok(None),
                    Err(e) => Err(USimpleError::new(2, e.to_string())),
                }
            }

            Regex::Fancy(re) => {
                let text = chunk.as_str()?;
                match re.find(text) {
                    Ok(Some(m)) => Ok(Some(Match::from_str(m.start(), m.end(), m.as_str()))),
                    Ok(None) => Ok(None),
                    Err(e) => Err(USimpleError::new(2, e.to_string())),
                }
            }
        }
    }
}

/// Unified enum for holding either byte or fancy capture iterators.
pub enum CaptureMatches<'t> {
    Literal(Box<dyn Iterator<Item = UResult<Captures<'t>>> + 't>),
    Byte(ByteCaptureMatches<'t, 't>),
    MappedByte(Box<dyn Iterator<Item = UResult<Captures<'t>>> + 't>),
    Fancy(FancyCaptureMatches<'t, 't>),
}

struct MappedByteCaptureMatches<'r, 't> {
    regex: &'r FancyRegex,
    text: String,
    haystack: &'t [u8],
    byte_offsets: Vec<usize>,
    last_end: usize,
    last_match: Option<usize>,
}

impl<'r, 't> MappedByteCaptureMatches<'r, 't> {
    fn new(regex: &'r FancyRegex, haystack: &'t [u8]) -> Self {
        let (text, byte_offsets) = byte_fancy_text(haystack);
        Self {
            regex,
            text,
            haystack,
            byte_offsets,
            last_end: 0,
            last_match: None,
        }
    }
}

impl<'t> Iterator for MappedByteCaptureMatches<'_, 't> {
    type Item = UResult<Captures<'t>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.last_end > self.text.len() {
                return None;
            }

            let search_start = self.last_end;
            let captures = match self.regex.captures_from_pos(&self.text, search_start) {
                Ok(Some(captures)) => captures,
                Ok(None) => return None,
                Err(e) => {
                    self.last_end = self.text.len() + 1;
                    return Some(Err(USimpleError::new(
                        2,
                        format!("error retrieving RE captures: {e}"),
                    )));
                }
            };
            let matched = captures
                .get(0)
                .expect("captures always include the full match");

            if matched.start() == matched.end() {
                self.last_end = self.text[matched.end()..]
                    .chars()
                    .next()
                    .map_or(self.text.len() + 1, |c| matched.end() + c.len_utf8());
                if Some(matched.end()) == self.last_match {
                    continue;
                }
            } else {
                self.last_end = matched.end();
            }

            self.last_match = Some(matched.end());
            return Some(Ok(mapped_fancy_captures(
                &captures,
                self.haystack,
                &self.byte_offsets,
            )));
        }
    }
}

impl<'t> Iterator for CaptureMatches<'t> {
    type Item = UResult<Captures<'t>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            CaptureMatches::Literal(iter) => iter.next(),
            CaptureMatches::Byte(iter) => iter.next().map(|caps| Ok(Captures::Byte(caps))),
            CaptureMatches::MappedByte(iter) => iter.next(),
            CaptureMatches::Fancy(iter) => match iter.next() {
                Some(Ok(caps)) => Some(Ok(Captures::Fancy(caps))),
                Some(Err(e)) => Some(Err(USimpleError::new(
                    2,
                    format!("error retrieving RE captures: {e}"),
                ))),
                None => None,
            },
        }
    }
}

#[derive(Clone, Debug)]
/// Result type for RE capture get(n)
pub struct Match<'t> {
    start: usize,          // Match start
    end: usize,            // Match end
    bytes: &'t [u8],       // Actual match bytes
    text: Option<&'t str>, // UTF-8 match text when available
}

/// Provide interface compatible with Regex::Match.
impl<'t> Match<'t> {
    /// Construct a match from raw bytes.
    pub fn from_bytes(start: usize, end: usize, bytes: &'t [u8]) -> Self {
        Self {
            start,
            end,
            bytes,
            text: None,
        }
    }

    /// Construct a match from UTF-8 text.
    pub fn from_str(start: usize, end: usize, text: &'t str) -> Self {
        Self {
            start,
            end,
            bytes: text.as_bytes(),
            text: Some(text),
        }
    }

    /// Return the byte offset where the match begins.
    pub fn start(&self) -> usize {
        self.start
    }

    /// Return the byte offset just after the match.
    pub fn end(&self) -> usize {
        self.end
    }

    /// Return the match text, or an empty string when the bytes are not UTF-8.
    pub fn as_str(&self) -> &'t str {
        self.text.unwrap_or_default()
    }

    /// Return the matched bytes.
    pub fn as_bytes(&self) -> &'t [u8] {
        self.bytes
    }
}

/// Provide interface compatible with Regex::Captures.
pub enum Captures<'t> {
    Literal(Match<'t>), // only group 0
    Byte(ByteCaptures<'t>),
    MappedByte(Vec<Option<Match<'t>>>),
    Fancy(FancyCaptures<'t>),
}

impl<'t> Captures<'t> {
    /// Get capture group at index `i`
    /// Returns Ok(None) if the group didn't match.
    /// Returns Err if UTF-8 conversion fails (in Byte variant).
    pub fn get(&self, i: usize) -> UResult<Option<Match<'t>>> {
        match self {
            Captures::Literal(m) => Ok(if i == 0 { Some(m.clone()) } else { None }),
            Captures::Byte(caps) => match caps.get(i) {
                Some(m) => Ok(Some(Match::from_bytes(m.start(), m.end(), m.as_bytes()))),
                None => Ok(None),
            },
            Captures::MappedByte(captures) => Ok(captures.get(i).cloned().flatten()),
            Captures::Fancy(caps) => match caps.get(i) {
                Some(m) => Ok(Some(Match::from_str(m.start(), m.end(), m.as_str()))),
                None => Ok(None),
            },
        }
    }

    /// Return the number of capture groups (including group 0).
    pub fn len(&self) -> usize {
        match self {
            Captures::Literal(_) => 1,
            Captures::Byte(caps) => caps.len(),
            Captures::MappedByte(captures) => captures.len(),
            Captures::Fancy(caps) => caps.len(),
        }
    }

    /// Return true if there are no captures.
    // Unused, but provided for completeness.
    pub fn is_empty(&self) -> bool {
        match self {
            Captures::Literal(_) => false, // A literal match always has group 0
            Captures::Byte(caps) => caps.len() == 0,
            Captures::MappedByte(captures) => captures.is_empty(),
            Captures::Fancy(caps) => caps.len() == 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // FANCY_RE
    #[test]
    fn test_needs_fancy_re_matches() {
        let should_match = [
            r"(\w+):\1", // back-reference \1
        ];

        for pat in &should_match {
            assert!(
                NEEDS_FANCY_RE.is_match(pat.as_bytes()),
                "Expected NEEDS_FANCY_RE to match: {pat:?}"
            );
        }
    }

    #[test]
    fn test_needs_fancy_re_does_not_match() {
        let should_not_match = [
            r"\ 1", // Non-adjacent
            r"\0",  // Only \[1-9]
            // Simple ASCII
            r"foo",
            r"foo|bar",
            r"^foo[0-9]+bar$",
        ];

        for pat in &should_not_match {
            assert!(
                !NEEDS_FANCY_RE.is_match(pat.as_bytes()),
                "Expected NEEDS_FANCY_RE to NOT match: {pat:?}"
            );
        }
    }

    // NEEDS_RE
    #[test]
    fn test_needs_re_matches() {
        let should_match = [
            r".",       // Single regex wildcard
            r"a+b",     // Regex +
            r"foo|bar", // Regex alternation
            r"abc?",    // Regex optional
            r"a*b",     // Regex star
            r"[abc]",   // Character class
            r"(abc)",   // Group
            r"{1,2}",   // Repetition
            r"\d",      // Class shorthand
            r"\S",      // Class shorthand
            r"\1",      // Backreference
            r"a\Pb",    // Unicode property
        ];

        for pat in &should_match {
            assert!(
                NEEDS_RE.is_match(pat.as_bytes()),
                "Expected NEEDS_RE to match: {pat:?}"
            );
        }
    }

    #[test]
    fn test_needs_re_does_not_match() {
        let should_not_match = [
            r"abc",
            r"a\.b", // Escaped dot
            r"hello world",
            r"^abc$",  // Anchors alone
            r"file\.", // Escaped dot
            r"literal123",
            r"\\", // Escaped backslash
        ];

        for pat in &should_not_match {
            assert!(
                !NEEDS_RE.is_match(pat.as_bytes()),
                "Expected NEEDS_RE to NOT match: {pat:?}"
            );
        }

        assert!(!NEEDS_RE.is_match(b"\xC2\xE7\xCF\xC2"));
    }

    // Regex::new
    #[test]
    fn assert_byte_selection() {
        let re = Regex::new(r"x*", CharacterMode::Utf8).unwrap();
        assert!(matches!(re, Regex::Byte(_)));
    }

    #[test]
    fn assert_byte_regex_builder_path_matches_non_utf8_bytes() {
        let re = Regex::new(b"ab.", CharacterMode::Byte).unwrap();
        assert!(matches!(re, Regex::Byte(_)));

        let mut chunk = IOChunk::new_from_str("");
        chunk.set_to_bytes(b"ab\xE9".to_vec(), true);
        assert!(re.is_match(&mut chunk).unwrap());
    }

    #[test]
    fn assert_fancy() {
        let re = Regex::new(r"(.)\1", CharacterMode::Utf8).unwrap();
        assert!(matches!(re, Regex::Fancy(_)));
    }

    #[test]
    fn assert_fancy_byte() {
        let re = Regex::new(r"(.)\1", CharacterMode::Byte).unwrap();
        assert!(matches!(re, Regex::FancyByte(_)));
    }

    #[test]
    fn fancy_byte_matches_and_captures_invalid_utf8() {
        let re = Regex::new(r"(.)\1", CharacterMode::Byte).unwrap();
        let mut chunk = IOChunk::new_from_str("");
        chunk.set_to_bytes(b"\xFF\xFF".to_vec(), true);

        assert!(re.is_match(&mut chunk).unwrap());
        let captures = re.captures(&chunk).unwrap().expect("captures");
        let full_match = captures.get(0).unwrap().expect("full match");
        let group = captures.get(1).unwrap().expect("capture group");

        assert_eq!((full_match.start(), full_match.end()), (0, 2));
        assert_eq!(full_match.as_bytes(), b"\xFF\xFF");
        assert_eq!((group.start(), group.end()), (0, 1));
        assert_eq!(group.as_bytes(), b"\xFF");
    }

    #[test]
    fn fancy_byte_find_maps_non_ascii_offsets() {
        let re = Regex::new(b"(\xE9)\\1", CharacterMode::Byte).unwrap();
        let mut chunk = IOChunk::new_from_str("");
        chunk.set_to_bytes(b"0\xE9\xE91".to_vec(), true);

        let matched = re.find(&chunk).unwrap().expect("match");

        assert_eq!((matched.start(), matched.end()), (1, 3));
        assert_eq!(matched.as_bytes(), b"\xE9\xE9");
        assert_eq!(re.captures_len(), 2);
    }

    #[test]
    fn fancy_byte_captures_optional_groups() {
        let re = Regex::new(r"(a)?(b)\2", CharacterMode::Byte).unwrap();
        let chunk = IOChunk::new_from_str("bb");

        let captures = re.captures(&chunk).unwrap().expect("captures");

        assert_eq!(captures.len(), 3);
        assert!(!captures.is_empty());
        assert!(captures.get(1).unwrap().is_none());
        assert_eq!(captures.get(2).unwrap().unwrap().as_bytes(), b"b");
        assert!(captures.get(3).unwrap().is_none());
    }

    #[test]
    fn fancy_byte_captures_iter_maps_multiple_invalid_utf8_matches() {
        let re = Regex::new(r"(.)\1", CharacterMode::Byte).unwrap();
        let mut chunk = IOChunk::new_from_str("");
        chunk.set_to_bytes(b"\xFF\xFFaa\x80\x80".to_vec(), true);

        let groups: Vec<Vec<u8>> = re
            .captures_iter(&chunk)
            .unwrap()
            .map(|captures| {
                captures
                    .unwrap()
                    .get(1)
                    .unwrap()
                    .expect("capture group")
                    .as_bytes()
                    .to_vec()
            })
            .collect();

        assert_eq!(groups, [b"\xFF".to_vec(), b"a".to_vec(), b"\x80".to_vec()]);
    }

    #[test]
    fn fancy_byte_captures_iter_advances_after_empty_matches() {
        let re = Regex::new(r"()\1", CharacterMode::Byte).unwrap();
        let mut chunk = IOChunk::new_from_str("");
        chunk.set_to_bytes(b"\xFFa".to_vec(), true);

        let matches: Vec<(usize, usize)> = re
            .captures_iter(&chunk)
            .unwrap()
            .map(|captures| {
                let captures = captures.unwrap();
                let matched = captures.get(0).unwrap().expect("full match");
                (matched.start(), matched.end())
            })
            .collect();

        assert_eq!(matches, [(0, 0), (1, 1), (2, 2)]);
    }

    #[test]
    fn fancy_byte_reports_no_match() {
        let re = Regex::new(r"(.)\1", CharacterMode::Byte).unwrap();
        let mut chunk = IOChunk::new_from_str("ab");

        assert!(!re.is_match(&mut chunk).unwrap());
        assert!(re.captures(&chunk).unwrap().is_none());
        assert!(re.find(&chunk).unwrap().is_none());
    }

    #[test]
    fn assert_literal() {
        let re = Regex::new(r"x\.", CharacterMode::Utf8).unwrap();
        assert!(matches!(re, Regex::Literal(_)));
    }

    #[test]
    fn handles_invalid_regex_gracefully() {
        let err = Regex::new("(", CharacterMode::Utf8)
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("unclosed group") || err.contains("error parsing"),
            "Unexpected error: {err:?}"
        );
    }

    #[test]
    fn test_byte_regex_pattern_leaves_ascii() {
        assert_eq!(byte_regex_pattern(br"a.\d"), r"a.\d");
    }

    #[test]
    fn test_byte_regex_pattern_escapes_non_ascii_bytes() {
        assert_eq!(byte_regex_pattern(b"a\x80\xFF"), r"a\x80\xFF");
    }

    #[test]
    fn test_match_from_bytes_does_not_construct_text() {
        let m = Match::from_bytes(0, 3, b"abc");
        assert_eq!(m.as_bytes(), b"abc");
        assert_eq!(m.as_str(), "");
    }

    #[test]
    fn test_regex_captures_iter_returns_byte_captures() {
        let re = Regex::new(r"([ab])", CharacterMode::Utf8).unwrap();
        let chunk = IOChunk::new_from_str("aba");
        let groups: Vec<Vec<u8>> = re
            .captures_iter(&chunk)
            .unwrap()
            .map(|caps| {
                caps.unwrap()
                    .get(1)
                    .unwrap()
                    .expect("group 1")
                    .as_bytes()
                    .to_vec()
            })
            .collect();

        assert_eq!(groups, [b"a".to_vec(), b"b".to_vec(), b"a".to_vec()]);
    }

    #[test]
    fn test_regex_find_returns_byte_match() {
        let re = Regex::new(b"ab.", CharacterMode::Byte).unwrap();
        let mut chunk = IOChunk::new_from_str("");
        chunk.set_to_bytes(b"0ab\xE91".to_vec(), true);
        let m = re.find(&chunk).unwrap().expect("match");

        assert_eq!(m.start(), 1);
        assert_eq!(m.end(), 4);
        assert_eq!(m.as_bytes(), b"ab\xE9");
        assert_eq!(m.as_str(), "");
    }

    // remove_escapes
    #[test]
    fn test_remove_escapes() {
        use super::remove_escapes;

        assert_eq!(remove_escapes(b"abc"), b"abc");
        assert_eq!(remove_escapes(br"a\.c"), b"a.c");
        assert_eq!(remove_escapes(br"\\d"), br"\d");
        assert_eq!(remove_escapes(br"\.\*\+\?"), b".*+?");
        assert_eq!(
            remove_escapes(br"escaped\\backslash"),
            br"escaped\backslash"
        );
        assert_eq!(remove_escapes(br"trailing\\"), br"trailing\");
    }

    // LiteralMatcher
    #[test]
    fn test_literal_matcher_basic_match() {
        let matcher = LiteralMatcher::new("needle");
        assert!(matcher.is_match(b"this is a needle in a haystack"));
        assert!(!matcher.is_match(b"no match here"));
    }

    #[test]
    fn test_literal_matcher_anchor_start_match() {
        let matcher = LiteralMatcher::new("^needle");
        assert!(matcher.is_match(b"needle in a haystack"));
        assert!(!matcher.is_match(b"no needle match here"));
        assert!(!matcher.is_match(b"no"));
    }

    #[test]
    fn test_literal_matcher_anchor_end_match() {
        let matcher = LiteralMatcher::new("needle$");
        assert!(matcher.is_match(b"In a haystack there's a needle"));
        assert!(!matcher.is_match(b"no needle match here"));
        assert!(!matcher.is_match(b"no"));
    }

    #[test]
    fn test_literal_matcher_anchor_begin_end_match() {
        let matcher = LiteralMatcher::new("^needle$");
        assert!(matcher.is_match(b"needle"));
        assert!(!matcher.is_match(b"no needle match"));
        assert!(!matcher.is_match(b"needle no match"));
        assert!(!matcher.is_match(b"no match needle"));
        assert!(!matcher.is_match(b"nada"));
    }

    #[test]
    fn test_literal_matcher_utf8_match() {
        let matcher = LiteralMatcher::new("✓"); // U+2713 CHECK MARK (3 bytes)
        let haystack = "contains ✓ unicode".as_bytes();
        assert!(matcher.is_match(haystack));
        let found = matcher.find(haystack).unwrap();
        assert_eq!(found.2, "✓".as_bytes());
    }

    #[test]
    fn test_literal_matcher_find_location() {
        let matcher = LiteralMatcher::new("abc");
        let haystack = b"___abc___";
        let result = matcher.find(haystack);
        assert!(result.is_some());
        let (start, end, text) = result.unwrap();
        assert_eq!((start, end), (3, 6));
        assert_eq!(text, b"abc");
    }

    #[test]
    fn test_literal_matcher_find_location_end() {
        let matcher = LiteralMatcher::new("abc$");
        let haystack = b"012abc";
        let result = matcher.find(haystack);
        assert!(result.is_some());
        let (start, end, text) = result.unwrap();
        assert_eq!((start, end), (3, 6));
        assert_eq!(text, b"abc");
    }

    #[test]
    fn test_literal_matcher_iter_multiple() {
        let matcher = LiteralMatcher::new("test");
        let haystack = b"this test is a test of test matching";
        let matches: Vec<_> = matcher.iter(haystack).collect();
        assert_eq!(matches.len(), 3);

        let strings: Vec<_> = matches.iter().map(|(_, _, s)| *s).collect();
        assert_eq!(
            strings,
            [b"test".as_slice(), b"test".as_slice(), b"test".as_slice()]
        );
    }

    #[test]
    fn test_literal_matcher_iter_begin() {
        let matcher = LiteralMatcher::new("^test");
        let haystack = b"test is a test of test matching";
        let matches: Vec<_> = matcher.iter(haystack).collect();
        assert_eq!(matches.len(), 1);

        let strings: Vec<_> = matches.iter().map(|(_, _, s)| *s).collect();
        assert_eq!(strings, [b"test".as_slice()]);
    }

    #[test]
    fn test_literal_matcher_iter_end() {
        let matcher = LiteralMatcher::new("test$");
        let haystack = b"this test is a test of test";
        let matches: Vec<_> = matcher.iter(haystack).collect();
        assert_eq!(matches.len(), 1);

        let strings: Vec<_> = matches.iter().map(|(_, _, s)| *s).collect();
        assert_eq!(strings, [b"test".as_slice()]);
    }

    #[test]
    fn test_literal_matcher_no_match() {
        let matcher = LiteralMatcher::new("missing");
        let haystack = b"nothing to see here";
        assert!(!matcher.is_match(haystack));
        assert!(matcher.find(haystack).is_none());
        assert_eq!(matcher.iter(haystack).count(), 0);
    }

    #[test]
    fn test_literal_matcher_anchored_no_match() {
        let matcher = LiteralMatcher::new("^see$");
        let haystack = b"nothing to see here";
        assert!(!matcher.is_match(haystack));
        assert!(matcher.find(haystack).is_none());
        assert_eq!(matcher.iter(haystack).count(), 0);
    }

    #[test]
    fn prepends_s_when_no_flag_group() {
        assert_eq!(ensure_dotall("abc"), "(?s)abc");
    }

    #[test]
    fn adds_s_when_no_m_or_s() {
        assert_eq!(ensure_dotall("(?i)abc"), "(?is)abc");
        assert_eq!(ensure_dotall("(?)abc"), "(?s)abc");
    }

    #[test]
    fn leaves_m_unchanged() {
        assert_eq!(ensure_dotall("(?m)abc"), "(?m)abc");
        assert_eq!(ensure_dotall("(?im)abc"), "(?im)abc");
        assert_eq!(ensure_dotall("(?mi)abc"), "(?mi)abc");
    }

    #[test]
    fn leaves_existing_s_unchanged() {
        assert_eq!(ensure_dotall("(?s)abc"), "(?s)abc");
        assert_eq!(ensure_dotall("(?is)abc"), "(?is)abc");
    }

    #[test]
    fn leaves_malformed_flag_group_unchanged() {
        assert_eq!(ensure_dotall("(?iabc"), "(?iabc");
    }
}
