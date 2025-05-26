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
use once_cell::sync::Lazy;
use regex::bytes::{
    CaptureMatches as ByteCaptureMatches, Captures as ByteCaptures, Regex as ByteRegex,
};
use std::error::Error;
use uucore::error::{UResult, USimpleError};

use crate::fast_io::IOChunk;

#[derive(Clone, Debug)]
/// A regular expression that can be implemented through Byte or Fancy-Regex
// This enables a fast bytes path where possible.
pub enum Regex {
    Byte(ByteRegex),
    Fancy(FancyRegex),
}

impl Regex {
    /// Construct the most efficient engine possible
    pub fn new(pattern: &str) -> Result<Self, Box<dyn Error>> {
        // REs requiring the fancy_regex capabilities rather than the
        // regex::bytes engine
        // Consider . as one character that requires fancy_regex,
        // because it can match more than one byte when matching a
        // two or more byte Unicode UTF-8 representation.
        // It is an RE . rather than a literal one in the following
        // example sitations.
        // .        First character of the line
        // [^\\].   Second character after non \
        //
        //   \*.    A consumed backslash anywhere on the line
        //   \\.    An escaped backslash anywhere on the line
        //   xx.    A non-escaped sequence anywhere on the line
        // But the following are literal dots and can be captured by bytes:
        // \.       escaped at the beginning of the line
        //   x\.    escaped after a non escaped \ anywhere on the line
        //
        // The following RE captures these situations.
        static NEEDS_FANCY_RE: Lazy<regex::Regex> = Lazy::new(|| {
            regex::Regex::new(
                r"(?x) # Turn on verbose mode
                  (                       # An ASCII-incompatible RE
                    ( ^                   # Non-escaped: i.e. at BOL
                      | ^[^\\]            # or after a BOL non \
                      | [^\\] {2}         # or after two non \ characters
                      | \\.               # or after a consumed or escaped \
                    )
                    (                     # A potentially incompatible match
                      \.                  # . matches any Unicode character
                      | \[\^              # Bracketed -ve character class
                      | \(\?i             # (Unicode) case insensitive 
                      | \\[WwDdSsBbPp]    # Unicode classes
                      | \\[0-9]           # Back-references need fancy
                    )
                  )
                  | [^\x01-\x7f]          # Any non-ASCII character
                ",
            )
            .unwrap()
        });

        if NEEDS_FANCY_RE.is_match(pattern) {
            Ok(Self::Fancy(FancyRegex::new(pattern)?))
        } else {
            Ok(Self::Byte(ByteRegex::new(pattern)?))
        }
    }

    /// Return true if this is a byte-based regex.
    pub fn is_byte(&self) -> bool {
        matches!(self, Regex::Byte(_))
    }

    /// Check if the regex matches the content of the IOChunk.
    pub fn is_match(&self, chunk: &mut IOChunk) -> UResult<bool> {
        match self {
            Regex::Byte(re) => Ok(re.is_match(chunk.as_bytes())),
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
            Regex::Byte(re) => Ok(CaptureMatches::Byte(re.captures_iter(chunk.as_bytes()))),
            Regex::Fancy(re) => {
                let text = chunk.as_str()?;
                Ok(CaptureMatches::Fancy(re.captures_iter(text)))
            }
        }
    }

    /// Return the number of capture groups, including group 0.
    pub fn captures_len(&self) -> usize {
        match self {
            Regex::Byte(re) => re.captures_len(),
            Regex::Fancy(re) => re.captures_len(),
        }
    }

    /// Return the elements of the first capture.
    pub fn captures<'t>(&self, chunk: &'t IOChunk) -> UResult<Option<Captures<'t>>> {
        match self {
            Regex::Byte(re) => {
                let bytes = chunk.as_bytes();
                Ok(re.captures(bytes).map(Captures::Byte))
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
            Regex::Byte(re) => {
                let haystack = chunk.as_bytes();
                if let Some(m) = re.find(haystack) {
                    // Attempt UTF-8 decode for the match region only
                    let text = std::str::from_utf8(&haystack[m.start()..m.end()])
                        .map_err(|e| USimpleError::new(2, e.to_string()))?;
                    Ok(Some(Match {
                        start: m.start(),
                        end: m.end(),
                        text,
                    }))
                } else {
                    Ok(None)
                }
            }
            Regex::Fancy(re) => {
                let text = chunk.as_str()?;
                match re.find(text) {
                    Ok(Some(m)) => Ok(Some(Match {
                        start: m.start(),
                        end: m.end(),
                        text: m.as_str(),
                    })),
                    Ok(None) => Ok(None),
                    Err(e) => Err(USimpleError::new(2, e.to_string())),
                }
            }
        }
    }
}

/// Unified enum for holding either byte or fancy capture iterators.
pub enum CaptureMatches<'t> {
    Byte(ByteCaptureMatches<'t, 't>),
    Fancy(FancyCaptureMatches<'t, 't>),
}

impl<'t> Iterator for CaptureMatches<'t> {
    type Item = UResult<Captures<'t>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            CaptureMatches::Byte(iter) => iter.next().map(|caps| Ok(Captures::Byte(caps))),
            CaptureMatches::Fancy(iter) => match iter.next() {
                Some(Ok(caps)) => Some(Ok(Captures::Fancy(caps))),
                Some(Err(e)) => Some(Err(USimpleError::new(
                    2,
                    format!("error retrieving RE captures: {}", e),
                ))),
                None => None,
            },
        }
    }
}

#[derive(Debug)]
/// Result type for RE capture get(n)
pub struct Match<'t> {
    start: usize,  // Match start
    end: usize,    // Match end
    text: &'t str, // Actual match
}

/// Provide interface compatible with Regex::Match.
impl<'t> Match<'t> {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn as_str(&self) -> &'t str {
        self.text
    }
}

/// Provide interface compatible with Regex::Captures.
pub enum Captures<'t> {
    Byte(ByteCaptures<'t>),
    Fancy(FancyCaptures<'t>),
}

impl<'t> Captures<'t> {
    /// Get capture group at index `i`
    /// Returns Ok(None) if the group didn't match.
    /// Returns Err if UTF-8 conversion fails (in Byte variant).
    pub fn get(&self, i: usize) -> UResult<Option<Match<'t>>> {
        match self {
            Captures::Byte(caps) => match caps.get(i) {
                Some(m) => Ok(Some(Match {
                    start: m.start(),
                    end: m.end(),
                    text: std::str::from_utf8(m.as_bytes())
                        .map_err(|e| USimpleError::new(1, e.to_string()))?,
                })),
                None => Ok(None),
            },
            Captures::Fancy(caps) => match caps.get(i) {
                Some(m) => Ok(Some(Match {
                    start: m.start(),
                    end: m.end(),
                    text: m.as_str(),
                })),
                None => Ok(None),
            },
        }
    }

    /// Return the number of capture groups (including group 0).
    pub fn len(&self) -> usize {
        match self {
            Captures::Byte(caps) => caps.len(),
            Captures::Fancy(caps) => caps.len(),
        }
    }

    /// Return true if there are no captures.
    // Unused, but provided for completeness.
    pub fn is_empty(&self) -> bool {
        match self {
            Captures::Byte(caps) => caps.len() == 0,
            Captures::Fancy(caps) => caps.len() == 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_byte(pattern: &str) {
        let re = Regex::new(pattern).unwrap();
        assert!(re.is_byte(), "Pattern {:?} should use Byte engine", pattern);
    }

    fn assert_fancy(pattern: &str) {
        let re = Regex::new(pattern).unwrap();
        assert!(
            !re.is_byte(),
            "Pattern {:?} should use Fancy engine",
            pattern
        );
    }

    #[test]
    fn selects_byte_regex_for_escpaped_dot_bol() {
        assert_byte(r"\.");
    }

    #[test]
    fn selects_byte_regex_for_escpaped_dot_non_bol() {
        assert_byte(r"x\.");
    }

    #[test]
    fn selects_byte_regex_for_escaped_class() {
        assert_byte(r"\[^x]");
    }

    #[test]
    fn selects_byte_regex_for_escaped_case_insensitive_flag() {
        assert_byte(r"\(?i\)");
    }

    #[test]
    fn selects_byte_regex_for_escaped_unicode_class() {
        assert_byte(r"\\w");
    }

    #[test]
    fn selects_byte_regex_for_simple_ascii() {
        assert_byte(r"foo");
        assert_byte(r"foo|bar");
        assert_byte(r"^foo[0-9]+bar$");
    }

    #[test]
    fn selects_fancy_for_unicode_class_bol() {
        assert_fancy(r"\p{L}+"); // Unicode letter class
        assert_fancy(r"\W"); // \W is Unicode-aware.
        assert_fancy(r"\S+"); // \S is Unicode-aware.
        assert_fancy(r"\d"); // \d includes all Unicode digits.
    }

    #[test]
    fn selects_fancy_for_unicode_class_non_bol() {
        assert_fancy(r"x\p{L}+"); // Unicode letter class
        assert_fancy(r"x\W"); // \W is Unicode-aware.
        assert_fancy(r"x\S+"); // \S is Unicode-aware.
        assert_fancy(r"x\d"); // \d includes all Unicode digits.
    }

    #[test]
    fn selects_fancy_for_dot() {
        assert_fancy(r".");
        assert_fancy(r"x.");
        assert_fancy(r"xx.");
    }

    #[test]
    fn selects_fancy_for_consumed_backslash() {
        assert_fancy(r"\*.");
        assert_fancy(r"x\*.");
    }

    #[test]
    fn selects_fancy_for_escaped_backslash() {
        assert_fancy(r"\\.");
        assert_fancy(r"x\\.");
    }

    #[test]
    fn selects_fancy_for_inline_flags() {
        assert_fancy(r"(?i)abc"); // Unicode case-insensitive
        assert_fancy(r"x(?i)abc"); // Unicode case-insensitive
    }

    #[test]
    fn selects_fancy_for_backrefs() {
        assert_fancy(r"(\w+):\1"); // back-reference \1
    }

    #[test]
    fn selects_fancy_for_non_ascii_literals() {
        assert_fancy("naïve"); // Contains literal non-ASCII.
        assert_fancy("café"); // Contains literal non-ASCII.
    }

    #[test]
    fn handles_invalid_regex_gracefully() {
        let err = Regex::new("(").unwrap_err().to_string();
        assert!(
            err.contains("unclosed group") || err.contains("error parsing"),
            "Unexpected error: {}",
            err
        );
    }
}
