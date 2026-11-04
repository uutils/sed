// Parse delimited character sequences
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::sed::command::{CharacterMode, ParsedTransliteration, RE_DUP_MAX, RegexMode};
use crate::sed::error_handling::compilation_error;
use crate::sed::script_char_provider::ScriptCharProvider;
use crate::sed::script_line_provider::ScriptLineProvider;

use std::char;
use std::ffi::OsString;
#[cfg(unix)]
use std::os::unix::ffi::OsStringExt;
use std::string::FromUtf8Error;
use uucore::error::UResult;

/// Construct an OS string from arbitrary bytes.
#[cfg(unix)]
pub fn os_string_from_bytes(bytes: Vec<u8>) -> Result<OsString, FromUtf8Error> {
    Ok(OsString::from_vec(bytes))
}

/// Construct an OS string from UTF-8 bytes on platforms without byte-native paths.
#[cfg(not(unix))]
pub fn os_string_from_bytes(bytes: Vec<u8>) -> Result<OsString, FromUtf8Error> {
    String::from_utf8(bytes).map(OsString::from)
}

/// Return true if c is a valid octal digit
fn is_ascii_octal_digit(c: char) -> bool {
    matches!(c, '0'..='7')
}

/// Parse a numeric character escape and return the corresponding char.
/// Advance line to the first character not part of the escape.
/// ndigits is the number of allowed digits and radix is the value's
/// radix (e.g. 8, 10, 16 for octal, decimal, and hex escapes).
/// For values up to 3 ndigits is the maximum number of allowed digits,
/// for values above 3 ndigits is the exact number of allowed digits.
/// Return `None` if no valid character has been specified, including digits
/// that do not form a Unicode scalar value (a surrogate or a value above
/// U+10FFFF); the digits are then left unconsumed.
fn parse_numeric_escape(
    line: &mut ScriptCharProvider,
    is_allowed_char: fn(char) -> bool,
    ndigits: usize,
    radix: u32,
) -> Option<char> {
    let mut valid_chars = Vec::new();

    for _ in 0..ndigits {
        if !line.eol() && is_allowed_char(line.current()) {
            valid_chars.push(line.current());
            line.advance();
        } else {
            break;
        }
    }

    if valid_chars.is_empty() {
        return None;
    }

    if ndigits > 3 && valid_chars.len() != ndigits {
        line.retreat(valid_chars.len());
        return None;
    }

    let ndigits_read = valid_chars.len();
    let char_string: String = valid_chars.into_iter().collect();
    let decoded = u32::from_str_radix(&char_string, radix)
        .ok()
        .and_then(char::from_u32);
    if decoded.is_none() {
        line.retreat(ndigits_read);
    }
    decoded
}

/// Transforms the specified character into the corresponding ASCII
/// control character as follows.
/// - Convert lowercase letters to uppercase
/// - XOR the ASCII value with 0x40 (inverts bit 6)
///
/// Return `None` if the result is not a valid Unicode scalar.
fn create_control_char(x: char) -> Option<char> {
    if !x.is_ascii() {
        return None;
    }

    let c = x.to_ascii_uppercase();

    let transformed = (c as u8) ^ 0x40;
    char::from_u32(u32::from(transformed))
}

/// Append a parsed script character according to the active character mode.
pub fn push_script_char(bytes: &mut Vec<u8>, ch: char, character_mode: CharacterMode) {
    match character_mode {
        CharacterMode::Byte if (ch as u32) <= 0xFF => bytes.push(ch as u8),
        _ => {
            let mut buf = [0u8; 4];
            bytes.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
        }
    }
}

/// Decode parsed script bytes as UTF-8 for character-mode parsing.
fn parsed_bytes_to_utf8(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    bytes: Vec<u8>,
    description: &str,
) -> UResult<String> {
    String::from_utf8(bytes).map_err(|e| {
        compilation_error::<String>(lines, line, format!("invalid UTF-8 in {description}: {e}"))
            .unwrap_err()
    })
}

/// Parse a character escape valid in all contexts (RE pattern, substitution,
/// transliterarion) and return the corresponding char.
/// At entry line.current() must have advanced after the `\\`.
/// Advance line to the first character not part of the escape.
/// Return `None` if an invalid escape has been specified, and an error
/// for `\c\` followed by anything other than a second backslash.
pub fn parse_char_escape(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
) -> UResult<Option<char>> {
    let decoded = match line.current() {
        'a' => {
            line.advance();
            Some('\x07')
        }
        'f' => {
            line.advance();
            Some('\x0c')
        }
        'n' => {
            line.advance();
            Some('\n')
        }
        'r' => {
            line.advance();
            Some('\r')
        }
        't' => {
            line.advance();
            Some('\t')
        }
        'v' => {
            line.advance();
            Some('\x0b')
        }

        'c' => {
            // Control character escape: \cC
            line.advance(); // move past 'c'
            if line.eol() {
                // A \c at the end of a script line takes the newline that
                // ends it, as GNU sed does: control-newline is 'J'.
                return Ok(create_control_char('\n'));
            }
            if line.current() == '\\' {
                // Control-backslash has to be written as \c\\
                line.advance();
                if line.eol() || line.current() != '\\' {
                    return compilation_error(
                        lines,
                        line,
                        "recursive escaping after \\c not allowed",
                    );
                }
                line.advance();
                return Ok(Some('\x1c'));
            }
            match create_control_char(line.current()) {
                Some(decoded) => {
                    line.advance();
                    Some(decoded)
                }
                None => Some('c'),
            }
        }

        'd' => {
            // Decimal escape: \dnnn
            line.advance(); // move past 'd'
            match parse_numeric_escape(line, |c| c.is_ascii_digit(), 3, 10) {
                Some(decoded) => Some(decoded),
                None => Some('d'),
            }
        }

        'o' => {
            // Octal escape: \onnn
            line.advance(); // move past 'o'
            match parse_numeric_escape(line, is_ascii_octal_digit, 3, 8) {
                Some(decoded) => Some(decoded),
                None => Some('o'),
            }
        }

        'u' => {
            // Short Unicode escape \uXXXX (exactly four hex digits)
            line.advance(); // move past 'x'
            match parse_numeric_escape(line, |c| c.is_ascii_hexdigit(), 4, 16) {
                Some(decoded) => Some(decoded),
                None => Some('u'),
            }
        }

        'U' => {
            // Short Unicode escape \UXXXXXXXX (exactly eight hex digits)
            line.advance(); // move past 'x'
            match parse_numeric_escape(line, |c| c.is_ascii_hexdigit(), 8, 16) {
                Some(decoded) => Some(decoded),
                None => Some('U'),
            }
        }

        'x' => {
            // Hexadecimal escape: \xnn
            line.advance(); // move past 'x'
            match parse_numeric_escape(line, |c| c.is_ascii_hexdigit(), 2, 16) {
                Some(decoded) => Some(decoded),
                None => Some('x'),
            }
        }
        _ => None,
    };
    Ok(decoded)
}

/// Parse a POSIX RE character class returning it as bytes.
/// This functionality is needed to avoid terminating delimited
/// sequences when a delimiter appears within a character class.
/// While at it, handle escaped characters for the sake of consistency.
/// Under --posix no backslash escape is recognized here, as GNU sed does,
/// although they all remain available outside bracket expressions.
fn parse_character_class(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    character_mode: CharacterMode,
    posix: bool,
) -> UResult<Vec<u8>> {
    let mut result = Vec::new();

    assert!(
        !line.eol() && line.current() == '[',
        "Invalid character class."
    );

    line.advance();
    result.push(b'[');

    // Optional negation
    if !line.eol() && line.current() == '^' {
        result.push(b'^');
        line.advance();
    }

    // Optional leading ']' inside the class
    if !line.eol() && line.current() == ']' {
        result.push(b']');
        line.advance();
    }

    while !line.eol() {
        let ch = line.current();

        if ch == ']' {
            result.push(b']');
            line.advance();
            return Ok(result);
        }

        if ch == '[' {
            line.advance();
            if line.eol() {
                result.push(b'[');
                continue;
            }
            let marker = line.current();
            // POSIX character class, collating symbol, or equivalence
            if marker == ':' || marker == '.' || marker == '=' {
                line.advance();

                result.push(b'[');
                result.push(marker as u8);

                let mut inner = Vec::new();
                let mut terminated = false;

                while !line.eol() {
                    let c = line.current();
                    if c == marker {
                        line.advance();
                        if !line.eol() && line.current() == ']' {
                            line.advance();
                            result.extend_from_slice(&inner);
                            result.push(marker as u8);
                            result.push(b']');
                            terminated = true;
                            break;
                        }
                        // False alarm, just part of the inner name
                        inner.push(marker as u8);
                    } else {
                        inner.push(line.current_byte());
                        line.advance();
                    }
                }

                if !terminated {
                    return compilation_error(
                        lines,
                        line,
                        "Unterminated POSIX character class, equivalence or collating symbol",
                    );
                }

                continue;
            }
            // Not a POSIX construct — an ordinary member of the set
            result.extend_from_slice(br"\[");
            continue;
        }

        if ch == '\\' {
            // Handle escape sequence
            line.advance();
            if line.eol() {
                break;
            }
            let decoded = if posix {
                None
            } else {
                parse_char_escape(lines, line)?
            };
            if let Some(decoded) = decoded {
                push_script_char(&mut result, decoded, character_mode);
            } else {
                // Outside the character escapes a \ is an ordinary member of the set,
                // so the next character is left for the loop to handle.  This is what
                // makes `[a\]bc]` end at the first `]`, as GNU sed does.
                result.extend_from_slice(br"\\");
            }
        } else {
            result.push(line.current_byte());
            line.advance();
        }
    }

    compilation_error(lines, line, "Unterminated bracket expression")
}

/// Scan and return the opening delimiter of a delimited string
/// Advances the line past the opening delimiter
fn scan_delimiter(lines: &ScriptLineProvider, line: &mut ScriptCharProvider) -> UResult<char> {
    // Sanity check
    if line.eol() {
        return compilation_error(lines, line, "unexpected end of line".to_string());
    }

    let delimiter = line.current();
    if delimiter == '\\' {
        return compilation_error(lines, line, "\\ cannot be used as a string delimiter");
    }
    line.advance(); // skip the opening delimiter
    Ok(delimiter)
}

/// Parse the regular expression delimited by the current line
/// character and return it as a string.
/// On return, the line is on the closing delimiter.
/// In Basic mode, quantifiers like {m,n} must be escaped (\{m,n\}).
/// In Extended mode, quantifiers like {m,n} don't require escaping.
/// Only for tests; the compiler calls parse_regex_for_mode, which is
/// where the character and --posix modes of the run are supplied.
#[cfg(test)]
fn parse_regex(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    regex_mode: RegexMode,
) -> UResult<Vec<u8>> {
    parse_regex_for_mode(lines, line, regex_mode, CharacterMode::Utf8, false)
}

/// Return true if a backslash followed by `c` is a GNU regular expression
/// extension that must be emitted as the literal character `c` under --posix.
/// In Extended mode `? + |` are excluded, because `\?` is already a literal there.
fn is_gnu_regex_escape(c: char, regex_mode: RegexMode) -> bool {
    match c {
        '?' | '+' | '|' => matches!(regex_mode, RegexMode::Basic),
        'w' | 'W' | 's' | 'S' | 'b' | 'B' | '<' | '>' | '`' | '\'' => true,
        _ => false,
    }
}

/// Parse a regular expression according to the current character mode.
/// Under `posix` the GNU regular expression extensions are disabled, with each
/// one standing for the literal character that follows its backslash.
pub fn parse_regex_for_mode(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    regex_mode: RegexMode,
    character_mode: CharacterMode,
    posix: bool,
) -> UResult<Vec<u8>> {
    let delimiter = scan_delimiter(lines, line)?;
    let mut result = Vec::new();
    // In an ERE, a quantifier must follow something it can repeat. GNU sed
    // rejects one at the start of the regex or right after `(`, `|`, `^` or
    // `$`, and reports it once the whole regex has been read.
    // In a BRE, a `*` with nothing to repeat stands for itself, so it is
    // emitted as `\*`. That is at the start of the regex, right after `\(`
    // or `\|`, or right after a `^` anchor in one of those places.
    let is_ere = matches!(regex_mode, RegexMode::Extended);
    let mut can_quantify = false;
    let mut quantifier_without_atom = false;
    let mut bre_at_start = true;
    while !line.eol() {
        let c = line.current();
        if is_ere && c != delimiter {
            let is_quantifier = matches!(c, '*' | '+' | '?') || (c == '{' && delimiter != '{');
            if is_quantifier && !can_quantify {
                quantifier_without_atom = true;
            }
            can_quantify = !matches!(c, '(' | '|' | '^' | '$');
        }
        if !is_ere && c != delimiter {
            if c == '*' && !can_quantify {
                result.extend_from_slice(b"\\*");
                can_quantify = true;
                bre_at_start = false;
                line.advance();
                continue;
            }
            let at_start = bre_at_start;
            bre_at_start = false;
            can_quantify = !(c == '^' && at_start);
            let next = line
                .get_line()
                .get(line.get_pos() + 1)
                .map(|&b| char::from(b));
            let opens = matches!(next, Some('(')) || (matches!(next, Some('|')) && !posix);
            if c == '\\' && opens && next != Some(delimiter) {
                can_quantify = false;
                bre_at_start = true;
            }
        }
        match c {
            '[' if delimiter != '[' => {
                let cc = parse_character_class(lines, line, character_mode, posix)?;
                result.extend_from_slice(&cc);
                continue;
            }
            '\\' => {
                line.advance();
                if line.eol() {
                    return compilation_error(lines, line, "unterminated regular expression");
                }
                if line.current() == delimiter {
                    // Push escaped delimiter
                    result.push(line.current_byte());
                    line.advance();
                    continue;
                }
                if posix && is_gnu_regex_escape(line.current(), regex_mode) {
                    result.push(line.current_byte());
                    line.advance();
                    continue;
                }
                if line.current() == '{' && matches!(regex_mode, RegexMode::Basic) {
                    validate_quantifier_structure(lines, line, delimiter, RegexMode::Basic)?;
                    let quantifier = validate_quantifier_numbers(lines, line)?;
                    result.push(b'\\');
                    result.push(b'{');
                    result.extend_from_slice(quantifier.as_bytes());
                    continue;
                }
                if line.current() == '}' {
                    result.push(b'\\');
                    result.push(b'}');
                    line.advance();
                    continue;
                }
                if let Some(decoded) = parse_char_escape(lines, line)? {
                    push_script_char(&mut result, decoded, character_mode);
                } else {
                    // Unknown letter and zero escapes are literal in sed;
                    // buffer anchors still need their regex-engine spelling.
                    let c = line.current();
                    let literal = c == '0'
                        || (c.is_ascii_alphabetic() && !is_gnu_regex_escape(c, regex_mode));
                    if !literal {
                        result.push(b'\\');
                    }
                    result.push(match line.current_byte() {
                        b'`' => b'A',
                        b'\'' => b'z',
                        c => c,
                    });
                    line.advance();
                }
                continue;
            }
            '{' if delimiter != '{' && matches!(regex_mode, RegexMode::Extended) => {
                validate_quantifier_structure(lines, line, delimiter, RegexMode::Extended)?;
                let quantifier = validate_quantifier_numbers(lines, line)?;
                result.push(b'{');
                result.extend_from_slice(quantifier.as_bytes());
                continue;
            }
            '}' if delimiter != '}' => {
                result.push(b'}');
                line.advance();
                continue;
            }

            c if c == delimiter => {
                if quantifier_without_atom {
                    return compilation_error(lines, line, "Invalid preceding regular expression");
                }
                return Ok(result);
            }
            _ => result.push(line.current_byte()),
        }
        line.advance();
    }
    compilation_error(lines, line, "unterminated regular expression")
}

// Check for closing brace and the structure/content.
fn validate_quantifier_structure(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    delimiter: char,
    regex_mode: RegexMode,
) -> UResult<()> {
    let invalid_content_error_msg = "Invalid content of \\{\\}";
    let mut found_closing_brace = false;
    let mut seen_comma = false;
    let mut invalid_content_detected = false;
    let mut is_quantifier_empty = true;
    let initial_pos = line.get_pos();
    line.advance();

    while !line.eol() && line.current() != delimiter {
        match regex_mode {
            RegexMode::Extended => {
                // In ERE mode, look for }
                if line.current() == '}' {
                    // Empty quantifier {} is not valid
                    if is_quantifier_empty {
                        invalid_content_detected = true;
                    }
                    found_closing_brace = true;
                    break;
                }
                // Entering means there is no } immediately after the {
                is_quantifier_empty = false;
                // Only digits and one comma allowed
                if line.current() == ',' {
                    if seen_comma {
                        invalid_content_detected = true;
                    }
                    seen_comma = true;
                } else if !line.current().is_ascii_digit() {
                    invalid_content_detected = true;
                }
                line.advance();
            }
            RegexMode::Basic => {
                // In BRE mode, look for \}
                if line.current() == '\\' {
                    line.advance();
                    if !line.eol() && line.current() == '}' {
                        if is_quantifier_empty {
                            invalid_content_detected = true;
                        }
                        found_closing_brace = true;
                    } else {
                        invalid_content_detected = true;
                    }
                    break;
                }
                is_quantifier_empty = false;
                if line.current() == ',' {
                    if seen_comma {
                        invalid_content_detected = true;
                    }
                    seen_comma = true;
                } else if !line.current().is_ascii_digit() {
                    invalid_content_detected = true;
                }
                line.advance();
            }
        }
    }

    if !found_closing_brace {
        return compilation_error(lines, line, "Unmatched \\{");
    }

    if invalid_content_detected {
        return compilation_error(lines, line, invalid_content_error_msg);
    }

    line.set_position(initial_pos);
    Ok(())
}

// Parse an already-structure-validated run of digits into a quantifier bound.
// `validate_quantifier_structure` guarantees the run contains only ASCII
// digits, so the sole failure mode is a value exceeding what fits, which sed
// reports as "Regular expression too big" (same as exceeding RE_DUP_MAX).
fn parse_quantifier_bound(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    digits: &str,
) -> UResult<usize> {
    match digits.parse::<usize>() {
        Ok(val) if val <= RE_DUP_MAX => Ok(val),
        _ => compilation_error(lines, line, "Regular expression too big"),
    }
}

// Performs validations on m and/or n values of the quantifier
// and returns the valid content as a string (without braces).
fn validate_quantifier_numbers(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
) -> UResult<String> {
    line.advance(); // Skip the opening brace.

    // Collect m. It may be empty for the {,n} and {,} forms, which mean {0,n}
    // and {0,} respectively.
    let mut m = String::new();
    while line.current() != ',' && line.current() != '}' && line.current() != '\\' {
        m.push(line.current());
        line.advance();
    }

    // Collect n when a comma is present.
    let has_comma = line.current() == ',';
    let mut n = String::new();
    if has_comma {
        line.advance();
        while line.current() != '}' && line.current() != '\\' {
            n.push(line.current());
            line.advance();
        }
    }

    // An absent m defaults to 0; both m and n are bounded by RE_DUP_MAX.
    let m_val = if m.is_empty() {
        0
    } else {
        parse_quantifier_bound(lines, line, &m)?
    };
    let n_val = if n.is_empty() {
        None
    } else {
        Some(parse_quantifier_bound(lines, line, &n)?)
    };

    // Validate m <= n if both present.
    if let Some(n_val) = n_val
        && m_val > n_val
    {
        return compilation_error(lines, line, "Invalid content of \\{\\}");
    }

    // Rebuild the validated content (without braces), defaulting an absent m
    // to 0 so the emitted pattern stays well-formed.
    let mut result = if m.is_empty() { "0".to_string() } else { m };
    if has_comma {
        result.push(',');
        result.push_str(&n);
    }

    Ok(result)
}

/// Parse the transliteration string delimited by the current line
/// character and return it as a string.
/// On return the line is on the closing delimiter.
pub fn parse_transliteration(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
) -> UResult<Vec<u8>> {
    parse_transliteration_bytes(lines, line, CharacterMode::Utf8)
}

/// Parse transliteration bytes according to the current character mode.
fn parse_transliteration_bytes(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    character_mode: CharacterMode,
) -> UResult<Vec<u8>> {
    let delimiter = scan_delimiter(lines, line)?;
    let mut result = Vec::new();

    while !line.eol() {
        match line.current() {
            '\\' => {
                line.advance();
                if line.eol() {
                    return compilation_error(lines, line, "unterminated transliteration string");
                }
                if line.current() == delimiter || line.current() == '\\' {
                    // Push only the escaped character
                    result.push(line.current_byte());
                    line.advance();
                    continue;
                }
                if let Some(decoded) = parse_char_escape(lines, line)? {
                    push_script_char(&mut result, decoded, character_mode);
                } else {
                    result.push(line.current_byte());
                    line.advance();
                }
                continue;
            }
            c if c == delimiter => return Ok(result),
            _ => result.push(line.current_byte()),
        }
        line.advance();
    }
    compilation_error(lines, line, "unterminated transliteration string")
}

/// Parse a transliteration string according to the current character mode.
pub fn parse_transliteration_for_mode(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    character_mode: CharacterMode,
) -> UResult<ParsedTransliteration> {
    let bytes = parse_transliteration_bytes(lines, line, character_mode)?;
    match character_mode {
        CharacterMode::Byte => Ok(ParsedTransliteration::Bytes(bytes)),
        CharacterMode::Utf8 => parsed_bytes_to_utf8(lines, line, bytes, "transliteration string")
            .map(ParsedTransliteration::Text),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    #[cfg(unix)]
    use std::os::unix::ffi::OsStrExt;

    #[test]
    #[cfg(unix)]
    fn test_os_string_from_bytes_preserves_non_utf8_on_unix() {
        let os = os_string_from_bytes(b"a\xFFb".to_vec()).unwrap();
        assert_eq!(os.as_os_str().as_bytes(), b"a\xFFb");
    }

    #[test]
    fn test_os_string_from_bytes_accepts_utf8() {
        let os = os_string_from_bytes("aé".as_bytes().to_vec()).unwrap();
        assert_eq!(os, OsString::from("aé"));
    }

    #[test]
    fn test_parsed_bytes_to_utf8_accepts_valid_utf8() {
        let lines = ScriptLineProvider::with_active_state("test.sed", 1);
        let line = ScriptCharProvider::new("");
        let text = parsed_bytes_to_utf8(&lines, &line, "é".as_bytes().to_vec(), "test").unwrap();
        assert_eq!(text, "é");
    }

    #[test]
    fn test_parsed_bytes_to_utf8_rejects_invalid_utf8() {
        let lines = ScriptLineProvider::with_active_state("test.sed", 1);
        let line = ScriptCharProvider::new("");
        let err = parsed_bytes_to_utf8(&lines, &line, b"\xE9".to_vec(), "test").unwrap_err();
        assert!(err.to_string().contains("invalid UTF-8 in test"));
    }

    fn make_providers(input: &str) -> (ScriptLineProvider, ScriptCharProvider) {
        let lines = ScriptLineProvider::new(vec![]); // Empty for tests
        let line = ScriptCharProvider::new(input);
        (lines, line)
    }

    /// Return the provider's current character, or `None` at end of line.
    fn current_or_eol(provider: &ScriptCharProvider) -> Option<char> {
        if provider.eol() {
            None
        } else {
            Some(provider.current())
        }
    }

    // parse_numeric_escape
    #[rstest]
    #[case::octal_escape("141rest", is_ascii_octal_digit, 3, 8, Some('a'), Some('r'))]
    #[case::octal_escape_eol("141", is_ascii_octal_digit, 3, 8, Some('a'), None)]
    // "65" is consumed, leaving the '9'
    #[case::decimal_escape("0659", |c: char| c.is_ascii_digit(), 3, 10, Some('A'), Some('9'))]
    #[case::decimal_invalid("QR", |c: char| c.is_ascii_digit(), 3, 10, None, Some('Q'))]
    #[case::hex_escape("3cZ", |c: char| c.is_ascii_hexdigit(), 2, 16, Some('<'), Some('Z'))]
    // Only '4' is valid hex
    #[case::hex_escape_truncated("4G", |c: char| c.is_ascii_hexdigit(), 2, 16, Some('\u{4}'), Some('G'))]
    // U+2665 = '♥'
    #[case::unicode_escape_short("26650", |c: char| c.is_ascii_hexdigit(), 4, 16, Some('♥'), Some('0'))]
    #[case::unicode_escape_short_invalid("123Q", |c: char| c.is_ascii_hexdigit(), 4, 16, None, Some('1'))]
    #[case::unicode_escape_long_invalid("1234567Q", |c: char| c.is_ascii_hexdigit(), 8, 16, None, Some('1'))]
    // U+1F600 = 😀
    #[case::unicode_escape_long("0001F6009", |c: char| c.is_ascii_hexdigit(), 8, 16, Some('😀'), Some('9'))]
    #[case::no_valid_digits("xyz", |c: char| c.is_ascii_digit(), 3, 10, None, Some('x'))]
    fn test_parse_numeric_escape(
        #[case] input: &str,
        #[case] is_allowed_char: fn(char) -> bool,
        #[case] ndigits: usize,
        #[case] radix: u32,
        #[case] expected: Option<char>,
        #[case] next: Option<char>,
    ) {
        let mut provider = ScriptCharProvider::new(input);
        let c = parse_numeric_escape(&mut provider, is_allowed_char, ndigits, radix);
        assert_eq!(c, expected);
        assert_eq!(current_or_eol(&provider), next);
    }

    // create_control_char
    #[rstest]
    #[case::lowercase_z('z', Some('\u{1a}'))] // 0x5A ^ 0x40 = 0x1A
    #[case::lowercase_a('a', Some('\u{01}'))] // 0x41 ^ 0x40 = 0x01
    #[case::uppercase_z('Z', Some('\u{1a}'))]
    #[case::uppercase_a('A', Some('\u{01}'))]
    #[case::symbol_open_brace('{', Some(';'))] // 0x7B ^ 0x40 = 0x3B
    #[case::symbol_semicolon(';', Some('{'))] // 0x3B ^ 0x40 = 0x7B
    #[case::non_ascii_char('é', None)] // outside ASCII
    #[case::edge_at_sign('@', Some('\0'))] // 0x40 ^ 0x40 = 0x00
    #[case::edge_del('\x7F', Some('\x3F'))] // 0x7F ^ 0x40 = 0x3F
    fn test_create_control_char(#[case] input: char, #[case] expected: Option<char>) {
        assert_eq!(create_control_char(input), expected);
    }

    // parse_char_escape
    #[rstest]
    #[case::control_escape_at_eol("c", Some('J'), None)]
    #[case::standard_escape_a_eol("a", Some('\x07'), None)]
    #[case::standard_escape_f_eol("f", Some('\x0c'), None)]
    #[case::standard_escape_n_eol("n", Some('\n'), None)]
    #[case::standard_escape_r_eol("r", Some('\r'), None)]
    #[case::standard_escape_t_eol("t", Some('\t'), None)]
    #[case::standard_escape_v_eol("v", Some('\x0b'), None)]
    #[case::standard_escape_a_more("a.", Some('\x07'), Some('.'))]
    #[case::standard_escape_f_more("f.", Some('\x0c'), Some('.'))]
    #[case::standard_escape_n_more("n.", Some('\n'), Some('.'))]
    #[case::standard_escape_r_more("r.", Some('\r'), Some('.'))]
    #[case::standard_escape_t_more("t.", Some('\t'), Some('.'))]
    #[case::standard_escape_v_more("v.", Some('\x0b'), Some('.'))]
    #[case::escape_invalid("zx", None, Some('z'))]
    #[case::control_escape_valid("cZ", Some('\x1A'), None)]
    #[case::control_backslash_escape(r"c\\x", Some('\x1c'), Some('x'))]
    #[case::control_escape_invalid("cé", Some('c'), Some('Ã'))]
    #[case::decimal_escape_valid("d065r", Some('A'), Some('r'))]
    #[case::octal_escape_valid("o141x", Some('a'), Some('x'))]
    #[case::hex_escape_valid("x41;", Some('A'), Some(';'))]
    #[case::short_unicode_escape_valid("u2665;", Some('♥'), Some(';'))]
    // Surrogates and values above U+10FFFF are not escapes; the letter
    // stands for itself and the digits are left in place.
    #[case::unicode_escape_surrogate("ud800", Some('u'), Some('d'))]
    #[case::unicode_escape_out_of_range("Udeadbeef", Some('U'), Some('d'))]
    #[case::unicode_escape_above_max("U00110000", Some('U'), Some('0'))]
    #[case::long_unicode_escape_valid("U0001F600;", Some('😀'), Some(';'))]
    #[case::decimal_escape_fallback("d;.", Some('d'), Some(';'))]
    #[case::octal_escape_fallback("o9x", Some('o'), Some('9'))]
    #[case::hex_escape_fallback("xyz", Some('x'), Some('y'))]
    #[case::unknown_escape("q", None, Some('q'))]
    fn test_parse_char_escape(
        #[case] input: &str,
        #[case] expected: Option<char>,
        #[case] next: Option<char>,
    ) {
        let (lines, mut provider) = make_providers(input);
        let result = parse_char_escape(&lines, &mut provider).unwrap();
        assert_eq!((result, current_or_eol(&provider)), (expected, next));
    }

    #[rstest]
    #[case::escaped_letter(r"c\d")]
    #[case::trailing_backslash(r"c\")]
    fn test_control_recursive_escape(#[case] input: &str) {
        let (lines, mut provider) = make_providers(input);
        let err = parse_char_escape(&lines, &mut provider).unwrap_err();
        assert!(
            err.to_string()
                .contains(r"recursive escaping after \c not allowed")
        );
    }

    #[rstest]
    #[case::byte_mode_single_byte(CharacterMode::Byte, b"\xE9")]
    #[case::utf8_mode_encodes_utf8(CharacterMode::Utf8, "é".as_bytes())]
    fn test_push_script_char(#[case] mode: CharacterMode, #[case] expected: &[u8]) {
        let mut bytes = Vec::new();
        push_script_char(&mut bytes, 'é', mode);
        assert_eq!(bytes, expected);
    }

    // parse_character_class
    fn test_lines() -> ScriptLineProvider {
        ScriptLineProvider::with_active_state("test.sed", 3)
    }

    #[rstest]
    #[case::basic_character_class("[qr]", b"[qr]")]
    #[case::negated_class("[^abc]", b"[^abc]")]
    #[case::leading_close_bracket("[]abc]", b"[]abc]")]
    #[case::leading_negated_close_bracket("[^]abc]", b"[^]abc]")]
    #[case::escaped_character_begin("[\\nabc]", b"[\nabc]")]
    #[case::escaped_character_middle("[a\\nbc]", b"[a\nbc]")]
    #[case::escaped_character_end("[abc\\n]", b"[abc\n]")]
    // A backslash does not quote the `]`, so the class ends at it and
    // holds `a` and a literal backslash, as GNU sed does.
    #[case::escaped_delimiter("[a\\]bc]", br"[a\\]")]
    #[case::posix_class("[[:digit:]]", b"[[:digit:]]")]
    #[case::colon_literal_character_class("[:]", b"[:]")]
    #[case::equivalence_class("[[=a=]]", b"[[=a=]]")]
    #[case::collating_symbol("[[.ch.]]", b"[[.ch.]]")]
    #[case::malformed_posix_like_pattern_treated_as_literal("[[x]yz]", br"[\[x]")]
    #[case::literal_open_bracket_in_character_class("[a[b]", br"[a\[b]")]
    fn test_parse_character_class(#[case] input: &str, #[case] expected: &[u8]) {
        let mut line = ScriptCharProvider::new(input);
        let lines = test_lines();
        let result = parse_character_class(&lines, &mut line, CharacterMode::Utf8, false).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::unterminated_class("[abc")] // missing closing ]
    #[case::unterminated_posix_class("[[:digit:]")]
    #[case::unterminated_escape("[abc\\")] // missing closing ]
    fn test_parse_character_class_error(#[case] input: &str) {
        let mut line = ScriptCharProvider::new(input);
        let lines = test_lines();
        let err = parse_character_class(&lines, &mut line, CharacterMode::Utf8, false);
        assert!(err.is_err());
    }

    #[test]
    fn test_open_bracket_at_eol_errors() {
        let mut line = ScriptCharProvider::new("[");
        let lines = test_lines();
        let err = parse_character_class(&lines, &mut line, CharacterMode::Utf8, false).unwrap_err();
        assert!(err.to_string().contains("Unterminated bracket expression"));
    }

    // parse_regex
    #[rstest]
    #[case::simple_regex("/abc/", RegexMode::Basic, b"abc", '/')]
    #[case::escaped_delimiter("/ab\\/c/", RegexMode::Basic, b"ab/c", '/')]
    #[case::capture(r"/\(.\)/c/", RegexMode::Basic, br"\(.\)", '/')]
    #[case::escape_sequence("/ab\\n/", RegexMode::Basic, b"ab\n", '/')]
    #[case::basic_regex_quantifier("/a\\{2,3\\}/p", RegexMode::Basic, br"a\{2,3\}", '/')]
    #[case::extended_regex_quantifier("/a{2,3}/p", RegexMode::Extended, b"a{2,3}", '/')]
    #[case::character_class("/[a-z]/", RegexMode::Basic, b"[a-z]", '/')]
    #[case::bracket_delimiter("[abc[", RegexMode::Basic, b"abc", '[')]
    #[case::bracket_regex_with_bracket_delimiter(
        "[a\\[0-9]bc[",
        RegexMode::Basic,
        b"a[0-9]bc",
        '['
    )]
    // The class ends at the `]`, leaving `z]` as literal characters.
    #[case::escaped_bracket_in_character_class("/[a\\]z]/", RegexMode::Basic, br"[a\\]z]", '/')]
    #[case::delimiter_inside_character_class("/[a/c]/", RegexMode::Basic, b"[a/c]", '/')]
    #[case::escaped_paren_and_backslash("/\\(\\\\/", RegexMode::Basic, br"\(\\", '/')]
    fn test_parse_regex(
        #[case] input: &str,
        #[case] mode: RegexMode,
        #[case] expected: &[u8],
        #[case] next: char,
    ) {
        let (lines, mut line) = make_providers(input);
        let parsed = parse_regex(&lines, &mut line, mode).unwrap();
        assert_eq!(parsed, expected);
        assert_eq!(line.current(), next);
    }

    #[rstest]
    #[case::basic_regex_with_unmatched_brace_quantifier(
        "/a\\{2,3/p",
        RegexMode::Basic,
        "Unmatched \\{"
    )]
    #[case::basic_regex_with_invalid_content(
        "/a\\{2d,3\\}/p",
        RegexMode::Basic,
        "Invalid content of \\{\\}"
    )]
    #[case::extended_regex_with_unmatched_brace_quantifier(
        "/a{2,3/p",
        RegexMode::Extended,
        "Unmatched \\{"
    )]
    #[case::extended_regex_with_empty_quantifier(
        "/a{}/p",
        RegexMode::Extended,
        "Invalid content of \\{\\}"
    )]
    #[case::extended_regex_with_whitespace_quantifier(
        "/a{}/p",
        RegexMode::Extended,
        "Invalid content of \\{\\}"
    )]
    #[case::extended_regex_with_invalid_m(
        "/a{2d,3}/p",
        RegexMode::Extended,
        "Invalid content of \\{\\}"
    )]
    #[case::extended_regex_with_invalid_n(
        "/a{2,-3}/p",
        RegexMode::Extended,
        "Invalid content of \\{\\}"
    )]
    #[case::extended_regex_with_m_gt_n(
        "/a{3,2}/p",
        RegexMode::Extended,
        "Invalid content of \\{\\}"
    )]
    #[case::unterminated_regex(
        "/unterminated",
        RegexMode::Basic,
        "unterminated regular expression"
    )]
    #[case::esc_at_re_eol("/foo\\", RegexMode::Basic, "unterminated regular expression")]
    #[case::backslash_delimiter(
        "\\bad",
        RegexMode::Basic,
        "\\ cannot be used as a string delimiter"
    )]
    fn test_parse_regex_error(#[case] input: &str, #[case] mode: RegexMode, #[case] message: &str) {
        let (lines, mut line) = make_providers(input);
        let err = parse_regex(&lines, &mut line, mode).unwrap_err();
        assert!(err.to_string().contains(message));
    }

    // parse_regex_for_mode: GNU extensions under --posix
    // In BREs, bre_to_ere() escapes the ERE metacharacters, so they are left
    // bare. In EREs \? \+ \| are already literals, so the backslash stays.
    #[rstest]
    #[case::posix_bre_emits_bare_metacharacters(r"/0\?a\+b\|/", RegexMode::Basic, b"0?a+b|")]
    #[case::posix_bre_emits_bare_gnu_escapes(r"/\w\s\b\<\`/", RegexMode::Basic, b"wsb<`")]
    #[case::posix_ere_keeps_metacharacters_escaped(
        r"/a\?b\+c\|/",
        RegexMode::Extended,
        br"a\?b\+c\|"
    )]
    #[case::posix_ere_emits_bare_gnu_escapes(r"/\w\s\b/", RegexMode::Extended, b"wsb")]
    fn test_parse_regex_for_mode_posix(
        #[case] input: &str,
        #[case] mode: RegexMode,
        #[case] expected: &[u8],
    ) {
        let (lines, mut line) = make_providers(input);
        let parsed =
            parse_regex_for_mode(&lines, &mut line, mode, CharacterMode::Utf8, true).unwrap();
        assert_eq!(parsed, expected);
    }

    // validate_quantifier_structure
    #[rstest]
    #[case::bre_valid("{2,3\\}", RegexMode::Basic)]
    #[case::ere_valid("{2,3}", RegexMode::Extended)]
    fn test_validate_quantifier_structure(#[case] input: &str, #[case] mode: RegexMode) {
        let (lines, mut line) = make_providers(input);
        validate_quantifier_structure(&lines, &mut line, '/', mode).unwrap();
        assert_eq!(line.current(), '{'); // Line should be back on the opening brace
    }

    #[rstest]
    #[case::bre_with_unmatched_brace("{2,3", RegexMode::Basic, "Unmatched \\{")]
    #[case::bre_with_empty_content("{\\}", RegexMode::Basic, "Invalid content of \\{\\}")]
    #[case::bre_with_invalid_char("{2d,3\\}", RegexMode::Basic, "Invalid content of \\{\\}")]
    #[case::bre_with_double_comma("{2,3,\\}", RegexMode::Basic, "Invalid content of \\{\\}")]
    #[case::ere_with_unmatched_brace("{2,3", RegexMode::Extended, "Unmatched \\{")]
    #[case::ere_with_empty_content("{}", RegexMode::Extended, "Invalid content of \\{\\}")]
    #[case::ere_with_invalid_char("{2d,3}", RegexMode::Extended, "Invalid content of \\{\\}")]
    #[case::ere_with_double_comma("{2,3,}", RegexMode::Extended, "Invalid content of \\{\\}")]
    fn test_validate_quantifier_structure_error(
        #[case] input: &str,
        #[case] mode: RegexMode,
        #[case] message: &str,
    ) {
        let (lines, mut line) = make_providers(input);
        let err = validate_quantifier_structure(&lines, &mut line, '/', mode).unwrap_err();
        assert!(err.to_string().contains(message));
    }

    // validate_quantifier_numbers
    #[rstest]
    #[case::with_m("{2}", "2")]
    #[case::with_single_comma("{,}", "0,")]
    #[case::with_comma_n("{,3}", "0,3")]
    #[case::valid("{2,3}", "2,3")]
    fn test_validate_quantifier_numbers(#[case] input: &str, #[case] expected: &str) {
        let (lines, mut line) = make_providers(input);
        let result = validate_quantifier_numbers(&lines, &mut line).unwrap();
        assert_eq!(result, expected);
        assert_eq!(line.current(), '}');
    }

    #[rstest]
    #[case::with_m_too_big("{32768}", "Regular expression too big")]
    #[case::with_n_too_big("{2,32768}", "Regular expression too big")]
    #[case::with_m_gt_n("{3,2}", "Invalid content of \\{\\}")]
    // The {,n} form must bound n by RE_DUP_MAX just like {m,n}.
    #[case::with_leading_comma_n_too_big("{,32768}", "Regular expression too big")]
    // A digit run too large for usize is reported as too big, not as
    // invalid content.
    #[case::with_overflowing_m("{99999999999999999999999}", "Regular expression too big")]
    fn test_validate_quantifier_numbers_error(#[case] input: &str, #[case] message: &str) {
        let (lines, mut line) = make_providers(input);
        let err = validate_quantifier_numbers(&lines, &mut line).unwrap_err();
        assert!(err.to_string().contains(message));
    }

    // parse_transliteration
    #[rstest]
    #[case::simple("/abc/", b"abc")]
    #[case::escaped_delimiter("/ab\\/c/", b"ab/c")]
    #[case::escaped_backslash("/ab\\\\c/", br"ab\c")]
    #[case::escape_sequence("/ab\\n/", b"ab\n")]
    fn test_parse_transliteration(#[case] input: &str, #[case] expected: &[u8]) {
        let (lines, mut line) = make_providers(input);
        let parsed = parse_transliteration(&lines, &mut line).unwrap();
        assert_eq!(parsed, expected);
        assert_eq!(line.current(), '/');
    }

    #[test]
    fn test_transliteration_backslash_character() {
        let (lines, mut line) = make_providers("/\\\\/");
        let parsed = parse_transliteration_bytes(&lines, &mut line, CharacterMode::Utf8).unwrap();
        assert_eq!(parsed, br"\");
        assert_eq!(line.current(), '/');
    }

    #[rstest]
    #[case::bytes(CharacterMode::Byte, ParsedTransliteration::Bytes(b"a\xE9".to_vec()))]
    #[case::utf8(CharacterMode::Utf8, ParsedTransliteration::Text("aé".to_string()))]
    fn test_parse_transliteration_for_mode(
        #[case] mode: CharacterMode,
        #[case] expected: ParsedTransliteration,
    ) {
        let (lines, mut line) = make_providers("/a\\xE9/");
        let parsed = parse_transliteration_for_mode(&lines, &mut line, mode).unwrap();
        assert_eq!(parsed, expected);
        assert_eq!(line.current(), '/');
    }

    #[rstest]
    #[case::unterminated_transliteration("/unterminated")]
    #[case::esc_at_tr_eol("/foo\\")]
    fn test_parse_transliteration_error(#[case] input: &str) {
        let (lines, mut line) = make_providers(input);
        let err = parse_transliteration(&lines, &mut line).unwrap_err();
        assert!(
            err.to_string()
                .contains("unterminated transliteration string")
        );
    }
}
