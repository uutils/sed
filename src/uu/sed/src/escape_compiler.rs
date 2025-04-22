// Compile escaped character sequences
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::script_char_provider::ScriptCharProvider;
use std::char;

/// Return true if c is a valid octal digit
fn is_ascii_octal_digit(c: char) -> bool {
    matches!(c, '0'..='7')
}

/// Compile a numeric character escape and return the corresponding char.
/// Advance line to the first character not part of the escape.
/// ndigits is the maximum number of allowed digits and radix is the value's
/// radix (e.g. 8, 10, 16 for octal, decimal, and hex escapes).
/// Return `None` if no valid character has been specified.
fn compile_numeric_escape(
    line: &mut ScriptCharProvider,
    is_allowed_char: fn(char) -> bool,
    ndigits: u32,
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

    let char_string: String = valid_chars.into_iter().collect();
    match u32::from_str_radix(&char_string, radix)
        .ok()
        .and_then(char::from_u32)
    {
        Some(decoded) => Some(decoded),
        None => panic!("Unable to decode numeric character escape."),
    }
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

    let mut c = x;
    if c.is_ascii_lowercase() {
        c = c.to_ascii_uppercase();
    }

    let transformed = (c as u8) ^ 0x40;
    char::from_u32(transformed as u32)
}

/// Compile a character escape valid in all contexts (RE pattern, substitution,
/// transliterarion) and return the corresponding char.
/// Advance line to the first character not part of the escape.
/// Return `None` if an invalid escape has been specified.
pub fn compile_char_escape(line: &mut ScriptCharProvider) -> Option<char> {
    match line.current() {
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
            match compile_numeric_escape(line, |c| c.is_ascii_digit(), 3, 10) {
                Some(decoded) => Some(decoded),
                None => Some('d'),
            }
        }

        'o' => {
            // Octal escape: \onnn
            line.advance(); // move past 'o'
            match compile_numeric_escape(line, is_ascii_octal_digit, 3, 8) {
                Some(decoded) => Some(decoded),
                None => Some('o'),
            }
        }

        'x' => {
            // Hexadecimal escape: \xnn
            line.advance(); // move past 'x'
            match compile_numeric_escape(line, |c| c.is_ascii_hexdigit(), 2, 16) {
                Some(decoded) => Some(decoded),
                None => Some('x'),
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // compile_numeric_escape
    #[test]
    fn test_compile_octal_escape() {
        let mut provider = ScriptCharProvider::new("141rest");
        let c = compile_numeric_escape(&mut provider, is_ascii_octal_digit, 3, 8);
        assert_eq!(c, Some('a'));
        assert_eq!(provider.current(), 'r'); // "141" was consumed
    }

    #[test]
    fn test_compile_octal_escape_eol() {
        let mut provider = ScriptCharProvider::new("141");
        let c = compile_numeric_escape(&mut provider, is_ascii_octal_digit, 3, 8);
        assert_eq!(c, Some('a'));
        assert!(provider.eol()); // "141" was consumed
    }

    #[test]
    fn test_compile_decimal_escape() {
        let mut provider = ScriptCharProvider::new("0659");
        let c = compile_numeric_escape(&mut provider, |c| c.is_ascii_digit(), 3, 10);
        assert_eq!(c, Some('A'));
        assert_eq!(provider.current(), '9'); // "65" was consumed
    }

    #[test]
    fn test_compile_hex_escape() {
        let mut provider = ScriptCharProvider::new("3cZ");
        let c = compile_numeric_escape(&mut provider, |c| c.is_ascii_hexdigit(), 2, 16);
        assert_eq!(c, Some('<'));
        assert_eq!(provider.current(), 'Z'); // "41" was consumed
    }

    #[test]
    fn test_compile_hex_escape_truncated() {
        let mut provider = ScriptCharProvider::new("4G");
        let c = compile_numeric_escape(&mut provider, |c| c.is_ascii_hexdigit(), 2, 16);
        assert_eq!(c, Some('\u{4}')); // Only '4' is valid hex
        assert_eq!(provider.current(), 'G'); // "41" was consumed
    }

    #[test]
    fn test_no_valid_digits() {
        let mut provider = ScriptCharProvider::new("xyz");
        let c = compile_numeric_escape(&mut provider, |c| c.is_ascii_digit(), 3, 10);
        assert_eq!(c, None);
        assert_eq!(provider.current(), 'x'); // No advancement
    }

    // create_control_char
    #[test]
    fn test_lowercase_letter() {
        assert_eq!(create_control_char('z'), Some('\u{1a}')); // 0x5A ^ 0x40 = 0x1A
        assert_eq!(create_control_char('a'), Some('\u{01}')); // 0x41 ^ 0x40 = 0x01
    }

    #[test]
    fn test_uppercase_letter() {
        assert_eq!(create_control_char('Z'), Some('\u{1a}'));
        assert_eq!(create_control_char('A'), Some('\u{01}'));
    }

    #[test]
    fn test_symbol_characters() {
        assert_eq!(create_control_char('{'), Some(';')); // 0x7B ^ 0x40 = 0x3B
        assert_eq!(create_control_char(';'), Some('{')); // 0x3B ^ 0x40 = 0x7B
    }

    #[test]
    fn test_non_ascii_char() {
        // This will not match any transformation and may panic if it overflows
        // But the current function only handles ASCII-safe chars
        assert_eq!(create_control_char('é'), None); // outside ASCII
    }

    #[test]
    fn test_edge_ascii_values() {
        assert_eq!(create_control_char('@'), Some('\0')); // 0x40 ^ 0x40 = 0x00
        assert_eq!(create_control_char('\x7F'), Some('\x3F')); // 0x7F ^ 0x40 = 0x3F
    }

    // compile_char_escape
    fn escape_result_with_current(input: &str) -> (Option<char>, Option<char>) {
        let mut provider = ScriptCharProvider::new(input);
        let result = compile_char_escape(&mut provider);
        let current = if provider.eol() {
            None
        } else {
            Some(provider.current())
        };
        (result, current)
    }
    #[test]

    fn test_standard_escapes() {
        assert_eq!(escape_result_with_current("a"), (Some('\x07'), None));
        assert_eq!(escape_result_with_current("f"), (Some('\x0c'), None));
        assert_eq!(escape_result_with_current("n"), (Some('\n'), None));
        assert_eq!(escape_result_with_current("r"), (Some('\r'), None));
        assert_eq!(escape_result_with_current("t"), (Some('\t'), None));
        assert_eq!(escape_result_with_current("v"), (Some('\x0b'), None));
    }

    #[test]
    fn test_escape_invalid() {
        assert_eq!(escape_result_with_current("zx"), (None, Some('z')));
    }

    #[test]
    fn test_control_escape_valid() {
        assert_eq!(escape_result_with_current("cZ"), (Some('\x1A'), None));
    }

    #[test]
    fn test_control_escape_invalid() {
        assert_eq!(escape_result_with_current("cé"), (Some('c'), Some('é')));
    }

    #[test]
    fn test_decimal_escape_valid() {
        assert_eq!(escape_result_with_current("d065r"), (Some('A'), Some('r')));
    }

    #[test]
    fn test_octal_escape_valid() {
        assert_eq!(escape_result_with_current("o141x"), (Some('a'), Some('x')));
    }

    #[test]
    fn test_hex_escape_valid() {
        assert_eq!(escape_result_with_current("x41;"), (Some('A'), Some(';')));
    }

    #[test]
    fn test_decimal_escape_fallback() {
        assert_eq!(escape_result_with_current("d;."), (Some('d'), Some(';')));
    }

    #[test]
    fn test_octal_escape_fallback() {
        assert_eq!(escape_result_with_current("o9x"), (Some('o'), Some('9')));
    }

    #[test]
    fn test_hex_escape_fallback() {
        assert_eq!(escape_result_with_current("xyz"), (Some('x'), Some('y')));
    }

    #[test]
    fn test_unknown_escape() {
        assert_eq!(escape_result_with_current("q"), (None, Some('q')));
    }
}
