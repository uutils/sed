// Provide the script contents byte by byte
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

#[derive(Debug)]
pub struct ScriptCharProvider {
    line: Vec<u8>,
    pos: usize,
}

impl ScriptCharProvider {
    /// Build from raw script bytes.
    pub fn new(line: impl AsRef<[u8]>) -> Self {
        Self {
            line: line.as_ref().to_vec(),
            pos: 0,
        }
    }

    /// Advances to the next byte, if not at end of line.
    pub fn advance(&mut self) {
        if self.pos < self.line.len() {
            self.pos += 1;
        }
    }

    /// Retreats current position by specified number or to beginning.
    pub fn retreat(&mut self, n: usize) {
        self.pos = self.pos.saturating_sub(n);
    }

    /// Sets new current position.
    pub fn set_position(&mut self, pos: usize) {
        self.pos = pos;
    }

    /// Returns the current byte as a character. Panics if out of bounds.
    pub fn current(&self) -> char {
        char::from(self.line[self.pos])
    }

    /// Returns the current script byte. Panics if out of bounds.
    pub fn current_byte(&self) -> u8 {
        self.line[self.pos]
    }

    /// Returns true if at the end of the line.
    pub fn eol(&self) -> bool {
        self.pos >= self.line.len()
    }

    /// Advances the position past any whitespace characters.
    pub fn eat_spaces(&mut self) {
        while self.pos < self.line.len() && self.current().is_whitespace() {
            self.advance();
        }
    }

    /// Return current position
    pub fn get_pos(&self) -> usize {
        self.pos
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_navigation() {
        let mut provider = ScriptCharProvider::new("abc");
        assert_eq!(provider.get_pos(), 0);
        assert_eq!(provider.current(), 'a');
        provider.advance();
        assert_eq!(provider.get_pos(), 1);
        assert_eq!(provider.current(), 'b');
        provider.advance();
        assert_eq!(provider.get_pos(), 2);
        assert_eq!(provider.current(), 'c');
        provider.advance();
        assert_eq!(provider.get_pos(), 3);
        assert!(provider.eol());
    }

    #[test]
    #[should_panic]
    fn test_current_panics_out_of_bounds() {
        let mut provider = ScriptCharProvider::new("x");
        provider.advance(); // now at end
        provider.current(); // should panic
    }

    #[test]
    fn test_eat_spaces() {
        let mut provider = ScriptCharProvider::new("   xyz");
        provider.eat_spaces();
        assert_eq!(provider.current(), 'x');
    }

    #[test]
    fn test_eol_on_empty() {
        let provider = ScriptCharProvider::new("");
        assert!(provider.eol());
    }

    #[test]
    fn test_eat_spaces_mixed() {
        let mut provider = ScriptCharProvider::new("  \t\nabc");
        provider.eat_spaces();
        assert_eq!(provider.current(), 'a');
    }

    #[test]
    fn test_retreat_normal() {
        let mut chars = ScriptCharProvider::new("abcdef");
        chars.pos = 4; // simulate position at 'e'
        chars.retreat(2);

        assert_eq!(chars.get_pos(), 2);
        assert_eq!(chars.current(), 'c');
    }

    #[test]
    fn test_retreat_to_start() {
        let mut chars = ScriptCharProvider::new("abcdef");
        chars.pos = 3; // simulate position at 'd'
        chars.retreat(5); // retreat more than current pos

        assert_eq!(chars.get_pos(), 0);
        assert_eq!(chars.current(), 'a');
    }

    #[test]
    fn test_retreat_zero() {
        let mut chars = ScriptCharProvider::new("abcdef");
        chars.pos = 2; // at 'c'
        chars.retreat(0); // retreat by 0

        assert_eq!(chars.get_pos(), 2);
        assert_eq!(chars.current(), 'c');
    }

    #[test]
    fn test_current_for_utf8_bytes() {
        let mut chars = ScriptCharProvider::new("αb");

        assert_eq!(chars.current(), char::from(0xCE));
        assert_eq!(chars.current_byte(), 0xCE);
        chars.advance();
        assert_eq!(chars.current(), char::from(0xB1));
        assert_eq!(chars.current_byte(), 0xB1);
        chars.advance();
        assert_eq!(chars.current(), 'b');
    }

    #[test]
    fn test_current_for_invalid_utf8_byte() {
        let mut chars = ScriptCharProvider::new(vec![0xC2, b'a']);

        assert_eq!(chars.current(), char::from(0xC2));
        assert_eq!(chars.current_byte(), 0xC2);
        chars.advance();
        assert_eq!(chars.current(), 'a');
        assert_eq!(chars.current_byte(), b'a');
    }
}
