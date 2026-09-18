// An abstraction for input files read one line at a time by the `R` command
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use std::cell::RefCell;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug)]
/// State of the file backing an `R` command, opened lazily on first use.
enum State {
    Unopened,
    Open(BufReader<File>),
    Exhausted,
}

#[derive(Debug)]
/// Reader that yields successive lines of a file for the GNU `R` command.
/// The file is opened on first use; a file that cannot be opened or read is
/// treated as having no more lines, matching GNU sed (no error is raised).
pub struct NamedReader {
    path: PathBuf,
    state: State,
}

impl NamedReader {
    /// Create a reader for `path` without opening it yet.
    pub fn new(path: PathBuf) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(NamedReader {
            path,
            state: State::Unopened,
        }))
    }

    /// Return the next line of the file, including its trailing newline if
    /// present, or `None` once the file is exhausted or could not be read.
    pub fn next_line(&mut self) -> Option<Vec<u8>> {
        if matches!(self.state, State::Unopened) {
            self.state = match File::open(&self.path) {
                Ok(file) => State::Open(BufReader::new(file)),
                Err(_) => State::Exhausted,
            };
        }

        let State::Open(reader) = &mut self.state else {
            return None;
        };

        let mut line = Vec::new();
        match reader.read_until(b'\n', &mut line) {
            Ok(0) | Err(_) => {
                self.state = State::Exhausted;
                None
            }
            Ok(_) => Some(line),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn yields_successive_lines_then_none() {
        let mut file = NamedTempFile::new().unwrap();
        file.write_all(b"one\ntwo\n").unwrap();
        let reader = NamedReader::new(file.path().to_path_buf());

        assert_eq!(reader.borrow_mut().next_line(), Some(b"one\n".to_vec()));
        assert_eq!(reader.borrow_mut().next_line(), Some(b"two\n".to_vec()));
        assert_eq!(reader.borrow_mut().next_line(), None);
        assert_eq!(reader.borrow_mut().next_line(), None);
    }

    #[test]
    fn last_line_without_newline_is_preserved() {
        let mut file = NamedTempFile::new().unwrap();
        file.write_all(b"abc").unwrap();
        let reader = NamedReader::new(file.path().to_path_buf());

        assert_eq!(reader.borrow_mut().next_line(), Some(b"abc".to_vec()));
        assert_eq!(reader.borrow_mut().next_line(), None);
    }

    #[test]
    fn missing_file_yields_no_lines() {
        let reader = NamedReader::new(PathBuf::from("/nonexistent/xyzzy-42-does-not-exist"));
        assert_eq!(reader.borrow_mut().next_line(), None);
    }
}
