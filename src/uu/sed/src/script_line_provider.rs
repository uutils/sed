// Provide the script contents line by line
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum ScriptValue {
    StringVal(String),
    PathVal(PathBuf),
}

pub struct ScriptLineProvider {
    sources: Vec<ScriptValue>,
    state: State,
}

enum State {
    NotStarted,
    Active {
        index: usize,
        reader: Box<dyn BufRead>,
    },
    Done,
}

impl ScriptLineProvider {
    pub fn new(sources: Vec<ScriptValue>) -> Self {
        Self {
            sources,
            state: State::NotStarted,
        }
    }

    pub fn next_line(&mut self) -> io::Result<Option<String>> {
        let mut line = String::new();

        loop {
            let advance = match &mut self.state {
                State::NotStarted => {
                    Some(0)
                }
                State::Active { index, reader } => {
                    line.clear();
                    let bytes = reader.read_line(&mut line)?;
                    if bytes == 0 {
                        Some(*index + 1) // finished reading this source
                    } else {
                        return Ok(Some(line));
                    }
                }
                State::Done => {
                    return Ok(None);
                }
            };

            if let Some(next_index) = advance {
                self.advance_source(next_index)?;
            }
        }
    }

    fn advance_source(&mut self, next_index: usize) -> io::Result<()> {
        if next_index >= self.sources.len() {
            self.state = State::Done;
            return Ok(());
        }

        match &self.sources[next_index] {
            ScriptValue::StringVal(s) => {
                let cursor = std::io::Cursor::new(s.clone());
                self.state = State::Active {
                    index: next_index,
                    reader: Box::new(BufReader::new(cursor)),
                };
            }
            ScriptValue::PathVal(p) => {
                if p.to_string_lossy() == "-" {
                    self.state = State::Active {
                        index: next_index,
                        reader: Box::new(BufReader::new(io::stdin())),
                    };
                } else {
                    let file = File::open(p)?;
                    self.state = State::Active {
                        index: next_index,
                        reader: Box::new(BufReader::new(file)),
                    };
                }
            }
        }

        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;
    use std::io::Write;

    #[test]
    fn test_string_source() {
        let input = vec![
            ScriptValue::StringVal("line one\nline two\n".to_string()),
            ScriptValue::StringVal("line three".to_string()),
        ];
        let mut provider = ScriptLineProvider::new(input);

        let mut lines = Vec::new();
        while let Some(line) = provider.next_line().unwrap() {
            lines.push(line.trim_end().to_string());
        }

        assert_eq!(
            lines,
            vec!["line one", "line two", "line three"]
        );
    }

    #[test]
    fn test_file_source() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "file line 1").unwrap();
        writeln!(temp_file, "file line 2").unwrap();

        let input = vec![
            ScriptValue::PathVal(temp_file.path().to_path_buf()),
        ];
        let mut provider = ScriptLineProvider::new(input);

        let mut lines = Vec::new();
        while let Some(line) = provider.next_line().unwrap() {
            lines.push(line.trim_end().to_string());
        }

        assert_eq!(lines, vec!["file line 1", "file line 2"]);
    }

    #[test]
    fn test_mixed_source() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "file line 1").unwrap();
        writeln!(temp_file, "file line 2").unwrap();
        let temp_file2 = NamedTempFile::new().unwrap();

        let input = vec![
            ScriptValue::PathVal(temp_file.path().to_path_buf()),
            ScriptValue::StringVal("script line 1".to_string()),
            ScriptValue::PathVal(temp_file.path().to_path_buf()),
            ScriptValue::StringVal("".to_string()),
            ScriptValue::PathVal(temp_file2.path().to_path_buf()),
            ScriptValue::StringVal("other script line 1".to_string()),
        ];
        let mut provider = ScriptLineProvider::new(input);

        let mut lines = Vec::new();
        while let Some(line) = provider.next_line().unwrap() {
            lines.push(line.trim_end().to_string());
        }

        assert_eq!(lines, vec![
            "file line 1",
            "file line 2",
            "script line 1",
            "file line 1",
            "file line 2",
            "other script line 1",
        ]);
    }
}
