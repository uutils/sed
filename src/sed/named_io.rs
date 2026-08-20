// An abstraction for output files created on entry and flushed on exit
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::sed::error_handling::{ScriptLocation, runtime_error};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use uucore::display::Quotable;
use uucore::error::UResult;

thread_local! {
    /// Readers indexed by canonical input path, used to share state
    /// between same-named path reads.
    static READERS: RefCell<HashMap<PathBuf, Rc<RefCell<NamedReader>>>> = RefCell::new(HashMap::new());
    /// Writers indexed by canonical output path, used to share same-named
    /// paths and to flush writes.
    static WRITERS: RefCell<HashMap<PathBuf, Rc<RefCell<NamedWriter>>>> = RefCell::new(HashMap::new());
}

#[derive(Debug)]
/// Reader that shares line-by-line state for GNU sed's R command.
pub struct NamedReader {
    path: PathBuf,
    reader: Option<BufReader<File>>,
    done: bool,
}

impl NamedReader {
    /// Create or retrieve the reader associated with `path`.
    pub fn new(path: PathBuf) -> Rc<RefCell<Self>> {
        let canonical_path = fs::canonicalize(&path).unwrap_or(path);
        READERS.with(|readers| {
            readers
                .borrow_mut()
                .entry(canonical_path.clone())
                .or_insert_with(|| {
                    Rc::new(RefCell::new(Self {
                        path: canonical_path,
                        reader: None,
                        done: false,
                    }))
                })
                .clone()
        })
    }

    /// Return the path associated with this reader.
    pub fn original_path(&self) -> &Path {
        &self.path
    }

    /// Read the next line, including its newline. Missing files and read errors
    /// are treated as end-of-file, as required by the R command.
    pub fn read_line(&mut self) -> Option<Vec<u8>> {
        if self.done {
            return None;
        }

        if self.reader.is_none() {
            if let Ok(file) = File::open(&self.path) {
                self.reader = Some(BufReader::new(file));
            } else {
                self.done = true;
                return None;
            }
        }

        let mut line = Vec::new();
        match self.reader.as_mut().unwrap().read_until(b'\n', &mut line) {
            Ok(0) | Err(_) => {
                self.done = true;
                None
            }
            Ok(_) => Some(line),
        }
    }
}

#[derive(Debug)]
/// Writer that tracks its file name for better error messages
pub struct NamedWriter {
    pub path: PathBuf,
    writer: BufWriter<File>,
    location: ScriptLocation,
}

impl NamedWriter {
    /// Create a new writer, truncate the file, and register it for flushing.
    pub fn new(path: PathBuf, location: ScriptLocation) -> UResult<Rc<RefCell<Self>>> {
        let canonical_path = canonicalize_output_path(&path, &location)?;

        if let Some(writer) = WRITERS.with(|writers| writers.borrow().get(&canonical_path).cloned())
        {
            return Ok(writer);
        }

        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&path)
            .map_err(|e| {
                runtime_error::<()>(&location, format!("creating file {}: {}", path.quote(), e))
                    .unwrap_err()
            })?;

        let writer = Rc::new(RefCell::new(NamedWriter {
            path,
            writer: BufWriter::new(file),
            location,
        }));

        WRITERS.with(|writers| {
            writers
                .borrow_mut()
                .insert(canonical_path, Rc::clone(&writer));
        });
        Ok(writer)
    }

    /// Return the path used when this writer was first opened.
    pub fn original_path(&self) -> &Path {
        &self.path
    }

    /// Write String to the file, possibly with a newline, returning errors.
    pub fn write_line(&mut self, line: &str, newline: bool) -> UResult<()> {
        self.write_line_bytes(line.as_bytes(), newline)
    }

    /// Write bytes to the file, possibly with a newline, returning errors.
    pub fn write_line_bytes(&mut self, line: &[u8], newline: bool) -> UResult<()> {
        self.writer
            .write_all(line)
            .and_then(|()| {
                if newline {
                    self.writer.write_all(b"\n")
                } else {
                    Ok(())
                }
            })
            .map_err(|e| {
                runtime_error::<()>(
                    &self.location,
                    format!("writing to file {}: {e}", self.path.quote()),
                )
                .unwrap_err()
            })
    }

    /// Flush the writer, returning a descriptive error.
    pub fn flush(&mut self) -> UResult<()> {
        self.writer.flush().map_err(|e| {
            runtime_error::<()>(
                &self.location,
                format!("writing to file {}: {}", self.path.quote(), e),
            )
            .unwrap_err()
        })
    }
}

fn canonicalize_output_path(path: &Path, location: &ScriptLocation) -> UResult<PathBuf> {
    let parent = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let canonical_parent = fs::canonicalize(parent).map_err(|e| {
        runtime_error::<()>(location, format!("creating file {}: {}", path.quote(), e)).unwrap_err()
    })?;

    Ok(match path.file_name() {
        Some(file_name) => canonical_parent.join(file_name),
        None => canonical_parent,
    })
}

/// Flush buffered content to all open files, returning descriptive errors.
pub fn flush_all() -> UResult<()> {
    WRITERS.with(|writers| {
        for writer in writers.borrow().values() {
            writer.borrow_mut().flush()?;
        }

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::{NamedTempFile, tempdir};

    #[test]
    fn test_reader_reads_lines_as_bytes() {
        let file = NamedTempFile::new().unwrap();
        let path = file.path().to_path_buf();
        fs::write(&path, b"first\nsecond\xE9").unwrap();
        let reader = NamedReader::new(path);

        assert_eq!(reader.borrow_mut().read_line(), Some(b"first\n".to_vec()));
        assert_eq!(
            reader.borrow_mut().read_line(),
            Some(b"second\xE9".to_vec())
        );
        assert_eq!(reader.borrow_mut().read_line(), None);
        assert_eq!(reader.borrow_mut().read_line(), None);
    }

    #[test]
    fn test_new_reuses_reader_and_shared_position_for_same_path() {
        let file = NamedTempFile::new().unwrap();
        let path = file.path().to_path_buf();
        fs::write(&path, b"first\nsecond\n").unwrap();
        let first = NamedReader::new(path.clone());
        let second = NamedReader::new(path);

        assert!(Rc::ptr_eq(&first, &second));
        assert_eq!(first.borrow_mut().read_line(), Some(b"first\n".to_vec()));
        assert_eq!(second.borrow_mut().read_line(), Some(b"second\n".to_vec()));
    }

    #[test]
    fn test_new_reuses_reader_for_canonical_duplicate_path() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("input");
        fs::write(&path, b"first\nsecond\n").unwrap();
        let duplicate_path = dir.path().join(".").join("input");
        let first = NamedReader::new(path.clone());
        let second = NamedReader::new(duplicate_path);

        assert!(Rc::ptr_eq(&first, &second));
        assert_eq!(
            first.borrow().original_path(),
            fs::canonicalize(path).unwrap()
        );
        assert_eq!(first.borrow_mut().read_line(), Some(b"first\n".to_vec()));
        assert_eq!(second.borrow_mut().read_line(), Some(b"second\n".to_vec()));
    }

    #[test]
    fn test_reader_silently_ignores_missing_file() {
        let dir = tempdir().unwrap();
        let reader = NamedReader::new(dir.path().join("missing"));

        assert_eq!(reader.borrow_mut().read_line(), None);
        assert_eq!(reader.borrow_mut().read_line(), None);
    }

    #[test]
    fn test_write_line_bytes_appends_newline() {
        let file = NamedTempFile::new().unwrap();
        let path = file.path().to_path_buf();
        let writer = NamedWriter::new(path.clone(), ScriptLocation::default()).unwrap();

        writer
            .borrow_mut()
            .write_line_bytes(b"a\xE9", true)
            .unwrap();
        writer.borrow_mut().flush().unwrap();

        assert_eq!(fs::read(path).unwrap(), b"a\xE9\n");
    }

    #[test]
    fn test_write_line_bytes_appends_no_newline() {
        let file = NamedTempFile::new().unwrap();
        let path = file.path().to_path_buf();
        let writer = NamedWriter::new(path.clone(), ScriptLocation::default()).unwrap();

        writer
            .borrow_mut()
            .write_line_bytes(b"a\xE9", false)
            .unwrap();
        writer.borrow_mut().flush().unwrap();

        assert_eq!(fs::read(path).unwrap(), b"a\xE9");
    }

    #[test]
    fn test_new_reuses_writer_for_same_path() {
        let file = NamedTempFile::new().unwrap();
        let path = file.path().to_path_buf();
        let first = NamedWriter::new(path.clone(), ScriptLocation::default()).unwrap();
        let second = NamedWriter::new(path.clone(), ScriptLocation::default()).unwrap();

        assert!(Rc::ptr_eq(&first, &second));
        assert_eq!(first.borrow().original_path(), path.as_path());

        first.borrow_mut().write_line("first", true).unwrap();
        second.borrow_mut().write_line("second", true).unwrap();
        first.borrow_mut().flush().unwrap();

        assert_eq!(fs::read_to_string(path).unwrap(), "first\nsecond\n");
    }

    #[test]
    fn test_new_reuses_writer_for_canonical_duplicate_path() {
        let dir = tempdir().unwrap();
        let subdir = dir.path().join("subdir");
        fs::create_dir(&subdir).unwrap();

        let path = subdir.join("output");
        let duplicate_path = subdir.join(".").join("output");
        let first = NamedWriter::new(path.clone(), ScriptLocation::default()).unwrap();
        let second = NamedWriter::new(duplicate_path, ScriptLocation::default()).unwrap();

        assert!(Rc::ptr_eq(&first, &second));
        assert_eq!(first.borrow().original_path(), path.as_path());
    }
}
