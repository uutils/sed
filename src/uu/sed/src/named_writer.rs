// An abstraction for output files created on entry and flushed on exit
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::error_handling::{ScriptLocation, runtime_error};

use std::cell::RefCell;
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::rc::Rc;

use uucore::display::Quotable;
use uucore::error::UResult;

thread_local! {
    /// Global list of all writers that should be flushed at shutdown
    static FLUSH_LIST: RefCell<Vec<Rc<RefCell<NamedWriter>>>> = const { RefCell::new(Vec::new()) };
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

        FLUSH_LIST.with(|list| list.borrow_mut().push(Rc::clone(&writer)));
        Ok(writer)
    }

    /// Write a line to the file with a newline, returning descriptive errors.
    pub fn write_line(&mut self, line: &str) -> UResult<()> {
        writeln!(self.writer, "{line}").map_err(|e| {
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

/// Flush buffered content to the file, returning descriptive errors.
pub fn flush_all() -> UResult<()> {
    FLUSH_LIST.with(|cell| {
        for handle in cell.borrow().iter() {
            handle.borrow_mut().flush()?;
        }

        Ok(())
    })
}
