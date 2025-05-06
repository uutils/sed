// Line-based I/O from multiple input files to multiple output files
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{ProcessingContext, ProcessingOptions};
use crate::fast_io::{LineReader, OutputBuffer, OutputChunk, OutputChunkRef};
use std::io::{self, stdout};
use std::path::PathBuf;
use uucore::error::UResult;

impl ProcessingContext {
    /// Create a new `ProcessingContext` taking ownership of processing_options
    pub fn new(input_files: Vec<PathBuf>, processing_options: ProcessingOptions) -> UResult<Self> {
        let first = input_files.first().expect("input_files must be non-empty");
        // Open the reader on the first path.
        let reader = LineReader::open(first)?;

        // TODO: Handle in-place editing of first file
        let output = OutputBuffer::new(Box::new(stdout()));

        Ok(ProcessingContext {
            reader,
            output,
            input_files,
            processing_options,
        })
    }

    /// Return the next line, if available, or None.
    pub fn get_line(&mut self) -> io::Result<Option<OutputChunkRef>> {
        // TODO: Handle iterating over all files
        self.reader.get_line()
    }

    /// Schedule the specified output chunk for eventual output
    pub fn write_chunk(&mut self, chunk: &OutputChunk) -> io::Result<()> {
        self.output.write_chunk(chunk)
    }

    pub fn flush(&mut self) -> io::Result<()> {
        self.output.flush()
    }
}
