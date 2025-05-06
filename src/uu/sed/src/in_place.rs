// Support for in-place editing
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::ProcessingOptions;
use crate::fast_io::OutputBuffer;
use std::io::stdout;
use std::path::Path;
use uucore::error::UResult;

/// Context for in-place editing
pub struct InPlace<'opts> {
    pub output: OutputBuffer,
    pub processing_options: &'opts ProcessingOptions,
}

impl<'opts> InPlace<'opts> {
    /// Create a new `ProcessingContext` taking ownership of processing_options
    pub fn new(processing_options: &'opts ProcessingOptions) -> UResult<Self> {
        let output = OutputBuffer::new(Box::new(stdout()));

        Ok(InPlace {
            output,
            processing_options,
        })
    }

    /// Return an OutputBuffer for outputting the edits to the specified file.
    pub fn begin(&mut self, _file_name: &Path) -> UResult<&mut OutputBuffer> {
        // TODO: Adjust output for in-place editing, if needed.
        Ok(&mut self.output)
    }

    /// Finish in-place editing.
    pub fn end(&mut self) -> UResult<()> {
        self.output.flush()?;
        // TODO: Rename and delete output file, if needed.
        Ok(())
    }
}
