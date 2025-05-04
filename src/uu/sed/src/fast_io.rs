// Zero-copy line-based I/O
//
// Abstractions that allow file lines to be processed and output
// in mmapped memory space.  By coallescing output requests an
// efficient write(2) system call can be issued for them, bypassing
// the copy required for output through BufWriter.
// Search for "main" to see a usage example.
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

#[cfg(unix)]
use memmap2::Mmap;
use std::borrow::Cow;
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};
#[cfg(unix)]
use std::os::unix::io::AsRawFd;
use std::path::PathBuf;
#[cfg(unix)]
use uucore::libc::{c_void, write};

// Define two cursors for iterating over lines:
// - MmapLineCursor based on mmap(2),
// - ReadLineCursorbased on BufReader.

/// Cursor for zero-copy iteration over mmap’d file.
pub struct MmapLineCursor<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> MmapLineCursor<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    /// Return the next line, if available, or None.
    pub fn get_line(&mut self) -> io::Result<Option<(&[u8], &[u8])>> {
        if self.pos >= self.data.len() {
            return Ok(None);
        }

        let start = self.pos;
        let mut end = start;
        while end < self.data.len() && self.data[end] != b'\n' {
            end += 1;
        }

        if end < self.data.len() {
            end += 1; // include \n in full span
        }

        self.pos = end;
        let full_span = &self.data[start..end];
        let content = if full_span.ends_with(b"\n") {
            &full_span[..full_span.len() - 1]
        } else {
            full_span
        };

        Ok(Some((content, full_span)))
    }
}

/// Buffered line reader from any BufRead input.
pub struct ReadLineCursor<R: BufRead> {
    lines: std::io::Lines<R>,
}

impl<R: BufRead> ReadLineCursor<R> {
    pub fn new(reader: R) -> Self {
        Self {
            lines: reader.lines(),
        }
    }

    /// Return the next line, if available, or None.
    pub fn get_line(&mut self) -> io::Result<Option<(Cow<'_, str>, usize)>> {
        match self.lines.next() {
            Some(Ok(line)) => Ok(Some((Cow::Owned(line.clone()), line.len()))),
            Some(Err(e)) => Err(e),
            None => Ok(None),
        }
    }
}

/// Data to be written to a file. It can come from the mmapped
/// memory space, in which case it is tracked to allow coallescing
/// and bypassing BufWriter, or it can be other data from the process's
/// memory space.
#[derive(Debug)]
#[cfg(unix)]
pub enum OutputChunk<'a> {
    MmapInput {
        content: &'a [u8],   // Line without newline
        full_span: &'a [u8], // Line including original newline
    },
    Owned(Vec<u8>), // Line content without newline
}

#[cfg(unix)]
type OutputChunkRef<'a> = OutputChunk<'a>;

// The same as above for non-Unix platforms, which lack mmap(2)
#[cfg(not(unix))]
pub enum OutputChunk {
    Owned(Vec<u8>), // Line content without newline
}

#[cfg(not(unix))]
type OutputChunkRef = OutputChunk;

/// Unified reader that uses mmap when possible, falls back to buffered reading.
pub enum LineReader {
    #[cfg(unix)]
    MmapInput {
        mapped_file: Mmap, // A handle that can derive the mapped file slice
        cursor: MmapLineCursor<'static>,
    },
    ReadInput(ReadLineCursor<BufReader<Box<dyn Read>>>),
}

/// Return a LineReader that uses the ReadInput method fot the specified file.
fn line_reader_read_input(file: File) -> io::Result<LineReader> {
    let boxed: Box<dyn Read> = Box::new(file);
    let reader = BufReader::new(boxed);
    Ok(LineReader::ReadInput(ReadLineCursor::new(reader)))
}

impl LineReader {
    pub fn open(path: &PathBuf) -> io::Result<Self> {
        if path.as_os_str() == "-" {
            let stdin = io::stdin();
            let boxed: Box<dyn Read> = Box::new(stdin.lock());
            let reader = BufReader::new(boxed);
            return Ok(LineReader::ReadInput(ReadLineCursor::new(reader)));
        }

        let file = File::open(path)?;

        #[cfg(unix)]
        {
            match unsafe { Mmap::map(&file) } {
                Ok(mapped_file) => {
                    // SAFETY: mmap owns the data and lives in the same variant
                    let slice: &'static [u8] = unsafe {
                        std::slice::from_raw_parts(mapped_file.as_ptr(), mapped_file.len())
                    };
                    let cursor = MmapLineCursor::new(slice);
                    Ok(LineReader::MmapInput {
                        mapped_file,
                        cursor,
                    })
                }
                // Fallback to ReadInput
                Err(_) => line_reader_read_input(file),
            }
        }

        #[cfg(not(unix))]
        {
            line_reader_read_input(file)
        }
    }

    /// Return the next line, if available, or None.
    pub fn get_line(&mut self) -> io::Result<Option<OutputChunkRef>> {
        match self {
            #[cfg(unix)]
            LineReader::MmapInput { cursor, .. } => {
                if let Some((content, full_span)) = cursor.get_line()? {
                    Ok(Some(OutputChunk::MmapInput { content, full_span }))
                } else {
                    Ok(None)
                }
            }
            LineReader::ReadInput(cursor) => {
                if let Some((line, _)) = cursor.get_line()? {
                    Ok(Some(OutputChunk::Owned(line.into_owned().into_bytes())))
                } else {
                    Ok(None)
                }
            }
        }
    }
}

/// Abstraction for outputting data, potentially from the mmapped file
/// Outputs from mmapped data are coallesced and written via a write(2)
/// system call without any copying if worthwhile.
/// All other output is buffered and writen via BufWriter.
/// The generic argument W is used for obtaining the output when
/// testing.
pub struct OutputBuffer<W: Write> {
    out: BufWriter<W>,
    #[cfg(unix)]
    mmap_ptr: Option<(*const u8, usize)>,
}

/// Type to use for writing
// Example: DynOutputBuffer::new(Box::new(io::stdout().lock())
pub type DynOutputBuffer = OutputBuffer<Box<dyn Write>>;

#[cfg(unix)]
fn write_syscall(fd: i32, ptr: *const u8, len: usize) -> io::Result<()> {
    let ret = unsafe { write(fd, ptr as *const c_void, len) };
    if ret < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

/// Threshold to use buffered writes for output
// This is half the size of the BufWriter buffer.
#[cfg(unix)]
const MIN_DIRECT_WRITE: usize = 4096;

impl<W: Write> OutputBuffer<W> {
    pub fn new(w: W) -> Self {
        Self {
            out: BufWriter::new(w),
            #[cfg(unix)]
            mmap_ptr: None,
        }
    }

    /// Schedule the specified output chunk for eventual output
    pub fn write_chunk(&mut self, chunk: &OutputChunk) -> io::Result<()> {
        match chunk {
            #[cfg(unix)]
            OutputChunk::MmapInput { full_span, .. } => {
                let ptr = full_span.as_ptr();
                let len = full_span.len();

                if let Some((p, l)) = self.mmap_ptr {
                    // Coalesce if adjacent
                    if unsafe { p.add(l) } == ptr {
                        self.mmap_ptr = Some((p, l + len));
                        return Ok(());
                    } else {
                        self.flush_mmap()?; // not contiguous
                    }
                }
                self.mmap_ptr = Some((ptr, len));
                Ok(())
            }

            OutputChunk::Owned(buf) => {
                #[cfg(unix)]
                {
                    self.flush_mmap()?;
                }
                self.out.write_all(buf)?;
                self.out.write_all(b"\n")?;
                Ok(())
            }
        }
    }

    /// Schedule the specified string for eventual output
    pub fn write_str(&mut self, s: &str) -> io::Result<()> {
        self.write_chunk(&OutputChunk::Owned(s.as_bytes().to_vec()))
    }

    // Flush any pending mmap data
    #[cfg(unix)]
    fn flush_mmap(&mut self) -> io::Result<()> {
        if let Some((ptr, len)) = self.mmap_ptr.take() {
            if len < MIN_DIRECT_WRITE {
                // SAFELY treat as &[u8] and write to buffered writer
                let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                return self.out.write_all(slice);
            } else {
                // Large enough: write directly using zero-copy
                let fd = io::stdout().as_raw_fd();
                self.out.flush()?; // sync any buffered data
                return write_syscall(fd, ptr, len);
            }
        }
        Ok(())
    }

    /// Flush everything: pending mmap and buffered data.
    pub fn flush(&mut self) -> io::Result<()> {
        #[cfg(unix)]
        {
            self.flush_mmap()?; // flush mmap if any
        }
        self.out.flush() // then flush buffered data
    }
}

// Usage example (never compiled)
#[cfg(any())]
pub fn main() -> io::Result<()> {
    let path = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| "-".into());
    let mut reader = LineReader::open(&path)?;
    let stdout = Box::new(io::stdout().lock());
    let mut output = OutputBuffer::new(stdout);

    while let Some(chunk) = reader.get_line()? {
        output.write_chunk(&chunk)?;
    }

    output.flush()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    impl OutputBuffer<Cursor<Vec<u8>>> {
        /// Grab the raw bytes written so far.
        pub fn test_contents(&self) -> &[u8] {
            // 1) get_ref() on BufWriter<Cursor<...>> → &Cursor<...>
            // 2) get_ref() on Cursor<Vec<u8>>        → &Vec<u8>
            // 3) as_slice() on Vec<u8>               → &[u8]
            self.out.get_ref().get_ref().as_slice()
        }
    }

    #[test]
    fn test_owned_line_output() {
        let sink = Cursor::new(Vec::new());
        let mut out = OutputBuffer::new(sink);

        out.write_str("foo").unwrap();
        out.write_str("bar").unwrap();
        out.flush().unwrap();

        assert_eq!(out.test_contents(), b"foo\nbar\n");
    }

    #[cfg(unix)]
    fn make_mmap_line<'a>(buf: &'a [u8]) -> OutputChunk<'a> {
        OutputChunk::MmapInput {
            content: &buf[..buf.len() - 1], // exclude \n
            full_span: buf,                 // include \n
        }
    }

    #[cfg(unix)]
    #[test]
    fn test_mmap_line_output_single() {
        let mmap_data = b"line one\nline two\n";
        let sink = Cursor::new(Vec::new());
        let mut out = OutputBuffer::new(sink);

        // first nine bytes are "line one\n"
        out.write_chunk(&make_mmap_line(&mmap_data[..9])).unwrap();
        // the rest are "line two\n"
        out.write_chunk(&make_mmap_line(&mmap_data[9..])).unwrap();
        out.flush().unwrap();

        assert_eq!(out.test_contents(), b"line one\nline two\n");
    }

    #[cfg(unix)]
    #[test]
    fn test_mixed_output_order_preserved() {
        let mmap_data = b"zero\none\n";
        let sink = Cursor::new(Vec::new());
        let mut out = OutputBuffer::new(sink);

        // "zero\n"
        out.write_chunk(&make_mmap_line(&mmap_data[..5])).unwrap();
        // now an owned line
        out.write_str("middle").unwrap();
        // then "one\n"
        out.write_chunk(&make_mmap_line(&mmap_data[5..])).unwrap();
        out.flush().unwrap();

        assert_eq!(out.test_contents(), b"zero\nmiddle\none\n");
    }
}
