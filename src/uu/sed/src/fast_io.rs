// Zero-copy line-based I/O
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

/// Cursor for zero-copy iteration over mmap’d file.
pub struct MmapLineCursor<'a> {
    data: &'a [u8],
    pos: usize,
}

#[derive(Debug)]
#[cfg(unix)]
pub enum LineChunk<'a> {
    #[cfg(unix)]
    Mmap {
        content: &'a [u8],   // line without newline
        full_span: &'a [u8], // line including original newline
    },
    Owned(Vec<u8>), // line content without newline
}

#[cfg(not(unix))]
pub enum LineChunk {
    Owned(Vec<u8>), // line content without newline
}

#[cfg(unix)]
type LineChunkRef<'a> = LineChunk<'a>;

#[cfg(not(unix))]
type LineChunkRef = LineChunk;

impl<'a> MmapLineCursor<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }
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

    pub fn get_line(&mut self) -> io::Result<Option<(Cow<'_, str>, usize)>> {
        match self.lines.next() {
            Some(Ok(line)) => Ok(Some((Cow::Owned(line.clone()), line.len()))),
            Some(Err(e)) => Err(e),
            None => Ok(None),
        }
    }
}

/// Unified reader that uses mmap when possible, falls back to buffered reading.
pub enum LineReader {
    #[cfg(unix)]
    Mmap {
        mmap: Mmap,
        cursor: MmapLineCursor<'static>,
    },
    Fallback(ReadLineCursor<BufReader<Box<dyn Read>>>),
}

fn fallback_reader(file: File) -> io::Result<LineReader> {
    let boxed: Box<dyn Read> = Box::new(file);
    let reader = BufReader::new(boxed);
    Ok(LineReader::Fallback(ReadLineCursor::new(reader)))
}

impl LineReader {
    pub fn open(path: &PathBuf) -> io::Result<Self> {
        if path.as_os_str() == "-" {
            let stdin = io::stdin();
            let boxed: Box<dyn Read> = Box::new(stdin.lock());
            let reader = BufReader::new(boxed);
            return Ok(LineReader::Fallback(ReadLineCursor::new(reader)));
        }

        let file = File::open(path)?;

        #[cfg(unix)]
        {
            match unsafe { Mmap::map(&file) } {
                Ok(mmap) => {
                    // SAFETY: mmap owns the data and lives in the same variant
                    let slice: &'static [u8] =
                        unsafe { std::slice::from_raw_parts(mmap.as_ptr(), mmap.len()) };
                    let cursor = MmapLineCursor::new(slice);
                    Ok(LineReader::Mmap { mmap, cursor })
                }
                Err(_) => fallback_reader(file),
            }
        }

        #[cfg(not(unix))]
        {
            fallback_reader(file)
        }
    }

    pub fn get_line(&mut self) -> io::Result<Option<LineChunkRef>> {
        match self {
            #[cfg(unix)]
            LineReader::Mmap { cursor, .. } => {
                if let Some((content, full_span)) = cursor.get_line()? {
                    Ok(Some(LineChunk::Mmap { content, full_span }))
                } else {
                    Ok(None)
                }
            }
            LineReader::Fallback(cursor) => {
                if let Some((line, _)) = cursor.get_line()? {
                    Ok(Some(LineChunk::Owned(line.into_owned().into_bytes())))
                } else {
                    Ok(None)
                }
            }
        }
    }
}

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

    pub fn write_chunk(&mut self, chunk: &LineChunk) -> io::Result<()> {
        match chunk {
            #[cfg(unix)]
            LineChunk::Mmap { full_span, .. } => {
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

            LineChunk::Owned(buf) => {
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

    fn make_owned_line(s: &str) -> LineChunk {
        LineChunk::Owned(s.as_bytes().to_vec())
    }

    #[test]
    fn test_owned_line_output() {
        let sink = Cursor::new(Vec::new());
        let mut out = OutputBuffer::new(sink);

        out.write_chunk(&make_owned_line("foo")).unwrap();
        out.write_chunk(&make_owned_line("bar")).unwrap();
        out.flush().unwrap();

        assert_eq!(out.test_contents(), b"foo\nbar\n");
    }

    #[cfg(unix)]
    fn make_mmap_line<'a>(buf: &'a [u8]) -> LineChunk<'a> {
        LineChunk::Mmap {
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
        out.write_chunk(&make_owned_line("middle")).unwrap();
        // then "one\n"
        out.write_chunk(&make_mmap_line(&mmap_data[5..])).unwrap();
        out.flush().unwrap();

        assert_eq!(out.test_contents(), b"zero\nmiddle\none\n");
    }
}
