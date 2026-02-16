// Zero-copy line-based I/O
//
// Abstractions that allow file lines to be processed and output
// in mmapped memory space.  By coalescing output requests an
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
use memchr::memchr;
#[cfg(unix)]
use memmap2::Mmap;

use std::cell::Cell;
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};

#[cfg(not(unix))]
use std::marker::PhantomData;

#[cfg(unix)]
use std::os::fd::RawFd;

#[cfg(unix)]
use std::os::unix::io::AsRawFd;

use std::str;

use std::path::PathBuf;
use uucore::error::UError;

#[cfg(unix)]
use uucore::error::USimpleError;

#[cfg(unix)]
use uucore::libc;

// Define two cursors for iterating over lines:
// - MmapLineCursor based on mmap(2),
// - ReadLineCursor based on BufReader.

/// Cursor for zero-copy iteration over mmap’d file.
#[cfg(unix)]
pub struct MmapLineCursor<'a> {
    _file: File,         // Mmapped file; keeps file open for fast copy
    fast_copy: FastCopy, // Data for fast file copy I/O
    data: &'a [u8],      // Mmapped data
    pos: usize,          // Position within the data
}

#[cfg(unix)]
/// Represents the get_line return: one line plus whether it was the last.
pub struct NextMmapLine<'a> {
    pub content: &'a [u8],
    pub full_span: &'a [u8],
}

#[cfg(unix)]
impl<'a> MmapLineCursor<'a> {
    fn new(file: File, data: &'a [u8]) -> Self {
        Self {
            fast_copy: FastCopy::new(&file),
            _file: file,
            data,
            pos: 0,
        }
    }

    /// Return the next line, if available, or None.
    fn get_line(&mut self) -> io::Result<Option<NextMmapLine<'a>>> {
        if self.pos >= self.data.len() {
            return Ok(None);
        }

        let start = self.pos;

        let mut end = if let Some(pos) = memchr(b'\n', &self.data[start..]) {
            pos + start
        } else {
            self.data.len()
        };

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

        Ok(Some(NextMmapLine { content, full_span }))
    }

    /// Return true if the previously returned line was the last one.
    fn last_line(&mut self) -> io::Result<bool> {
        Ok(self.pos >= self.data.len())
    }
}

/// Buffered line reader from any BufRead input.
pub struct ReadLineCursor {
    reader: Box<dyn BufRead>,
    buffer: String,
}

impl ReadLineCursor {
    /// Construct from anything that implements `Read`.
    fn new<R: Read + 'static>(r: R) -> Self {
        let buf = BufReader::new(r);
        Self {
            reader: Box::new(buf),
            buffer: String::new(),
        }
    }

    /// If a line is available, return it and its \n termination.
    fn get_line(&mut self) -> io::Result<Option<(String, bool)>> {
        self.buffer.clear();
        // read_line *includes* the '\n' if present
        let bytes_read = self.reader.read_line(&mut self.buffer)?;
        if bytes_read == 0 {
            return Ok(None);
        }
        // O(1) check whether it ended in '\n'
        let has_newline = self.buffer.ends_with('\n');
        // strip it if you don’t want to expose it to the caller
        if has_newline {
            self.buffer.pop();
        }
        let line = std::mem::take(&mut self.buffer);
        Ok(Some((line, has_newline)))
    }

    /// Return true if the previously returned line was the last one.
    fn last_line(&mut self) -> io::Result<bool> {
        // FIXME(rust-lang#86423): Replace with BufRead::has_data_left()
        // when/if method becomes stable.
        Ok(self.reader.fill_buf()?.is_empty())
    }
}

/// A chunk of data that is input and can be output, often very efficiently
#[derive(Debug, PartialEq, Eq)]
pub struct IOChunk<'a> {
    utf8_verified: Cell<bool>, // True if the contents are valid UTF-8
    content: IOChunkContent<'a>,
}

impl<'a> IOChunk<'a> {
    /// Construct an IOChunk from the given content
    fn from_content(content: IOChunkContent<'a>) -> Self {
        Self {
            utf8_verified: Cell::new(false),
            content,
        }
    }

    /// Clear the object's contents, converting it into Owned if needed.
    pub fn clear(&mut self) {
        self.utf8_verified.set(true);
        match &mut self.content {
            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => {
                content.clear();
                *has_newline = false;
            }
            #[cfg(unix)]
            _ => {
                self.content = IOChunkContent::new_owned(String::new(), false);
            }
        }
    }

    /// Return true if the content is empty.
    pub fn is_empty(&self) -> bool {
        self.content.len() == 0
    }

    /// Return true if the content ends with a newline.
    pub fn is_newline_terminated(&self) -> bool {
        match &self.content {
            IOChunkContent::Owned { has_newline, .. } => *has_newline,
            #[cfg(unix)]
            IOChunkContent::MmapInput { full_span, .. } => {
                if let Some(&last) = full_span.last() {
                    last == b'\n'
                } else {
                    false
                }
            }
        }
    }

    #[cfg(test)]
    /// Create an Owned newline-terminated IOChunk from a string.
    pub fn new_from_str(s: &str) -> Self {
        IOChunk {
            content: IOChunkContent::new_owned(s.to_string(), true),
            utf8_verified: Cell::new(false),
        }
    }

    /// Set the object's contents to the specified string.
    /// Convert it into Owned if needed.
    pub fn set_to_string(&mut self, new_content: String, add_newline: bool) {
        self.utf8_verified.set(true);
        match &mut self.content {
            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => {
                *content = new_content;
                *has_newline = add_newline;
            }
            #[cfg(unix)]
            _ => {
                self.content = IOChunkContent::new_owned(new_content, add_newline);
            }
        }
    }

    /// Return the content as a str.
    pub fn as_str(&self) -> Result<&str, Box<dyn UError>> {
        match &self.content {
            #[cfg(unix)]
            IOChunkContent::MmapInput { content, .. } => {
                if self.utf8_verified.get() {
                    // Use cached result
                    Ok(unsafe { self.content.as_str_unchecked() })
                } else {
                    let result = str::from_utf8(content);
                    self.utf8_verified.set(true);
                    result.map_err(|e| USimpleError::new(2, e.to_string()))
                }
            }
            IOChunkContent::Owned { content, .. } => Ok(content),
        }
    }

    /// Return the raw byte content (always safe).
    pub fn as_bytes(&self) -> &[u8] {
        match &self.content {
            #[cfg(unix)]
            IOChunkContent::MmapInput { content, .. } => content,
            IOChunkContent::Owned { content, .. } => content.as_bytes(),
        }
    }

    /// Convert content to the Owned variant if it's not already.
    /// Fails if the conversion to UTF-8 fails.
    pub fn ensure_owned(&mut self) -> Result<(), Box<dyn UError>> {
        match &self.content {
            IOChunkContent::Owned { .. } => Ok(()), // already owned
            #[cfg(unix)]
            IOChunkContent::MmapInput {
                content, full_span, ..
            } => match std::str::from_utf8(content) {
                Ok(valid_str) => {
                    let has_newline = full_span.last().copied() == Some(b'\n');
                    self.content = IOChunkContent::new_owned(valid_str.to_string(), has_newline);
                    self.utf8_verified.set(true);
                    Ok(())
                }
                Err(e) => Err(USimpleError::new(2, e.to_string())),
            },
        }
    }

    /// Return mutable access to the content and has_newline fields.
    pub fn fields_mut(&mut self) -> Result<(&mut String, &mut bool), Box<dyn UError>> {
        self.ensure_owned()?;

        match &mut self.content {
            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => Ok((content, has_newline)),
            #[allow(unreachable_patterns)]
            _ => unreachable!("ensure_owned should convert to Owned"),
        }
    }
}

/// Data to be written to a file. It can come from the mmapped
/// memory space, in which case it is tracked to allow coalescing
/// and bypassing BufWriter, or it can be other data from the process's
/// memory space.
#[derive(Debug, PartialEq, Eq)]
enum IOChunkContent<'a> {
    #[cfg(unix)]
    MmapInput {
        fast_copy: FastCopy, // Data for fast file copy I/O
        base: *const u8,     // Mmap start address
        content: &'a [u8],   // Line without newline
        full_span: &'a [u8], // Line including original newline, if any
    },
    Owned {
        content: String,   // Line content without newline
        has_newline: bool, // True if \n-terminated
        #[cfg(not(unix))]
        _phantom: PhantomData<&'a ()>, // Silence E0392 warning
    },
}

impl IOChunkContent<'_> {
    /// Construct a new Owned chunk.
    pub fn new_owned(content: String, has_newline: bool) -> Self {
        #[cfg(unix)]
        return IOChunkContent::Owned {
            content,
            has_newline,
        };

        #[cfg(not(unix))]
        return IOChunkContent::Owned {
            content,
            has_newline,
            // Avoid E0063 missing _phantom initialization errors
            _phantom: std::marker::PhantomData,
        };
    }

    #[cfg(unix)]
    unsafe fn as_str_unchecked(&self) -> &str {
        match self {
            IOChunkContent::MmapInput { content, .. } => unsafe {
                std::str::from_utf8_unchecked(content)
            },
            IOChunkContent::Owned { content, .. } => content,
        }
    }

    /// Return the content's length (in bytes or characters).
    pub fn len(&self) -> usize {
        match self {
            #[cfg(unix)]
            IOChunkContent::MmapInput { content, .. } => content.len(),

            IOChunkContent::Owned { content, .. } => content.len(),
        }
    }
}

/// Information required for performing I/O using fast file copy
/// operations, such as copy_file_range(2).
#[cfg(unix)]
#[derive(Debug, PartialEq, Eq, Clone)]
struct FastCopy {
    fd: i32,           // Raw file descriptor
    is_regular: bool,  // True if this is a regular file
    block_size: usize, // Filesystem block size
}

#[cfg(unix)]
impl FastCopy {
    /// Construct with an object on which as_raw_fd() can be called.
    pub fn new<T: AsRawFd + ?Sized>(f: &T) -> Self {
        let fd = f.as_raw_fd();

        let mut st: libc::stat = unsafe { std::mem::zeroed() };

        let ret = unsafe { libc::fstat(fd, &raw mut st) };
        if ret == -1 {
            // All fstat errors are programmer rather user faults
            // so panic is appropriate.
            let err = std::io::Error::last_os_error();
            panic!("fstat failed on fd {}: {}", fd, err);
        }

        let ftype = st.st_mode & libc::S_IFMT;

        Self {
            fd,
            is_regular: ftype == libc::S_IFREG,
            block_size: st.st_blksize as usize,
        }
    }
}

#[cfg(test)]
#[cfg(unix)]
impl Default for FastCopy {
    fn default() -> Self {
        FastCopy {
            fd: -1,
            is_regular: false,
            block_size: 0,
        }
    }
}

/// Unified reader that uses mmap when possible, falls back to buffered reading.
pub enum LineReader<'a> {
    #[cfg(unix)]
    MmapInput {
        mapped_file: Mmap, // A handle that can derive the mapped file slice
        cursor: MmapLineCursor<'a>,
    },
    ReadInput(ReadLineCursor),
    #[cfg(not(unix))]
    _Phantom(std::marker::PhantomData<&'a ()>),
}

/// Return a LineReader that uses the ReadInput method fot the specified file.
fn line_reader_read_input(file: File) -> io::Result<LineReader<'static>> {
    let boxed: Box<dyn Read> = Box::new(file);
    let reader = BufReader::new(boxed);
    Ok(LineReader::ReadInput(ReadLineCursor::new(reader)))
}

impl<'a> LineReader<'a> {
    /// Open the specified file for line input.
    // Use "-" to read from the standard input.
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
                    let cursor = MmapLineCursor::new(file, slice);
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

    /// Open the specified file to read as a stream.
    #[cfg(test)]
    pub fn open_stream(path: &PathBuf) -> io::Result<Self> {
        let file = File::open(path)?;
        line_reader_read_input(file)
    }

    /// Return the next line, if available.
    pub fn get_line(&mut self) -> io::Result<Option<IOChunk<'a>>> {
        match self {
            #[cfg(unix)]
            LineReader::MmapInput { cursor, .. } => {
                // Obtain fields to prevent borrowing issues.
                let fast_copy = cursor.fast_copy.clone();
                let base = cursor.data.as_ptr();
                if let Some(NextMmapLine { content, full_span }) = cursor.get_line()? {
                    let chunk = IOChunk::from_content(IOChunkContent::MmapInput {
                        fast_copy,
                        base,
                        content,
                        full_span,
                    });

                    Ok(Some(chunk))
                } else {
                    Ok(None)
                }
            }

            LineReader::ReadInput(cursor) => {
                if let Some((line, _has_newline)) = cursor.get_line()? {
                    let chunk =
                        IOChunk::from_content(IOChunkContent::new_owned(line, _has_newline));
                    Ok(Some(chunk))
                } else {
                    Ok(None)
                }
            }

            #[cfg(not(unix))]
            LineReader::_Phantom(_) => unreachable!("_Phantom should never be constructed"),
        }
    }

    /// Return true if the previously returned line was the last one.
    pub fn last_line(&mut self) -> io::Result<bool> {
        match self {
            #[cfg(unix)]
            LineReader::MmapInput { cursor, .. } => cursor.last_line(),

            LineReader::ReadInput(cursor) => cursor.last_line(),

            #[cfg(not(unix))]
            LineReader::_Phantom(_) => unreachable!("_Phantom should never be constructed"),
        }
    }
}

// Define a trait combining two: workaround for Rust's corresponding inability.
#[cfg(unix)]
pub trait OutputWrite: Write + AsRawFd {}
#[cfg(unix)]
impl<T: Write + AsRawFd> OutputWrite for T {}

#[cfg(not(unix))]
pub trait OutputWrite: Write {}
#[cfg(not(unix))]
impl<T: Write> OutputWrite for T {}

/// An output data chunk from the mmapped file
/// Data elements allow output to be performed through write(2)
/// or through copy_file_range(2).
#[cfg(unix)]
#[derive(Clone)]
struct MmapOutput {
    in_fast_copy: FastCopy, // Data for fast file copy I/O
    base_ptr: *const u8,    // Base of the entire mmapped region
    out_ptr: *const u8,     // Start of the output data chunk
    len: usize,             // Output data chunk size
}

/// Abstraction for outputting data, potentially from the mmapped file
/// Outputs from mmapped data are coalesced and written via the Linux
/// copy_file_range(2) system call without any copying, if possible
/// and worthwhile.  As a fallback write(2) is used, which requires
/// the OS to copy data from the mmapped region to the output file
/// page cache.
/// All other output is buffered and writen via BufWriter.
pub struct OutputBuffer {
    out: BufWriter<Box<dyn OutputWrite + 'static>>, // Where to write
    #[cfg(unix)]
    fast_copy: FastCopy,            // Data for fast file copy ops
    #[cfg(unix)]
    max_pending_write: usize,       // Max bytes to keep before flushing
    #[cfg(unix)]
    mmap_chunk: Option<MmapOutput>, // Chunk to write
    #[cfg(test)]
    low_level_flushes: usize,       // Number of system call flushes
}

/// Threshold to use buffered writes for output
// These 4k are half the 8k size of the BufWriter buffer.
// The constant guarantees that, at worst, mmapped output will
// result in a doubling of the issued write(2) system calls.
// Taking into account the non-copied data, this should result
// in overall fewer CPU instructions.
#[cfg(unix)]
const MIN_DIRECT_WRITE: usize = 4 * 1024;

/// Maximum size of a pending write buffer for files
// Once more than the specified bytes accumulate, issue a write or
// a copy_file_range(2).  This is kept high to reduce the number of
// system calls and (where supported) file extents.
#[cfg(unix)]
const MAX_PENDING_WRITE_FILE: usize = 1024 * 1024;

/// Maximum size of a pending write buffer for non-files (likely pipes)
// Once more than the specified bytes accumulate, issue a write.
// This is set to the common size of Linux pipe buffer to maximize
// throughput and liveness across the pipeline.
#[cfg(unix)]
const MAX_PENDING_WRITE_NON_FILE: usize = 64 * 1024;

impl OutputBuffer {
    #[cfg(not(unix))]
    pub fn new(w: Box<dyn OutputWrite + 'static>) -> Self {
        Self {
            out: BufWriter::new(w),
            #[cfg(test)]
            low_level_flushes: 0,
        }
    }

    #[cfg(unix)]
    pub fn new(w: Box<dyn OutputWrite + 'static>) -> Self {
        let fast_copy = FastCopy::new(&*w);
        let max_pending_write = if fast_copy.is_regular {
            MAX_PENDING_WRITE_FILE
        } else {
            MAX_PENDING_WRITE_NON_FILE
        };
        Self {
            out: BufWriter::new(w),
            fast_copy,
            max_pending_write,
            mmap_chunk: None,
            #[cfg(test)]
            low_level_flushes: 0,
        }
    }

    /// Schedule the specified String or &strfor eventual output
    pub fn write_str<S: Into<String>>(&mut self, s: S) -> io::Result<()> {
        self.write_chunk(&IOChunk::from_content(IOChunkContent::new_owned(
            s.into(),
            false,
        )))
    }

    /// Copy the specified file to the output.
    pub fn copy_file(&mut self, path: &PathBuf) -> io::Result<()> {
        // Flush mmap writes, if any.
        #[cfg(unix)]
        {
            self.flush_mmap(WriteRange::Complete)?;
        }

        let file = match File::open(path) {
            Ok(f) => f,
            // Per POSIX, if the file can't be read treat it as empty.
            Err(_) => return Ok(()),
        };

        let mut reader = BufReader::new(file);
        io::copy(&mut reader, &mut self.out)?;
        Ok(())
    }
}

/// Implementation of the std::io::Write trait
impl Write for OutputBuffer {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let s =
            std::str::from_utf8(buf).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        self.write_str(s)?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush()
    }
}

#[cfg(unix)]
#[derive(Debug, PartialEq)]
enum WriteRange {
    Complete, // Write all specified data.
    Blocks,   // Finish write on a block boundary (to help alignment).
    None,     // No writing is needed.
}

#[cfg(unix)]
impl OutputBuffer {
    /// Schedule the specified output chunk for eventual output
    pub fn write_chunk(&mut self, new_chunk: &IOChunk) -> io::Result<()> {
        match &new_chunk.content {
            IOChunkContent::MmapInput {
                full_span,
                fast_copy,
                base,
                ..
            } => {
                let new_ptr = full_span.as_ptr();
                let new_len = full_span.len();

                // Set whether a flush is needed and whether the
                // mmap_chunk needs to be reset to the new input.
                // This avoids calling mmap_chunk (which borrows self)
                // when old_chunk is already borrowed.
                let (flush_action, reset) = if let Some(old_chunk) = self.mmap_chunk.as_mut() {
                    // Coalesce if adjacent.
                    if unsafe { old_chunk.out_ptr.add(old_chunk.len) } == new_ptr {
                        // Coalesce.
                        old_chunk.len += new_len;
                        if old_chunk.len > self.max_pending_write {
                            // Too much data; flush some full blocks.
                            (WriteRange::Blocks, false)
                        } else {
                            (WriteRange::None, false)
                        }
                    } else {
                        // Not contiguous
                        (WriteRange::Complete, true)
                    }
                } else {
                    // No chunk yet; start a new one.
                    (WriteRange::None, true)
                };

                if flush_action != WriteRange::None {
                    self.flush_mmap(flush_action)?;
                }
                if reset {
                    self.mmap_chunk = Some(MmapOutput {
                        in_fast_copy: fast_copy.clone(),
                        base_ptr: *base,
                        out_ptr: new_ptr,
                        len: new_len,
                    });
                }
            }

            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => {
                self.flush_mmap(WriteRange::Complete)?;
                self.out.write_all(content.as_bytes())?;
                if *has_newline {
                    self.out.write_all(b"\n")?;
                }
            }
        }
        Ok(())
    }

    /// Flush any pending mmap data.
    /// Cover specifies whether the written data shall terminate
    /// on a block boundary.
    /// This is only respected when copy_file_range is employed.
    /// Return the number of bytes written.
    #[cfg(unix)]
    fn flush_mmap(&mut self, cover: WriteRange) -> io::Result<()> {
        if let Some(chunk) = self.mmap_chunk.as_mut() {
            let written = if chunk.len < MIN_DIRECT_WRITE {
                // SAFELY treat as &[u8] and write to buffered writer
                let slice = unsafe { std::slice::from_raw_parts(chunk.out_ptr, chunk.len) };
                self.out.write_all(slice)?;
                slice.len()
            } else {
                // Large enough: write directly using zero-copy
                self.out.flush()?; // sync any buffered data
                #[cfg(test)]
                {
                    self.low_level_flushes += 1;
                }
                if chunk.in_fast_copy.is_regular && self.fast_copy.is_regular {
                    portable_copy_file_range(
                        chunk.out_ptr,
                        chunk.in_fast_copy.fd,
                        // Input file offset
                        unsafe { chunk.out_ptr.offset_from(chunk.base_ptr) } as i64,
                        self.fast_copy.fd,
                        chunk.len,
                        // Alignment block size: the largest of the two
                        chunk.in_fast_copy.block_size.max(self.fast_copy.block_size),
                        cover,
                    )?
                } else {
                    reliable_write(self.fast_copy.fd, chunk.out_ptr, chunk.len)?
                }
            };

            chunk.len -= written;
            unsafe { chunk.out_ptr = chunk.out_ptr.add(written) };
        }
        Ok(())
    }

    /// Flush everything: pending mmap and buffered data.
    pub fn flush(&mut self) -> io::Result<()> {
        self.flush_mmap(WriteRange::Complete)?; // flush mmap if any
        self.out.flush() // then flush buffered data
    }
}

#[cfg(not(unix))]
impl OutputBuffer {
    /// Schedule the specified output chunk for eventual output
    pub fn write_chunk(&mut self, chunk: &IOChunk) -> io::Result<()> {
        match &chunk.content {
            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => {
                self.out.write_all(content.as_bytes())?;
                if *has_newline {
                    self.out.write_all(b"\n")?;
                }
                Ok(())
            }
        }
    }

    /// Flush everything: pending mmap and buffered data.
    pub fn flush(&mut self) -> io::Result<()> {
        self.out.flush() // then flush buffered data
    }
}

/// Wrapper that issues the write(2) system calls via write_all
// This takes care of partial writes and EAGAIN, EWOULDBLOCK, EINTR.
// Return the number of bytes written.
#[cfg(unix)]
fn reliable_write(fd: i32, ptr: *const u8, len: usize) -> std::io::Result<usize> {
    // A thin Write-compatible wrapper around a raw file descriptor
    // This allows us to issue and utilize the write_all implementatin.
    struct FdWriter(RawFd);

    impl Write for FdWriter {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            let ret =
                unsafe { libc::write(self.0, buf.as_ptr() as *const libc::c_void, buf.len()) };
            if ret < 0 {
                Err(io::Error::last_os_error())
            } else {
                Ok(ret as usize)
            }
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    let mut writer = FdWriter(fd);
    let buf: &[u8] = unsafe { std::slice::from_raw_parts(ptr, len) };
    writer.write_all(buf)?;
    Ok(len)
}

/// Copy efficiently len data from the input to the output file.
/// Fall back to write(2) if the platform doesn't support copy_file_range(2).
/// Return the number of bytes written.
#[cfg(unix)]
#[allow(unused_variables)]
fn portable_copy_file_range(
    in_ptr: *const u8,
    in_fd: i32,
    in_off: libc::off_t,
    out_fd: i32,
    len: usize,
    block_size: usize,
    cover: WriteRange,
) -> std::io::Result<usize> {
    if len == 0 {
        return Ok(0);
    }

    #[cfg(all(target_os = "linux", target_env = "gnu"))]
    {
        aligned_copy_file_range(in_ptr, in_fd, in_off, out_fd, len, block_size, cover)
    }
    #[cfg(not(all(target_os = "linux", target_env = "gnu")))]
    {
        reliable_write(out_fd, in_ptr, len)
    }
}

/// Copy efficiently len data from the input to the output file.
/// Handle partial copies and fall back to write(2) if the
/// file system or options don't support copy_file_range(2).
/// Return the number of bytes written.
#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn reliable_copy_file_range(
    in_ptr: *const u8,
    in_fd: i32,
    mut in_off: libc::off_t,
    out_fd: i32,
    len: usize,
) -> std::io::Result<usize> {
    let mut pending = len;
    while pending > 0 {
        let ret = unsafe {
            libc::copy_file_range(
                in_fd,
                &raw mut in_off,
                out_fd,
                std::ptr::null_mut(), // Use and update output offset
                pending,
                0,
            )
        };
        if ret < 0 {
            let err = io::Error::last_os_error();
            return match err.raw_os_error() {
                Some(libc::ENOSYS) | Some(libc::EOPNOTSUPP) | Some(libc::EXDEV) => {
                    // Fallback to write(2).
                    reliable_write(out_fd, in_ptr, pending)
                }
                _ => Err(err),
            };
        } else if ret == 0 {
            // EOF reached
            break;
        } else {
            pending -= ret as usize;
        }
    }
    Ok(len)
}

/// Copy efficiently len data from the input to the output file.
/// Try to call copy_file_range(2) on block-aligned data, so
/// as to help the filesystem maintain cross-file extents.
/// Return the number of bytes written.
#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn aligned_copy_file_range(
    mut in_ptr: *const u8,
    in_fd: i32,
    mut in_off: libc::off_t,
    out_fd: i32,
    len: usize,
    block_size: usize,
    cover: WriteRange,
) -> std::io::Result<usize> {
    // 1. Get current output offset.
    let res = unsafe { libc::lseek(out_fd, 0, libc::SEEK_CUR) as i64 };
    if res < 0 {
        return Err(std::io::Error::last_os_error());
    }
    let out_off = res as usize;
    let mut pending = len;

    // Obtain head alignment.
    // Bytes to end of block:
    let remainder = in_off as usize % block_size;
    // Bytes to write (block_size becomes 0):
    let head_align = (block_size - remainder) % block_size;

    if !(out_off + head_align).is_multiple_of(block_size) {
        // No hope of alignment, so just copy everything.
        return reliable_copy_file_range(in_ptr, in_fd, in_off, out_fd, pending);
    }

    if head_align > 0 {
        // Align the two files on a block boundary.
        let head_len = head_align.min(pending);
        reliable_write(out_fd, in_ptr, head_len)?;
        in_ptr = unsafe { in_ptr.add(head_len) };
        in_off += head_len as i64;
        pending -= head_len;
    }

    // Copy aligned blocks.
    let aligned_len = pending - (pending % block_size);
    let _ = reliable_copy_file_range(in_ptr, in_fd, in_off, out_fd, aligned_len);
    pending -= aligned_len;

    // Copy tail if needed.
    if pending > 0 && cover == WriteRange::Complete {
        in_ptr = unsafe { in_ptr.add(aligned_len) };
        pending -= reliable_write(out_fd, in_ptr, pending)?;
    }

    Ok(len - pending)
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
    use std::fs;
    #[cfg(unix)]
    use std::fs::File;
    #[cfg(all(target_os = "linux", target_env = "gnu"))]
    use std::io::{self, Write};
    use std::io::{Seek, SeekFrom};
    use tempfile::NamedTempFile;
    use tempfile::tempfile;

    /// Helper: produce a 4k-byte Vec of `'.'`s ending in `'\n'`.
    #[cfg(unix)]
    fn make_dot_line_4k() -> Vec<u8> {
        let mut buf = Vec::with_capacity(4096);
        buf.extend(std::iter::repeat_n(b'.', 4095));
        buf.push(b'\n');
        buf
    }

    #[cfg(unix)]
    pub fn new_content_mmap_input<'a>(
        content: &'a [u8],
        full_span: &'a [u8],
    ) -> IOChunkContent<'a> {
        IOChunkContent::MmapInput {
            fast_copy: FastCopy::default(),
            base: std::ptr::null(),
            content,
            full_span,
        }
    }

    #[test]
    fn test_owned_line_output() -> io::Result<()> {
        let tmp = NamedTempFile::new()?;
        {
            let file = tmp.reopen()?;
            let mut out = OutputBuffer::new(Box::new(file));
            out.write_str("foo\n")?;
            out.write_str("bar\n")?;
            out.flush()?;
            assert_eq!(out.low_level_flushes, 0);
        } // File closes here as it leaves the scope

        let contents = fs::read(tmp.path())?;
        assert_eq!(contents.as_slice(), b"foo\nbar\n");
        Ok(())
    }

    #[test]
    #[cfg(unix)]
    fn test_mmap_line_output_single() -> io::Result<()> {
        use std::fs;
        use std::io::Write;
        use tempfile::NamedTempFile;

        // Prepare the input buffer: two lines in one contiguous mmap region
        let mmap_data = b"line one\nline two\n";

        // Write that into a temp file
        let mut input = NamedTempFile::new()?;
        input.write_all(mmap_data)?;
        input.flush()?;
        let input_path = input.path().to_path_buf();

        // Open the reader on that file
        let mut reader = LineReader::open(&input_path)?;

        // Prepare an output temp file and wrap it in our OutputBuffer
        let output = NamedTempFile::new()?;
        let output_path = output.path().to_path_buf();
        let out_file = std::fs::File::create(&output_path)?;
        let mut out = OutputBuffer::new(Box::new(Box::new(out_file)));

        // Drain reader → writer
        while let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
        }
        out.flush()?;

        assert_eq!(out.low_level_flushes, 0);

        let written = fs::read(&output_path)?;
        assert_eq!(written.as_slice(), mmap_data);

        Ok(())
    }

    #[test]
    #[cfg(unix)]
    fn test_mixed_output_order_preserved() -> io::Result<()> {
        use std::fs;
        use std::fs::File;
        use std::io::Write;
        use tempfile::NamedTempFile;

        // Prepare an input file containing two lines: "zero\none\n"
        let data = b"zero\none\n";
        let mut input = NamedTempFile::new()?;
        input.write_all(data)?;
        input.flush()?;
        let input_path = input.path().to_path_buf();
        let mut reader = LineReader::open(&input_path)?;

        // Prepare an empty output file
        let output = NamedTempFile::new()?;
        let output_path = output.path().to_path_buf();
        let out_file = File::create(&output_path)?;
        let mut out = OutputBuffer::new(Box::new(out_file));

        // Read the first mmap line ("zero\n") and write it
        if let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
        }

        // Write an owned line ("middle\n")
        out.write_str("middle\n")?;

        // Read the second mmap line ("one\n") and write it
        if let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
        }

        out.flush()?;

        // Since all writes are small (<4K), we expect zero zero copy syscalls
        assert_eq!(out.low_level_flushes, 0);

        // Read both files back and compare
        let expected = {
            let mut v = Vec::new();
            v.extend_from_slice(b"zero\n");
            v.extend_from_slice(b"middle\n");
            v.extend_from_slice(b"one\n");
            v
        };
        let actual = fs::read(&output_path)?;
        assert_eq!(actual, expected);

        Ok(())
    }

    #[test]
    #[cfg(unix)]
    fn test_large_file_zero_copy() -> io::Result<()> {
        // Create and fill the input temp file:
        let mut input = NamedTempFile::new()?;
        write!(input, "first line\nsecond line\n")?;
        let dot_line = make_dot_line_4k();
        input.write_all(&dot_line)?;
        input.flush()?;
        let input_path = input.path().to_path_buf();

        // Open reader on input file:
        let mut reader = LineReader::open(&input_path)?;

        // Create the output temp file (empty):
        let output = NamedTempFile::new()?;
        let output_path = output.path().to_path_buf();
        let out_file = File::create(&output_path)?;

        // Wrap it in your OutputBuffer and run the loop:
        let mut out = OutputBuffer::new(Box::new(out_file));
        let mut nline = 0;
        while let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
            nline += 1;
        }
        assert_eq!(nline, 3);

        out.flush()?;
        assert_eq!(out.low_level_flushes, 1);

        // Verify that files match:
        let expected = fs::read(&input_path)?;
        let actual = fs::read(&output_path)?;
        assert_eq!(actual, expected);
        Ok(())
    }

    #[test]
    #[cfg(unix)]
    fn test_large_file_zero_copy_unterminated() -> io::Result<()> {
        // Create and fill the input temp file:
        let mut input = NamedTempFile::new()?;
        write!(input, "first line\nsecond line\n")?;
        let dot_line = make_dot_line_4k();
        input.write_all(&dot_line)?;
        write!(input, "last line (unterminated)")?;
        input.flush()?;
        let input_path = input.path().to_path_buf();

        // Open reader on input file:
        let mut reader = LineReader::open(&input_path)?;

        // Create the output temp file (empty):
        let output = NamedTempFile::new()?;
        let output_path = output.path().to_path_buf();
        let out_file = File::create(&output_path)?;

        // Wrap it in your OutputBuffer and run the loop:
        let mut out = OutputBuffer::new(Box::new(out_file));
        let mut nline = 0;
        while let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
            nline += 1;
        }
        assert_eq!(nline, 4);

        out.flush()?;
        assert_eq!(out.low_level_flushes, 1);

        // Verify that files match:
        let expected = fs::read(&input_path)?;
        let actual = fs::read(&output_path)?;
        assert_eq!(actual, expected);
        Ok(())
    }

    #[test]
    fn test_small_file_unterminated() -> io::Result<()> {
        // Create and fill the input temp file:
        let mut input = NamedTempFile::new()?;
        write!(input, "first line\nsecond line\nlast line (unterminated)")?;
        input.flush()?;
        let input_path = input.path().to_path_buf();

        // Open reader on input file:
        let mut reader = LineReader::open(&input_path)?;

        // Create the output temp file (empty):
        let output = NamedTempFile::new()?;
        let output_path = output.path().to_path_buf();
        let out_file = File::create(&output_path)?;

        // Wrap it in your OutputBuffer and run the loop:
        let mut out = OutputBuffer::new(Box::new(out_file));
        let mut nline = 0;
        while let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
            nline += 1;
        }
        assert_eq!(nline, 3);

        out.flush()?;
        assert_eq!(out.low_level_flushes, 0);

        // Verify that files match:
        let expected = fs::read(&input_path)?;
        let actual = fs::read(&output_path)?;
        assert_eq!(actual, expected);
        Ok(())
    }

    #[test]
    fn test_small_file_unterminated_stream() -> io::Result<()> {
        // Create and fill the input temp file:
        let mut input = NamedTempFile::new()?;
        write!(input, "first line\nsecond line\nlast line (unterminated)")?;
        input.flush()?;
        let input_path = input.path().to_path_buf();

        // Open reader on input file:
        let mut reader = LineReader::open_stream(&input_path)?;

        // Create the output temp file (empty):
        let output = NamedTempFile::new()?;
        let output_path = output.path().to_path_buf();
        let out_file = File::create(&output_path)?;

        // Wrap it in your OutputBuffer and run the loop:
        let mut out = OutputBuffer::new(Box::new(out_file));
        let mut nline = 0;
        while let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
            nline += 1;
        }
        assert_eq!(nline, 3);

        out.flush()?;
        assert_eq!(out.low_level_flushes, 0);

        // Verify that files match:
        let expected = fs::read(&input_path)?;
        let actual = fs::read(&output_path)?;
        assert_eq!(actual, expected);
        Ok(())
    }

    #[test]
    #[cfg(unix)]
    fn test_large_file_zero_copy_with_flush() -> io::Result<()> {
        // Create and fill the input temp file:
        let mut input = NamedTempFile::new()?;
        write!(input, "first line\nsecond line\n")?;
        let dot_line = make_dot_line_4k();
        // Create more than MAX... to ensure one flush when writing
        let nline_in_file = MAX_PENDING_WRITE_FILE / dot_line.len() + 4;
        for _i in 0..nline_in_file {
            input.write_all(&dot_line)?;
        }
        input.flush()?;
        let input_path = input.path().to_path_buf();

        // Open reader on input file:
        let mut reader = LineReader::open(&input_path)?;

        // Create the output temp file (empty):
        let output = NamedTempFile::new()?;
        let output_path = output.path().to_path_buf();
        let out_file = File::create(&output_path)?;

        // Wrap it in OutputBuffer and run the loop:
        let mut out = OutputBuffer::new(Box::new(out_file));
        let mut nline_written = 0;
        while let Some(chunk) = reader.get_line()? {
            out.write_chunk(&chunk)?;
            nline_written += 1;
        }
        assert_eq!(nline_written, nline_in_file + 2);

        out.flush()?;
        assert_eq!(out.low_level_flushes, 2);

        // Verify that files match:
        let expected = fs::read(&input_path)?;
        let actual = fs::read(&output_path)?;

        // Fast fail with a readable message, without displaying large output
        assert_eq!(actual.len(), expected.len());

        assert_eq!(actual, expected);
        Ok(())
    }

    #[test]
    fn test_stream_read() -> std::io::Result<()> {
        // Create temporary file with known contents
        let mut tmp = NamedTempFile::new()?;
        write!(tmp, "first line\nsecond line\nlast line\n")?;
        tmp.flush()?;

        let path = tmp.path().to_path_buf();
        let mut reader = LineReader::open_stream(&path)?;

        // Verify the reader's operation
        if let Some(IOChunk {
            content:
                IOChunkContent::Owned {
                    content,
                    has_newline,
                    ..
                },
            utf8_verified,
            ..
        }) = reader.get_line()?
        {
            assert_eq!(content, "first line");
            assert_eq!(content.len(), 10);
            assert!(has_newline);
            assert!(!utf8_verified.get());
            assert!(!reader.last_line().unwrap());
        } else {
            panic!("Expected IOChunkContent::Owned");
        }

        if let Some(IOChunk {
            content:
                IOChunkContent::Owned {
                    content,
                    has_newline,
                    ..
                },
            ..
        }) = reader.get_line()?
        {
            assert_eq!(content, "second line");
            assert!(has_newline);
            assert!(!reader.last_line().unwrap());
        } else {
            panic!("Expected IOChunkContent::Owned");
        }

        if let Some(content) = reader.get_line()? {
            assert_eq!(content.as_str().unwrap(), "last line");
            assert!(reader.last_line().unwrap());
        } else {
            panic!("Expected IOChunk");
        }

        assert_eq!(reader.get_line()?, None);

        Ok(())
    }

    #[test]
    #[cfg(unix)]
    fn test_mmap_read() -> std::io::Result<()> {
        // Create temporary file with known contents
        let mut tmp = NamedTempFile::new()?;
        write!(tmp, "first line\nsecond line\nlast line\n")?;
        tmp.flush()?;

        let path = tmp.path().to_path_buf();
        let mut reader = LineReader::open(&path)?;

        // Verify the reader's operation
        if let Some(IOChunk {
            content:
                IOChunkContent::MmapInput {
                    content, full_span, ..
                },
            utf8_verified,
            ..
        }) = reader.get_line()?
        {
            assert_eq!(content, b"first line");
            assert_eq!(content.len(), 10);
            assert_eq!(full_span, b"first line\n");
            assert!(!utf8_verified.get());
            assert!(!reader.last_line().unwrap());
        } else {
            panic!("Expected IOChunkContent::MapInput");
        }

        if let Some(IOChunk {
            content:
                IOChunkContent::MmapInput {
                    content, full_span, ..
                },
            utf8_verified,
            ..
        }) = reader.get_line()?
        {
            assert_eq!(content, b"second line");
            assert_eq!(full_span, b"second line\n");
            assert!(!utf8_verified.get());
            assert!(!reader.last_line().unwrap());
        } else {
            panic!("Expected IOChunkContent::MapInput");
        }

        if let Some(content) = reader.get_line()? {
            assert_eq!(content.as_bytes(), b"last line");
            assert_eq!(content.as_str().unwrap(), "last line");
            assert!(content.utf8_verified.get());
            assert!(reader.last_line().unwrap());
            // Cached version
            assert_eq!(content.as_str().unwrap(), "last line");
        } else {
            panic!("Expected IOChunk");
        }

        assert_eq!(reader.get_line()?, None);

        Ok(())
    }

    // is_newline_terminated, is_empty
    #[test]
    fn test_owned_newline_terminated_non_empty() {
        let chunk = IOChunk::from_content(IOChunkContent::new_owned("line".to_string(), true));
        assert!(chunk.is_newline_terminated());
        assert!(!chunk.is_empty());
    }

    #[test]
    fn test_owned_newline_terminated_empty() {
        let chunk = IOChunk::from_content(IOChunkContent::new_owned(String::new(), true));
        assert!(chunk.is_newline_terminated());
        assert!(chunk.is_empty());
    }

    #[test]
    fn test_owned_not_newline_terminated() {
        let chunk = IOChunk::from_content(IOChunkContent::new_owned("line".to_string(), false));
        assert!(!chunk.is_newline_terminated());
    }

    #[cfg(unix)]
    #[test]
    fn test_mmap_newline_terminated() {
        let content = b"line";
        let full_span = b"line\n";
        let chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));
        assert!(chunk.is_newline_terminated());
    }

    #[cfg(unix)]
    #[test]
    fn test_mmap_not_newline_terminated() {
        let content = b"line";
        let full_span = b"line";
        let chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));
        assert!(!chunk.is_newline_terminated());
    }

    #[cfg(unix)]
    #[test]
    fn test_mmap_empty() {
        let content = b"";
        let full_span = b"";
        let chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));
        assert!(!chunk.is_newline_terminated());
    }

    // ensure_owned()
    #[test]
    fn test_ensure_owned_on_owned() {
        let mut chunk =
            IOChunk::from_content(IOChunkContent::new_owned("already owned".to_string(), true));

        let result = chunk.ensure_owned();
        assert!(result.is_ok());

        // Content must be unchanged
        match &chunk.content {
            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => {
                assert_eq!(content, "already owned");
                assert!(*has_newline);
            }
            #[cfg(unix)]
            _ => panic!("Expected Owned variant"),
        }
    }

    #[cfg(unix)]
    #[test]
    fn test_ensure_owned_on_mmap_valid_utf8() {
        let content = b"mmap string";
        let full_span = b"mmap string\n";

        let mut chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));

        let result = chunk.ensure_owned();
        assert!(result.is_ok());

        match &chunk.content {
            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => {
                assert_eq!(content, "mmap string");
                assert!(*has_newline);
            }
            _ => panic!("Expected Owned variant after ensure_owned"),
        }
    }

    #[cfg(unix)]
    #[test]
    fn test_ensure_owned_on_mmap_valid_utf8_no_newline() {
        let content = b"no newline";
        let full_span = b"no newline";

        let mut chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));

        let result = chunk.ensure_owned();
        assert!(result.is_ok());

        match &chunk.content {
            IOChunkContent::Owned {
                content,
                has_newline,
                ..
            } => {
                assert_eq!(content, "no newline");
                assert!(!*has_newline);
            }
            _ => panic!("Expected Owned variant after ensure_owned"),
        }
    }

    #[cfg(unix)]
    #[test]
    fn test_ensure_owned_on_mmap_invalid_utf8() {
        let content = b"bad\xFFutf8";
        let full_span = b"bad\xFFutf8\n";

        let mut chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));

        let result = chunk.ensure_owned();
        assert!(result.is_err());
        let err_msg = format!("{}", result.unwrap_err());
        assert!(
            err_msg.contains("invalid utf-8"),
            "Unexpected error message: {}",
            err_msg
        );
    }

    // fields_mut
    #[test]
    fn test_fields_mut_on_owned() {
        let mut chunk =
            IOChunk::from_content(IOChunkContent::new_owned("hello".to_string(), false));

        let (s, _) = chunk.fields_mut().unwrap();
        s.push_str(" world");

        assert_eq!(chunk.as_str().unwrap(), "hello world");
    }

    #[cfg(unix)]
    #[test]
    fn test_fields_mut_on_mmap_input_valid_utf8() {
        let content = b"foo";
        let full_span = b"foo\n";
        let mut chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));

        {
            let (s, _) = chunk.fields_mut().unwrap();
            s.push_str("bar");
        }

        assert_eq!(chunk.as_str().unwrap(), "foobar");
    }

    #[cfg(unix)]
    #[test]
    fn test_fields_mut_on_utf8_multibyte() {
        let content = "Ζωντανά!".as_bytes();
        let full_span = "Ζωντανά!\n".as_bytes();
        let mut chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));

        let (s, _) = chunk.fields_mut().unwrap();
        s.push_str(" Δεδομένα");

        assert_eq!(chunk.as_str().unwrap(), "Ζωντανά! Δεδομένα");
    }

    #[cfg(unix)]
    #[test]
    fn test_fields_mut_invalid_utf8() {
        let content = b"abc\xFF"; // invalid UTF-8
        let full_span = b"abc\xFF\n";
        let mut chunk = IOChunk::from_content(new_content_mmap_input(content, full_span));

        let result = chunk.fields_mut();
        assert!(result.is_err());
        assert!(format!("{}", result.unwrap_err()).contains("invalid utf-8"));
    }

    #[cfg(unix)]
    #[test]
    fn fastcopy_regular_file() {
        let mut file = tempfile().expect("create temp file");
        writeln!(file, "hello").unwrap();

        let fc = FastCopy::new(&file);

        assert!(fc.is_regular, "expected regular file");
        assert!(fc.block_size > 0, "block size should be > 0");
    }

    #[cfg(unix)]
    #[test]
    fn fastcopy_non_regular_devnull() {
        let file = File::open("/dev/null").expect("open /dev/null");

        let fc = FastCopy::new(&file);

        assert!(
            !fc.is_regular,
            "expected /dev/null to be reported as non-regular"
        );
    }

    #[cfg(unix)]
    #[test]
    fn test_reliable_write_to_file() {
        // Create an anonymous temporary file.
        let mut file = tempfile().expect("failed to create tempfile");

        // The data to write
        let data = b"hello reliable_write!";
        let fd = file.as_raw_fd();

        // Call our function
        let written = reliable_write(fd, data.as_ptr(), data.len()).expect("reliable_write failed");

        assert_eq!(written, data.len());

        // Rewind file and read back contents
        file.rewind().unwrap();
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).unwrap();

        assert_eq!(&buf, data);
    }

    #[cfg(unix)]
    #[test]
    fn test_reliable_write_handles_partial_write() {
        use std::thread;
        use std::time::Duration;

        // Create a pipe
        let mut fds = [0; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        let read_fd = fds[0];
        let write_fd = fds[1];

        // Spawn a reader that drains slowly
        thread::spawn(move || {
            let mut buf = [0u8; 1024];
            loop {
                let n = unsafe { libc::read(read_fd, buf.as_mut_ptr() as *mut _, buf.len()) };
                if n <= 0 {
                    break;
                }
                thread::sleep(Duration::from_millis(10));
            }
        });

        // Prepare data bigger than pipe capacity to force partial writes
        let big = vec![b'x'; 200_000];

        // This will require multiple kernel writes, exercising write_all
        let n = reliable_write(write_fd, big.as_ptr(), big.len()).expect("reliable_write failed");

        assert_eq!(n, big.len());

        unsafe { libc::close(write_fd) };
    }

    #[test]
    #[cfg(all(target_os = "linux", target_env = "gnu"))]
    fn test_aligned_copy_cover_includes_tail() {
        let mut infile = tempfile().unwrap();
        let mut outfile = tempfile().unwrap();

        // Input data = 10 bytes
        let data = b"abcdefghij";
        infile.write_all(data).unwrap();
        infile.rewind().unwrap();

        let in_fd = infile.as_raw_fd();
        let out_fd = outfile.as_raw_fd();
        let in_ptr = data.as_ptr();

        // Copy with block size 4, cover = Complete
        let copied = aligned_copy_file_range(
            in_ptr,
            in_fd,
            0,
            out_fd,
            data.len(),
            4,
            WriteRange::Complete,
        )
        .unwrap();

        assert_eq!(copied, data.len());

        // Verify outfile contents = full input data
        outfile.rewind().unwrap();
        let mut buf = Vec::new();
        outfile.read_to_end(&mut buf).unwrap();
        assert_eq!(&buf, data);
    }

    #[test]
    #[cfg(all(target_os = "linux", target_env = "gnu"))]
    fn test_aligned_copy_blocks_skips_tail() {
        let mut infile = tempfile().unwrap();
        let mut outfile = tempfile().unwrap();

        // Input data = 10 bytes
        let data = b"abcdefghij";
        infile.write_all(data).unwrap();
        infile.rewind().unwrap();

        let in_fd = infile.as_raw_fd();
        let out_fd = outfile.as_raw_fd();
        let in_ptr = data.as_ptr();

        // Copy with block size 4, cover = Blocks
        let copied =
            aligned_copy_file_range(in_ptr, in_fd, 0, out_fd, data.len(), 4, WriteRange::Blocks)
                .unwrap();

        // Only full 8 bytes (2 blocks of 4) should be copied
        assert_eq!(copied, 8);

        // Verify outfile contents = only the first 8 bytes
        outfile.rewind().unwrap();
        let mut buf = Vec::new();
        outfile.read_to_end(&mut buf).unwrap();
        assert_eq!(&buf, b"abcdefgh");
    }

    ///////////////////////////////
    // Unit tests for write_chunk()
    ///////////////////////////////

    fn new_for_test() -> (OutputBuffer, std::fs::File) {
        let file = tempfile().unwrap();
        let buf = OutputBuffer {
            out: BufWriter::new(Box::new(file.try_clone().unwrap())),
            #[cfg(unix)]
            fast_copy: FastCopy {
                fd: -1,
                is_regular: false,
                block_size: 2,
            },
            #[cfg(unix)]
            max_pending_write: 8,
            #[cfg(unix)]
            mmap_chunk: None,
            low_level_flushes: 0,
        };
        (buf, file)
    }

    #[cfg(unix)]
    fn make_mmap_chunk(bytes: &'static [u8]) -> IOChunk<'static> {
        IOChunk {
            utf8_verified: Cell::new(true),
            content: IOChunkContent::MmapInput {
                fast_copy: FastCopy {
                    fd: -1,
                    is_regular: false,
                    block_size: 1,
                },
                base: bytes.as_ptr(),
                content: bytes,
                full_span: bytes,
            },
        }
    }

    fn make_owned_chunk(s: &str, has_nl: bool) -> IOChunk<'_> {
        IOChunk {
            utf8_verified: Cell::new(true),
            content: IOChunkContent::Owned {
                content: s.to_string(),
                has_newline: has_nl,
                #[cfg(not(unix))]
                _phantom: std::marker::PhantomData,
            },
        }
    }

    #[cfg(unix)]
    #[test]
    fn mmap_new_chunk_single() {
        let (mut outbuf, _file) = new_for_test(); // OutputBuffer

        let c1 = make_mmap_chunk(b"abc");

        outbuf.write_chunk(&c1).unwrap();

        assert_eq!(outbuf.mmap_chunk.as_ref().unwrap().len, 3);
    }

    #[cfg(unix)]
    #[test]
    fn mmap_new_chunk_and_coalesce() {
        let (mut outbuf, _file) = new_for_test(); // OutputBuffer

        let backing = b"abcdefgh"; // contiguous buffer
        let c1 = make_mmap_chunk(&backing[0..4]); // "abcd"
        let c2 = make_mmap_chunk(&backing[4..8]); // "efgh"

        outbuf.write_chunk(&c1).unwrap();
        outbuf.write_chunk(&c2).unwrap();

        assert_eq!(outbuf.mmap_chunk.as_ref().unwrap().len, 8);
    }

    #[test]
    #[cfg(unix)]
    fn mmap_not_contiguous_triggers_flush() {
        let (mut buf, _file) = new_for_test();
        let backing = b"abcdefghi";
        let c1 = make_mmap_chunk(&backing[0..4]); // "abcd"
        // Guaranteed non-coalescable.  Surprisingly, on macOS
        // passing two strings resulted in coalescible data.
        let c2 = make_mmap_chunk(&backing[5..9]); // "fghi"

        buf.write_chunk(&c1).unwrap();
        assert_eq!(buf.mmap_chunk.as_ref().unwrap().len, 4);
        buf.write_chunk(&c2).unwrap();
        // No coalescing
        assert_eq!(buf.mmap_chunk.as_ref().unwrap().len, 4);
    }

    #[test]
    #[cfg(unix)]
    fn mmap_coalesce_and_flush_blocks() {
        let (mut buf, _file) = new_for_test();
        buf.max_pending_write = 4;
        let backing = b"abcdefgh"; // contiguous buffer
        let c1 = make_mmap_chunk(&backing[0..5]); // "abcde"
        let c2 = make_mmap_chunk(&backing[5..8]); // "fgh"

        buf.write_chunk(&c1).unwrap();
        buf.write_chunk(&c2).unwrap();
        // After a flush
        assert_eq!(buf.mmap_chunk.as_ref().unwrap().len, 0);
    }

    #[test]
    fn owned_without_newline() {
        let (mut buf, mut file) = new_for_test();
        let chunk = make_owned_chunk("hello", false);
        buf.write_chunk(&chunk).unwrap();

        buf.out.flush().unwrap();
        file.seek(SeekFrom::Start(0)).unwrap();
        let mut out = String::new();
        file.read_to_string(&mut out).unwrap();

        assert_eq!(out, "hello");
    }

    #[test]
    fn owned_with_newline() {
        let (mut buf, mut file) = new_for_test();
        let chunk = make_owned_chunk("world", true);
        buf.write_chunk(&chunk).unwrap();

        buf.out.flush().unwrap();
        file.seek(SeekFrom::Start(0)).unwrap();
        let mut out = String::new();
        file.read_to_string(&mut out).unwrap();

        assert_eq!(out, "world\n");
    }
}
