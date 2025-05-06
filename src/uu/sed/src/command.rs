// Definitions for the compiled code data structures
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

// TODO: remove when compile is implemented
#![allow(dead_code)]

use crate::fast_io::LineReader;
use crate::fast_io::OutputBuffer;
use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf; // For file descriptors and equivalent

// Compilation and processing options provided mostly through the
// command-line interface
#[derive(Debug, Default)]
pub struct CliOptions {
    // Command-line flags with corresponding names
    pub all_output_files: bool,
    pub debug: bool,
    pub regexp_extended: bool,
    pub follow_symlinks: bool,
    pub in_place: bool,
    pub in_place_suffix: Option<String>,
    pub length: usize,
    pub quiet: bool,
    pub posix: bool,
    pub separate: bool,
    pub sandbox: bool,
    pub unbuffered: bool,
    pub null_data: bool,
}

// The specification of a script: through a string or a file
#[derive(Debug, PartialEq)]
pub enum ScriptValue {
    StringVal(String),
    PathVal(PathBuf),
}

/*
 * Types of address specifications
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressType {
    Re,      // Line that matches regex
    Line,    // Specific line
    RelLine, // Relative line
    Last,    // Last line
}

/*
 * Format of an address
 */
#[derive(Debug)]
pub struct Address {
    pub atype: AddressType,  // Address type
    pub value: AddressValue, // Line number or regex
}

#[derive(Debug)]
pub enum AddressValue {
    LineNumber(usize),
    Regex(Regex),
}

/*
 * Substitution command
 */
#[derive(Debug)]
pub struct Substitution {
    pub occurrence: usize,             // Which occurrence to substitute
    pub print_flag: bool,              // True if 'p' flag
    pub ignore_case: bool,             // True if 'I' flag
    pub write_file: Option<PathBuf>,   // Path to file if 'w' flag is used
    pub file_descriptor: Option<File>, // Cached file descriptor
    pub regex: Regex,                  // Regular expression
    pub max_backref: u32,              // Largest backreference
    pub line_number: usize,            // Line number
    pub replacement: String,           // Replacement text
}

// Transliteration command (y)
#[derive(Debug)]
pub struct Transliteration {
    pub byte_table: [u8; 256],          // Byte translation table
    pub multi_map: HashMap<char, char>, // Direct mapping from one char to another
}

/*
 * An internally compiled command.
 */
#[derive(Debug)]
pub struct Command {
    pub code: char,                 // Command code
    pub addr1: Option<Address>,     // Start address
    pub addr2: Option<Address>,     // End address
    pub non_select: bool,           // True if '!'
    pub start_line: Option<usize>,  // Start line number (or None)
    pub text: Option<String>,       // Text for ':', 'a', 'c', 'i', 'r', 'w'
    pub data: CommandData,          // Command-specific data
    pub next: Option<Box<Command>>, // Pointer to next command
}

impl Default for Command {
    fn default() -> Self {
        Command {
            code: '_',
            addr1: None,
            addr2: None,
            non_select: false,
            start_line: Some(0),
            text: None,
            data: CommandData::None,
            next: None,
        }
    }
}

#[derive(Debug)]
pub enum CommandData {
    None,
    SubCommands(Vec<Command>),             // Commands for 'b', 't', '{'
    Substitution(Box<Substitution>),       // Substitute command 's'
    Transliteration(Box<Transliteration>), // Transliteration command 'y'
    WriteFileDescriptor(File),             // File descriptor for 'w'
}

/*
 * Structure containing things to append before a line is read
 */
#[derive(Debug)]
pub struct AppendBuffer {
    append_type: AppendType,
    content: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppendType {
    String,
    File,
}

/*
 * Special flag for space modifications
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceFlag {
    Append,  // Append to contents
    Replace, // Replace contents
}

/*
 * Structure for a processing space (process, hold, otherwise).
 */
#[derive(Debug)]
pub struct Space {
    pub current: String,      // Current space content
    pub deleted: bool,        // Whether content was deleted
    pub append_newline: bool, // Whether originally terminated by \n
    pub backup: String,       // Backing memory
}

/// Context for processing multiple files and in-place replacements
pub struct ProcessingContext {
    pub reader: LineReader,
    pub output: OutputBuffer,
    pub input_files: Vec<PathBuf>,
    pub cli_options: CliOptions,
}
