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

use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf; // For file descriptors and equivalent

// Compilation and processing context
#[derive(Debug)]
pub struct Context {
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
enum AddressType {
    Re,      // Line that matches regex
    Line,    // Specific line
    RelLine, // Relative line
    Last,    // Last line
}

/*
 * Format of an address
 */
#[derive(Debug)]
struct Address {
    atype: AddressType,  // Address type
    value: AddressValue, // Line number or regex
}

#[derive(Debug)]
enum AddressValue {
    LineNumber(u64),
    Regex(Regex),
}

/*
 * Substitution command
 */
#[derive(Debug)]
struct Substitution {
    occurrence: usize,             // Which occurrence to substitute
    print_flag: bool,              // True if 'p' flag
    ignore_case: bool,             // True if 'I' flag
    write_file: Option<PathBuf>,   // Path to file if 'w' flag is used
    file_descriptor: Option<File>, // Cached file descriptor
    regex: Regex,                  // Regular expression
    max_backref: u32,              // Largest backreference
    line_number: u64,              // Line number
    replacement: String,           // Replacement text
}

/*
 * Translate command.
 */
#[derive(Debug)]
struct TranslateCommand {
    byte_table: [u8; 256],          // Byte translation table
    multi_map: HashMap<char, char>, // Direct mapping from one char to another
}

/*
 * An internally compiled command.
 */
#[derive(Debug)]
pub struct Command {
    next: Option<Box<Command>>, // Pointer to next command
    addr1: Option<Address>,     // Start address
    addr2: Option<Address>,     // End address
    start_line: Option<u64>,    // Start line number (or None)
    text: Option<String>,       // Text for ':', 'a', 'c', 'i', 'r', 'w'
    data: CommandData,          // Union equivalent
    code: char,                 // Command code
    non_select: bool,           // True if '!'
}

#[derive(Debug)]
enum CommandData {
    SubCommands(Vec<Command>),        // Commands for 'b', 't', '{'
    Substitution(Box<Substitution>),  // Substitute command
    Translate(Box<TranslateCommand>), // Replace command array
    WriteFileDescriptor(File),        // File descriptor for 'w'
}

/*
 * Structure containing things to append before a line is read
 */
#[derive(Debug)]
struct AppendBuffer {
    append_type: AppendType,
    content: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AppendType {
    String,
    File,
}

/*
 * Special flag for space modifications
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SpaceFlag {
    Append,  // Append to contents
    Replace, // Replace contents
}

/*
 * Structure for a processing space (process, hold, otherwise).
 */
#[derive(Debug)]
struct Space {
    current: String,      // Current space content
    deleted: bool,        // Whether content was deleted
    append_newline: bool, // Whether originally terminated by \n
    backup: String,       // Backing memory
}
