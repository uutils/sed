// Program entry point and CLI processing
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

pub mod command;
pub mod compiler;
pub mod delimited_parser;
pub mod fast_io;
pub mod in_place;
pub mod named_writer;
pub mod processor;
pub mod script_char_provider;
pub mod script_line_provider;

use crate::command::{ProcessingContext, ScriptValue};
use crate::compiler::compile;
use crate::processor::process_all_files;
use clap::{arg, Arg, ArgMatches, Command};
use std::path::PathBuf;
use uucore::error::{UResult, UUsageError};
use uucore::format_usage;

const ABOUT: &str = "Stream editor for filtering and transforming text";
const USAGE: &str = "sed [OPTION]... [script] [file]...";

#[uucore::main]
pub fn uumain(args: impl uucore::Args) -> UResult<()> {
    let matches = uu_app().try_get_matches_from(args)?;
    let (scripts, files) = get_scripts_files(&matches)?;
    let mut processing_context = build_context(&matches);

    let executable = compile(scripts, &mut processing_context)?;
    process_all_files(executable, files, processing_context)?;
    Ok(())
}

#[allow(clippy::cognitive_complexity)]
pub fn uu_app() -> Command {
    Command::new(uucore::util_name())
        .about(ABOUT)
        .override_usage(format_usage(USAGE))
        .infer_long_args(true)
        .args([
            arg!([script] "Script to execute if not otherwise provided."),
            Arg::new("file")
                .help("Input files")
                .value_parser(clap::value_parser!(PathBuf))
                .num_args(0..),
            Arg::new("all-output-files")
                .long("all-output-files")
                .short('a')
                .help("Create or truncate all output files before processing.")
                .action(clap::ArgAction::SetTrue),
            arg!(--debug "Annotate program execution."),
            Arg::new("regexp-extended")
                .short('E')
                .long("regexp-extended")
                .short_alias('r')
                .help("Use extended regular expressions.")
                .action(clap::ArgAction::SetTrue),
            arg!(-e --expression <SCRIPT> "Add script to executed commands.")
                .action(clap::ArgAction::Append),
            // Access with .get_many::<PathBuf>("file")
            Arg::new("script-file")
                .short('f')
                .long("script-file")
                .help("Specify script file.")
                .value_parser(clap::value_parser!(PathBuf))
                .action(clap::ArgAction::Append),
            Arg::new("follow-symlinks")
                .long("follow-symlinks")
                .help("Follow symlinks when processing in place.")
                .action(clap::ArgAction::SetTrue),
            // Access with .get_one::<String>("in-place")
            Arg::new("in-place")
                .short('i')
                .long("in-place")
                .help("Edit files in place, making a backup if SUFFIX is supplied.")
                .num_args(0..=1)
                .default_missing_value(""),
            // Access with .get_one::<u32>("line-length")
            arg!(-l --length <NUM> "Specify the 'l' command line-wrap length.")
                .value_parser(clap::value_parser!(u32)),
            arg!(-n --quiet "Suppress automatic printing of pattern space.").aliases(["silent"]),
            arg!(--posix "Disable all POSIX extensions."),
            arg!(-s --separate "Consider files as separate rather than as a long stream."),
            arg!(--sandbox "Operate in a sandbox by disabling e/r/w commands."),
            arg!(-u --unbuffered "Load minimal input data and flush output buffers regularly."),
            Arg::new("null-data")
                .short('z')
                .long("null-data")
                .help("Separate lines by NUL characters.")
                .action(clap::ArgAction::SetTrue),
        ])
}

// Iterate through script and file arguments specified in matches and
// return vectors of all scripts and input files in the specified order.
// If no script is specified fail with "missing script" error.
fn get_scripts_files(matches: &ArgMatches) -> UResult<(Vec<ScriptValue>, Vec<PathBuf>)> {
    let mut indexed_scripts: Vec<(usize, ScriptValue)> = Vec::new();
    let mut files: Vec<PathBuf> = Vec::new();

    let script_through_options =
        // The specification of a script: through a string or a file.
        matches.contains_id("expression") || matches.contains_id("script-file");

    if script_through_options {
        // Second and third POSIX usage cases; clap script arg is actually an input file
        // sed [-En] -e script [-e script]... [-f script_file]... [file...]
        // sed [-En] [-e script]... -f script_file [-f script_file]... [file...]
        if let Some(val) = matches.get_one::<String>("script") {
            files.push(PathBuf::from(val.to_owned()));
        }
    } else {
        // First POSIX spec usage case; script is the first arg.
        // sed [-En] script [file...]
        if let Some(val) = matches.get_one::<String>("script") {
            indexed_scripts.push((0, ScriptValue::StringVal(val.to_owned())));
        } else {
            return Err(UUsageError::new(1, "missing script"));
        }
    }

    // Capture -e occurrences (STRING)
    if let Some(indices) = matches.indices_of("expression") {
        for (idx, val) in indices.zip(matches.get_many::<String>("expression").unwrap_or_default())
        {
            indexed_scripts.push((idx, ScriptValue::StringVal(val.to_owned())));
        }
    }

    // Capture -f occurrences (FILE)
    if let Some(indices) = matches.indices_of("script-file") {
        for (idx, val) in indices.zip(
            matches
                .get_many::<PathBuf>("script-file")
                .unwrap_or_default(),
        ) {
            indexed_scripts.push((idx, ScriptValue::PathVal(val.to_owned())));
        }
    }

    // Sort by index to preserve argument order.
    indexed_scripts.sort_by_key(|k| k.0);
    // Keep only the values.
    let scripts = indexed_scripts
        .into_iter()
        .map(|(_, value)| value)
        .collect();

    let rest_files: Vec<PathBuf> = matches
        .get_many::<PathBuf>("file")
        .unwrap_or_default()
        .cloned()
        .collect();
    if !rest_files.is_empty() {
        files.extend(rest_files);
    }

    // Read from stdin if no file has been specified.
    if files.is_empty() {
        files.push(PathBuf::from("-"));
    }

    Ok((scripts, files))
}

// Parse CLI flag arguments and return a ProcessingContext struct based on them
fn build_context(matches: &ArgMatches) -> ProcessingContext {
    ProcessingContext {
        all_output_files: matches.get_flag("all-output-files"),
        debug: matches.get_flag("debug"),
        regexp_extended: matches.get_flag("regexp-extended"),
        follow_symlinks: matches.get_flag("follow-symlinks"),
        in_place: matches.contains_id("in-place"),
        in_place_suffix: matches.get_one::<String>("in-place").and_then(|s| {
            if s.is_empty() {
                None
            } else {
                Some(s.clone())
            }
        }),
        length: matches
            .get_one::<u32>("length")
            .map(|v| *v as usize)
            .unwrap_or(70),
        quiet: matches.get_flag("quiet"),
        posix: matches.get_flag("posix"),
        separate: matches.get_flag("separate"),
        sandbox: matches.get_flag("sandbox"),
        unbuffered: matches.get_flag("unbuffered"),
        null_data: matches.get_flag("null-data"),

        // Other context
        line_number: 0,
        last_address: false,
        last_line: false,
        last_file: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Allows access to private functions/items in this module

    // get_scripts_files

    // Helper function for supplying arguments
    fn get_test_matches(args: &[&str]) -> ArgMatches {
        uu_app().get_matches_from(["myapp"].iter().chain(args.iter()))
    }

    #[test]
    fn test_script_as_first_argument() {
        let matches = get_test_matches(&["1d", "file1.txt"]);
        let (scripts, files) = get_scripts_files(&matches).expect("Should succeed");

        assert_eq!(scripts, vec![ScriptValue::StringVal("1d".to_string())]);
        assert_eq!(files, vec![PathBuf::from("file1.txt")]);
    }

    #[test]
    fn test_expression_argument() {
        let matches = get_test_matches(&["-e", "s/foo/bar/", "file1.txt"]);
        let (scripts, files) = get_scripts_files(&matches).expect("Should succeed");

        assert_eq!(
            scripts,
            vec![ScriptValue::StringVal("s/foo/bar/".to_string())]
        );
        assert_eq!(files, vec![PathBuf::from("file1.txt")]);
    }

    #[test]
    fn test_script_file_argument() {
        let matches = get_test_matches(&["-f", "script.sed", "file1.txt"]);
        let (scripts, files) = get_scripts_files(&matches).expect("Should succeed");

        assert_eq!(
            scripts,
            vec![ScriptValue::PathVal(PathBuf::from("script.sed"))]
        );
        assert_eq!(files, vec![PathBuf::from("file1.txt")]);
    }

    #[test]
    fn test_multiple_files() {
        let matches = get_test_matches(&["-e", "s/foo/bar/", "file1.txt", "file2.txt"]);
        let (scripts, files) = get_scripts_files(&matches).expect("Should succeed");

        assert_eq!(
            scripts,
            vec![ScriptValue::StringVal("s/foo/bar/".to_string())]
        );
        assert_eq!(
            files,
            vec![PathBuf::from("file1.txt"), PathBuf::from("file2.txt")]
        );
    }

    #[test]
    fn test_multiple_files_script() {
        let matches = get_test_matches(&["s/foo/bar/", "file1.txt", "file2.txt"]);
        let (scripts, files) = get_scripts_files(&matches).expect("Should succeed");

        assert_eq!(
            scripts,
            vec![ScriptValue::StringVal("s/foo/bar/".to_string())]
        );
        assert_eq!(
            files,
            vec![PathBuf::from("file1.txt"), PathBuf::from("file2.txt")]
        );
    }

    #[test]
    fn test_stdin_when_no_files() {
        let matches = get_test_matches(&["-e", "s/foo/bar/"]);
        let (scripts, files) = get_scripts_files(&matches).expect("Should succeed");

        assert_eq!(
            scripts,
            vec![ScriptValue::StringVal("s/foo/bar/".to_string())]
        );
        assert_eq!(files, vec![PathBuf::from("-")]); // Stdin should be used
    }

    #[test]
    fn test_stdin_when_no_files_script() {
        let matches = get_test_matches(&["s/foo/bar/"]);
        let (scripts, files) = get_scripts_files(&matches).expect("Should succeed");

        assert_eq!(
            scripts,
            vec![ScriptValue::StringVal("s/foo/bar/".to_string())]
        );
        assert_eq!(files, vec![PathBuf::from("-")]); // Stdin should be used
    }

    // build_context
    fn test_matches(args: &[&str]) -> ArgMatches {
        uu_app().get_matches_from(["sed"].into_iter().chain(args.iter().copied()))
    }

    #[test]
    fn test_defaults() {
        let matches = test_matches(&[]);
        let ctx = build_context(&matches);

        assert!(!ctx.all_output_files);
        assert!(!ctx.debug);
        assert!(!ctx.regexp_extended);
        assert!(!ctx.follow_symlinks);
        assert!(!ctx.in_place);
        assert_eq!(ctx.in_place_suffix, None);
        assert_eq!(ctx.length, 70);
        assert!(!ctx.quiet);
        assert!(!ctx.posix);
        assert!(!ctx.separate);
        assert!(!ctx.sandbox);
        assert!(!ctx.unbuffered);
        assert!(!ctx.null_data);
    }

    #[test]
    fn test_all_flags() {
        let matches = test_matches(&[
            "--all-output-files",
            "--debug",
            "-E",
            "--follow-symlinks",
            "-i",
            "-l",
            "80",
            "-n",
            "--posix",
            "-s",
            "--sandbox",
            "-u",
            "-z",
        ]);

        let ctx = build_context(&matches);

        assert!(ctx.all_output_files);
        assert!(ctx.debug);
        assert!(ctx.regexp_extended);
        assert!(ctx.follow_symlinks);
        assert!(ctx.in_place);
        assert!(ctx.in_place_suffix.is_none());
        assert_eq!(ctx.length, 80);
        assert!(ctx.quiet);
        assert!(ctx.posix);
        assert!(ctx.separate);
        assert!(ctx.sandbox);
        assert!(ctx.unbuffered);
        assert!(ctx.null_data);
    }

    #[test]
    fn test_in_place_with_suffix() {
        let matches = test_matches(&["-i", ".bak"]);
        let ctx = build_context(&matches);

        assert!(ctx.in_place);
        assert_eq!(ctx.in_place_suffix, Some(".bak".to_string()));
    }

    #[test]
    fn test_length_default_and_custom() {
        let matches_default = test_matches(&[]);
        let matches_custom = test_matches(&["-l", "120"]);

        let ctx_default = build_context(&matches_default);
        let ctx_custom = build_context(&matches_custom);

        assert_eq!(ctx_default.length, 70);
        assert_eq!(ctx_custom.length, 120);
    }
}
