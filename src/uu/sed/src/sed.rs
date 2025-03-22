// This file is part of the uutils sed package.
//
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

pub mod command;
pub mod compiler;
pub mod processor;

use crate::command::ScriptValue;
use crate::compiler::compile;
use crate::processor::process;
use clap::{arg, Arg, ArgMatches, Command};
use std::path::PathBuf;
use uucore::error::{UResult, UUsageError};
use uucore::format_usage;

const ABOUT: &str = "Stream editor for filtering and transforming text";
const USAGE: &str = "sed [OPTION]... [script] [file]...";

/*
 * Iterate through script and file arguments specified in matches and
 * return vectors of all scripts and input files in the specified order.
 * If no script is specified fail with "missing script" error.
 */
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

#[uucore::main]
pub fn uumain(args: impl uucore::Args) -> UResult<()> {
    let matches = uu_app().try_get_matches_from(args)?;
    let (scripts, files) = get_scripts_files(&matches)?;
    let executable = compile(scripts)?;
    process(executable, files)?;
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
                .help("Use extended regular expressions."),
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

#[cfg(test)]
mod tests {
    use super::*; // Allows access to private functions/items in this module

    // Test the get_scripts_files function

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
}
