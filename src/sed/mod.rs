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
pub mod error_handling;
pub mod fast_io;
pub mod fast_regex;
pub mod in_place;
pub mod named_reader;
pub mod named_writer;
pub mod processor;
pub mod script_char_provider;
pub mod script_line_provider;

use crate::sed::command::{ByteSpace, CharacterMode, ProcessingContext};
use crate::sed::compiler::compile;
use crate::sed::processor::process_all_files;
use crate::sed::script_line_provider::ScriptValue;
use clap::{Arg, ArgMatches, Command, arg};
use std::collections::HashMap;
use std::env;
use std::ffi::OsString;
use std::path::PathBuf;
use uucore::error::{UResult, USimpleError, UUsageError};
use uucore::format_usage;

const ABOUT: &str = "Stream editor for filtering and transforming text (part of uutils)";
const USAGE: &str = "sed [OPTION]... [script] [file]...";
const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " (uutils)");

#[uucore::main]
pub fn uumain(args: impl uucore::Args) -> UResult<()> {
    let matches = parse_args(args)?;

    // Don't use arg_required_else_help when declaring command
    // as it exits with code 2 and we use it to check
    // default matches in tests.
    if !matches.args_present() {
        let _ = uu_app().print_help();
        std::process::exit(1);
    }

    let (scripts, files) = get_scripts_files(&matches)?;
    let mut context = build_context(&matches)?;

    let executable = compile(scripts, &mut context)?;
    process_all_files(executable, files, &mut context)?;
    Ok(())
}

fn parse_args(args: impl IntoIterator<Item = OsString>) -> clap::error::Result<ArgMatches> {
    let mut cmd = uu_app();
    cmd.build();
    let args = gnu_in_place_args(&cmd, args);
    cmd.try_get_matches_from(args)
}

/// Rewrite GNU's `-iSUFFIX`, which clap cannot parse, as `--in-place=SUFFIX`,
/// splitting it from flags clustered before it: `-ni.bak` becomes
/// `-n --in-place=.bak`. The values of other options are left as they are.
fn gnu_in_place_args(cmd: &Command, args: impl IntoIterator<Item = OsString>) -> Vec<OsString> {
    let mut args = args.into_iter();
    // The program name.
    let mut out: Vec<OsString> = args.next().into_iter().collect();
    while let Some(arg) = args.next() {
        // Pass non-UTF-8 arguments to clap unchanged.
        let arg = match arg.into_string() {
            Ok(arg) => arg,
            Err(arg) => {
                out.push(arg);
                continue;
            }
        };
        if arg == "--" {
            out.push(arg.into());
            break;
        }
        match option_kind(cmd, &arg) {
            OptionKind::InPlace(pos) => {
                if pos > 1 {
                    out.push(arg[..pos].into());
                }
                out.push(format!("--in-place={}", &arg[pos + 1..]).into());
            }
            OptionKind::ValueFollows => {
                out.push(arg.into());
                // Keep the value as is, even if it looks like `-iSUFFIX`.
                out.extend(args.next());
            }
            OptionKind::Other => out.push(arg.into()),
        }
    }
    out.extend(args);
    out
}

enum OptionKind {
    /// A cluster like `-ni.bak`, with the position of its `i`.
    InPlace(usize),
    /// An option whose value is the next argument, like `-f FILE`.
    ValueFollows,
    Other,
}

fn option_kind(cmd: &Command, arg: &str) -> OptionKind {
    if let Some(name) = arg.strip_prefix("--") {
        return match long_option(cmd, name) {
            Some(opt) if opt.get_action().takes_values() && !opt.is_require_equals_set() => {
                OptionKind::ValueFollows
            }
            _ => OptionKind::Other,
        };
    }
    let Some(cluster) = arg.strip_prefix('-') else {
        return OptionKind::Other;
    };
    for (pos, c) in cluster.char_indices() {
        // Leave unknown options for clap to report.
        let Some(opt) = cmd.get_arguments().find(|a| {
            a.get_short() == Some(c) || a.get_all_short_aliases().is_some_and(|s| s.contains(&c))
        }) else {
            return OptionKind::Other;
        };
        let attached = pos + c.len_utf8() < cluster.len();
        if opt.get_id() == "in-place" {
            return if attached {
                OptionKind::InPlace(pos + 1)
            } else {
                OptionKind::Other
            };
        }
        if opt.get_action().takes_values() {
            // The rest of the cluster, or else the next argument, is the value.
            return if attached {
                OptionKind::Other
            } else {
                OptionKind::ValueFollows
            };
        }
    }
    OptionKind::Other
}

/// Find a long option by its name or, as `infer_long_args` allows, a unique
/// prefix of it. `None` if `name` includes a value.
fn long_option<'a>(cmd: &'a Command, name: &str) -> Option<&'a Arg> {
    if name.contains('=') {
        return None;
    }
    let names = |a: &'a Arg| {
        a.get_long()
            .into_iter()
            .chain(a.get_all_aliases().into_iter().flatten())
    };
    if let Some(opt) = cmd.get_arguments().find(|a| names(a).any(|n| n == name)) {
        return Some(opt);
    }
    let mut found = cmd
        .get_arguments()
        .filter(|a| names(a).any(|n| n.starts_with(name)));
    let opt = found.next()?;
    found.next().is_none().then_some(opt)
}

#[allow(clippy::cognitive_complexity)]
pub fn uu_app() -> Command {
    #[cfg(windows)]
    let util_name = "sed";
    #[cfg(not(windows))]
    let util_name = uucore::util_name();

    Command::new(util_name)
        .version(VERSION)
        .about(ABOUT)
        .override_usage(format_usage(USAGE))
        .args_override_self(true)
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
            // As in GNU sed, a value may begin with `-`.
            arg!(-e --expression <SCRIPT> "Add script to executed commands.")
                .allow_hyphen_values(true)
                .action(clap::ArgAction::Append),
            // Access with .get_many::<PathBuf>("file")
            Arg::new("script-file")
                .short('f')
                .long("script-file")
                .help("Specify script file.")
                .value_parser(clap::value_parser!(PathBuf))
                .allow_hyphen_values(true)
                .action(clap::ArgAction::Append),
            Arg::new("follow-symlinks")
                .long("follow-symlinks")
                .help("Follow symlinks when processing in place.")
                .action(clap::ArgAction::SetTrue),
            // Access with .get_one::<String>("in-place")
            // The SUFFIX must be attached, as in GNU sed.
            Arg::new("in-place")
                .short('i')
                .long("in-place")
                .help("Edit files in place, making a backup if SUFFIX is supplied.")
                .value_name("SUFFIX")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value(""),
            // Access with .get_one::<u32>("line-length")
            arg!(-l --"line-length" <NUM> "Specify the 'l' command line-wrap length.")
                // The long name used before GNU sed's --line-length was accepted.
                .alias("length")
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(u32)),
            arg!(-n --quiet "Suppress automatic printing of pattern space.").aliases(["silent"]),
            arg!(--posix "Disable non-POSIX extensions."),
            arg!(-s --separate "Consider files as separate rather than as a long stream."),
            arg!(--sandbox "Operate in a sandbox by disabling e/r/w commands."),
            arg!(-u --unbuffered "Load minimal input data and flush output buffers regularly."),
            Arg::new("uutil-extensions")
                .short('U')
                .long("uutil-extensions")
                .help("Enable incompatible extensions.")
                .action(clap::ArgAction::SetTrue),
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

/// Return the character interpretation mode implied by the process locale.
///
/// The mode is determined from the first non-empty of LC_ALL, LC_CTYPE, and
/// LANG. The C and POSIX locales select byte mode. UTF-8 locales (including
/// C.UTF-8) select UTF-8 mode. All other locales result in an error.
pub fn character_mode_for_locale(locale: &str) -> UResult<CharacterMode> {
    if locale == "C" || locale == "POSIX" {
        Ok(CharacterMode::Byte)
    } else if locale.eq_ignore_ascii_case("C.UTF-8")
        || locale.to_ascii_lowercase().ends_with(".utf-8")
        || locale.to_ascii_lowercase().ends_with(".utf8")
    {
        Ok(CharacterMode::Utf8)
    } else {
        Err(USimpleError::new(
            1,
            format!("unsupported locale: {locale}"),
        ))
    }
}

/// Return a ProcessingContext based on parsed CLI flags and environment.
fn build_context(matches: &ArgMatches) -> UResult<ProcessingContext> {
    let locale = ["LC_ALL", "LC_CTYPE", "LANG"]
        .into_iter()
        .find_map(|name| {
            let value = env::var(name).ok()?;
            (!value.is_empty()).then_some(value)
        })
        // Same default as GNU sed
        .unwrap_or_else(|| "C".to_string());

    Ok(ProcessingContext {
        // CLI arguments
        all_output_files: matches.get_flag("all-output-files"),
        debug: matches.get_flag("debug"),
        regex_extended: matches.get_flag("regexp-extended"),
        follow_symlinks: matches.get_flag("follow-symlinks"),
        in_place: matches.contains_id("in-place"),
        in_place_suffix: matches
            .get_one::<String>("in-place")
            .and_then(|s| if s.is_empty() { None } else { Some(s.clone()) }),
        length: matches
            .get_one::<u32>("line-length")
            .map_or(70, |v| *v as usize),
        quiet: matches.get_flag("quiet"),
        posix: matches.get_flag("posix"),
        separate: matches.get_flag("separate") || matches.contains_id("in-place"),
        sandbox: matches.get_flag("sandbox"),
        unbuffered: matches.get_flag("unbuffered"),
        null_data: matches.get_flag("null-data"),
        uutil_extensions: matches.get_flag("uutil-extensions"),

        // Environment
        character_mode: character_mode_for_locale(&locale)?,

        // Other context
        input_name: PathBuf::from("-"),
        line_number: 0,
        last_address: false,
        last_line: false,
        last_file: false,
        stop_processing: false,
        saved_regex: None,
        input_action: None,
        hold: ByteSpace {
            content: Vec::new(),
            has_newline: true,
        },
        parsed_block_nesting: 0,
        label_to_command_map: HashMap::new(),
        named_readers: HashMap::new(),
        range_commands: Vec::new(),
        substitution_made: false,
        append_elements: Vec::new(),
    })
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
        let ctx = build_context(&matches).unwrap();

        assert!(!ctx.all_output_files);
        assert!(!ctx.debug);
        assert!(!ctx.regex_extended);
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
        assert!(!ctx.uutil_extensions);
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
            "-U",
            "-z",
        ]);

        let ctx = build_context(&matches).unwrap();

        assert!(ctx.all_output_files);
        assert!(ctx.debug);
        assert!(ctx.regex_extended);
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
        assert!(ctx.uutil_extensions);
    }

    #[test]
    fn test_multiple_same_arguments() {
        let matches = test_matches(&["-E", "-r"]);
        let ctx = build_context(&matches).unwrap();

        assert!(ctx.regex_extended);
    }

    // In-place argument rewriting
    fn in_place_args(args: &[&str]) -> Vec<String> {
        let mut cmd = uu_app();
        cmd.build();
        gnu_in_place_args(&cmd, ["sed"].iter().chain(args).map(OsString::from))
            .into_iter()
            .skip(1)
            .map(|arg| arg.into_string().unwrap())
            .collect()
    }

    #[test]
    fn test_in_place_args_rewrite_attached_suffix() {
        let cases: &[(&[&str], &[&str])] = &[
            (&["-i.bak"], &["--in-place=.bak"]),
            // Everything after the `i` is the suffix, as in GNU sed.
            (&["-iE"], &["--in-place=E"]),
            (&["-i=.bak"], &["--in-place==.bak"]),
            // Flags clustered before `-i` are kept.
            (&["-ni.bak"], &["-n", "--in-place=.bak"]),
            (&["-Esi.bak"], &["-Es", "--in-place=.bak"]),
            (&["-ri.bak"], &["-r", "--in-place=.bak"]),
            (
                &["-i.bak", "--", "-i.keep"],
                &["--in-place=.bak", "--", "-i.keep"],
            ),
            // Only the next argument is another option's value.
            (&["-e", "p", "-i.bak"], &["-e", "p", "--in-place=.bak"]),
            (
                &["--expression=p", "-i.bak"],
                &["--expression=p", "--in-place=.bak"],
            ),
            (&["--quiet", "-i.bak"], &["--quiet", "--in-place=.bak"]),
            (
                &["--in-place", "-i.bak"],
                &["--in-place", "--in-place=.bak"],
            ),
            // A `--` that is an option's value does not end the options.
            (&["-e", "--", "-i.bak"], &["-e", "--", "--in-place=.bak"]),
        ];
        for (args, expected) in cases {
            assert_eq!(in_place_args(args), *expected, "args: {args:?}");
        }
    }

    #[test]
    fn test_in_place_args_leave_other_arguments_alone() {
        let cases: &[&[&str]] = &[
            &["-i", "s/a/b/", "file"],
            &["-Ei", "s/a/b/", "file"],
            &["-En", "s/a/b/", "file"],
            &["--in-place=.bak", "s/a/b/", "file"],
            &["--in=.bak", "s/a/b/", "file"],
            // Values attached to other options.
            &["-fi.sed", "file"],
            &["-nfi.sed", "file"],
            &["-ei", "file"],
            // Values that follow other options.
            &["-f", "-ifoo.sed", "file"],
            &["-nf", "-ifoo.sed", "file"],
            &["-l", "-i5"],
            &["--expression", "-i.bak"],
            &["--expr", "-i.bak"],
            // Unknown options are left for clap to report.
            &["-xi.bak"],
            &["--", "-i.bak"],
        ];
        for args in cases {
            assert_eq!(in_place_args(args), *args, "args: {args:?}");
        }
    }

    #[test]
    fn test_length_default_and_custom() {
        let matches_default = test_matches(&[]);
        let matches_custom = test_matches(&["-l", "120"]);

        let ctx_default = build_context(&matches_default).unwrap();
        let ctx_custom = build_context(&matches_custom).unwrap();

        assert_eq!(ctx_default.length, 70);
        assert_eq!(ctx_custom.length, 120);
    }

    #[test]
    fn c_locale_selects_byte_mode() {
        assert_eq!(character_mode_for_locale("C").unwrap(), CharacterMode::Byte);
    }

    #[test]
    fn posix_locale_selects_byte_mode() {
        assert_eq!(
            character_mode_for_locale("POSIX").unwrap(),
            CharacterMode::Byte
        );
    }

    #[test]
    fn c_utf8_locale_selects_utf8_mode() {
        assert_eq!(
            character_mode_for_locale("C.UTF-8").unwrap(),
            CharacterMode::Utf8
        );
    }

    #[test]
    fn dot_utf8_locale_selects_utf8_mode() {
        assert_eq!(
            character_mode_for_locale("en_US.UTF-8").unwrap(),
            CharacterMode::Utf8
        );
    }

    #[test]
    fn dot_utf8_locale_is_case_insensitive() {
        assert_eq!(
            character_mode_for_locale("el_GR.utf8").unwrap(),
            CharacterMode::Utf8
        );
    }

    #[test]
    fn unsupported_locale_reports_locale_name() {
        let err = character_mode_for_locale("el_GR.ISO-8859-7").unwrap_err();

        assert_eq!(err.code(), 1);
        assert!(
            err.to_string()
                .contains("unsupported locale: el_GR.ISO-8859-7"),
            "{err}"
        );
    }
}
