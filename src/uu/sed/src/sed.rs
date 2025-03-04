// This file is part of the uutils sed package.
//
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use clap::{arg, Arg, Command};
use std::path::PathBuf;
use uucore::error::{UResult, UUsageError};
use uucore::format_usage;

const ABOUT: &str = "Stream editor for filtering and transforming text";
const USAGE: &str = "sed [OPTION]... default-script [input-file]...";

#[uucore::main]
pub fn uumain(args: impl uucore::Args) -> UResult<()> {
    let matches = uu_app().try_get_matches_from(args)?;

    let script_through_options =
        matches.contains_id("expression") || matches.contains_id("script-file");
    let no_positional_script = matches.get_many::<String>("script").is_none();
    if !script_through_options && no_positional_script {
        return Err(UUsageError::new(1, "missing script"));
    }

    // TODO
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
            arg!([file] ... "Input files"),
            Arg::new("all-output-files")
                .long("all-output-files")
                .short('a')
                .help("Create or truncate all output files before processing.")
                .action(clap::ArgAction::SetTrue),
            arg!(-b --binary "Treat files as binary: do not process CR+LFs."),
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
