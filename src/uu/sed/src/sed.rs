// This file is part of the uutils sed package.
//
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use clap::{arg, Arg, Command};
//use clap::builder::OsStr;
use std::ffi::OsString;
use std::path::PathBuf;
use uucore::{error::UResult, format_usage};

const ABOUT: &str = "Stream editor for filtering and transforming text";
const USAGE: &str = "sed [OPTION]... {singular-script} [input-file]...";

#[uucore::main]
pub fn uumain(args: impl uucore::Args) -> UResult<()> {
    // Skip binary name
    let args: Vec<OsString> = args.skip(1).collect();
    // TODO remove underscore prefix when var is used
    let _matches = uu_app().try_get_matches_from(args)?;
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
            arg!(<script> "Script to execute"),
            arg!([files] ... "Input files"),
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
            arg!(-e --expression <SCRIPT> "Add script to executed commands."),
            // Access with .get_one::<PathBuf>("file")
            arg!(-f --file <FILE> "Specify script file.")
                .value_parser(clap::value_parser!(PathBuf)),
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
            Arg::new("quiet")
                .short('n')
                .long("quiet")
                .aliases(["silent"])
                .help("Suppress automatic printing of pattern space."),
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
