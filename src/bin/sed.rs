// This file is part of the uutils sed package.
//
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use std::ffi::OsString;
use std::process;

fn main() {
    // Detect multicall vs single-call BEFORE any uucore calls
    // This must happen first to set the utility name detection mode correctly
    let raw_args: Vec<OsString> = std::env::args_os().collect();
    if raw_args.len() > 1 && raw_args[1] == "sed" {
        // Multicall binary mode: tell uucore to use args[1] for util_name()
        uucore::set_utility_is_second_arg();
    }

    uucore::panic::mute_sigpipe_panic();

    let mut args = raw_args;

    // Strip .exe extension from binary name on Windows for consistent error messages
    #[cfg(windows)]
    if let Some(binary_name) = args.get_mut(0) {
        let binary_str = binary_name.to_string_lossy();
        if let Some(stripped) = binary_str.strip_suffix(".exe") {
            *binary_name = OsString::from(stripped);
        }
    }

    // Handle both single-call and multi-call binary compatibility
    // If first argument after binary name is "sed", skip it (multi-call compatibility)
    if args.len() > 1 && args[1] == "sed" {
        args.remove(1);
    }

    let code = sed::sed::uumain(args.into_iter());
    process::exit(code);
}
