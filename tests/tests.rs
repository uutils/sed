// This file is part of the uutils sed package.
//
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.
use std::env;

pub const TESTS_BINARY: &str = env!("CARGO_BIN_EXE_sed");

// Use the ctor attribute to run this function before any tests
#[ctor::ctor(unsafe)]
fn init() {
    unsafe {
        // Necessary for uutests to be able to find the binary
        std::env::set_var("UUTESTS_BINARY_PATH", TESTS_BINARY);
        // For single-call binaries, tell uutests not to auto-add the utility name
        std::env::remove_var("UUTESTS_UTIL_NAME");
        std::env::set_var("UUTESTS_UTIL_NAME", "");
        std::env::set_var("UUTILS_MULTICALL", "0");
        // Keep fixture-based integration tests in UTF-8 mode; individual
        // locale tests override LC_ALL to exercise byte mode.
        std::env::set_var("LC_ALL", "C.UTF-8");
    }
}

#[cfg(feature = "sed")]
#[path = "by-util/test_sed.rs"]
mod test_sed;
