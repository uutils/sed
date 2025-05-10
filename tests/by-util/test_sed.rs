// Integration tests
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use std::io::Write;
use tempfile::NamedTempFile;
use uutests::new_ucmd;
use uutests::util::TestScenario;
use uutests::util_name;

// Test application's invocation
#[test]
fn test_invalid_arg() {
    new_ucmd!().arg("--definitely-invalid").fails().code_is(1);
}

#[test]
fn test_debug() {
    new_ucmd!().args(&["--debug", ""]).succeeds();
}

#[test]
fn test_silent_alias() {
    new_ucmd!().args(&["--silent", ""]).succeeds();
}

#[test]
fn test_missing_script_argument() {
    new_ucmd!()
        .fails()
        .code_is(1)
        .stderr_contains("missing script");
}

#[test]
fn test_positional_script_ok() {
    new_ucmd!().arg("l").succeeds().code_is(0);
}

#[test]
fn test_empty_positional_script_ok() {
    new_ucmd!().arg("").succeeds().code_is(0);
}

#[test]
fn test_e_script_ok() {
    new_ucmd!().args(&["-e", "l"]).succeeds();
}

#[test]
fn test_f_script_ok() {
    let mut temp = NamedTempFile::new().expect("Failed to create temp file");
    writeln!(temp, "l").expect("Failed to write to temp file");
    let path = temp.path();

    new_ucmd!().arg("-f").arg(path).succeeds();
}

const INPUT_FILES: &[&str] = &[
    "two-lines.txt",
    "no-new-line.txt",
    "dots-4k.txt",
    "dots-8k.txt",
    "dots-64k.txt",
];

#[test]
fn test_no_script_stdin() {
    for fixture in INPUT_FILES {
        new_ucmd!()
            .arg("")
            .pipe_in_fixture(fixture)
            .succeeds()
            .stdout_is_fixture(fixture);
    }
}

#[test]
fn test_no_script_file() {
    for fixture in INPUT_FILES {
        new_ucmd!()
            .args(&["-e", "", fixture])
            .succeeds()
            .stdout_is_fixture(fixture);
    }
}

#[test]
fn test_delete_stdin() {
    for fixture in INPUT_FILES {
        new_ucmd!()
            .arg("d")
            .pipe_in_fixture(fixture)
            .succeeds()
            .no_stdout();
    }
}

#[test]
fn test_delete_file() {
    for fixture in INPUT_FILES {
        new_ucmd!()
            .args(&["-e", "d", fixture])
            .succeeds()
            .no_stdout();
    }
}

/// Create a new test function to verify an execution for specified output.
macro_rules! check_output {
    ($name:ident, $args:expr) => {
        #[test]
        fn $name() {
            new_ucmd!()
                .args(&$args)
                .succeeds()
                .stdout_is_fixture(&format!("output/{}", stringify!($name)));
        }
    };
}

// Test address ranges
check_output!(addr_one_line, ["-n", "-e", "4p", "lines1"]);
check_output!(addr_straddle, ["-n", "-e", "20p", "lines1", "lines2"]);
check_output!(addr_last_one_file, ["-n", "-e", "$p", "lines1"]);
check_output!(addr_last_two_files, ["-n", "-e", "$p", "lines1", "lines2"]);

// TODO: Enable and configure for Unix/Windows, when "a" is implemented.
#[cfg(any())]
check_output!(addr_append_with_empty, ["-e", "$a\nhello", "/dev/null"]);

#[cfg(unix)]
check_output!(
    addr_last_with_empty,
    ["-n", "-e", "$p", "lines1", "/dev/null", "lines2"]
);

#[cfg(windows)]
check_output!(
    addr_last_with_empty,
    ["-n", "-e", "$p", "lines1", "NUL", "lines2"]
);

check_output!(addr_past_last, ["-n", "-e", "20p", "lines1"]);
check_output!(addr_not_found, ["-n", "-e", "/NOTFOUND/p", "lines1"]);
check_output!(addr_found, ["-n", "/l1_7/p", "lines1"]);
check_output!(addr_found_space, ["-n", " /l1_7/ p", "lines1"]);
check_output!(addr_escaped_delimiter, ["-n", "\\_l1\\_7_p", "lines1"]);
check_output!(addr_range_numeric, ["-n", "1,4p", "lines1"]);
check_output!(addr_range_to_last, ["-n", "1,$p", "lines1", "lines2"]);
check_output!(
    addr_range_to_pattern,
    ["-n", "1,/l2_9/p", "lines1", "lines2"]
);
check_output!(addr_pattern_to_last, ["-n", "/4/,$p", "lines1", "lines2"]);
check_output!(
    addr_pattern_to_straddle,
    ["-n", "/4/,20p", "lines1", "lines2"]
);
check_output!(
    addr_pattern_to_pattern,
    ["-n", "/4/,/10/p", "lines1", "lines2"]
);
check_output!(
    addr_pattern_straddle,
    ["-n", "/l2_3/,/l1_8/p", "lines1", "lines2"]
);
check_output!(addr_range_reverse, ["-n", "12,3p", "lines1", "lines2"]);
check_output!(
    addr_pattern_range_reverse,
    ["-n", "/l1_7/,3p", "lines1", "lines2"]
);
check_output!(
    addr_numeric_to_relative,
    ["-n", "13,+4p", "lines1", "lines2"]
);
check_output!(
    addr_pattern_to_relative,
    ["-n", "/l1_6/,+2p", "lines1", "lines2"]
);
check_output!(addr_numeric_relative_straddle, ["-n", "12,+1p", "lines1"]);
check_output!(
    addr_first_separate,
    ["-n", "--separate", "1p", "lines1", "lines2"]
);
check_output!(addr_last_separate, ["-ns", "$p", "lines1", "lines2"]);
