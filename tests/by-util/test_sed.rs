// Integration tests
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use std::fs;
use std::io::{Read, Write};
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
    "input/two-lines.txt",
    "input/no-new-line.txt",
    "input/dots-4k.txt",
    "input/dots-8k.txt",
    "input/dots-64k.txt",
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

// Input files
const LINES1: &str = "input/lines1";
const LINES2: &str = "input/lines2";
const NO_NEW_LINE: &str = "input/no-new-line.txt";

// Test address ranges
check_output!(addr_one_line, ["-n", "-e", "4p", LINES1]);
check_output!(addr_straddle, ["-n", "-e", "20p", LINES1, LINES2]);
check_output!(addr_last_one_file, ["-n", "-e", "$p", LINES1]);
check_output!(addr_last_two_files, ["-n", "-e", "$p", LINES1, LINES2]);

// TODO: Enable and configure for Unix/Windows, when "a" is implemented.
#[cfg(any())]
check_output!(addr_append_with_empty, ["-e", "$a\nhello", "/dev/null"]);

#[cfg(unix)]
check_output!(
    addr_last_with_empty,
    ["-n", "-e", "$p", LINES1, "/dev/null", LINES2]
);

#[cfg(windows)]
check_output!(
    addr_last_with_empty,
    ["-n", "-e", "$p", LINES1, "NUL", LINES2]
);

check_output!(addr_past_last, ["-n", "-e", "20p", LINES1]);
check_output!(addr_not_found, ["-n", "-e", "/NOTFOUND/p", LINES1]);
check_output!(addr_found, ["-n", "/l1_7/p", LINES1]);
check_output!(addr_found_space, ["-n", " /l1_7/ p", LINES1]);
check_output!(addr_escaped_delimiter, ["-n", "\\_l1\\_7_p", LINES1]);
check_output!(addr_range_numeric, ["-n", "1,4p", LINES1]);
check_output!(addr_range_to_last, ["-n", "1,$p", LINES1, LINES2]);
check_output!(addr_range_to_pattern, ["-n", "1,/l2_9/p", LINES1, LINES2]);
check_output!(addr_pattern_to_last, ["-n", "/4/,$p", LINES1, LINES2]);
check_output!(addr_pattern_to_straddle, ["-n", "/4/,20p", LINES1, LINES2]);
check_output!(addr_pattern_to_pattern, ["-n", "/4/,/10/p", LINES1, LINES2]);
check_output!(
    addr_pattern_straddle,
    ["-n", "/l2_3/,/l1_8/p", LINES1, LINES2]
);
check_output!(addr_range_reverse, ["-n", "12,3p", LINES1, LINES2]);
check_output!(
    addr_pattern_range_reverse,
    ["-n", "/l1_7/,3p", LINES1, LINES2]
);
check_output!(addr_numeric_to_relative, ["-n", "13,+4p", LINES1, LINES2]);
check_output!(
    addr_pattern_to_relative,
    ["-n", "/l1_6/,+2p", LINES1, LINES2]
);
check_output!(addr_numeric_relative_straddle, ["-n", "12,+1p", LINES1]);
check_output!(
    addr_first_separate,
    ["-n", "--separate", "1p", LINES1, LINES2]
);
check_output!(addr_last_separate, ["-ns", "$p", LINES1, LINES2]);
check_output!(addr_two_lines_semicolon, ["-n", "-e", "4p;8p", LINES1]);
check_output!(addr_two_lines_newline, ["-n", "-e", "4p\n8p", LINES1]);
check_output!(addr_three_lines_semicolon, ["-n", "-e", "4p;8p;1p", LINES1]);
check_output!(addr_one_line_negate, ["-n", "-e", "4!p", LINES1]);
check_output!(addr_range_numeric_negate, ["-n", "1,4!p", LINES1]);
check_output!(
    addr_pattern_to_pattern_negate,
    ["-n", "/1_4/,/10/!p", LINES1]
);
check_output!(addr_empty_re_reuse, ["-n", "/_2/,//p", LINES1, LINES2]);

// Test substitutions
check_output!(subst_any, ["-e", r"s/./X/g", LINES1]);
check_output!(subst_any_global, ["-e", r"s,.,X,g", LINES1]);
check_output!(subst_escaped_magic_separator, ["-e", r"s.\..X.g", LINES1]);
check_output!(subst_escaped_braced_separator, ["-e", r"s/[\/]/Q/", LINES1]);
check_output!(subst_escaped_separator, ["-e", r"s_\__X_", LINES1]);
check_output!(subst_whole_match_group, ["-e", r"s/./(&)/g", LINES1]);
check_output!(subst_print, ["-ne", "s/1_1/S&/p", LINES1]);
check_output!(
    subst_escaped_whole_match_group,
    ["-e", r"s/./(\&)/g", LINES1]
);
check_output!(
    subst_numerical_groups,
    ["-e", r"s/\(.\)\(.\)\(.\)/x\3x\2x\1/g", LINES1]
);
check_output!(
    subst_ere_numerical_groups,
    [
        "--regexp-extended",
        "-e",
        r"s/(.)(.)(.)/x\3x\2x\1/g",
        LINES1
    ]
);
check_output!(subst_multiline, ["-e", "s/_/u0\\\nu1\\\nu2/g", LINES1]);
check_output!(subst_numbered_replacement, ["-e", r"s/./X/4", LINES1]);
check_output!(subst_brace, ["-e", r"s/[123]/X/g", LINES1]);
check_output!(subst_case_insensitive, ["-e", r"s/L/Line/", LINES1]);
check_output!(subst_no_new_line, ["-e", r"s/l/L/g", NO_NEW_LINE]);

#[test]
fn subst_write_file() -> std::io::Result<()> {
    let temp = NamedTempFile::new()?;
    let path = temp.path();
    let cmd = format!("s/_1/S_1/w {}", path.display());

    new_ucmd!().args(&["-n", &cmd, LINES1]).succeeds();

    let mut actual = String::new();
    temp.reopen()?.read_to_string(&mut actual)?;

    let expected = fs::read_to_string("tests/fixtures/sed/output/subst_write_file")?;
    assert_eq!(actual, expected, "Output did not match fixture");

    Ok(())
}

check_output!(trans_simple, ["-e", r"y/0123456789/9876543210/", LINES1]);
check_output!(
    trans_delimiter,
    ["-e", r"y10\123456789198765432\101", LINES1]
);
check_output!(trans_no_new_line, ["-e", r"y/l/L/", NO_NEW_LINE]);
check_output!(trans_newline, ["-e", r"1N;2y/\n/X/", LINES1]);

// TODO: Enable when "{}" is implemented.
#[cfg(any())]
check_output!(subst_newline_class, ["-n", r"1{;N;s/[\n]/X/;p;}", LINES1]);

// TODO: Enable when "{}" is implemented.
#[cfg(any())]
check_output!(subst_newline_re, ["-n", r"1{;N;s/\n/X/;p;}", LINES1]);

// TODO: Enable when "{}" is implemented.
#[cfg(any())]
check_output!(print_to_newline, ["-n", r"1{;N;P;P;p;}", LINES1]);

check_output!(pattern_next_print, ["-n", r"N;N;P", LINES1]);
check_output!(pattern_delete_to_newline, ["-n", r"2N;3p;3D;3p", LINES1]);
check_output!(pattern_delete_no_newline, ["-e", r"2D", LINES1]);
check_output!(pattern_delete_print, ["-n", r"4d;p", LINES1]);

// FreeBSD sed does not produce any output for the following two
check_output!(pattern_append_delete, ["-e", r"N;N;N;D", LINES1]);
check_output!(pattern_append_delete_2, ["-e", r"N;N;N;D", LINES1, LINES2]);

check_output!(
    pattern_append_delete2_separate,
    ["-s", r"N;N;N;D", LINES1, LINES2]
);
check_output!(
    pattern_hold_append_swap,
    ["-e", r"2h;3H;4g;5G;6x;6p;6x;6p", LINES1]
);
check_output!(pattern_next_output, ["-e", r"4n", LINES1]);
check_output!(pattern_next_no_output, ["-n", "-e", r"4n", LINES1]);
check_output!(pattern_next_print_output, ["-e", r"4n;p", LINES1]);
check_output!(pattern_next_print_no_output, ["-n", "-e", r"4n;p", LINES1]);
check_output!(pattern_quit, [r"5q", LINES1]);
check_output!(pattern_quit_2, [r"5q", LINES1, LINES2]);
