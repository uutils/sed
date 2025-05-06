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
