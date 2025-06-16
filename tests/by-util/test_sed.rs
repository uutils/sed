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

#[cfg(unix)]
use assert_fs::fixture::{FileWriteStr, PathChild};

use tempfile::NamedTempFile;
use uutests::new_ucmd;
use uutests::util::TestScenario;
use uutests::util_name;

////////////////////////////////////////////////////////////
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
    new_ucmd!().arg("-f").arg("script/hanoi.sed").succeeds();
}

////////////////////////////////////////////////////////////
// Test simple I/O processing
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

////////////////////////////////////////////////////////////
// Individual command tests

// Input files
const LINES1: &str = "input/lines1";
const LINES2: &str = "input/lines2";
const NO_NEW_LINE: &str = "input/no-new-line.txt";

////////////////////////////////////////////////////////////
// Address ranges: One and two, numeric and pattern
check_output!(addr_one_line, ["-n", "-e", "4p", LINES1]);
check_output!(addr_straddle, ["-n", "-e", "20p", LINES1, LINES2]);
check_output!(addr_last_one_file, ["-n", "-e", "$p", LINES1]);
check_output!(addr_last_two_files, ["-n", "-e", "$p", LINES1, LINES2]);

check_output!(addr_append_empty, ["-e", "$a\\\nhello", "input/empty"]);

check_output!(
    addr_last_empty,
    ["-n", "-e", "$p", LINES1, "input/empty", LINES2]
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
check_output!(addr_simple_negation, ["-e", r"4,12!s/^/^/", LINES1]);

////////////////////////////////////////////////////////////
// Substitution: s
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
check_output!(subst_re_reuse, ["-e", r"2s//M/;1s/l/L/", LINES1]);
check_output!(subst_newline_class, ["-n", r"1{;N;s/[\n]/X/;p;}", LINES1]);
check_output!(subst_newline_re, ["-n", r"1{;N;s/\n/X/;p;}", LINES1]);

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

////////////////////////////////////////////////////////////
// Transliteration: y
check_output!(trans_simple, ["-e", r"y/0123456789/9876543210/", LINES1]);
check_output!(
    trans_delimiter,
    ["-e", r"y10\123456789198765432\101", LINES1]
);
check_output!(trans_no_new_line, ["-e", r"y/l/L/", NO_NEW_LINE]);
check_output!(trans_newline, ["-e", r"1N;2y/\n/X/", LINES1]);

////////////////////////////////////////////////////////////
// Pattern space manipulation: D, d, H, h, N, n, P, p, q, x
check_output!(pattern_print_to_newline, ["-n", r"1{;N;P;P;p;}", LINES1]);
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
check_output!(pattern_re_reuse, ["-n", r"/_1/p;//p", LINES1]);
check_output!(pattern_subst_re_reuse, ["-n", r"/_1/p;s//-N/p", LINES1]);

#[test]
fn test_quit_exit_code() {
    new_ucmd!()
        .args(&["5q 42", LINES1])
        .fails()
        .code_is(42)
        .stdout_is_fixture("output/pattern_quit");
}

#[test]
fn test_quit_now_exit_code() {
    new_ucmd!()
        .args(&["6Q 12", LINES1])
        .fails()
        .code_is(12)
        .stdout_is_fixture("output/pattern_quit");
}

////////////////////////////////////////////////////////////
// Command blocks: {}
check_output!(
    block_simple_range,
    [
        "-e",
        r#"
4,12 {
	s/^/^/
	s/$/$/
	s/_/T/
}"#,
        LINES1
    ]
);

check_output!(
    block_negative_range,
    [
        "-e",
        r#"
4,12 !{
	s/^/^/
	s/$/$/
	s/_/T/
}"#,
        LINES1
    ]
);

check_output!(
    block_negative_range_2,
    [
        "-e",
        r#"
4,12 !{
	s/^/^/
	s/$/$/
	s/_/T/
}"#,
        LINES1,
        LINES2
    ]
);

check_output!(
    block_nested_selection,
    [
        "-e",
        r#"
4,12 {
	s/^/^/
	/6/,/10/ {
		s/$/$/
		/8/ s/_/T/
	}
}"#,
        LINES1
    ]
);

check_output!(
    block_nested_negative_selection,
    [
        "-e",
        r#"
4,12 !{
	s/^/^/
	/6/,/10/ !{
		s/$/$/
		/8/ !s/_/T/
	}
}"#,
        LINES1
    ]
);

check_output!(
    branch_plain,
    [
        "-n",
        "-e",
        r#"
b label4
:label3
s/^/label3_/p
b end
:label4
2,12b label1
b label2
:label1
s/^/label1_/p
b
:label2
s/^/label2_/p
b label3
:end
"#,
        LINES1
    ]
);

////////////////////////////////////////////////////////////
// Branching: :, b, t
check_output!(
    branch_conditional_simple,
    [
        "-n",
        "-e",
        r#"
s/l1_/l2_/
t ok
b
:ok
s/^/tested /p
"#,
        LINES1,
        LINES2
    ]
);

// SunOS and GNU sed behave as follows: lines 9-$ aren"#,t printed at all
check_output!(
    branch_to_block,
    [
        "-n",
        "-e",
        r#"
5,8b inside
1,5 {
	s/^/^/p
	:inside
	s/$/$/p
}
"#,
        LINES1
    ]
);

// Check that t clears the substitution done flag
check_output!(
    branch_test_clears,
    [
        "-n",
        "-e",
        r#"
1,8s/^/^/
t l1
:l1
t l2
s/$/$/p
b
:l2
s/^/ERROR/
"#,
        LINES1
    ]
);

// Check that reading a line clears the substitution done flag
check_output!(
    branch_cycle_clears,
    [
        "-n",
        "-e",
        r#"
t l2
1,8s/^/^/p
2,7N
b
:l2
s/^/ERROR/p
"#,
        LINES1
    ]
);

check_output!(
    branch_conditional_boundary,
    [
        "-e",
        r#"
{
:b
}
s/l/m/
tb"#,
        LINES1
    ]
);

////////////////////////////////////////////////////////////
// Text: a, c, i
check_output!(
    text_insert_quit,
    [
        "-e",
        r#"
5i\
hello
5q
"#,
        LINES1
    ]
);

check_output!(
    text_insert_between_subst,
    [
        "-n",
        "-e",
        r#"
s/^/before_i/p
20i\
inserted
s/^/after_i/p
"#,
        LINES1,
        LINES2
    ]
);

check_output!(
    text_append_between_subst,
    [
        "-n",
        "-e",
        r#"
5,12s/^/5-12/
s/^/before_a/p
/5-12/a\
appended
s/^/after_a/p
"#,
        LINES1,
        LINES2
    ]
);

check_output!(
    text_append_before_next,
    [
        "-n",
        "-e",
        r#"
s/^/^/p
/l1_/a\
appended
8,10N
s/$/$/p
"#,
        LINES1,
        LINES2
    ]
);

check_output!(
    text_change_global,
    [
        "-n",
        "-e",
        r#"
c\
hello
"#,
        LINES1
    ]
);

check_output!(
    text_change_line,
    [
        "-n",
        "-e",
        r#"
8c\
hello
"#,
        LINES1
    ]
);

check_output!(
    text_change_range,
    [
        "-n",
        "-e",
        r#"
3,14c\
hello
"#,
        LINES1
    ]
);

// SunOS and GNU sed behave differently.   We follow POSIX.
check_output!(
    text_change_reverse_range,
    [
        "-n",
        "-e",
        r#"
8,3c\
hello
"#,
        LINES1
    ]
);

check_output!(text_delete, ["d", LINES1]);

// Check that the pattern space is deleted.
check_output!(
    text_change_print,
    [
        "-n",
        "-e",
        r#"
c\
changed
p
"#,
        LINES1
    ]
);

////////////////////////////////////////////////////////////
// r, w commands
check_output!(read_ok, [format!("4r {LINES2}"), LINES1.to_string()]);
check_output!(read_missing, ["5r /xyzzyxyzy42", LINES1]);
check_output!(read_empty, ["6r input/empty", LINES1]);

#[test]
fn write_single_file() -> std::io::Result<()> {
    let temp = NamedTempFile::new()?;
    let cmd = format!("3,12w {}", temp.path().display());

    new_ucmd!().args(&["-e", &cmd, LINES1]).succeeds();

    let mut actual = String::new();
    temp.reopen()?.read_to_string(&mut actual)?;

    let expected = fs::read_to_string("tests/fixtures/sed/output/write_single_file")?;
    assert_eq!(actual, expected, "Output did not match fixture");

    Ok(())
}

#[test]
fn write_two_files() -> std::io::Result<()> {
    let temp1 = NamedTempFile::new()?;
    let temp2 = NamedTempFile::new()?;
    let cmd = format!(
        "3,12w {}\n1,2w {}",
        temp1.path().display(),
        temp2.path().display()
    );

    new_ucmd!().args(&["-e", &cmd, LINES1]).succeeds();

    let mut actual = String::new();

    temp1.reopen()?.read_to_string(&mut actual)?;
    let expected = fs::read_to_string("tests/fixtures/sed/output/write_two_files_1")?;
    assert_eq!(actual, expected, "Output 1 did not match fixture");

    actual.clear();

    temp2.reopen()?.read_to_string(&mut actual)?;
    let expected = fs::read_to_string("tests/fixtures/sed/output/write_two_files_2")?;
    assert_eq!(actual, expected, "Output 2 did not match fixture");

    Ok(())
}

////////////////////////////////////////////////////////////
// =, l commands
check_output!(number_continuous, ["/l2_/=", LINES1, LINES2]);
check_output!(number_separate, ["-s", "/l._8/=", LINES1, LINES2]);

check_output!(list_ascii, ["-n", "l 60", "input/ascii"]);
check_output!(list_empty, ["-n", "l 60", "input/empty"]);
check_output!(list_unicode, ["l 60", "input/unicode"]);

////////////////////////////////////////////////////////////
// In-place editing
#[test]
fn in_place_edit_replace() -> std::io::Result<()> {
    let mut temp = NamedTempFile::new()?;
    writeln!(temp.as_file_mut(), "hello, world")?;

    // Get the file path before converting to TempPath
    let path = temp.path().to_path_buf();

    // Close temp file and preserve path
    let temp_path = temp.into_temp_path();

    // Call your tool on the path
    new_ucmd!()
        .args(&["-i", "-e", "s/world/universe/", path.to_str().unwrap()])
        .succeeds();

    // Read the file using standard fs
    let actual = std::fs::read_to_string(&path)?;
    temp_path.close()?; // Clean up

    assert_eq!(actual, "hello, universe\n");
    Ok(())
}

#[test]
fn in_place_edit_backup() -> std::io::Result<()> {
    let mut temp = NamedTempFile::new()?;
    writeln!(temp.as_file_mut(), "hello, world")?;

    let path = temp.path().to_path_buf();
    let temp_path = temp.into_temp_path();

    // Run the sed-like command with -i (in-place edit)
    new_ucmd!()
        .args(&[
            "-i",
            ".bak",
            "-e",
            "s/world/universe/",
            path.to_str().unwrap(),
        ])
        .succeeds();

    // Read edited file
    let actual = std::fs::read_to_string(&path)?;
    assert_eq!(actual, "hello, universe\n");

    // Read backup file
    let backup_path = path.with_file_name(format!(
        "{}.bak",
        path.file_name().unwrap().to_string_lossy()
    ));
    let backup = std::fs::read_to_string(&backup_path)?;
    assert_eq!(backup, "hello, world\n");

    temp_path.close()?; // Cleanup
    std::fs::remove_file(backup_path)?; // Cleanup backup

    Ok(())
}

#[cfg(unix)]
#[test]
fn in_place_edit_follow_symlink_edits_target() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = assert_fs::TempDir::new()?;
    let target = temp_dir.child("target.txt");
    let link = temp_dir.child("link.txt");

    target.write_str("hello, world\n")?;

    std::os::unix::fs::symlink(target.path(), link.path())?;

    new_ucmd!()
        .args(&[
            "--follow-symlinks",
            "-i",
            "-e",
            "s/world/universe/",
            link.path().to_str().unwrap(),
        ])
        .succeeds();

    let actual = std::fs::read_to_string(target.path())?;
    assert_eq!(actual, "hello, universe\n");

    Ok(())
}

#[cfg(unix)]
#[test]
fn in_place_edit_symlink_replaced_when_not_following() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = assert_fs::TempDir::new()?;
    let target = temp_dir.child("target.txt");
    let link = temp_dir.child("link.txt");

    target.write_str("hello, world\n")?;

    std::os::unix::fs::symlink(target.path(), link.path())?;

    // Run command without --follow-symlinks
    new_ucmd!()
        .args(&[
            "-i",
            "-e",
            "s/world/universe/",
            link.path().to_str().unwrap(),
        ])
        .succeeds();

    // The original target should be untouched
    let original = std::fs::read_to_string(target.path())?;
    assert_eq!(original, "hello, world\n");

    // The symlink path should now contain the edited content
    let edited = std::fs::read_to_string(link.path())?;
    assert_eq!(edited, "hello, universe\n");

    Ok(())
}

#[cfg(unix)]
#[test]
fn in_place_edit_follow_symlink_with_backup() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = assert_fs::TempDir::new()?;
    let target = temp_dir.child("target.txt");
    let link = temp_dir.child("link.txt");

    target.write_str("hello, world\n")?;

    std::os::unix::fs::symlink(target.path(), link.path())?;

    new_ucmd!()
        .args(&[
            "--follow-symlinks",
            "-i",
            ".bak",
            "-e",
            "s/world/universe/",
            link.path().to_str().unwrap(),
        ])
        .succeeds();

    // Verify target was modified
    let edited = std::fs::read_to_string(target.path())?;
    assert_eq!(edited, "hello, universe\n");

    // Backup file is created alongside the target
    let backup_path = target.path().with_file_name(format!(
        "{}.bak",
        target.file_name().unwrap().to_string_lossy()
    ));
    let backup = std::fs::read_to_string(&backup_path)?;
    assert_eq!(backup, "hello, world\n");

    Ok(())
}

#[cfg(unix)]
#[test]
fn in_place_edit_symlink_replaced_with_backup() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = assert_fs::TempDir::new()?;
    let target = temp_dir.child("target.txt");
    let link = temp_dir.child("link.txt");

    target.write_str("hello, world\n")?;

    std::os::unix::fs::symlink(target.path(), link.path())?;

    new_ucmd!()
        .args(&[
            "-i",
            ".bak",
            "-e",
            "s/world/universe/",
            link.path().to_str().unwrap(),
        ])
        .succeeds();

    // Target should remain untouched
    let unchanged = std::fs::read_to_string(target.path())?;
    assert_eq!(unchanged, "hello, world\n");

    // Symlink path should now contain the updated content
    let edited = std::fs::read_to_string(link.path())?;
    assert_eq!(edited, "hello, universe\n");

    // Backup of the symlink file (not the target) should exist
    let backup_path = link.path().with_file_name(format!(
        "{}.bak",
        link.file_name().unwrap().to_string_lossy()
    ));
    let backup = std::fs::read_to_string(&backup_path)?;
    assert_eq!(backup, "hello, world\n");

    Ok(())
}

////////////////////////////////////////////////////////////
// Large complex scripts

// Math expression evaluation
check_output!(math1, ["-f", "script/math.sed", "input/expression1"]);

// Calculate π (scaled) to several decimal places
check_output!(pi, ["-f", "script/math.sed", "input/pi"]);

// Solve the Towers of Hanoi puzzle
check_output!(hanoi, ["-f", "script/hanoi.sed", "input/hanoi"]);

////////////////////////////////////////////////////////////
// Error handling
#[test]
fn test_invalid_backreference() {
    new_ucmd!()
        .args(&["-n", "-e", r"s/./X/;s//\1/", LINES1])
        .fails()
        .code_is(2)
        .stderr_is("sed: <script argument 1>:1:8: error: invalid reference \\1 on command's RHS\n");
}

#[test]
fn test_duplicate_label() {
    new_ucmd!()
        .args(&[":foo;:foo"])
        .fails()
        .code_is(1)
        .stderr_is("sed: <script argument 1>:1:6: error: duplicate label `foo'\n");
}

#[test]
fn test_undefined_label() {
    new_ucmd!()
        .args(&["b foo"])
        .fails()
        .code_is(1)
        .stderr_is("sed: <script argument 1>:1:1: error: undefined label `foo'\n");
}

#[test]
fn test_fancy_regex_error() {
    new_ucmd!()
        .args(&["-E", r"/(\.+)+\1b$/p", "input/dots-4k.txt"])
        .fails()
        .code_is(2)
        .stderr_is("sed: <script argument 1>:1:1: error: Error executing regex: Max limit for backtracking count exceeded\n");
}

#[test]
fn test_write_file_failure() {
    new_ucmd!()
        .args(&["w /xyzzy/xyzy", LINES1])
        .fails()
        .code_is(2)
        .stderr_is("sed: <script argument 1>:1:1: error: creating file '/xyzzy/xyzy': The system cannot find the path specified. (os error 3)\n");
}

#[test]
fn test_missing_substitute_re() {
    new_ucmd!()
        .args(&["l;s//foo/", LINES1])
        .fails()
        .code_is(2)
        .stderr_is("sed: <script argument 1>:1:3: error: no previous regular expression\n");
}

#[test]
fn test_missing_address_re() {
    new_ucmd!()
        .args(&["l\np;//s/foo/bar/", LINES1])
        .fails()
        .code_is(2)
        .stderr_is("sed: <script argument 1>:2:3: error: no previous regular expression\n");
}
