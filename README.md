[![Crates.io](https://img.shields.io/crates/v/sed.svg)](https://crates.io/crates/sed)
[![Discord](https://img.shields.io/badge/discord-join-7289DA.svg?logo=discord&longCache=true&style=flat)](https://discord.gg/wQVJbvJ)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/uutils/sed/blob/main/LICENSE)
[![dependency status](https://deps.rs/repo/github/uutils/sed/status.svg)](https://deps.rs/repo/github/uutils/sed)

[![CodeCov](https://codecov.io/gh/uutils/sed/branch/master/graph/badge.svg)](https://codecov.io/gh/uutils/sed)

# sed

Rust reimplementation of the [sed utility](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/sed.html)
with some [GNU sed](https://www.gnu.org/software/sed/manual/sed.html),
[FreeBSD sed](https://man.freebsd.org/cgi/man.cgi?sed(1)),
and other extensions.

## Status

At this state _sed_ implements all POSIX commands
and can run correctly the two complex scripts of its integration tests:
[hanoi.sed](https://github.com/uutils/sed/blob/main/tests/fixtures/sed/script/hanoi.sed) (solves the Towers of Hanoi puzzle) and
[math.sed](https://github.com/uutils/sed/blob/main/tests/fixtures/sed/script/math.sed)  (implements an arbitrary precision integer math calculator).

The performance of this Rust implementation is now better than the GNU and FreeBSD implementations for most benchmarked cases.

Further work aims to:
* Adjust buffering on terminal output to match current implementations,
* Implement more GNU extensions,
* Improve performance where possible.

## Installation

Ensure you have Rust installed on your system. You can install Rust through [rustup](https://rustup.rs/).

Clone the repository and build the project using Cargo:

```bash
git clone https://github.com/uutils/sed.git
cd sed
cargo build --release
cargo run --release
```

The binary is named `sed` in `target/release/sed`.

## Testing

### GNU sed Compatibility Testing

Test compatibility against GNU sed using the comprehensive testsuite (47+ tests, ~10% pass rate):

```bash
# Clone GNU sed testsuite (one time setup)
git clone https://github.com/mirror/sed.git ../gnu.sed

# Run compatibility tests
./util/run-gnu-testsuite.sh

# Generate JSON results for CI
./util/run-gnu-testsuite.sh --json-output results.json
```

The testsuite extracts test cases from the GNU sed repository and tests them against expected outputs.

### Unit Tests

```bash
cargo test
```

## Extensions and incompatibilities
### Supported GNU extensions
* Command-line arguments can be specified in long (`--`) form.
* Spaces can precede a regular expression modifier.
* `I` can be used in as a synonym for the `i` (case insensitive) substitution
  flag.
* In addition to `\n`, other escape sequences (octal, hex, C) are supported
  in the strings of the `y` command.
  Under POSIX these yield undefined behavior.
* The `a`, `c`, and `i` commands do not require an initial backslash,
  allow text to appear on the same line, and support escape sequences
  in the specified text.
* The substitution command replacement group `\0` is a synonym for &.
* A `Q` command (optionally followed by an exit code) quits immediately.
* The `q` command can be optionally followed by an exit code.
* The `l` command can be optionally followed by the output width.
* The `--follow-symlinks` flag for in-place editing.

### Supported BSD and GNU extensions
* The second address in a range can be specified as a relative address with +N.
* In-place editing of file with the `-i` flag.

### New extensions
* Unicode characters can be specified in regular expression pattern, replacement
  and transliteration sequences using `\uXXXX` or `\UXXXXXXXX` sequences.
* The `l` command lists Unicode characters using the `\uXXXX` and `\UXXXXXXXX`
  sequences.

### Incompatibilities
* The input is assumed to be valid UTF-8 (this includes 7-bit ASCII).
  If the input is in another code page, consider converting it through UTF-8
  in order to avoid errors on invalid UTF-8 sequences and for the correct
  handling of regular expressions.
  This _sed_ program can also handle arbitrary byte sequences if no part of the
  input is treated as string.
* The command will report an error and fail if duplicate labels are found
  in the script.
  This matches the BSD behavior. The GNU version accepts duplicate labels.
* The last line (`$`) address is interpreted as the last non-empty line of
  the last file.  If files specified in subsequent arguments until the last
  one are empty, then the last line condition will never be triggered.
  This behavior is consistent with the
  [original implementation](https://github.com/dspinellis/unix-history-repo/blob/Research-V7/usr/src/cmd/sed/sed1.c#L665).
* Labels are parsed for alphanumeric characters. The BSD version parses them
  until the end of the line, preventing ; to be used as a separator.

## GNU test suite compatibility

Below is the evolution of how many GNU tests uutils passes.

![Evolution over time](https://github.com/uutils/sed-tracking/blob/main/gnu-results.svg?raw=true)


## License

sed is licensed under the MIT License - see the `LICENSE` file for details
