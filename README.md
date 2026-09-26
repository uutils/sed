[![Crates.io](https://img.shields.io/crates/v/sed.svg)](https://crates.io/crates/sed)
[![Discord](https://img.shields.io/badge/discord-join-7289DA.svg?logo=discord&longCache=true&style=flat)](https://discord.gg/wQVJbvJ)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/uutils/sed/blob/main/LICENSE)
[![dependency status](https://deps.rs/repo/github/uutils/sed/status.svg)](https://deps.rs/repo/github/uutils/sed)

[![CodeCov](https://codecov.io/gh/uutils/sed/branch/main/graph/badge.svg)](https://codecov.io/gh/uutils/sed)

# sed

Rust reimplementation of the [sed utility](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/sed.html)
with some [GNU sed](https://www.gnu.org/software/sed/manual/sed.html),
[FreeBSD sed](https://man.freebsd.org/cgi/man.cgi?sed(1)),
and other extensions.

## Status

At this state _sed_ implements all [POSIX features](https://pubs.opengroup.org/onlinepubs/9799919799/)
and can run correctly the three complex scripts of its integration tests:
[hanoi.sed](https://github.com/uutils/sed/blob/main/tests/fixtures/sed/script/hanoi.sed) (solves the Towers of Hanoi puzzle),
[mandelbrot.sed](https://github.com/uutils/sed/blob/main/tests/fixtures/sed/script/mandelbrot.sed), (draws the Mandelbrot set) and
[math.sed](https://github.com/uutils/sed/blob/main/tests/fixtures/sed/script/math.sed)  (implements an arbitrary precision integer math calculator).

The performance of this Rust implementation is now better than the GNU and FreeBSD implementations for most benchmarked cases.

Further work aims to:
* improve GNU _sed_ compatibility, especially on the regular expression front,
* implement more GNU extensions, and
* improve performance where possible.

## Installation and Use

We provide a Linux x86_64 binary archive from the main branch at
https://github.com/uutils/sed/releases/tag/latest-commit .

If you have [cargo-binstall](https://github.com/cargo-bins/cargo-binstall),
the released binaries can be installed directly with:

```bash
cargo binstall sed
```

For other platforms, ensure you have Rust installed on your system. You can install Rust through [rustup](https://rustup.rs/).

Clone the repository and build the project using Cargo:

```bash
git clone https://github.com/uutils/sed.git
cd sed
cargo build --release
cargo run --release
```

The binary is named `sed` in `target/release/sed`.

You can also try *sed* on the web
through the [uutils Playground](https://uutils.org//playground/)
by clicking on the `Load sed` button.

## Testing

### GNU sed Compatibility Testing

Test compatibility against GNU sed by running the upstream testsuite shell scripts
with a lightweight gnulib test-framework shim:

```bash
# Clone GNU sed testsuite (one time setup)
git clone https://github.com/mirror/sed.git ../gnu.sed

# Run compatibility tests
./util/run-gnu-testsuite.sh

# Verbose mode shows failure details
./util/run-gnu-testsuite.sh -v

# Generate JSON results for CI
./util/run-gnu-testsuite.sh --json-output results.json
```

The harness executes each `.sh` test from the GNU sed testsuite directly, injecting
our Rust sed binary via `PATH` and providing shim implementations of the gnulib test
framework functions (`compare_`, `returns_`, `skip_`, etc.).

### Unit Tests

```bash
cargo test
```

## Extensions and incompatibilities
The GNU, BSD and new extensions _sed_ supports, and where it differs from GNU
_sed_, are listed in [docs/src/extensions.md](docs/src/extensions.md).

## GNU test suite compatibility

Below is the evolution of how many GNU tests uutils passes.

![Evolution over time](https://github.com/uutils/sed-tracking/blob/main/gnu-results.svg?raw=true)


## License

sed is licensed under the MIT License - see the `LICENSE` file for details
