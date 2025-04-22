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

## Installation

Ensure you have Rust installed on your system. You can install Rust through [rustup](https://rustup.rs/).

Clone the repository and build the project using Cargo:

```bash
git clone https://github.com/uutils/sed.git
cd sed
cargo build --release
cargo run --release
```
## Extensions
### GNU
* Command-line arguments can be specified in long (`--`) form.

### Other
* Unicode characters can be specified in regular expression pattern, replacement
  and transliteration sequences using `\uXXXX` or `\UXXXXXXXX` sequences.

## License

sed is licensed under the MIT License - see the `LICENSE` file for details
