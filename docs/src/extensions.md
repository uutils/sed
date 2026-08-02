# Extensions and incompatibilities

The main goal of the project is compatibility with GNU _sed_, but _sed_ also
supports features that GNU _sed_ does not, and differs from it in a few places.
Below is a list of these extensions and incompatibilities.

## Supported GNU extensions
* Command-line arguments can be specified in long (`--`) form.
* Spaces can precede a regular expression modifier.
* `I` can be used in as a synonym for the `i` (case insensitive) substitution
  flag.
* `M` and `m` substitution flags allow multi-line matching.
* In addition to `\n`, other escape sequences (octal, hex, C) are supported
  in the strings of the `y` command.
  Under POSIX these yield undefined behavior.
* The `a`, `c`, and `i` commands do not require an initial backslash,
  allow text to appear on the same line, and support escape sequences
  in the specified text.
* The `a`, `i`, `=`, `l`, `q` and `r` commands support address range as an extension to POSIX.
* The substitution command replacement group `\0` is a synonym for &.
* An `F` command outputs the name of the file currently being processed.
* A `Q` command (optionally followed by an exit code) quits immediately.
* The `q` command can be optionally followed by an exit code.
* A `W` command writes to a file the pattern's first line.
* An `R` command reads one line at a time from a file.
* The `l` command can be optionally followed by the output width.
* The `--follow-symlinks` option for in-place editing.
* The `--sandbox` option that limits potentially destructive commands.
* Address 0 can be used to specify an address range that is already
  active on line 1 and can finish with the specified regular expression.
* Address steps can be specified in the form of start~step and start,~step
  ranges.
* Address 0 can be used in the `r` command to prepend a file.

## Supported BSD and GNU extensions
* The second address in a range can be specified as a relative address with +N.
* In-place editing of file with the `-i` flag.

## New extensions
* Unicode characters can be specified in regular expression pattern, replacement
  and transliteration sequences using `\uXXXX` or `\UXXXXXXXX` sequences.
* Script errors are reported with the offending line quoted back and the
  character at fault underlined, whenever standard error is a terminal. See
  [Script error diagnostics](#script-error-diagnostics).

## Script error diagnostics
When standard error is a terminal, a script error is followed by the script
line it was found on, with the character at fault underlined. The usual
one-line message always comes first and is unchanged, and when standard error
is a pipe or a file it is the whole of the output, so nothing that parses
_sed_'s output is affected.

The `UUTILS_DIAG` environment variable overrides the terminal check, as it does
for the other uutils: `never` always gives the single-line message, and
`always` gives the report even when redirected. Colors follow `NO_COLOR`.

Positions are `input:line:column`, where input is the script file name or
`<script argument N>`, and the column counts bytes, as in GNU _sed_.

An invalid substitute flag:
```
$ sed 's/a/b/q'
sed: <script argument 1>:1:7: error: invalid substitute flag: 'q'
   ╭─[ <script argument 1>:1:7 ]
   │
 1 │ s/a/b/q
   │       ┬
   │       ╰── here
───╯
```

An error at the end of the line points just past the last character:
```
$ sed '/adrift'
sed: <script argument 1>:1:8: error: unterminated regular expression
   ╭─[ <script argument 1>:1:8 ]
   │
 1 │ /adrift
   │        ┬
   │        ╰── here
───╯
```

In a script file, the file name and line number are shown:
```
$ cat edit.sed
s/foo/bar/
/start/,/end/{
  s/x/y/g
  y/abc/de/
}
$ sed -f edit.sed
sed: edit.sed:4:11: error: transliteration strings are not the same length
   ╭─[ edit.sed:4:11 ]
   │
 4 │   y/abc/de/
   │           ┬
   │           ╰── here
───╯
```

Errors found only once the whole script is compiled, such as a branch to an
undefined label, still quote the line they came from:
```
$ sed -e p -e 'b nowhere'
sed: <script argument 2>:1:1: error: undefined label `nowhere'
   ╭─[ <script argument 2>:1:1 ]
   │
 1 │ b nowhere
   │ ┬
   │ ╰── here
───╯
```

Other errors reported this way include an unknown command (`sed k`), an
unexpected `}` (`sed 'p}'`), a repeated `!` (`sed '/x/!!p'`), a missing label
or file name (`sed ':'`, `sed r`), a missing `a` text (`sed 'a'`) and an invalid
regular expression (`sed 's/\(a/b/'`).

## Incompatible extensions
The `-U` or `--uutil-extensions` option enables useful extensions or bug fixes
that aren't compatible with GNU sed or POSIX.

* The `l` command lists Unicode characters using the `\uXXXX` and `\UXXXXXXXX`
  escapes rather than as octal UTF-8 byte sequences.

## Incompatibilities
* Similarly to GNU _sed_, input is processed as raw bytes or as valid UTF-8
  (this includes 7-bit ASCII) based on the locale as specified by the
  `LC_ALL`, `LC_CTYPE`, and `LANG` environment variables,
  with the default being byte processing.
  However, in contrast with GNU _sed_, other locales (e.g. ISO-8859-1)
  are not supported. If the input is in another code page or encoding
  and requires locale-specific processing (e.g. ignore/map case,
  character classes), consider converting it through UTF-8 to ensure
  the correct handling of locale-specific regular expressions.
  This _sed_ program can also handle arbitrary byte sequences
  if no part of the input requires treating it as a Rust String.
* Back-references aren't supported when input is processed as bytes
  (`LC_ALL=C`).
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
