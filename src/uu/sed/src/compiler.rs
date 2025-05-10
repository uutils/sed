// Compile the scripts into the internal representation of commands
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{
    Address, AddressType, AddressValue, Command, CommandData, ProcessingContext, ReplacementPart,
    ReplacementTemplate, ScriptValue, Substitution,
};
use crate::delimited_parser::{compilation_error, parse_char_escape, parse_regex};
use crate::named_writer::NamedWriter;
use crate::script_char_provider::ScriptCharProvider;
use crate::script_line_provider::ScriptLineProvider;
use once_cell::sync::Lazy;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use uucore::error::UResult;

thread_local! {
    /// The previously saved RE. It is reused when specifying an empty one.
    static SAVED_REGEX: RefCell<Option<Regex>> = const { RefCell::new(None) };
}

// A global, immutable map of command properties, initialized on first access
static CMD_MAP: Lazy<HashMap<char, CommandSpec>> = Lazy::new(build_command_map);

// Types of command arguments recognized by the parser
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CommandArgs {
    Empty,      // d D g G h H l n N p P q x = \0
    Text,       // a c i
    NonSelect,  // !
    Group,      // {
    EndGroup,   // }
    Comment,    // #
    Branch,     // b t
    Label,      // :
    ReadFile,   // r
    WriteFile,  // w
    Substitute, // s
    Translate,  // y
}

// Command specification
#[derive(Debug, Clone, Copy)]
struct CommandSpec {
    code: char,        // Command letter used by sed
    n_addr: usize,     // Number of supported addresses
    args: CommandArgs, // Type of command arguments
}

// Build the command specification map (char -> CommandSpec)
fn build_command_map() -> HashMap<char, CommandSpec> {
    let formats = [
        CommandSpec {
            code: '{',
            n_addr: 2,
            args: CommandArgs::Group,
        },
        CommandSpec {
            code: '}',
            n_addr: 0,
            args: CommandArgs::EndGroup,
        },
        CommandSpec {
            code: 'a',
            n_addr: 1,
            args: CommandArgs::Text,
        },
        CommandSpec {
            code: 'b',
            n_addr: 2,
            args: CommandArgs::Branch,
        },
        CommandSpec {
            code: 'c',
            n_addr: 2,
            args: CommandArgs::Text,
        },
        CommandSpec {
            code: 'd',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'D',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'g',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'G',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'h',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'H',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'i',
            n_addr: 1,
            args: CommandArgs::Text,
        },
        CommandSpec {
            code: 'l',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'n',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'N',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'p',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'P',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'q',
            n_addr: 1,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'r',
            n_addr: 1,
            args: CommandArgs::ReadFile,
        },
        CommandSpec {
            code: 's',
            n_addr: 2,
            args: CommandArgs::Substitute,
        },
        CommandSpec {
            code: 't',
            n_addr: 2,
            args: CommandArgs::Branch,
        },
        CommandSpec {
            code: 'w',
            n_addr: 2,
            args: CommandArgs::WriteFile,
        },
        CommandSpec {
            code: 'x',
            n_addr: 2,
            args: CommandArgs::Empty,
        },
        CommandSpec {
            code: 'y',
            n_addr: 2,
            args: CommandArgs::Translate,
        },
        CommandSpec {
            code: '!',
            n_addr: 2,
            args: CommandArgs::NonSelect,
        },
        CommandSpec {
            code: ':',
            n_addr: 0,
            args: CommandArgs::Label,
        },
        CommandSpec {
            code: '#',
            n_addr: 0,
            args: CommandArgs::Comment,
        },
        CommandSpec {
            code: '=',
            n_addr: 1,
            args: CommandArgs::Empty,
        },
    ];

    formats.into_iter().map(|f| (f.code, f)).collect()
}

// How to continue after processing a command
#[derive(Debug)]
pub enum ContinueAction {
    NextLine,
    NextChar,
}

pub fn compile(
    scripts: Vec<ScriptValue>,
    processing_context: &mut ProcessingContext,
) -> UResult<Option<Rc<RefCell<Command>>>> {
    let mut make_providers = ScriptLineProvider::new(scripts);

    let result = compile_thread(&mut make_providers, processing_context)?;
    // TODO: fix-up labels, check used labels, setup append & match structures
    Ok(result)
}

// Compile provided scripts into a thread of commands
fn compile_thread(
    lines: &mut ScriptLineProvider,
    _processing_context: &mut ProcessingContext,
) -> UResult<Option<Rc<RefCell<Command>>>> {
    let mut head: Option<Rc<RefCell<Command>>> = None;
    // A mutable reference to the place we’ll insert next
    let mut next_p = &mut head;

    'next_line: loop {
        match lines.next_line().unwrap() {
            None => {
                // TODO: Error if stack isn't empty
                return Ok(head);
            }
            Some(line_string) => {
                let mut line = ScriptCharProvider::new(&line_string);

                // TODO: set processing_context.quiet for StringVal starting with #n
                'next_char: loop {
                    line.eat_spaces();
                    if line.eol() || line.current() == '#' {
                        continue 'next_line;
                    } else if line.current() == ';' {
                        line.advance();
                        continue 'next_char;
                    }

                    let mut cmd = Rc::new(RefCell::new(Command::default()));
                    let n_addr = compile_address_range(lines, &mut line, &mut cmd)?;
                    let mut cmd_spec = get_cmd_spec(lines, &line, n_addr)?;

                    // The ! command shall be followed by another one
                    if cmd_spec.args == CommandArgs::NonSelect {
                        line.advance();
                        line.eat_spaces();
                        cmd.borrow_mut().non_select = true;
                        cmd_spec = get_cmd_spec(lines, &line, n_addr)?;
                    }

                    // Move cmd into next_p, transferring its ownership
                    let action = compile_command(lines, &mut line, &mut cmd, cmd_spec)?;

                    *next_p = Some(cmd);
                    // Intermediate let binding to avoid the temporary drop
                    let cmd_rc = next_p.as_mut().unwrap();
                    let cmd_ptr =
                        &mut cmd_rc.borrow_mut().next as *mut Option<Rc<RefCell<Command>>>;
                    unsafe {
                        next_p = &mut *cmd_ptr;
                    }

                    match action {
                        ContinueAction::NextLine => continue 'next_line,
                        ContinueAction::NextChar => continue 'next_char,
                    }
                }
            }
        }
    }
}

/// Return true if c is a valid character for specifying a context address
fn is_address_char(c: char) -> bool {
    matches!(c, '0'..='9' | '/' | '\\' | '$')
}

/// Compile a command's optional address range into cmd.
/// Return the number of addresses encountered.
fn compile_address_range(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Rc<RefCell<Command>>,
) -> UResult<usize> {
    let mut n_addr = 0;
    let mut cmd = cmd.borrow_mut();

    line.eat_spaces();
    if !line.eol() && is_address_char(line.current()) {
        if let Ok(addr1) = compile_address(lines, line) {
            cmd.addr1 = Some(addr1);
            n_addr += 1;
        }
    }

    line.eat_spaces();
    if n_addr == 1 && !line.eol() && line.current() == ',' {
        line.advance();
        line.eat_spaces();
        if !line.eol() {
            if let Ok(addr2) = compile_address(lines, line) {
                cmd.addr2 = Some(addr2);
                n_addr += 1;
            }
        }
    }

    Ok(n_addr)
}

/// Compile and return a single range address specification.
fn compile_address(lines: &ScriptLineProvider, line: &mut ScriptCharProvider) -> UResult<Address> {
    let mut icase = false;

    if line.eol() {
        return compilation_error(lines, line, "expected context address");
    }

    match line.current() {
        '\\' | '/' => {
            // Regular expression
            if line.current() == '\\' {
                // The next character is an arbitrary delimiter
                line.advance();
            }
            let re = parse_regex(lines, line)?;
            // Skip over delimiter
            line.advance();

            line.eat_spaces();
            if !line.eol() && line.current() == 'I' {
                icase = true;
                line.advance();
            }

            Ok(Address {
                atype: AddressType::Re,
                value: AddressValue::Regex(compile_regex(lines, line, &re, icase)?),
            })
        }
        '$' => {
            line.advance();
            Ok(Address {
                atype: AddressType::Last,
                value: AddressValue::LineNumber(0),
            })
        }
        '+' => {
            line.advance();
            let number = parse_number(lines, line)?;
            Ok(Address {
                atype: AddressType::RelLine,
                value: AddressValue::LineNumber(number),
            })
        }
        c if c.is_ascii_digit() => {
            let number = parse_number(lines, line)?;
            Ok(Address {
                atype: AddressType::Line,
                value: AddressValue::LineNumber(number),
            })
        }
        _ => panic!("invalid context address"),
    }
}

/// Parse and return the decimal number at the current line position.
/// Advance the line to first non-digit or EOL.
fn parse_number(lines: &ScriptLineProvider, line: &mut ScriptCharProvider) -> UResult<usize> {
    let mut num_str = String::new();

    while !line.eol() && line.current().is_ascii_digit() {
        num_str.push(line.current());
        line.advance();
    }

    num_str
        .parse::<usize>()
        .map_err(|_| format!("invalid number '{}'", num_str))
        .map_err(|msg| compilation_error::<usize>(lines, line, msg).unwrap_err())
}

/// Compile the provided regular expression string into a corresponding engine.
fn compile_regex(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    pattern: &str,
    icase: bool,
) -> UResult<Regex> {
    if pattern.is_empty() {
        SAVED_REGEX.with(|cell| {
            if let Some(existing) = &*cell.borrow() {
                Ok(existing.clone())
            } else {
                compilation_error(lines, line, "no previously compiled regex available")
            }
        })
    } else {
        let full_pattern = if icase {
            if pattern.is_empty() {
                return compilation_error(lines, line, "cannot specify a modifier on an empty RE");
            }
            format!("(?i){}", pattern)
        } else {
            pattern.to_string()
        };

        let compiled = Regex::new(&full_pattern).map_err(|e| {
            compilation_error::<Regex>(lines, line, format!("invalid regex '{}': {}", pattern, e))
                .unwrap_err()
        })?;

        SAVED_REGEX.with(|cell| {
            *cell.borrow_mut() = Some(compiled.clone());
        });
        Ok(compiled)
    }
}

/// Compile a regular expression replacement string.
pub fn compile_replacement(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
) -> UResult<ReplacementTemplate> {
    let mut parts = Vec::new();
    let mut literal = String::new();

    let delimiter = line.current();
    line.advance();

    loop {
        while !line.eol() {
            match line.current() {
                '\\' => {
                    line.advance();

                    // Line continuation
                    if line.eol() {
                        if let Some(next_line_string) = lines.next_line()? {
                            *line = ScriptCharProvider::new(&next_line_string);
                            continue;
                        } else {
                            return compilation_error(
                                lines,
                                line,
                                "unterminated substitute replacement (unexpected EOF)",
                            );
                        }
                    }

                    match line.current() {
                        // \1 - \9
                        c @ '1'..='9' => {
                            let ref_num = c.to_digit(10).unwrap();

                            if !literal.is_empty() {
                                parts.push(ReplacementPart::Literal(std::mem::take(&mut literal)));
                            }
                            parts.push(ReplacementPart::Group(ref_num));
                            line.advance();
                        }

                        // literal \ and &
                        '\\' | '&' => {
                            literal.push(line.current());
                            line.advance();
                        }

                        // other escape sequences
                        _ => match parse_char_escape(line) {
                            Some(decoded) => literal.push(decoded),
                            None => {
                                literal.push('\\');
                                literal.push(line.current());
                                line.advance();
                            }
                        },
                    }
                }

                '&' => {
                    if !literal.is_empty() {
                        parts.push(ReplacementPart::Literal(std::mem::take(&mut literal)));
                    }
                    parts.push(ReplacementPart::WholeMatch);
                    line.advance();
                }

                '\n' => {
                    return compilation_error(
                        lines,
                        line,
                        "unescaped newline inside substitute replacement",
                    );
                }

                c if c == delimiter => {
                    line.advance(); // skip closing delimiter
                    if !literal.is_empty() {
                        parts.push(ReplacementPart::Literal(literal));
                    }
                    return Ok(ReplacementTemplate { parts });
                }

                c => {
                    literal.push(c);
                    line.advance();
                }
            }
        }

        // Fetch next line for continued replacement string
        if let Some(next_line_string) = lines.next_line()? {
            *line = ScriptCharProvider::new(&next_line_string);
        } else {
            return compilation_error(lines, line, "unterminated substitute replacement");
        }
    }
}

pub fn compile_subst_command(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
) -> UResult<ContinueAction> {
    line.advance(); // move past 's'

    let delimiter = line.current();
    if delimiter == '\0' || delimiter == '\\' {
        return compilation_error(
            lines,
            line,
            "substitute pattern cannot be delimited by newline or backslash",
        );
    }

    let pattern = parse_regex(lines, line)?;
    if pattern.is_empty() {
        return compilation_error(lines, line, "unterminated substitute pattern");
    }

    let mut subst = Box::new(Substitution {
        occurrence: 0,
        print_flag: false,
        ignore_case: false,
        write_file: None,
        regex: compile_regex(lines, line, &pattern, false)?, // temp compile
        line_number: lines.get_line_number(),
        replacement: ReplacementTemplate::default(),
    });

    subst.replacement = compile_replacement(lines, line)?;
    compile_subst_flags(lines, line, &mut subst)?;

    // Recompile regex with actual ignore_case flag
    subst.regex = compile_regex(lines, line, &pattern, subst.ignore_case)?;

    line.eat_spaces();
    if !line.eol() && line.current() == ';' {
        line.advance();
        cmd.data = CommandData::Substitution(subst);
        return Ok(ContinueAction::NextChar);
    }

    if !line.eol() {
        return compilation_error(
            lines,
            line,
            format!("extra characters at the end of the {} command", cmd.code),
        );
    }

    cmd.data = CommandData::Substitution(subst);
    Ok(ContinueAction::NextLine)
}

/// Parse the substitution command's optional flags
pub fn compile_subst_flags(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    subst: &mut Substitution,
) -> UResult<()> {
    let mut seen_g_or_n = false;

    subst.occurrence = 1; // default
    subst.print_flag = false;
    subst.ignore_case = false;
    subst.write_file = None;

    while !line.eol() {
        line.eat_spaces();

        match line.current() {
            'g' => {
                if seen_g_or_n {
                    return compilation_error(
                        lines,
                        line,
                        "multiple 'g' or numeric flags in substitute command",
                    );
                }
                seen_g_or_n = true;
                subst.occurrence = 0;
                line.advance();
            }

            'p' => {
                subst.print_flag = true;
                line.advance();
            }

            'i' | 'I' => {
                subst.ignore_case = true;
                line.advance();
            }

            _c @ '1'..='9' => {
                if seen_g_or_n {
                    return compilation_error(
                        lines,
                        line,
                        "multiple 'g' or numeric flags in substitute command",
                    );
                }

                let mut number = 0usize;
                while !line.eol() && line.current().is_ascii_digit() {
                    number = number
                        .checked_mul(10)
                        .and_then(|n| n.checked_add(line.current().to_digit(10).unwrap() as usize))
                        .ok_or_else(|| {
                            compilation_error::<()>(
                                lines,
                                line,
                                "overflow in numeric substitute flag",
                            )
                            .unwrap_err()
                        })?;
                    line.advance();
                }

                subst.occurrence = number;
                seen_g_or_n = true;
            }

            'w' => {
                line.advance();
                line.eat_spaces();

                let mut path = String::new();
                while !line.eol() && line.current() != ';' {
                    path.push(line.current());
                    line.advance();
                }

                if path.is_empty() {
                    return compilation_error(lines, line, "missing filename after 'w' flag");
                }

                subst.write_file = Some(NamedWriter::new(PathBuf::from(path))?);
                // NOTE: subst.write_handle is resolved later at runtime
                return Ok(()); // 'w' is the last flag allowed
            }

            ';' | '\n' => break,

            other => {
                return compilation_error(
                    lines,
                    line,
                    format!("invalid substitute flag: '{}'", other),
                );
            }
        }
    }

    Ok(())
}

/// Compile a command that doesn't take any arguments
// Handles d D g G h H l n N p P q x =
pub fn compile_empty_command(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
) -> UResult<ContinueAction> {
    line.advance(); // Skip the command character
    line.eat_spaces(); // Skip any trailing whitespace

    if !line.eol() && line.current() == ';' {
        line.advance();
        return Ok(ContinueAction::NextChar);
    }

    if !line.eol() {
        return compilation_error(
            lines,
            line,
            format!("extra characters at the end of the {} command", cmd.code),
        );
    }

    Ok(ContinueAction::NextLine)
}

// Compile the specified command
fn compile_command(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Rc<RefCell<Command>>,
    cmd_spec: &'static CommandSpec,
) -> UResult<ContinueAction> {
    let mut cmd = cmd.borrow_mut();
    cmd.code = line.current();

    match cmd_spec.args {
        CommandArgs::Empty => {
            // d D g G h H l n N p P q x =
            return compile_empty_command(lines, line, &mut cmd);
        }
        CommandArgs::NonSelect => { // !
             // Implemented at a heigher level.
        }
        CommandArgs::Substitute => {
            // s
            return compile_subst_command(lines, line, &mut cmd);
        }
        // TODO
        CommandArgs::Text => { // a c i
        }
        CommandArgs::Group => { // {
        }
        CommandArgs::EndGroup => { // }
        }
        CommandArgs::Comment => { // #
        }
        CommandArgs::Branch => { // b t
        }
        CommandArgs::Label => { // :
        }
        CommandArgs::ReadFile => { // r
        }
        CommandArgs::WriteFile => { // w
        }
        CommandArgs::Translate => { // y
        }
    }

    Ok(ContinueAction::NextLine)
}

// Return the specification for the command letter at the current line position
// checking for diverse errors.
fn get_cmd_spec(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    n_addr: usize,
) -> UResult<&'static CommandSpec> {
    if line.eol() {
        return compilation_error(lines, line, "command expected");
    }

    let ch = line.current();
    let opt_cmd_spec = lookup_command(ch);

    if opt_cmd_spec.is_none() {
        return compilation_error(lines, line, format!("invalid command code {}", ch));
    }

    let cmd_spec = opt_cmd_spec.unwrap();
    if n_addr > cmd_spec.n_addr {
        return compilation_error(
            lines,
            line,
            format!(
                "command {} expects up to {} address(es), found {}",
                ch, cmd_spec.n_addr, n_addr
            ),
        );
    }

    Ok(cmd_spec)
}

// Look up a command format by its command code.
fn lookup_command(cmd: char) -> Option<&'static CommandSpec> {
    CMD_MAP.get(&cmd)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_providers(input: &str) -> (ScriptLineProvider, ScriptCharProvider) {
        let lines = ScriptLineProvider::new(vec![]); // Empty for tests
        let line = ScriptCharProvider::new(input);
        (lines, line)
    }

    // lookup_command
    #[test]
    fn test_lookup_empty_command() {
        let cmd = lookup_command('d').unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Empty);
    }

    #[test]
    fn test_lookup_text_command() {
        let cmd = lookup_command('a').unwrap();
        assert_eq!(cmd.n_addr, 1);
        assert_eq!(cmd.args, CommandArgs::Text);
    }

    #[test]
    fn test_lookup_nonselect_command() {
        let cmd = lookup_command('!').unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::NonSelect);
    }

    #[test]
    fn test_lookup_group_command() {
        let cmd = lookup_command('{').unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Group);
    }

    #[test]
    fn test_lookup_endgroup_command() {
        let cmd = lookup_command('}').unwrap();
        assert_eq!(cmd.n_addr, 0);
        assert_eq!(cmd.args, CommandArgs::EndGroup);
    }

    #[test]
    fn test_lookup_comment_command() {
        let cmd = lookup_command('#').unwrap();
        assert_eq!(cmd.n_addr, 0);
        assert_eq!(cmd.args, CommandArgs::Comment);
    }

    #[test]
    fn test_lookup_branch_command() {
        let cmd = lookup_command('b').unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Branch);
    }

    #[test]
    fn test_lookup_label_command() {
        let cmd = lookup_command(':').unwrap();
        assert_eq!(cmd.n_addr, 0);
        assert_eq!(cmd.args, CommandArgs::Label);
    }

    #[test]
    fn test_lookup_readfile_command() {
        let cmd = lookup_command('r').unwrap();
        assert_eq!(cmd.n_addr, 1);
        assert_eq!(cmd.args, CommandArgs::ReadFile);
    }

    #[test]
    fn test_lookup_writefile_command() {
        let cmd = lookup_command('w').unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::WriteFile);
    }

    #[test]
    fn test_lookup_substitute_command() {
        let cmd = lookup_command('s').unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Substitute);
    }

    #[test]
    fn test_lookup_translate_command() {
        let cmd = lookup_command('y').unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Translate);
    }

    #[test]
    fn test_lookup_invalid_command() {
        let result = lookup_command('Z');
        assert!(result.is_none());
    }

    // Utility to create a ScriptCharProvider from a &str
    fn char_provider_from(s: &str) -> ScriptCharProvider {
        ScriptCharProvider::new(s)
    }

    // compilation_error
    #[test]
    fn test_compilation_error_message_format() {
        let lines = ScriptLineProvider::with_active_state("test.sed", 42);
        let mut line = char_provider_from("whatever");
        line.advance(); // move to position 1
        line.advance(); // move to position 2
        line.advance(); // move to position 3
        line.advance(); // now at position 4

        let msg = "unexpected token";
        let result: UResult<()> = compilation_error(&lines, &line, msg);

        assert!(result.is_err());

        let err = result.unwrap_err();
        let msg = err.to_string();

        assert!(msg.contains("test.sed:42:4: error: unexpected token"));
    }

    #[test]
    fn test_compilation_error_with_format_message() {
        let lines = ScriptLineProvider::with_active_state("input.txt", 3);
        let line = char_provider_from("x");
        // We're at position 0

        let result: UResult<()> =
            compilation_error(&lines, &line, format!("invalid command '{}'", 'x'));

        assert!(result.is_err());

        let err = result.unwrap_err();
        let msg = err.to_string();

        assert_eq!(msg, "input.txt:3:0: error: invalid command 'x'");
    }

    // get_cmd_spec
    #[test]
    fn test_missing_command_character() {
        let lines = ScriptLineProvider::with_active_state("test.sed", 1);
        let line = char_provider_from("");
        let result = get_cmd_spec(&lines, &line, 0);

        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("test.sed:1:0: error: command expected"));
    }

    #[test]
    fn test_invalid_command_character() {
        let lines = ScriptLineProvider::with_active_state("script.sed", 2);
        let line = char_provider_from("@");
        let result = get_cmd_spec(&lines, &line, 0);

        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("script.sed:2:0: error: invalid command code @"));
    }

    #[test]
    fn test_too_many_addresses() {
        let lines = ScriptLineProvider::with_active_state("input.sed", 3);
        let line = char_provider_from("q"); // q takes one address
        let result = get_cmd_spec(&lines, &line, 2);

        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("input.sed:3:0: error: command q expects up to 1 address(es), found 2")
        );
    }

    #[test]
    fn test_valid_command_spec() {
        let lines = ScriptLineProvider::with_active_state("input.sed", 4);
        let line = char_provider_from("a"); // valid command
        let result = get_cmd_spec(&lines, &line, 1);

        assert!(result.is_ok());
        let spec = result.unwrap();
        assert_eq!(spec.code, 'a');
    }

    // parse_number
    #[test]
    fn test_parse_number_basic() {
        let (lines, mut chars) = make_providers("123abc");
        assert_eq!(parse_number(&lines, &mut chars).unwrap(), 123);
        assert_eq!(chars.current(), 'a'); // Should stop at first non-digit
    }

    #[test]
    fn test_parse_number_invalid() {
        let (lines, mut chars) = make_providers("abc");
        assert!(parse_number(&lines, &mut chars).is_err());
    }

    // compile_re
    fn dummy_providers() -> (ScriptLineProvider, ScriptCharProvider) {
        make_providers("dummy input")
    }

    #[test]
    fn test_compile_re_basic() {
        let (lines, chars) = dummy_providers();
        let regex = compile_regex(&lines, &chars, "abc", false).unwrap();
        assert!(regex.is_match("abc"));
        assert!(!regex.is_match("ABC"));
    }

    #[test]
    fn test_compile_re_case_insensitive() {
        let (lines, chars) = dummy_providers();
        let regex = compile_regex(&lines, &chars, "abc", true).unwrap();
        assert!(regex.is_match("abc"));
        assert!(regex.is_match("ABC"));
        assert!(regex.is_match("AbC"));
    }

    #[test]
    fn test_compile_re_saved_and_reuse() {
        // Save a regex
        let (lines1, chars1) = dummy_providers();
        let _ = compile_regex(&lines1, &chars1, "abc", false).unwrap();

        // Now try to reuse it
        let (lines2, chars2) = dummy_providers();
        let reused = compile_regex(&lines2, &chars2, "", false).unwrap();

        assert!(reused.is_match("abc"));
    }

    #[test]
    fn test_compile_re_empty_and_not_saved() {
        // Clear saved regex
        SAVED_REGEX.with(|cell| {
            *cell.borrow_mut() = None;
        });

        let (lines, chars) = dummy_providers();
        let result = compile_regex(&lines, &chars, "", false);
        assert!(result.is_err()); // Should fail because nothing was saved
    }

    #[test]
    fn test_compile_re_invalid() {
        let (lines, chars) = dummy_providers();
        let result = compile_regex(&lines, &chars, "a[d", false);
        assert!(result.is_err()); // Should fail due to open bracketed expression
    }

    // compile_address
    #[test]
    fn test_compile_addr_line_number() {
        let (lines, mut chars) = make_providers("42");
        let addr = compile_address(&lines, &mut chars).unwrap();
        assert!(matches!(addr.atype, AddressType::Line));
        if let AddressValue::LineNumber(n) = addr.value {
            assert_eq!(n, 42);
        } else {
            panic!("expected LineNumber address value");
        }
    }

    #[test]
    fn test_compile_addr_relative_line() {
        let (lines, mut chars) = make_providers("+7");
        let addr = compile_address(&lines, &mut chars).unwrap();
        assert!(matches!(addr.atype, AddressType::RelLine));
        if let AddressValue::LineNumber(n) = addr.value {
            assert_eq!(n, 7);
        } else {
            panic!("expected LineNumber address value");
        }
    }

    #[test]
    fn test_compile_addr_last_line() {
        let (lines, mut chars) = make_providers("$");
        let addr = compile_address(&lines, &mut chars).unwrap();
        assert!(matches!(addr.atype, AddressType::Last));
    }

    #[test]
    fn test_compile_addr_regex() {
        let (lines, mut chars) = make_providers("/hello/");
        let addr = compile_address(&lines, &mut chars).unwrap();
        assert!(matches!(addr.atype, AddressType::Re));
        if let AddressValue::Regex(re) = addr.value {
            assert!(re.is_match("hello"));
        } else {
            panic!("expected Regex address value");
        }
    }

    #[test]
    fn test_compile_addr_regex_other_delimiter() {
        let (lines, mut chars) = make_providers("\\#hello#");
        let addr = compile_address(&lines, &mut chars).unwrap();
        assert!(matches!(addr.atype, AddressType::Re));
        if let AddressValue::Regex(re) = addr.value {
            assert!(re.is_match("hello"));
        } else {
            panic!("expected Regex address value");
        }
    }

    #[test]
    fn test_compile_addr_regex_with_modifier() {
        let (lines, mut chars) = make_providers("/hello/I");
        let addr = compile_address(&lines, &mut chars).unwrap();
        assert!(matches!(addr.atype, AddressType::Re));
        if let AddressValue::Regex(re) = addr.value {
            assert!(re.is_match("HELLO")); // case-insensitive
        } else {
            panic!("expected Regex address value");
        }
    }

    #[test]
    fn test_compile_addr_empty_regex_saved() {
        // First save a regex
        let (lines1, mut chars1) = make_providers("/saved/");
        let _ = compile_address(&lines1, &mut chars1).unwrap();

        // Then reuse it with empty regex
        let (lines2, mut chars2) = make_providers("//");
        let addr = compile_address(&lines2, &mut chars2).unwrap();
        assert!(matches!(addr.atype, AddressType::Re));
        if let AddressValue::Regex(re) = addr.value {
            assert!(re.is_match("saved"));
        } else {
            panic!("expected Regex address value");
        }
    }

    // compile_address_range
    #[test]
    fn test_compile_single_line_address() {
        let (lines, mut chars) = make_providers("42");
        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd).unwrap();

        assert_eq!(n_addr, 1);
        assert!(matches!(
            cmd.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Line
        ));
    }

    #[test]
    fn test_compile_relative_address_range() {
        let (lines, mut chars) = make_providers("2,+3");
        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd).unwrap();

        assert_eq!(n_addr, 2);

        assert!(matches!(
            cmd.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Line
        ));
        let v1 = match &cmd.borrow().addr1.as_ref().unwrap().value {
            AddressValue::LineNumber(n) => *n,
            _ => panic!(),
        };
        assert_eq!(v1, 2);

        assert!(matches!(
            cmd.borrow().addr2.as_ref().unwrap().atype,
            AddressType::RelLine
        ));
        let v2 = match &cmd.borrow().addr2.as_ref().unwrap().value {
            AddressValue::LineNumber(n) => *n,
            _ => panic!(),
        };
        assert_eq!(v2, 3);
    }

    #[test]
    fn test_compile_last_address() {
        let (lines, mut chars) = make_providers("$");
        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd).unwrap();

        assert_eq!(n_addr, 1);
        assert!(matches!(
            cmd.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Last
        ));
    }

    #[test]
    fn test_compile_absolute_address_range() {
        let (lines, mut chars) = make_providers("5,10");
        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd).unwrap();

        assert_eq!(n_addr, 2);
        assert!(matches!(
            cmd.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Line
        ));
        assert!(matches!(
            cmd.borrow().addr2.as_ref().unwrap().atype,
            AddressType::Line
        ));
    }

    #[test]
    fn test_compile_regex_address() {
        let (lines, mut chars) = make_providers("/foo/");
        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd).unwrap();

        assert_eq!(n_addr, 1);
        assert!(matches!(
            cmd.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Re
        ));
        if let AddressValue::Regex(re) = &cmd.borrow().addr1.as_ref().unwrap().value {
            assert!(re.is_match("foo"));
            assert!(!re.is_match("bar"));
        } else {
            panic!("expected a regex address");
        };
    }

    #[test]
    fn test_compile_regex_address_range_other_delimiter() {
        let (lines, mut chars) = make_providers("\\#foo# , \\|bar|");
        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd).unwrap();

        assert_eq!(n_addr, 2);

        assert!(matches!(
            cmd.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Re
        ));
        if let AddressValue::Regex(re) = &cmd.borrow().addr1.as_ref().unwrap().value {
            assert!(re.is_match("foo"));
            assert!(!re.is_match("bar"));
        } else {
            panic!("expected a regex address");
        }

        assert!(matches!(
            cmd.borrow().addr2.as_ref().unwrap().atype,
            AddressType::Re
        ));
        if let AddressValue::Regex(re) = &cmd.borrow().addr2.as_ref().unwrap().value {
            assert!(re.is_match("bar"));
            assert!(!re.is_match("foo"));
        } else {
            panic!("expected a regex address");
        };
    }

    #[test]
    fn test_compile_regex_with_modifier() {
        let (lines, mut chars) = make_providers("/foo/I");
        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd).unwrap();

        assert_eq!(n_addr, 1);
        assert!(matches!(
            cmd.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Re
        ));
        if let AddressValue::Regex(re) = &cmd.borrow().addr1.as_ref().unwrap().value {
            assert!(re.is_match("FOO"));
            assert!(re.is_match("foo"));
        } else {
            panic!("expected a regex address with case-insensitive match");
        };
    }

    #[test]
    fn test_compile_re_reuse_saved() {
        // First save a regex
        let (lines1, mut chars1) = make_providers("/abc/");
        let mut cmd1 = Rc::new(RefCell::new(Command::default()));
        compile_address_range(&lines1, &mut chars1, &mut cmd1).unwrap();

        // Now reuse it
        let (lines2, mut chars2) = make_providers("//");
        let mut cmd2 = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines2, &mut chars2, &mut cmd2).unwrap();

        assert_eq!(n_addr, 1);
        assert!(matches!(
            cmd2.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Re
        ));
        if let AddressValue::Regex(re) = &cmd2.borrow().addr1.as_ref().unwrap().value {
            assert!(re.is_match("abc"));
        };
    }

    // compile_thread
    fn make_provider(lines: &[&str]) -> ScriptLineProvider {
        let input = lines
            .iter()
            .map(|s| ScriptValue::StringVal(s.to_string()))
            .collect();
        ScriptLineProvider::new(input)
    }

    fn make_processing_context() -> ProcessingContext {
        ProcessingContext::default()
    }

    #[test]
    fn test_compile_thread_empty_input() {
        let mut provider = make_provider(&[]);
        let mut opts = make_processing_context();

        let result = compile_thread(&mut provider, &mut opts).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_compile_thread_comment_only() {
        let mut provider = make_provider(&["# comment", "   ", ";;"]);
        let mut opts = make_processing_context();

        let result = compile_thread(&mut provider, &mut opts).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_compile_thread_single_command() {
        let mut provider = make_provider(&["42q"]);
        let mut opts = make_processing_context();

        let result = compile_thread(&mut provider, &mut opts).unwrap();
        let binding = result.unwrap();
        let cmd = binding.borrow();

        assert_eq!(cmd.code, 'q');
        assert!(!cmd.non_select);

        let addr = cmd.addr1.as_ref().expect("addr1 should be set");
        assert!(matches!(addr.atype, AddressType::Line));

        let value = match &addr.value {
            AddressValue::LineNumber(n) => *n,
            _ => panic!(),
        };
        assert_eq!(value, 42);

        assert!(cmd.next.is_none());
    }

    #[test]
    fn test_compile_thread_non_selected_single_command() {
        let mut provider = make_provider(&["42!p"]);
        let mut opts = make_processing_context();

        let result = compile_thread(&mut provider, &mut opts).unwrap();
        let binding = result.unwrap();
        let cmd = binding.borrow();

        assert_eq!(cmd.code, 'p');
        assert!(cmd.non_select);

        let addr = cmd.addr1.as_ref().expect("addr1 should be set");
        assert!(matches!(addr.atype, AddressType::Line));

        let value = match &addr.value {
            AddressValue::LineNumber(n) => *n,
            _ => panic!(),
        };
        assert_eq!(value, 42);

        assert!(cmd.next.is_none());
    }

    #[test]
    fn test_compile_thread_multiple_lines() {
        let mut provider = make_provider(&["1q", "2d"]);
        let mut opts = make_processing_context();

        let result = compile_thread(&mut provider, &mut opts).unwrap();
        let binding = result.unwrap();
        let first = binding.borrow();

        assert_eq!(first.code, 'q');
        let binding = first.next.clone().unwrap();
        let second = binding.borrow();
        assert_eq!(second.code, 'd');
        assert!(second.next.is_none());
    }

    #[test]
    fn test_compile_thread_single_line_multiple_commands() {
        let mut provider = make_provider(&["1q;2d"]);
        let mut opts = make_processing_context();

        let result = compile_thread(&mut provider, &mut opts).unwrap();
        let binding = result.unwrap();
        let first = binding.borrow();

        assert_eq!(first.code, 'q');
        let binding = first.next.clone().unwrap();
        let second = binding.borrow();
        assert_eq!(second.code, 'd');
        assert!(second.next.is_none());
    }

    // compile
    #[test]
    fn test_compile_single_command() {
        let scripts = vec![ScriptValue::StringVal("1q".to_string())];
        let mut opts = ProcessingContext::default();

        let result = compile(scripts, &mut opts).unwrap();
        let binding = result.unwrap();
        let cmd = binding.borrow();

        assert_eq!(cmd.code, 'q');

        let addr = cmd.addr1.as_ref().unwrap();
        assert!(matches!(addr.atype, AddressType::Line));

        let line = match &addr.value {
            AddressValue::LineNumber(n) => *n,
            _ => panic!(),
        };
        assert_eq!(line, 1);

        assert!(cmd.next.is_none());
    }

    // compile_replacement
    #[test]
    fn test_compile_replacement_literal() {
        let (mut lines, mut chars) = make_providers("/hello/");
        let template = compile_replacement(&mut lines, &mut chars).unwrap();
        dbg!(&template);

        assert_eq!(template.parts.len(), 1);
        assert!(matches!(&template.parts[0], ReplacementPart::Literal(s) if s == "hello"));
    }

    #[test]
    fn test_compile_replacement_backrefs_and_literal() {
        let (mut lines, mut chars) = make_providers("/prefix \\1 and \\2/");
        let template = compile_replacement(&mut lines, &mut chars).unwrap();

        assert_eq!(template.parts.len(), 4);
        assert!(matches!(&template.parts[0], ReplacementPart::Literal(s) if s == "prefix "));
        assert!(matches!(&template.parts[1], ReplacementPart::Group(1)));
        assert!(matches!(&template.parts[2], ReplacementPart::Literal(s) if s == " and "));
        assert!(matches!(&template.parts[3], ReplacementPart::Group(2)));
    }

    #[test]
    fn test_compile_replacement_whole_match() {
        let (mut lines, mut chars) = make_providers("/The match was: &/");
        let template = compile_replacement(&mut lines, &mut chars).unwrap();

        assert_eq!(template.parts.len(), 2);
        assert!(
            matches!(&template.parts[0], ReplacementPart::Literal(s) if s == "The match was: ")
        );
        assert!(matches!(&template.parts[1], ReplacementPart::WholeMatch));
    }

    #[test]
    fn test_compile_replacement_ampersand() {
        let (mut lines, mut chars) = make_providers("/Simon \\& Garfunkel/");
        let template = compile_replacement(&mut lines, &mut chars).unwrap();

        assert_eq!(template.parts.len(), 1);
        assert!(
            matches!(&template.parts[0], ReplacementPart::Literal(s) if s == "Simon & Garfunkel")
        );
    }

    #[test]
    fn test_compile_replacement_escape_sequences() {
        let (mut lines, mut chars) = make_providers("/line\\nnewline\\tend/");
        let template = compile_replacement(&mut lines, &mut chars).unwrap();

        assert_eq!(template.parts.len(), 1);
        assert!(matches!(
            &template.parts[0],
            ReplacementPart::Literal(s) if s == "line\nnewline\tend"
        ));
    }

    #[test]
    fn test_compile_replacement_line_continuation() {
        let script = vec![
            ScriptValue::StringVal("/first line\\".to_string()),
            ScriptValue::StringVal(" continued/".to_string()),
        ];
        let mut provider = ScriptLineProvider::new(script);
        let first_line = provider.next_line().unwrap().unwrap();
        let mut chars = ScriptCharProvider::new(&first_line);

        let template = compile_replacement(&mut provider, &mut chars).unwrap();
        assert_eq!(template.parts.len(), 1);
        assert!(matches!(
            &template.parts[0],
            ReplacementPart::Literal(s) if s == "first line continued"
        ));
    }

    // compile_subst_flags
    #[test]
    fn test_compile_subst_flag_g() {
        let (lines, mut chars) = make_providers("g");
        let mut subst = Substitution::default();

        compile_subst_flags(&lines, &mut chars, &mut subst).unwrap();
        assert_eq!(subst.occurrence, 0); // 'g' means all occurrences
    }

    #[test]
    fn test_compile_subst_flag_p() {
        let (lines, mut chars) = make_providers("p");
        let mut subst = Substitution::default();

        compile_subst_flags(&lines, &mut chars, &mut subst).unwrap();
        assert!(subst.print_flag);
    }

    #[test]
    fn test_compile_subst_flag_uppercase_i() {
        let (lines, mut chars) = make_providers("I");
        let mut subst = Substitution::default();

        compile_subst_flags(&lines, &mut chars, &mut subst).unwrap();
        assert!(subst.ignore_case);
    }

    #[test]
    fn test_compile_subst_flag_i_lowercase() {
        let (lines, mut chars) = make_providers("i");
        let mut subst = Substitution::default();

        compile_subst_flags(&lines, &mut chars, &mut subst).unwrap();
        assert!(subst.ignore_case);
    }

    #[test]
    fn test_compile_subst_flag_number() {
        let (lines, mut chars) = make_providers("3");
        let mut subst = Substitution::default();

        compile_subst_flags(&lines, &mut chars, &mut subst).unwrap();
        assert_eq!(subst.occurrence, 3);
    }

    #[test]
    fn test_compile_subst_flag_g_and_number_should_fail() {
        let (lines, mut chars) = make_providers("g3");
        let mut subst = Substitution::default();

        let err = compile_subst_flags(&lines, &mut chars, &mut subst).unwrap_err();
        assert!(err
            .to_string()
            .contains("multiple 'g' or numeric flags in substitute command"));
    }

    #[test]
    fn test_compile_subst_flag_number_and_g_should_fail() {
        let (lines, mut chars) = make_providers("2g");
        let mut subst = Substitution::default();

        let err = compile_subst_flags(&lines, &mut chars, &mut subst).unwrap_err();
        assert!(err
            .to_string()
            .contains("multiple 'g' or numeric flags in substitute command"));
    }

    #[test]
    fn test_compile_subst_flag_w_missing_filename() {
        let (lines, mut chars) = make_providers("w ");
        let mut subst = Substitution::default();

        let err = compile_subst_flags(&lines, &mut chars, &mut subst).unwrap_err();
        assert!(err.to_string().contains("missing filename"));
    }

    #[test]
    fn test_compile_subst_flag_w_with_filename() {
        let (lines, mut chars) = make_providers("w out.txt");
        let mut subst = Substitution::default();

        compile_subst_flags(&lines, &mut chars, &mut subst).unwrap();
        assert_eq!(
            subst.write_file.as_ref().map(|w| w.borrow().path.clone()),
            Some(std::path::PathBuf::from("out.txt"))
        );
    }

    #[test]
    fn test_compile_subst_flag_invalid_flag() {
        let (lines, mut chars) = make_providers("z");
        let mut subst = Substitution::default();

        let err = compile_subst_flags(&lines, &mut chars, &mut subst).unwrap_err();
        assert!(err.to_string().contains("invalid substitute flag"));
    }

    // compile_subst_command
    #[test]
    fn test_compile_subst_invalid_delimiter_backslash() {
        let (mut lines, mut chars) = make_providers("s\\foo\\bar\\");
        let mut cmd = Command::default();

        let err = compile_subst_command(&mut lines, &mut chars, &mut cmd).unwrap_err();
        assert!(err
            .to_string()
            .contains("substitute pattern cannot be delimited"));
    }

    #[test]
    fn test_compile_subst_empty_pattern() {
        let (mut lines, mut chars) = make_providers("s//bar/");
        let mut cmd = Command::default();

        let err = compile_subst_command(&mut lines, &mut chars, &mut cmd).unwrap_err();
        assert!(err.to_string().contains("unterminated substitute pattern"));
    }

    #[test]
    fn test_compile_subst_extra_characters_at_end() {
        let (mut lines, mut chars) = make_providers("s/foo/bar/x");
        let mut cmd = Command::default();

        let err = compile_subst_command(&mut lines, &mut chars, &mut cmd).unwrap_err();
        assert!(err.to_string().contains("invalid substitute flag"));
    }

    #[test]
    fn test_compile_subst_semicolon_indicates_continue() {
        let (mut lines, mut chars) = make_providers("s/foo/bar/;");
        let mut cmd = Command::default();

        let result = compile_subst_command(&mut lines, &mut chars, &mut cmd).unwrap();
        assert!(matches!(result, ContinueAction::NextChar));

        if let CommandData::Substitution(subst) = &cmd.data {
            assert_eq!(subst.replacement.parts.len(), 1);
        } else {
            panic!("Expected CommandData::Substitution");
        }
    }

    #[test]
    fn test_compile_subst_sets_command_data() {
        let (mut lines, mut chars) = make_providers("s/foo/bar/");
        let mut cmd = Command::default();

        let result = compile_subst_command(&mut lines, &mut chars, &mut cmd).unwrap();
        assert!(matches!(result, ContinueAction::NextLine));

        match &cmd.data {
            CommandData::Substitution(subst) => {
                assert_eq!(subst.replacement.parts.len(), 1);
                assert!(
                    matches!(&subst.replacement.parts[0], ReplacementPart::Literal(s) if s == "bar")
                );
            }
            _ => panic!("Expected CommandData::Substitution"),
        }
    }
}
