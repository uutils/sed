// Compile the scripts into the internal representation of commands
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{CliOptions, Command, CommandData, ScriptValue};
use crate::script_char_provider::ScriptCharProvider;
use crate::script_line_provider::ScriptLineProvider;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use uucore::error::{UResult, USimpleError};

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
enum ContinueAction {
    NextLine,
    NextChar,
}

pub fn compile(
    scripts: Vec<ScriptValue>,
    cli_options: &mut CliOptions,
) -> UResult<Option<Box<Command>>> {
    let mut line_provider = ScriptLineProvider::new(scripts);

    let result = compile_thread(&mut line_provider, cli_options)?;
    // TODO: fix-up labels, check used labels, setup append & match structures
    Ok(result)
}

// Compile provided scripts into a thread of commands
fn compile_thread(
    lines: &mut ScriptLineProvider,
    _cli_options: &mut CliOptions,
) -> UResult<Option<Box<Command>>> {
    let mut head: Option<Box<Command>> = None;
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

                // TODO: set cli_options.quiet for StringVal starting with #n
                'next_char: loop {
                    line.eat_spaces();
                    if line.eol() || line.current() == '#' {
                        continue 'next_line;
                    } else if line.current() == ';' {
                        line.advance();
                        continue 'next_char;
                    }

                    let mut cmd = Box::new(Command {
                        next: None,
                        addr1: None,
                        addr2: None,
                        start_line: Some(0),
                        text: None,
                        data: CommandData::None,
                        code: '_',
                        non_select: false,
                    });

                    let n_addr = compile_addresses(&mut line, &mut cmd);
                    let mut cmd_spec = get_cmd_spec(lines, &line, n_addr)?;

                    if cmd_spec.args == CommandArgs::NonSelect {
                        line.advance();
                        line.eat_spaces();
                        cmd.non_select = true;
                        cmd_spec = get_cmd_spec(lines, &line, n_addr)?;
                    }

                    // Move cmd into next_p, transferring its ownership
                    let action = compile_command(lines, &mut line, &mut cmd, cmd_spec)?;

                    *next_p = Some(cmd);
                    next_p = &mut next_p.as_mut().unwrap().next;

                    match action {
                        ContinueAction::NextLine => continue 'next_line,
                        ContinueAction::NextChar => continue 'next_char,
                    }
                }
            }
        }
    }
}

// Compile a command's addresses into cmd.
// Return the number of addresses encountered.
fn compile_addresses(_line: &mut ScriptCharProvider, _cmd: &mut Command) -> usize {
    // TODO: implement address parsing
    0
}

// Compile the specified command
fn compile_command(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
    cmd_spec: &'static CommandSpec,
) -> UResult<ContinueAction> {
    cmd.code = line.current();

    match cmd_spec.args {
        CommandArgs::Empty => {
            // d D g G h H l n N p P q x =
            line.advance();
            line.eat_spaces();
            if !line.eol() && line.current() == ';' {
                line.advance();
                // TODO: update link
                return Ok(ContinueAction::NextChar);
            }
            if !line.eol() {
                return compile_error(
                    lines,
                    line,
                    format!("extra characters at the end of the {} command", cmd.code),
                );
            }
        }
        // TODO
        CommandArgs::Text => { // a c i
        }
        CommandArgs::NonSelect => { // !
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
        CommandArgs::Substitute => { // s
        }
        CommandArgs::Translate => { // y
        }
    }

    Ok(ContinueAction::NextLine)
}

// Fail with msg as a compile error at the current location
fn compile_error<T>(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    msg: impl ToString,
) -> UResult<T> {
    Err(USimpleError::new(
        1,
        format!(
            "{}:{}:{}: error: {}",
            lines.get_input_name(),
            lines.get_line_number(),
            line.get_pos(),
            msg.to_string()
        ),
    ))
}

// Return the specification for the command letter at the current line position
// checking for diverse errors.
fn get_cmd_spec(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    n_addr: usize,
) -> UResult<&'static CommandSpec> {
    if line.eol() {
        return compile_error(lines, line, "command expected");
    }

    let ch = line.current();
    let opt_cmd_spec = lookup_command(ch);

    if opt_cmd_spec.is_none() {
        return compile_error(lines, line, format!("invalid command code {}", ch));
    }

    let cmd_spec = opt_cmd_spec.unwrap();
    if n_addr > cmd_spec.n_addr {
        return compile_error(
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

    // compile_error
    #[test]
    fn test_compile_error_message_format() {
        let lines = ScriptLineProvider::with_active_state("test.sed", 42);
        let mut line = char_provider_from("whatever");
        line.advance(); // move to position 1
        line.advance(); // move to position 2
        line.advance(); // move to position 3
        line.advance(); // now at position 4

        let msg = "unexpected token";
        let result: UResult<()> = compile_error(&lines, &line, msg);

        assert!(result.is_err());

        let err = result.unwrap_err();
        let msg = err.to_string();

        assert!(msg.contains("test.sed:42:4: error: unexpected token"));
    }

    #[test]
    fn test_compile_error_with_format_message() {
        let lines = ScriptLineProvider::with_active_state("input.txt", 3);
        let mut line = char_provider_from("x");
        // We're at position 0

        let result: UResult<()> =
            compile_error(&lines, &line, format!("invalid command '{}'", 'x'));

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
}
