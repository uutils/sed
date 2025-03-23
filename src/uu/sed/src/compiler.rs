// Compile the scripts into the internal representation of commands
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{Command, CommandData, Context, ScriptValue};
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

pub fn compile(scripts: Vec<ScriptValue>, context: &mut Context) -> UResult<Option<Box<Command>>> {
    let mut line_provider = ScriptLineProvider::new(scripts);

    let result = compile_thread(&mut line_provider, context)?;
    // TODO: fix-up labels, check used labels, setup append & match structures
    Ok(result)
}

// Compile provided scripts into a thread of commands
fn compile_thread(
    lines: &mut ScriptLineProvider,
    _context: &mut Context,
) -> UResult<Option<Box<Command>>> {
    // Initialize the head of the list as None
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
                // Line as a vector we can index through pos
                let line: Vec<char> = line_string.chars().collect();
                let mut pos: usize = 0;

                // TODO: set context.quiet for StringVal starting with #n
                'next_char: loop {
                    eat_spaces(&line, &mut pos);
                    if pos == line.len() || line[pos] == '#' {
                        continue 'next_line;
                    } else if line[pos] == ';' {
                        pos += 1;
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

                    let n_addr = compile_addresses(&line, &mut pos, &mut cmd);
                    let mut cmd_spec = get_cmd_spec(lines, &line, pos, n_addr)?;

                    if cmd_spec.args == CommandArgs::NonSelect {
                        pos += 1;
                        eat_spaces(&line, &mut pos);
                        cmd.non_select = true;
                        cmd_spec = get_cmd_spec(lines, &line, pos, n_addr)?;
                    }
                    let action = compile_command(lines, &line, &mut pos, &mut cmd, cmd_spec)?;

                    // Move cmd into next_p, transferring its ownership
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

// Compile the specified command
fn compile_command(
    lines: &mut ScriptLineProvider,
    line: &[char],
    pos: &mut usize,
    cmd: &mut Command,
    cmd_spec: &'static CommandSpec,
) -> UResult<ContinueAction> {
    cmd.code = line[*pos];
    // TODO
    match cmd_spec.args {
        CommandArgs::Empty => {
            // d D g G h H l n N p P q x = \0
            *pos += 1;
            eat_spaces(line, pos);
            if *pos < line.len() && line[*pos] == ';' {
                *pos += 1;
                // TODO: update link
                return Ok(ContinueAction::NextChar);
            }
            if *pos < line.len() {
                return compile_error(
                    lines,
                    *pos,
                    format!("extra characters at the end of the {} command", cmd.code),
                );
            }
        }
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

// Compile a command's addresses into cmd.
// Return the number of addresses encountered.
fn compile_addresses(_line: &[char], _pos: &mut usize, _cmd: &mut Command) -> usize {
    // TODO: let n_addr = 0; ...
    0
}

// Fail with msg as a compile error at the current location
fn compile_error<T>(lines: &ScriptLineProvider, pos: usize, msg: impl ToString) -> UResult<T> {
    Err(USimpleError::new(
        1,
        format!(
            "{}:{}:{}: error: {}",
            lines.get_input_name(),
            lines.get_line_number(),
            pos,
            msg.to_string()
        ),
    ))
}

// Return the specification for the command letter line[pos]
// checking for diverse errors.
fn get_cmd_spec(
    lines: &ScriptLineProvider,
    line: &[char],
    pos: usize,
    n_addr: usize,
) -> UResult<&'static CommandSpec> {
    if pos == line.len() {
        return compile_error(lines, pos, "command expected");
    }
    let opt_cmd_spec = lookup_command(line[pos]);
    if opt_cmd_spec.is_none() {
        return compile_error(lines, pos, format!("invalid command code {}", line[pos]));
    }
    let cmd_spec = opt_cmd_spec.unwrap();
    if n_addr > cmd_spec.n_addr {
        return compile_error(
            lines,
            pos,
            format!(
                "command {} expects up to {} address(es), found {}",
                line[pos], cmd_spec.n_addr, n_addr
            ),
        );
    }
    Ok(cmd_spec)
}

// Advance pos over any white space occuring in chars
fn eat_spaces(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_whitespace() {
        *pos += 1;
    }
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

    // eat_spaces
    #[test]
    fn test_eat_spaces() {
        let input = "   \t\n  hello";
        let chars: Vec<char> = input.chars().collect();
        let mut pos = 0;

        eat_spaces(&chars, &mut pos);

        // Should skip all whitespace and land at 'h'
        assert_eq!(chars[pos], 'h');
        assert_eq!(pos, 7); // 3 spaces + 1 tab + 1 newline + 2 spaces
    }

    #[test]
    fn test_eat_spaces_only_whitespace() {
        let input = " \t\n  ";
        let chars: Vec<char> = input.chars().collect();
        let mut pos = 0;

        eat_spaces(&chars, &mut pos);

        // Should move pos to the end
        assert_eq!(pos, chars.len());
    }

    #[test]
    fn test_eat_spaces_no_whitespace() {
        let input = "hello";
        let chars: Vec<char> = input.chars().collect();
        let mut pos = 0;

        eat_spaces(&chars, &mut pos);

        // Should not change pos
        assert_eq!(pos, 0);
    }

    // compile_error
    #[test]
    fn test_compile_error_message_format() {
        let lines = ScriptLineProvider::with_active_state("test.sed", 42);
        let pos = 7;
        let msg = "unexpected token";

        let result: UResult<()> = compile_error(&lines, pos, msg);

        assert!(result.is_err());

        let err = result.unwrap_err();
        let msg = err.to_string();

        assert!(msg.contains("test.sed:42:7: error: unexpected token"));
    }

    #[test]
    fn test_compile_error_with_format_message() {
        let lines = ScriptLineProvider::with_active_state("input.txt", 3);
        let pos = 1;

        let result: UResult<()> = compile_error(&lines, pos, format!("invalid command '{}'", 'x'));

        assert!(result.is_err());

        let err = result.unwrap_err();
        let msg = err.to_string();

        assert_eq!(msg, "input.txt:3:1: error: invalid command 'x'");
    }

    // get_cmd_spec
    fn line_from_str(s: &str) -> Vec<char> {
        s.chars().collect()
    }

    #[test]
    fn test_missing_command_character() {
        let lines = ScriptLineProvider::with_active_state("test.sed", 1);
        let line = line_from_str("");
        let result = get_cmd_spec(&lines, &line, 0, 0);

        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("test.sed:1:0: error: command expected"));
    }

    #[test]
    fn test_invalid_command_character() {
        let lines = ScriptLineProvider::with_active_state("script.sed", 2);
        let line = line_from_str("@");
        let result = get_cmd_spec(&lines, &line, 0, 0);

        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("script.sed:2:0: error: invalid command code @"));
    }

    #[test]
    fn test_too_many_addresses() {
        let lines = ScriptLineProvider::with_active_state("input.sed", 3);
        let line = line_from_str("q"); // q takes one address
        let result = get_cmd_spec(&lines, &line, 0, 2);

        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("input.sed:3:0: error: command q expects up to 1 address(es), found 2")
        );
    }

    #[test]
    fn test_valid_command_spec() {
        let lines = ScriptLineProvider::with_active_state("input.sed", 4);
        let line = line_from_str("a"); // valid command
        let result = get_cmd_spec(&lines, &line, 0, 1);

        assert!(result.is_ok());
        let spec = result.unwrap();
        assert_eq!(spec.code, 'a');
    }
}
