// Compile the scripts into the internal representation of commands
//
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Diomidis Spinellis
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use crate::command::{Command, Context, ScriptValue};
use crate::script_line_provider::ScriptLineProvider;
use std::collections::HashMap;
use uucore::error::UResult;

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
struct SFormat {
    code: char,         // Command letter used by sed
    n_addr: usize,      // Number of supported addresses
    args: CommandArgs,  // Type of command arguments
}

// Build the command specification map (char -> SFormat)
fn build_command_map() -> HashMap<char, SFormat> {
    let formats = [
        SFormat { code: '{', n_addr: 2, args: CommandArgs::Group },
        SFormat { code: '}', n_addr: 0, args: CommandArgs::EndGroup },
        SFormat { code: 'a', n_addr: 1, args: CommandArgs::Text },
        SFormat { code: 'b', n_addr: 2, args: CommandArgs::Branch },
        SFormat { code: 'c', n_addr: 2, args: CommandArgs::Text },
        SFormat { code: 'd', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'D', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'g', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'G', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'h', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'H', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'i', n_addr: 1, args: CommandArgs::Text },
        SFormat { code: 'l', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'n', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'N', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'p', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'P', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'q', n_addr: 1, args: CommandArgs::Empty },
        SFormat { code: 'r', n_addr: 1, args: CommandArgs::ReadFile },
        SFormat { code: 's', n_addr: 2, args: CommandArgs::Substitute },
        SFormat { code: 't', n_addr: 2, args: CommandArgs::Branch },
        SFormat { code: 'w', n_addr: 2, args: CommandArgs::WriteFile },
        SFormat { code: 'x', n_addr: 2, args: CommandArgs::Empty },
        SFormat { code: 'y', n_addr: 2, args: CommandArgs::Translate },
        SFormat { code: '!', n_addr: 2, args: CommandArgs::NonSelect },
        SFormat { code: ':', n_addr: 0, args: CommandArgs::Label },
        SFormat { code: '#', n_addr: 0, args: CommandArgs::Comment },
        SFormat { code: '=', n_addr: 1, args: CommandArgs::Empty },
        SFormat { code: '\0', n_addr: 0, args: CommandArgs::Comment },
    ];

    formats.into_iter().map(|f| (f.code, f)).collect()
}

// Look up a command format by its command code.
fn lookup_command(cmd: char, map: &HashMap<char, SFormat>) -> Option<&SFormat> {
    map.get(&cmd)
}

pub fn compile(scripts: Vec<ScriptValue>, _context: &mut Context) -> UResult<Option<Command>> {
    let _cmd_map = build_command_map();
    let mut _line_provider = ScriptLineProvider::new(scripts);

    // TODO
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_empty_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('d', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Empty);
    }

    #[test]
    fn test_lookup_text_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('a', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 1);
        assert_eq!(cmd.args, CommandArgs::Text);
    }

    #[test]
    fn test_lookup_nonselect_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('!', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::NonSelect);
    }

    #[test]
    fn test_lookup_group_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('{', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Group);
    }

    #[test]
    fn test_lookup_endgroup_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('}', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 0);
        assert_eq!(cmd.args, CommandArgs::EndGroup);
    }

    #[test]
    fn test_lookup_comment_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('#', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 0);
        assert_eq!(cmd.args, CommandArgs::Comment);
    }

    #[test]
    fn test_lookup_branch_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('b', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Branch);
    }

    #[test]
    fn test_lookup_label_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command(':', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 0);
        assert_eq!(cmd.args, CommandArgs::Label);
    }

    #[test]
    fn test_lookup_readfile_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('r', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 1);
        assert_eq!(cmd.args, CommandArgs::ReadFile);
    }

    #[test]
    fn test_lookup_writefile_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('w', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::WriteFile);
    }

    #[test]
    fn test_lookup_substitute_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('s', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Substitute);
    }

    #[test]
    fn test_lookup_translate_command() {
        let cmd_map = build_command_map();
        let cmd = lookup_command('y', &cmd_map).unwrap();
        assert_eq!(cmd.n_addr, 2);
        assert_eq!(cmd.args, CommandArgs::Translate);
    }

    #[test]
    fn test_lookup_invalid_command() {
        let cmd_map = build_command_map();
        let result = lookup_command('Z', &cmd_map);
        assert!(result.is_none());
    }
}
