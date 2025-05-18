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
    ReplacementTemplate, ScriptValue, Substitution, Transliteration,
};
use crate::delimited_parser::{
    compilation_error, parse_char_escape, parse_regex, parse_transliteration,
};
use crate::named_writer::NamedWriter;
use crate::script_char_provider::ScriptCharProvider;
use crate::script_line_provider::ScriptLineProvider;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::LazyLock;
use uucore::error::{UResult, USimpleError};

// A global, immutable map of command properties, initialized on first access
static CMD_MAP: LazyLock<HashMap<char, CommandSpec>> = LazyLock::new(build_command_map);

// Types of command arguments recognized by the parser
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CommandArgs {
    Empty,         // d D g G h H l n N p P q x = \0
    Text,          // a c i
    NonSelect,     // !
    BeginGroup,    // {
    EndGroup,      // }
    Label,         // b t :
    ReadFile,      // r
    WriteFile,     // w
    Substitute,    // s
    Transliterate, // y
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
            args: CommandArgs::BeginGroup,
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
            args: CommandArgs::Label,
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
            args: CommandArgs::Label,
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
            args: CommandArgs::Transliterate,
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
            code: '=',
            n_addr: 1,
            args: CommandArgs::Empty,
        },
    ];

    formats.into_iter().map(|f| (f.code, f)).collect()
}

/// Compile the scripts into an executable data structure.
pub fn compile(
    scripts: Vec<ScriptValue>,
    context: &mut ProcessingContext,
) -> UResult<Option<Rc<RefCell<Command>>>> {
    let mut make_providers = ScriptLineProvider::new(scripts);

    let mut empty_line = ScriptCharProvider::new("");
    let result = compile_sequence(&mut make_providers, &mut empty_line, context)?;

    // Link the ends of command blocks to their following commands.
    if context.parsed_block_nesting > 0 {
        return Err(USimpleError::new(1, "unmatched `{'"));
    }
    patch_block_endings(result.clone());

    // Link branch commands to the target label commands.
    populate_label_map(result.clone(), context)?;
    resolve_branch_targets(result.clone(), context)?;

    // Comment-out the following to show the compiled script.
    #[cfg(any())]
    dbg!(&result);

    // TODO: setup append & match structures
    Ok(result)
}

/// For every Command in the top-level `head` chain, look for
/// `CommandData::Block(Some(sub_head))`.  Recursively patch
/// the sub-chain, then splice its tail back to the original
/// “next” pointer of the *parent* (falling back to its own
/// parent_next if its own next was `None`).
fn patch_block_endings(head: Option<Rc<RefCell<Command>>>) {
    fn patch_block_endings_to_parent(
        mut cur: Option<Rc<RefCell<Command>>>,
        parent_next: Option<Rc<RefCell<Command>>>,
    ) {
        while let Some(rc_cmd) = cur {
            // Borrow mutably just long enough to inspect/rewire this node
            let cmd = rc_cmd.borrow_mut();
            // Save this node’s own next pointer
            let own_next = cmd.next.clone();
            // Decide what “splice target” to use:
            //   - if this node has its own_next, use that
            //   - otherwise, fall back to parent_next
            let splice_target = own_next.clone().or(parent_next.clone());

            // If it has a sub-block, recurse and then patch its tail
            if let CommandData::Block(Some(ref sub_head)) = cmd.data {
                // 1) recurse into the sub-chain, passing along splice_target
                patch_block_endings_to_parent(Some(sub_head.clone()), splice_target.clone());

                // 2) find the tail of that sub-chain
                let mut tail = sub_head.clone();
                loop {
                    let next_in_sub = tail.borrow().next.clone();
                    match next_in_sub {
                        Some(n) => tail = n,
                        None => break,
                    }
                }

                // 3) splice the tail’s `.next` to splice_target
                tail.borrow_mut().next = splice_target.clone();
            }

            // drop the borrow before moving on
            drop(cmd);

            // advance to the next sibling in this level
            cur = own_next;
        }
    }

    // top-level has no parent, so pass None
    patch_block_endings_to_parent(head, None);
}

/// Populate the context's label map with references to associated commands.
fn populate_label_map(
    mut cur: Option<Rc<RefCell<Command>>>,
    context: &mut ProcessingContext,
) -> UResult<()> {
    while let Some(rc_cmd) = cur {
        // Borrow mutably just long enough to inspect/rewire this node
        let cmd = rc_cmd.borrow_mut();

        // Extract any label to insert after borrow ends
        let maybe_label = match &cmd.data {
            CommandData::Block(Some(sub_head)) => {
                populate_label_map(Some(sub_head.clone()), context)?;
                None
            }
            CommandData::Label(Some(label)) => Some(label.clone()),
            _ => None,
        };

        if let Some(label) = maybe_label {
            if cmd.code == ':' {
                if context.label_to_command_map.contains_key(&label) {
                    return Err(USimpleError::new(2, format!("duplicate label `{}'", label)));
                }
                context.label_to_command_map.insert(label, rc_cmd.clone());
            }
        }

        cur = cmd.next.clone();
    }
    Ok(())
}

/// Replace branch labels with references to the corresponding commands.
/// Raise an error on undefined labels.
fn resolve_branch_targets(
    mut cur: Option<Rc<RefCell<Command>>>,
    context: &mut ProcessingContext,
) -> UResult<()> {
    while let Some(rc_cmd) = cur {
        // Borrow mutably just long enough to inspect/rewire this node
        let mut cmd = rc_cmd.borrow_mut();

        // Recurse into blocks
        if let CommandData::Block(Some(sub_head)) = &cmd.data {
            resolve_branch_targets(Some(sub_head.clone()), context)?;
        }

        // Only for 't' or 'b' commands:
        if matches!(cmd.code, 't' | 'b') {
            // Take ownership of the current data
            let old_data = mem::replace(&mut cmd.data, CommandData::None);

            // Build the replacement
            let new_data = match old_data {
                CommandData::Label(Some(label)) => {
                    let target = context
                        .label_to_command_map
                        .get(&label)
                        .cloned()
                        .ok_or_else(|| {
                            USimpleError::new(2, format!("undefined label `{}'", label))
                        })?;
                    CommandData::BranchTarget(Some(target))
                }
                CommandData::Label(None) => CommandData::BranchTarget(None),
                other => other, // put back anything else unchanged
            };

            // Store it back
            cmd.data = new_data;
        }

        // Advance to the next sibling
        cur = cmd.next.clone();
    }
    Ok(())
}

/// Compile provided scripts into a sequence of commands.
fn compile_sequence(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
    context: &mut ProcessingContext,
) -> UResult<Option<Rc<RefCell<Command>>>> {
    let mut head: Option<Rc<RefCell<Command>>> = None;
    let mut tail: Option<Rc<RefCell<Command>>> = None;

    loop {
        line.eat_spaces();
        if line.eol() || line.current() == '#' {
            // TODO: set context.quiet for StringVal starting with #n
            match lines.next_line()? {
                None => {
                    return Ok(head);
                }
                Some(line_string) => {
                    *line = ScriptCharProvider::new(&line_string);
                }
            }
            continue;
        } else if line.current() == ';' {
            line.advance();
            continue;
        }

        let mut cmd = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(lines, line, &mut cmd, context)?;
        line.eat_spaces();
        let mut cmd_spec = get_cmd_spec(lines, line, n_addr)?;

        // The ! command shall be followed by another one
        match cmd_spec.args {
            CommandArgs::NonSelect => {
                line.advance();
                line.eat_spaces();
                cmd.borrow_mut().non_select = true;
                cmd_spec = get_cmd_spec(lines, line, n_addr)?;
            }
            CommandArgs::EndGroup => {
                if context.parsed_block_nesting == 0 {
                    return compilation_error(lines, line, "unexpected `}'");
                }
                context.parsed_block_nesting -= 1;
                line.advance();
                line.eat_spaces();
                let mut cmd_ref = cmd.borrow_mut();
                parse_command_ending(lines, line, &mut cmd_ref)?;
                return Ok(head);
            }
            _ => (),
        }

        compile_command(lines, line, &mut cmd, cmd_spec, context)?;
        if let Some(ref t) = tail {
            // there's already a tail: link it
            t.borrow_mut().next = Some(cmd.clone());
        } else {
            // first element: set head
            head = Some(cmd.clone());
        }
        tail = Some(cmd);
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
    context: &ProcessingContext,
) -> UResult<usize> {
    let mut n_addr = 0;
    let mut cmd = cmd.borrow_mut();

    line.eat_spaces();
    if !line.eol() && is_address_char(line.current()) {
        if let Ok(addr1) = compile_address(lines, line, context) {
            cmd.addr1 = Some(addr1);
            n_addr += 1;
        }
    }

    line.eat_spaces();
    if n_addr == 1 && !line.eol() && line.current() == ',' {
        line.advance();
        line.eat_spaces();
        if !line.eol() {
            if let Ok(addr2) = compile_address(lines, line, context) {
                cmd.addr2 = Some(addr2);
                n_addr += 1;
            }
        }
    }

    Ok(n_addr)
}

/// Compile and return a single range address specification.
fn compile_address(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    context: &ProcessingContext,
) -> UResult<Address> {
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
                value: AddressValue::Regex(compile_regex(lines, line, &re, context, icase)?),
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

/// Parse the end of a command, failing with an error on extra characters.
fn parse_command_ending(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
) -> UResult<()> {
    if !line.eol() && line.current() == ';' {
        line.advance();
        return Ok(());
    }

    if !line.eol() {
        return compilation_error(
            lines,
            line,
            format!("extra characters at the end of the {} command", cmd.code),
        );
    }

    Ok(())
}

/// Convert a primitive BRE pattern to a safe ERE-compatible pattern string.
/// - Replacces `\(` and `\)` with `(` and `)`
/// - Escapes ERE-only metacharacters: `+ ? { } | ( )`
/// - Leaves all other characters as-is
fn bre_to_ere(pattern: &str) -> String {
    let mut result = String::with_capacity(pattern.len());
    let mut chars = pattern.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.peek() {
                Some('(') => {
                    chars.next();
                    result.push('('); // group start
                }
                Some(')') => {
                    chars.next();
                    result.push(')'); // group end
                }
                Some(&next) => {
                    chars.next();
                    result.push('\\');
                    result.push(next); // preserve other escaped characters
                }
                None => {
                    result.push('\\'); // trailing backslash, keep it
                }
            }
        } else {
            match c {
                '+' | '?' | '{' | '}' | '|' | '(' | ')' => {
                    result.push('\\'); // escape unsupported ERE metacharacters
                    result.push(c);
                }
                _ => result.push(c),
            }
        }
    }

    result
}

/// Compile the provided regular expression string into a corresponding engine.
fn compile_regex(
    lines: &ScriptLineProvider,
    line: &ScriptCharProvider,
    pattern: &str,
    context: &ProcessingContext,
    icase: bool,
) -> UResult<Regex> {
    if pattern.is_empty() {
        let maybe_existing = context.saved_regex.borrow();
        if let Some(existing) = &*maybe_existing {
            Ok(existing.clone())
        } else {
            compilation_error(lines, line, "no previously compiled regex available")
        }
    } else {
        // Convert basic to extended regular expression if needed.
        let ere_pattern = if context.regex_extended {
            pattern
        } else {
            &bre_to_ere(pattern)
        };

        // Add case-insensitive modifier if needed.
        let full_pattern = if icase {
            if ere_pattern.is_empty() {
                return compilation_error(lines, line, "cannot specify a modifier on an empty RE");
            }
            format!("(?i){}", ere_pattern)
        } else {
            ere_pattern.to_string()
        };

        // Compile into engine.
        let compiled = Regex::new(&full_pattern).map_err(|e| {
            compilation_error::<Regex>(lines, line, format!("invalid regex '{}': {}", pattern, e))
                .unwrap_err()
        })?;

        *context.saved_regex.borrow_mut() = Some(compiled.clone());

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

                    // Line input_action
                    if line.eol() {
                        if let Some(next_line_string) = lines.next_line()? {
                            literal.push('\n');
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

fn compile_subst_command(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
    context: &ProcessingContext,
) -> UResult<()> {
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
        line_number: lines.get_line_number(),
        ..Default::default()
    });

    subst.replacement = compile_replacement(lines, line)?;
    compile_subst_flags(lines, line, &mut subst)?;

    // Compile regex with now known ignore_case flag.
    subst.regex = compile_regex(lines, line, &pattern, context, subst.ignore_case)?;

    let re_captures: u32 = subst
        .regex
        .captures_len()
        .saturating_sub(1)
        .try_into()
        .unwrap();
    let max_group_number = subst.replacement.max_group_number();
    if max_group_number > re_captures {
        return compilation_error(
            lines,
            line,
            format!(
                "group number \\{} is larger than the {} available RE groups",
                max_group_number, re_captures
            ),
        );
    }

    cmd.data = CommandData::Substitution(subst);

    parse_command_ending(lines, line, cmd)
}

fn compile_trans_command(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
) -> UResult<()> {
    line.advance(); // move past 'y'

    let delimiter = line.current();
    if delimiter == '\0' || delimiter == '\\' {
        return compilation_error(
            lines,
            line,
            "transliteration string cannot be delimited by newline or backslash",
        );
    }

    let source = parse_transliteration(lines, line)?;
    let target = parse_transliteration(lines, line)?;
    if source.chars().count() != target.chars().count() {
        return compilation_error(
            lines,
            line,
            "transliteration strings are not the same length",
        );
    }

    let transliteration = Box::new(Transliteration::from_strings(&source, &target));
    cmd.data = CommandData::Transliteration(transliteration);

    line.advance(); // move past last delimiter
    parse_command_ending(lines, line, cmd)
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

    loop {
        line.eat_spaces();
        if line.eol() {
            break;
        }

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
fn compile_empty_command(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
) -> UResult<()> {
    line.advance(); // Skip the command character
    line.eat_spaces(); // Skip any trailing whitespace

    parse_command_ending(lines, line, cmd)
}

fn compile_label_command(
    lines: &ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Command,
) -> UResult<()> {
    /// Return true if `c` is in the POSIX portable filename character set.
    fn is_portable_filename_char(c: char) -> bool {
        c.is_ascii_alphanumeric()  // A–Z, a–z, 0–9
        || matches!(c, '.' | '_' | '-')
    }

    line.advance(); // Skip the command character
    line.eat_spaces(); // Skip any leading whitespace

    let mut label = String::new();
    while !line.eol() && is_portable_filename_char(line.current()) {
        label.push(line.current());
        line.advance();
    }

    if label.is_empty() {
        if cmd.code == ':' {
            return compilation_error(lines, line, "empty label");
        }
        cmd.data = CommandData::Label(None);
    } else {
        cmd.data = CommandData::Label(Some(label));
    }

    line.eat_spaces(); // Skip any trailing whitespace
    parse_command_ending(lines, line, cmd)
}

// Compile the specified command
fn compile_command(
    lines: &mut ScriptLineProvider,
    line: &mut ScriptCharProvider,
    cmd: &mut Rc<RefCell<Command>>,
    cmd_spec: &'static CommandSpec,
    context: &mut ProcessingContext,
) -> UResult<()> {
    let mut cmd = cmd.borrow_mut();
    cmd.code = line.current();

    match cmd_spec.args {
        CommandArgs::BeginGroup => {
            // {
            line.advance(); // move past '{'
            context.parsed_block_nesting += 1;
            let block_body = compile_sequence(lines, line, context)?;
            cmd.data = CommandData::Block(block_body);
        }
        CommandArgs::EndGroup => { // }
            // Implemented at a higher level.
        }
        CommandArgs::Empty => {
            // d D g G h H l n N p P q x =
            return compile_empty_command(lines, line, &mut cmd);
        }
        CommandArgs::Label => {
            // b t :
            compile_label_command(lines, line, &mut cmd)?;
        }
        CommandArgs::NonSelect => { // !
            // Implemented at a higher level.
        }
        CommandArgs::Substitute => {
            // s
            return compile_subst_command(lines, line, &mut cmd, context);
        }
        CommandArgs::Transliterate => {
            // y
            return compile_trans_command(lines, line, &mut cmd);
        }
        // TODO
        CommandArgs::Text => { // a c i
        }
        CommandArgs::ReadFile => { // r
        }
        CommandArgs::WriteFile => { // w
        }
    }

    Ok(())
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
        return compilation_error(lines, line, format!("invalid command code `{}'", ch));
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

    /// Return a default ProcessingContext for use in tests.
    pub fn ctx() -> ProcessingContext {
        ProcessingContext::default()
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
        assert_eq!(cmd.args, CommandArgs::BeginGroup);
    }

    #[test]
    fn test_lookup_endgroup_command() {
        let cmd = lookup_command('}').unwrap();
        assert_eq!(cmd.n_addr, 0);
        assert_eq!(cmd.args, CommandArgs::EndGroup);
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
        assert_eq!(cmd.args, CommandArgs::Transliterate);
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
        assert!(msg.contains("script.sed:2:0: error: invalid command code `@'"));
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
        let regex = compile_regex(&lines, &chars, "abc", &ctx(), false).unwrap();
        assert!(regex.is_match("abc"));
        assert!(!regex.is_match("ABC"));
    }

    #[test]
    fn test_compile_re_case_insensitive() {
        let (lines, chars) = dummy_providers();
        let regex = compile_regex(&lines, &chars, "abc", &ctx(), true).unwrap();
        assert!(regex.is_match("abc"));
        assert!(regex.is_match("ABC"));
        assert!(regex.is_match("AbC"));
    }

    #[test]
    fn test_compile_re_saved_and_reuse() {
        let context = ctx();
        // Save a regex
        let (lines1, chars1) = dummy_providers();
        let _ = compile_regex(&lines1, &chars1, "abc", &context, false).unwrap();

        // Now try to reuse it
        let (lines2, chars2) = dummy_providers();
        let reused = compile_regex(&lines2, &chars2, "", &context, false).unwrap();

        assert!(reused.is_match("abc"));
    }

    #[test]
    fn test_compile_re_empty_and_not_saved() {
        let (lines, chars) = dummy_providers();
        let result = compile_regex(&lines, &chars, "", &ctx(), false);
        assert!(result.is_err()); // Should fail because nothing was saved
    }

    #[test]
    fn test_compile_re_invalid() {
        let (lines, chars) = dummy_providers();
        let result = compile_regex(&lines, &chars, "a[d", &ctx(), false);
        assert!(result.is_err()); // Should fail due to open bracketed expression
    }

    // compile_address
    #[test]
    fn test_compile_addr_line_number() {
        let (lines, mut chars) = make_providers("42");
        let addr = compile_address(&lines, &mut chars, &ctx()).unwrap();
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
        let addr = compile_address(&lines, &mut chars, &ctx()).unwrap();
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
        let addr = compile_address(&lines, &mut chars, &ctx()).unwrap();
        assert!(matches!(addr.atype, AddressType::Last));
    }

    #[test]
    fn test_compile_addr_regex() {
        let (lines, mut chars) = make_providers("/hello/");
        let addr = compile_address(&lines, &mut chars, &ctx()).unwrap();
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
        let addr = compile_address(&lines, &mut chars, &ctx()).unwrap();
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
        let addr = compile_address(&lines, &mut chars, &ctx()).unwrap();
        assert!(matches!(addr.atype, AddressType::Re));
        if let AddressValue::Regex(re) = addr.value {
            assert!(re.is_match("HELLO")); // case-insensitive
        } else {
            panic!("expected Regex address value");
        }
    }

    #[test]
    fn test_compile_addr_empty_regex_saved() {
        let context = ctx();
        // First save a regex
        let (lines1, mut chars1) = make_providers("/saved/");
        let _ = compile_address(&lines1, &mut chars1, &context).unwrap();

        // Then reuse it with empty regex
        let (lines2, mut chars2) = make_providers("//");
        let addr = compile_address(&lines2, &mut chars2, &context).unwrap();
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
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd, &ctx()).unwrap();

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
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd, &ctx()).unwrap();

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
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd, &ctx()).unwrap();

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
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd, &ctx()).unwrap();

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
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd, &ctx()).unwrap();

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
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd, &ctx()).unwrap();

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
        let n_addr = compile_address_range(&lines, &mut chars, &mut cmd, &ctx()).unwrap();

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
        let context = ctx();
        // First save a regex
        let (lines1, mut chars1) = make_providers("/abc/");
        let mut cmd1 = Rc::new(RefCell::new(Command::default()));
        compile_address_range(&lines1, &mut chars1, &mut cmd1, &context).unwrap();

        // Now reuse it
        let (lines2, mut chars2) = make_providers("//");
        let mut cmd2 = Rc::new(RefCell::new(Command::default()));
        let n_addr = compile_address_range(&lines2, &mut chars2, &mut cmd2, &context).unwrap();

        assert_eq!(n_addr, 1);
        assert!(matches!(
            cmd2.borrow().addr1.as_ref().unwrap().atype,
            AddressType::Re
        ));
        if let AddressValue::Regex(re) = &cmd2.borrow().addr1.as_ref().unwrap().value {
            assert!(re.is_match("abc"));
        };
    }

    // compile_sequence
    fn make_provider(lines: &[&str]) -> ScriptLineProvider {
        let input = lines
            .iter()
            .map(|s| ScriptValue::StringVal(s.to_string()))
            .collect();
        ScriptLineProvider::new(input)
    }

    fn empty_line() -> ScriptCharProvider {
        ScriptCharProvider::new("")
    }

    #[test]
    fn test_compile_sequence_empty_input() {
        let mut provider = make_provider(&[]);
        let mut opts = ctx();

        let result = compile_sequence(&mut provider, &mut empty_line(), &mut opts).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_compile_sequence_comment_only() {
        let mut provider = make_provider(&["# comment", "   ", ";;"]);
        let mut opts = ctx();

        let result = compile_sequence(&mut provider, &mut empty_line(), &mut opts).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_compile_sequence_single_command() {
        let mut provider = make_provider(&["42q"]);
        let mut opts = ctx();

        let result = compile_sequence(&mut provider, &mut empty_line(), &mut opts).unwrap();
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
    fn test_compile_sequence_non_selected_single_command() {
        let mut provider = make_provider(&["42!p"]);
        let mut opts = ctx();

        let result = compile_sequence(&mut provider, &mut empty_line(), &mut opts).unwrap();
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
    fn test_compile_sequence_multiple_lines() {
        let mut provider = make_provider(&["1q", "2d"]);
        let mut opts = ctx();

        let result = compile_sequence(&mut provider, &mut empty_line(), &mut opts).unwrap();
        let binding = result.unwrap();
        let first = binding.borrow();

        assert_eq!(first.code, 'q');
        let binding = first.next.clone().unwrap();
        let second = binding.borrow();
        assert_eq!(second.code, 'd');
        assert!(second.next.is_none());
    }

    #[test]
    fn test_compile_sequence_single_line_multiple_commands() {
        let mut provider = make_provider(&["1q;2d"]);
        let mut opts = ctx();

        let result = compile_sequence(&mut provider, &mut empty_line(), &mut opts).unwrap();
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
            ReplacementPart::Literal(s) if s == "first line\n continued"
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
        assert!(
            err.to_string()
                .contains("multiple 'g' or numeric flags in substitute command")
        );
    }

    #[test]
    fn test_compile_subst_flag_number_and_g_should_fail() {
        let (lines, mut chars) = make_providers("2g");
        let mut subst = Substitution::default();

        let err = compile_subst_flags(&lines, &mut chars, &mut subst).unwrap_err();
        assert!(
            err.to_string()
                .contains("multiple 'g' or numeric flags in substitute command")
        );
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

        let err = compile_subst_command(&mut lines, &mut chars, &mut cmd, &ctx()).unwrap_err();
        assert!(
            err.to_string()
                .contains("substitute pattern cannot be delimited")
        );
    }

    #[test]
    fn test_compile_subst_empty_pattern() {
        let (mut lines, mut chars) = make_providers("s//bar/");
        let mut cmd = Command::default();

        let err = compile_subst_command(&mut lines, &mut chars, &mut cmd, &ctx()).unwrap_err();
        assert!(err.to_string().contains("unterminated substitute pattern"));
    }

    #[test]
    fn test_compile_subst_extra_characters_at_end() {
        let (mut lines, mut chars) = make_providers("s/foo/bar/x");
        let mut cmd = Command::default();

        let err = compile_subst_command(&mut lines, &mut chars, &mut cmd, &ctx()).unwrap_err();
        assert!(err.to_string().contains("invalid substitute flag"));
    }

    #[test]
    fn test_compile_subst_semicolon_indicates_continue() {
        let (mut lines, mut chars) = make_providers("s/foo/bar/;");
        let mut cmd = Command::default();

        compile_subst_command(&mut lines, &mut chars, &mut cmd, &ctx()).unwrap();

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

        compile_subst_command(&mut lines, &mut chars, &mut cmd, &ctx()).unwrap();
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

    #[test]
    fn test_compile_subst_invalid_group_number() {
        let (mut lines, mut chars) = make_providers(r"s/\(.\)\(.\)/\3\2\1/");
        let mut cmd = Command::default();

        let err = compile_subst_command(&mut lines, &mut chars, &mut cmd, &ctx()).unwrap_err();
        assert!(
            err.to_string()
                .contains("group number \\3 is larger than the 2 available RE groups")
        );
    }

    // bre_to_ere
    #[test]
    fn test_bre_group_translation() {
        assert_eq!(bre_to_ere(r"\(abc\)"), "(abc)");
        assert_eq!(bre_to_ere(r"a\(b\)c"), "a(b)c");
    }

    #[test]
    fn test_ere_metacharacters_escaped() {
        assert_eq!(bre_to_ere(r"a+b?c{1}|(d)"), r"a\+b\?c\{1\}\|\(d\)");
    }

    #[test]
    fn test_literal_backslashes_preserved() {
        assert_eq!(bre_to_ere(r"foo\\bar"), r"foo\\bar");
        assert_eq!(bre_to_ere(r"\."), r"\.");
    }

    #[test]
    fn test_character_classes_unchanged() {
        assert_eq!(bre_to_ere(r"[a-z]"), "[a-z]");
        assert_eq!(bre_to_ere(r"[^0-9]"), "[^0-9]");
    }

    #[test]
    fn test_anchors_and_dot_and_star() {
        assert_eq!(bre_to_ere(r"^a.*b$"), "^a.*b$");
    }

    #[test]
    fn test_trailing_backslash_is_preserved() {
        assert_eq!(bre_to_ere(r"abc\"), r"abc\");
    }

    // patch_block_endings

    // Create a command with the specified code.
    fn command_with_code(code: char) -> Rc<RefCell<Command>> {
        Rc::new(RefCell::new(Command {
            code,
            ..Default::default()
        }))
    }

    // Link the vector of passed commands into a list, returning head.
    fn link_commands(cmds: Vec<Rc<RefCell<Command>>>) -> Option<Rc<RefCell<Command>>> {
        for i in 0..cmds.len().saturating_sub(1) {
            cmds[i].borrow_mut().next = Some(cmds[i + 1].clone());
        }
        cmds.first().cloned()
    }

    // Return the command codes along the passed linked list.
    fn collect_codes(mut head: Option<Rc<RefCell<Command>>>) -> Vec<char> {
        let mut result = Vec::new();
        while let Some(cmd) = head {
            let cmd_ref = cmd.borrow();
            result.push(cmd_ref.code);
            head = cmd_ref.next.clone();
        }
        result
    }

    #[test]
    fn test_flat_chain() {
        let a = command_with_code('a');
        let b = command_with_code('b');
        let head = link_commands(vec![a.clone(), b.clone()]);

        patch_block_endings(head.clone());

        assert_eq!(collect_codes(head), vec!['a', 'b']);
    }

    #[test]
    fn test_simple_block_relinks_tail() {
        // a ; { x ; y ; } b
        let a = command_with_code('a');
        let block = command_with_code('{');
        let x = command_with_code('x');
        let y = command_with_code('y');
        let b = command_with_code('b');

        let head = link_commands(vec![a.clone(), block.clone(), b.clone()]);
        let sub_head = link_commands(vec![x.clone(), y.clone()]);
        block.borrow_mut().data = CommandData::Block(sub_head.clone());

        patch_block_endings(head.clone());

        // Expect x -> y -> b
        assert_eq!(collect_codes(sub_head), vec!['x', 'y', 'b']);
        // Expect a -> { -> b still valid
        assert_eq!(collect_codes(Some(a)), vec!['a', '{', 'b']);
    }

    #[test]
    fn test_empty_block_no_panic() {
        let a = command_with_code('a');
        a.borrow_mut().data = CommandData::Block(None);

        patch_block_endings(Some(a.clone()));

        assert_eq!(collect_codes(Some(a)), vec!['a']);
    }

    #[test]
    fn test_nested_blocks() {
        // a
        // {
        //   m
        //   {
        //     x
        //     y
        //   }
        //   n
        // }
        // b
        let a = command_with_code('a');
        let b = command_with_code('b');
        let x = command_with_code('x');
        let y = command_with_code('y');
        let m = command_with_code('m');
        let n = command_with_code('n');
        let outer_block = command_with_code('{');
        let inner_block = command_with_code('{');

        let head = link_commands(vec![a.clone(), outer_block.clone(), b.clone()]);
        let outer = link_commands(vec![m.clone(), inner_block.clone(), n.clone()]);
        let inner = link_commands(vec![x.clone(), y.clone()]);
        outer_block.borrow_mut().data = CommandData::Block(outer.clone());
        inner_block.borrow_mut().data = CommandData::Block(inner.clone());

        patch_block_endings(head.clone());

        assert_eq!(collect_codes(head), vec!['a', '{', 'b']);
        assert_eq!(collect_codes(inner), vec!['x', 'y', 'n', 'b']);
        assert_eq!(collect_codes(outer), vec!['m', '{', 'n', 'b']);
    }

    #[test]
    fn test_empty_nested_blocks() {
        // a
        // {
        //   {
        //     x
        //   }
        // }
        // b
        let a = command_with_code('a');
        let b = command_with_code('b');
        let x = command_with_code('x');
        let outer_block = command_with_code('{');
        let inner_block = command_with_code('{');

        let head = link_commands(vec![a.clone(), outer_block.clone(), b.clone()]);
        let outer = link_commands(vec![inner_block.clone()]);
        let inner = link_commands(vec![x.clone()]);
        outer_block.borrow_mut().data = CommandData::Block(outer.clone());
        inner_block.borrow_mut().data = CommandData::Block(inner.clone());

        patch_block_endings(head.clone());

        assert_eq!(collect_codes(head), vec!['a', '{', 'b']);
        assert_eq!(collect_codes(outer), vec!['{', 'b']);
        assert_eq!(collect_codes(inner), vec!['x', 'b']);
    }

    // compile_label_command
    #[test]
    fn test_compile_label_command() {
        let (mut lines, mut chars) = make_providers(": foo");
        let mut cmd = Command::default();

        compile_label_command(&mut lines, &mut chars, &mut cmd).unwrap();
        match &cmd.data {
            CommandData::Label(label) => {
                let name = label.clone().unwrap();
                assert_eq!(name, "foo");
            }
            _ => panic!("Expected CommandData::Label"),
        }
    }

    #[test]
    fn test_compile_missing_label_command() {
        let (mut lines, mut chars) = make_providers(": ;");
        let mut cmd = Command::default();

        cmd.code = ':';
        let err = compile_label_command(&mut lines, &mut chars, &mut cmd).unwrap_err();
        assert!(err.to_string().contains("empty label"));
    }

    #[test]
    fn test_compile_empty_label_command() {
        let (mut lines, mut chars) = make_providers("b ;");
        let mut cmd = Command::default();

        cmd.code = 'b';
        compile_label_command(&mut lines, &mut chars, &mut cmd).unwrap();
        match &cmd.data {
            CommandData::Label(label) => {
                assert!(label.is_none());
            }
            _ => panic!("Expected CommandData::Label(None)"),
        }
    }

    // populate_label_map
    fn command_with_data(data: CommandData) -> Rc<RefCell<Command>> {
        Rc::new(RefCell::new(Command {
            data,
            ..Default::default()
        }))
    }

    #[test]
    fn test_single_label() {
        let cmd = command_with_data(CommandData::Label(Some("start".to_string())));
        cmd.borrow_mut().code = ':';
        let mut context = ProcessingContext::default();

        populate_label_map(Some(cmd.clone()), &mut context).unwrap();

        assert_eq!(context.label_to_command_map.len(), 1);
        assert!(context.label_to_command_map.contains_key("start"));
        assert!(Rc::ptr_eq(&context.label_to_command_map["start"], &cmd));
    }

    #[test]
    fn test_label_inside_block() {
        let nested = command_with_data(CommandData::Label(Some("inside".to_string())));
        nested.borrow_mut().code = ':';
        let block = command_with_data(CommandData::Block(Some(nested.clone())));
        let mut context = ProcessingContext::default();

        populate_label_map(Some(block.clone()), &mut context).unwrap();

        assert_eq!(context.label_to_command_map.len(), 1);
        assert!(context.label_to_command_map.contains_key("inside"));
        assert!(Rc::ptr_eq(&context.label_to_command_map["inside"], &nested));
    }

    #[test]
    fn test_multiple_labels() {
        let a = command_with_data(CommandData::Label(Some("a".to_string())));
        a.borrow_mut().code = ':';
        let b = command_with_data(CommandData::Label(Some("b".to_string())));
        b.borrow_mut().code = ':';
        let head = link_commands(vec![a.clone(), b.clone()]);

        let mut context = ProcessingContext::default();
        populate_label_map(head, &mut context).unwrap();

        assert_eq!(context.label_to_command_map.len(), 2);
        assert!(context.label_to_command_map.contains_key("a"));
        assert!(context.label_to_command_map.contains_key("b"));
    }

    #[test]
    fn test_no_labels() {
        let a = command_with_data(CommandData::None);
        let b = command_with_data(CommandData::None);
        let head = link_commands(vec![a.clone(), b.clone()]);

        let mut context = ProcessingContext::default();
        populate_label_map(head, &mut context).unwrap();

        assert_eq!(context.label_to_command_map.len(), 0);
    }

    #[test]
    fn test_label_none_is_ignored() {
        let cmd = command_with_data(CommandData::Label(None));
        let mut context = ProcessingContext::default();

        populate_label_map(Some(cmd.clone()), &mut context).unwrap();

        // The map should remain empty since the label is None
        assert_eq!(context.label_to_command_map.len(), 0);
    }

    #[test]
    fn test_duplicate_label_gives_error() {
        let a1 = command_with_data(CommandData::Label(Some("dup".to_string())));
        a1.borrow_mut().code = ':';

        let a2 = command_with_data(CommandData::Label(Some("dup".to_string())));
        a2.borrow_mut().code = ':';

        let head = link_commands(vec![a1.clone(), a2.clone()]);
        let mut context = ProcessingContext::default();

        let result = populate_label_map(head, &mut context);

        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("duplicate label `dup'"));
    }

    // resolve_branch_targets
    #[test]
    fn test_branch_target_resolved() {
        let target = command_with_data(CommandData::Label(Some("end".to_string())));
        target.borrow_mut().code = ':';

        let branch = command_with_data(CommandData::Label(Some("end".to_string())));
        branch.borrow_mut().code = 'b';

        let head = link_commands(vec![branch.clone(), target.clone()]);
        let mut context = ProcessingContext::default();

        populate_label_map(head.clone(), &mut context).unwrap();
        let result = resolve_branch_targets(head.clone(), &mut context);
        assert!(result.is_ok());

        match &branch.borrow().data {
            CommandData::BranchTarget(Some(ptr)) => {
                assert!(Rc::ptr_eq(ptr, &target));
            }
            _ => panic!("Expected BranchTarget(Some(...))"),
        }
    }

    #[test]
    fn test_branch_target_missing_label_gives_error() {
        let branch = command_with_data(CommandData::Label(Some("nope".to_string())));
        branch.borrow_mut().code = 't';

        let mut context = ProcessingContext::default();
        let result = resolve_branch_targets(Some(branch.clone()), &mut context);

        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("undefined label `nope'"));
    }

    #[test]
    fn test_branch_with_no_label_resolves_to_none() {
        let branch = command_with_data(CommandData::Label(None));
        branch.borrow_mut().code = 'b';

        let mut context = ProcessingContext::default();
        let result = resolve_branch_targets(Some(branch.clone()), &mut context);

        assert!(result.is_ok());
        match &branch.borrow().data {
            CommandData::BranchTarget(None) => {} // ok
            _ => panic!("Expected BranchTarget(None)"),
        }
    }

    #[test]
    fn test_non_branch_label_is_unchanged() {
        let cmd = command_with_data(CommandData::Label(Some("unchanged".to_string())));
        cmd.borrow_mut().code = 'q'; // not a branch command

        let mut context = ProcessingContext::default();
        let result = resolve_branch_targets(Some(cmd.clone()), &mut context);
        assert!(result.is_ok());

        match &cmd.borrow().data {
            CommandData::Label(Some(label)) => assert_eq!(label, "unchanged"),
            _ => panic!("Expected Label(Some(...)) to remain unchanged"),
        }
    }

    #[test]
    fn test_branch_in_nested_block() {
        let label = command_with_data(CommandData::Label(Some("inner".to_string())));
        label.borrow_mut().code = ':';

        let branch = command_with_data(CommandData::Label(Some("inner".to_string())));
        branch.borrow_mut().code = 't';

        let block = command_with_data(CommandData::Block(Some(label.clone())));
        let head = link_commands(vec![branch.clone(), block]);

        let mut context = ProcessingContext::default();
        populate_label_map(Some(label.clone()), &mut context).unwrap();
        let result = resolve_branch_targets(head.clone(), &mut context);

        assert!(result.is_ok());
        match &branch.borrow().data {
            CommandData::BranchTarget(Some(ptr)) => assert!(Rc::ptr_eq(ptr, &label)),
            _ => panic!("Expected BranchTarget(Some(...))"),
        }
    }
}
