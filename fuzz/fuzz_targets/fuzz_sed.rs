// This file is part of the uutils sed package.
//
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.
#![no_main]
use libfuzzer_sys::fuzz_target;
use rand::prelude::*;
use std::ffi::OsString;
use uu_sed::uumain;
mod fuzz_common;
use crate::fuzz_common::{
    CommandResult, compare_result, generate_and_run_uumain, generate_random_string, run_gnu_cmd,
};
use rand::rng;

static CMD_PATH: &str = "sed";

fn generate_sed_args() -> Vec<String> {
    let mut rng = rng();
    let mut args = Vec::new();

    let opts = ["-n", "-E", "-i", "--posix"];
    for opt in &opts {
        if rng.random_bool(0.2) {
            args.push((*opt).to_string());
        }
    }

    // Choose sed script type: either inline (-e) or script commands
    let use_inline_script = rng.random_bool(0.9); // Mostly use inline scripts

    if use_inline_script {
        args.push("-e".to_string());
        args.push(generate_sed_script(&mut rng));
    } else {
        // For a script file approach, we would need to create a temporary file
        // but that's complex for fuzzing, so we'll stick with inline scripts
        args.push("-e".to_string());
        args.push(generate_sed_script(&mut rng));
    }

    args
}

fn generate_sed_script(rng: &mut ThreadRng) -> String {
    // Generate a random sed script
    // Most common sed operations: substitute, delete, print, append, insert
    let operations = ["s", "d", "p", "a\\", "i\\", "c\\", "=", "q", "l"];
    let operation = operations.choose(rng).unwrap();

    match *operation {
        "s" => {
            // Substitution: s/pattern/replacement/flags
            let pattern = generate_pattern(rng);
            let replacement = generate_replacement(rng);
            let flags = if rng.random_bool(0.3) {
                let flag_options = ["g", "i", "p"];
                (*flag_options.choose(rng).unwrap()).to_string()
            } else {
                String::new()
            };

            format!("s/{pattern}/{replacement}/{flags}")
        }
        "d" => {
            // Delete: [addr]d
            if rng.random_bool(0.3) {
                format!("{}d", generate_address(rng))
            } else {
                "d".to_string()
            }
        }
        "p" => {
            // Print: [addr]p
            if rng.random_bool(0.3) {
                format!("{}p", generate_address(rng))
            } else {
                "p".to_string()
            }
        }
        "a\\" => {
            // Append: [addr]a\text
            let text = generate_random_string(rng.random_range(1..10));
            if rng.random_bool(0.3) {
                format!("{}a\\{}", generate_address(rng), text)
            } else {
                format!("a\\{text}")
            }
        }
        "i\\" => {
            // Insert: [addr]i\text
            let text = generate_random_string(rng.random_range(1..10));
            if rng.random_bool(0.3) {
                format!("{}i\\{}", generate_address(rng), text)
            } else {
                format!("i\\{text}")
            }
        }
        "c\\" => {
            // Change: [addr]c\text
            let text = generate_random_string(rng.random_range(1..10));
            if rng.random_bool(0.3) {
                format!("{}c\\{}", generate_address(rng), text)
            } else {
                format!("c\\{text}")
            }
        }
        "=" => {
            // Print line number: [addr]=
            if rng.random_bool(0.3) {
                format!("{}=", generate_address(rng))
            } else {
                "=".to_string()
            }
        }
        "q" => {
            // Quit: [addr]q
            if rng.random_bool(0.3) {
                format!("{}q", generate_address(rng))
            } else {
                "q".to_string()
            }
        }
        "l" => {
            // List non-printable characters: [addr]l
            if rng.random_bool(0.3) {
                format!("{}l", generate_address(rng))
            } else {
                "l".to_string()
            }
        }
        _ => "s/./X/".to_string(), // Fallback
    }
}

fn generate_address(rng: &mut ThreadRng) -> String {
    // Generate 0, 1, or 2 addresses for sed commands
    let addr_count_options = [0, 1, 2];
    let addr_count = *addr_count_options.choose(rng).unwrap();

    match addr_count {
        0 => {
            // No address - command applies to all lines
            String::new()
        }
        1 => {
            // Single address: line number or regex
            let addr_types = ["line_num", "regex"];
            let addr_type = addr_types.choose(rng).unwrap();

            match *addr_type {
                "line_num" => {
                    // Line number
                    rng.random_range(1..100).to_string()
                }
                "regex" => {
                    // Regex pattern
                    format!("/{}/", generate_pattern(rng))
                }
                _ => String::new(),
            }
        }
        2 => {
            // Two addresses: range
            if rng.random_bool(0.5) {
                // Number range
                let start = rng.random_range(1..50);
                let end = rng.random_range(start..100);
                format!("{start},{end}")
            } else {
                // Mixed range: can be number,number or regex,regex or number,regex or regex,number
                let start = if rng.random_bool(0.5) {
                    rng.random_range(1..50).to_string()
                } else {
                    format!("/{}/", generate_pattern(rng))
                };

                let end = if rng.random_bool(0.5) {
                    rng.random_range(1..100).to_string()
                } else {
                    format!("/{}/", generate_pattern(rng))
                };

                format!("{start},{end}")
            }
        }
        _ => unreachable!(),
    }
}

fn generate_pattern(rng: &mut ThreadRng) -> String {
    // Generate a simple regex pattern
    // Keeping it simple to avoid invalid regex issues
    let simple_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    let pattern_length = rng.random_range(1..5);
    let pattern: String = (0..pattern_length)
        .map(|_| {
            let idx = rng.random_range(0..simple_chars.len());
            simple_chars.chars().nth(idx).unwrap()
        })
        .collect();

    // Sometimes add regex metacharacters
    if rng.random_bool(0.3) {
        let meta_chars = [".", "*", "+", "?", "^", "$", "[a-z]", "\\w", "\\d"];
        let meta = meta_chars.choose(rng).unwrap();
        if rng.random_bool(0.5) {
            format!("{pattern}{meta}")
        } else {
            format!("{meta}{pattern}")
        }
    } else {
        pattern
    }
}

fn generate_replacement(rng: &mut ThreadRng) -> String {
    // Generate a replacement string
    // Can be simple text, with or without backreferences
    let replacement_length = rng.random_range(0..10);
    let replacement: String = (0..replacement_length)
        .map(|_| rng.random_range(b'a'..=b'z') as char)
        .collect();

    // Sometimes add backreferences
    if rng.random_bool(0.2) {
        let backrefs = ["\\0", "\\1", "\\2", "&"];
        let backref = backrefs.choose(rng).unwrap();
        if rng.random_bool(0.5) {
            format!("{replacement}{backref}")
        } else {
            format!("{backref}{replacement}")
        }
    } else {
        replacement
    }
}

fuzz_target!(|_data: &[u8]| {
    let sed_args = generate_sed_args();
    let mut args = vec![OsString::from("sed")];
    args.extend(sed_args.iter().map(OsString::from));

    // Generate random input text
    let input_text = generate_random_string(200);

    // Run uutils implementation
    let rust_result = generate_and_run_uumain(&args, uumain, Some(&input_text));

    // Run GNU implementation
    let gnu_result = match run_gnu_cmd(CMD_PATH, &args[1..], false, Some(&input_text)) {
        Ok(result) => result,
        Err(error_result) => {
            eprintln!("Failed to run GNU command:");
            eprintln!("Stderr: {}", error_result.stderr);
            eprintln!("Exit Code: {}", error_result.exit_code);
            CommandResult {
                stdout: String::new(),
                stderr: error_result.stderr,
                exit_code: error_result.exit_code,
            }
        }
    };

    // Compare results
    compare_result(
        "sed",
        &format!("{:?}", &args[1..]),
        Some(&input_text),
        &rust_result,
        &gnu_result,
        false,
    );
});
