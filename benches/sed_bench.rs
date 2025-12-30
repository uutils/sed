// Definitions for the compiled code data structures
//
// SPDX-License-Identifier: MIT
//
// This file is part of the uutils sed package.
// It is licensed under the MIT License.
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

use divan::{Bencher, black_box};
use sed::sed::uumain;
use uucore::benchmark::{create_test_file, run_util_function};

/// Benchmark no-op on short lines
#[divan::bench]
fn no_op_short(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..10_000_000 {
        data.extend_from_slice(format!("{i}\n").as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(uumain, &["", file_path_str]));
    });
}

/// Benchmark access log no-op
#[divan::bench]
fn access_log_no_op(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..5_000_000 {
        let line = format!(
            "192.168.{}.{} - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n",
            (i / 256) % 256,
            i % 256
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(uumain, &["", file_path_str]));
    });
}

/// Benchmark access log no substitution
#[divan::bench]
fn access_log_no_subst(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..1_000_000 {
        let line = format!(
            "192.168.{}.{} - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n",
            (i / 256) % 256,
            i % 256
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(
            uumain,
            &["s/Chrome/Chromium/", file_path_str],
        ));
    });
}

/// Benchmark access log substitution
#[divan::bench]
fn access_log_subst(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..1_000_000 {
        let line = format!(
            "192.168.{}.{} - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n",
            (i / 256) % 256,
            i % 256
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(
            uumain,
            &["s/Mozilla/Chromium/", file_path_str],
        ));
    });
}

/// Benchmark access log no deletion
#[divan::bench]
fn access_log_no_del(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..1_000_000 {
        let line = format!(
            "192.168.{}.{} - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n",
            (i / 256) % 256,
            i % 256
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(uumain, &["/Chrome/d", file_path_str]));
    });
}

/// Benchmark access log full deletion
#[divan::bench]
fn access_log_all_del(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..1_000_000 {
        let line = format!(
            "192.168.{}.{} - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n",
            (i / 256) % 256,
            i % 256
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(uumain, &["/Mozilla/d", file_path_str]));
    });
}

/// Benchmark transliteration
#[divan::bench]
fn access_log_translit(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..500_000 {
        let line = format!(
            "192.168.{}.{} - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n",
            (i / 256) % 256,
            i % 256
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(
            uumain,
            &["y/0123456789/9876543210/", file_path_str],
        ));
    });
}

/// Benchmark text append
#[divan::bench]
fn access_log_append(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..2_000_000 {
        let line = format!(
            "192.168.{}.{} - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n",
            (i / 256) % 256,
            i % 256
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(
            uumain,
            &["athe-line-ends-here", file_path_str],
        ));
    });
}

/// Benchmark remove carriage return
#[divan::bench]
fn remove_cr(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..500_000 {
        let line = format!("line {i} with windows endings\r\n");
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(uumain, &["s/\r$//", file_path_str]));
    });
}

/// Benchmark genomic data substitution
#[divan::bench]
fn genome_subst(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..100_000 {
        let chr = format!("chr{}", 1 + i % 22);
        let line = format!(
            "{}\t{}\t{}\t.\t{}\t.\t.\n",
            chr,
            i * 100,
            if i % 2 == 1 { "A" } else { "T" },
            if i % 2 == 1 { "G" } else { "C" }
        );
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(
            uumain,
            &["/^#/d; s/\t\\./\tNA/g; s/\\.$/NA/", file_path_str],
        ));
    });
}

/// Benchmark number formatting
#[divan::bench]
fn number_fix(bencher: Bencher) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut data = Vec::new();
    for i in 0..100_000 {
        let euros = i % 10000;
        let cents = i % 100;
        let thousands = euros / 1000;
        let remainder = euros % 1000;
        let line = format!("{thousands}.{remainder:03},{cents:02}\n");
        data.extend_from_slice(line.as_bytes());
    }
    let file_path = create_test_file(&data, temp_dir.path());
    let file_path_str = file_path.to_str().unwrap();

    bencher.bench(|| {
        black_box(run_util_function(
            uumain,
            &[
                "s/\\([0-9]\\)\\.\\([0-9]\\)/\\1\\2/g;s/\\([0-9]\\),\\([0-9]\\)/\\1.\\2/g",
                file_path_str,
            ],
        ));
    });
}

fn main() {
    divan::main();
}
