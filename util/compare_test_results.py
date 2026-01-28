#!/usr/bin/env python3

"""
Compare the current GNU test results to the last results gathered from the main branch to
highlight if a PR is making the results better/worse.
Don't exit with error code if all failing tests are in the ignore-intermittent.txt list.
"""

import json
import sys
import argparse
from pathlib import Path


def load_ignore_list(ignore_file):
    """Load list of intermittent test names to ignore from file."""
    ignore_set = set()
    if ignore_file and Path(ignore_file).exists():
        with open(ignore_file, "r") as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith("#"):
                    ignore_set.add(line)
    return ignore_set


def extract_test_results(json_data):
    """Extract test results from JSON data."""
    if not json_data or "summary" not in json_data:
        return {"total": 0, "passed": 0, "failed": 0, "skipped": 0}, []

    summary = json_data["summary"]
    tests = json_data.get("tests", [])

    # Extract failed test names
    failed_tests = []
    for test in tests:
        if test.get("status") == "FAIL":
            failed_tests.append(test.get("name", "unknown"))

    return summary, failed_tests


def compare_results(current_file, reference_file, ignore_file=None, output_file=None):
    """Compare current results with reference results."""
    # Load ignore list
    ignore_set = load_ignore_list(ignore_file)

    # Load JSON files
    try:
        with open(current_file, "r") as f:
            current_data = json.load(f)
        current_summary, current_failed = extract_test_results(current_data)
    except Exception as e:
        print(f"Error loading current results: {e}")
        return 1

    try:
        with open(reference_file, "r") as f:
            reference_data = json.load(f)
        reference_summary, reference_failed = extract_test_results(reference_data)
    except Exception as e:
        print(f"Error loading reference results: {e}")
        return 1

    # Calculate differences
    pass_diff = int(current_summary.get("passed", 0)) - int(
        reference_summary.get("passed", 0)
    )
    fail_diff = int(current_summary.get("failed", 0)) - int(
        reference_summary.get("failed", 0)
    )
    total_diff = int(current_summary.get("total", 0)) - int(
        reference_summary.get("total", 0)
    )

    # Find new failures and improvements
    current_failed_set = set(current_failed)
    reference_failed_set = set(reference_failed)

    new_failures = current_failed_set - reference_failed_set
    improvements = reference_failed_set - current_failed_set

    # Filter out intermittent failures
    non_intermittent_new_failures = new_failures - ignore_set

    # Check if results are identical (no changes)
    no_changes = (
        pass_diff == 0
        and fail_diff == 0
        and total_diff == 0
        and not new_failures
        and not improvements
    )

    # If no changes, write empty output to prevent comment posting
    if no_changes:
        with open(output_file, "w") as f:
            f.write("")
        return 0

    # Prepare output message
    output_lines = []

    # Show current vs reference numbers for debugging
    output_lines.append("Test results comparison:")
    output_lines.append(
        f"  Current:   TOTAL: {current_summary.get('total', 0)} / PASSED: {current_summary.get('passed', 0)} / FAILED: {current_summary.get('failed', 0)} / SKIPPED: {current_summary.get('skipped', 0)}"
    )
    output_lines.append(
        f"  Reference: TOTAL: {reference_summary.get('total', 0)} / PASSED: {reference_summary.get('passed', 0)} / FAILED: {reference_summary.get('failed', 0)} / SKIPPED: {reference_summary.get('skipped', 0)}"
    )
    output_lines.append("")

    # Summary of changes
    if pass_diff != 0 or fail_diff != 0 or total_diff != 0:
        output_lines.append("Changes from main branch:")
        output_lines.append(f"  TOTAL: {total_diff:+d}")
        output_lines.append(f"  PASSED: {pass_diff:+d}")
        output_lines.append(f"  FAILED: {fail_diff:+d}")
        output_lines.append("")

    # New failures
    if new_failures:
        output_lines.append(f"New test failures ({len(new_failures)}):")
        for test in sorted(new_failures):
            if test in ignore_set:
                output_lines.append(f"  - {test} (intermittent)")
            else:
                output_lines.append(f"  - {test}")
        output_lines.append("")

    # Improvements
    if improvements:
        output_lines.append(f"Test improvements ({len(improvements)}):")
        for test in sorted(improvements):
            output_lines.append(f"  + {test}")
        output_lines.append("")

    # Write output
    output_text = "\n".join(output_lines)
    if output_file:
        with open(output_file, "w") as f:
            f.write(output_text)
    else:
        print(output_text)

    # Return appropriate exit code
    if non_intermittent_new_failures:
        print(
            f"ERROR: Found {len(non_intermittent_new_failures)} new non-intermittent test failures"
        )
        return 1

    return 0


def main():
    parser = argparse.ArgumentParser(description="Compare GNU test results")
    parser.add_argument("current", help="Current test results JSON file")
    parser.add_argument("reference", help="Reference test results JSON file")
    parser.add_argument(
        "--ignore-file", help="File containing intermittent test names to ignore"
    )
    parser.add_argument("--output", help="Output file for comparison results")

    args = parser.parse_args()

    return compare_results(args.current, args.reference, args.ignore_file, args.output)


if __name__ == "__main__":
    sys.exit(main())
