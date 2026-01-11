#!/usr/bin/env python3

"""
Unit tests for compare_test_results.py
"""

import json
import tempfile
import unittest
from pathlib import Path
from compare_test_results import load_ignore_list, extract_test_results, compare_results


class TestCompareTestResults(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = Path(self.temp_dir.name)

        # Sample test data
        self.current_data = {
            "timestamp": "2025-01-11T10:00:00Z",
            "summary": {"total": 50, "passed": 40, "failed": 8, "skipped": 2},
            "tests": [
                {"name": "test1", "status": "PASS"},
                {"name": "test2", "status": "FAIL"},
                {"name": "test3", "status": "PASS"},
                {"name": "new_test", "status": "FAIL"},
            ],
        }

        self.reference_data = {
            "timestamp": "2025-01-10T10:00:00Z",
            "summary": {"total": 48, "passed": 42, "failed": 5, "skipped": 1},
            "tests": [
                {"name": "test1", "status": "PASS"},
                {"name": "old_failure", "status": "FAIL"},
                {"name": "test3", "status": "PASS"},
            ],
        }

    def tearDown(self):
        """Clean up test fixtures."""
        self.temp_dir.cleanup()

    def test_load_ignore_list_empty_file(self):
        """Test loading ignore list from empty file."""
        ignore_file = self.temp_path / "empty_ignore.txt"
        ignore_file.write_text("")

        result = load_ignore_list(str(ignore_file))
        self.assertEqual(result, set())

    def test_load_ignore_list_with_content(self):
        """Test loading ignore list with actual content."""
        ignore_file = self.temp_path / "ignore.txt"
        ignore_file.write_text("# Comment\ntest1\ntest2\n\n# Another comment\ntest3")

        result = load_ignore_list(str(ignore_file))
        self.assertEqual(result, {"test1", "test2", "test3"})

    def test_load_ignore_list_nonexistent_file(self):
        """Test loading ignore list from nonexistent file."""
        result = load_ignore_list("nonexistent.txt")
        self.assertEqual(result, set())

    def test_extract_test_results_valid_data(self):
        """Test extracting test results from valid JSON data."""
        summary, failed_tests = extract_test_results(self.current_data)

        self.assertEqual(summary["total"], 50)
        self.assertEqual(summary["passed"], 40)
        self.assertEqual(summary["failed"], 8)
        self.assertEqual(summary["skipped"], 2)
        self.assertEqual(set(failed_tests), {"test2", "new_test"})

    def test_extract_test_results_missing_summary(self):
        """Test extracting test results from data without summary."""
        data = {"tests": [{"name": "test1", "status": "PASS"}]}
        summary, failed_tests = extract_test_results(data)

        self.assertEqual(summary["total"], 0)
        self.assertEqual(failed_tests, [])

    def test_extract_test_results_empty_data(self):
        """Test extracting test results from empty data."""
        summary, failed_tests = extract_test_results({})

        self.assertEqual(summary["total"], 0)
        self.assertEqual(failed_tests, [])

    def test_compare_results_improvements_and_new_failures(self):
        """Test comparison showing both improvements and new failures."""
        current_file = self.temp_path / "current.json"
        reference_file = self.temp_path / "reference.json"
        output_file = self.temp_path / "output.txt"

        current_file.write_text(json.dumps(self.current_data))
        reference_file.write_text(json.dumps(self.reference_data))

        result = compare_results(
            str(current_file), str(reference_file), output_file=str(output_file)
        )

        # Should return 1 because there are new failures
        self.assertEqual(result, 1)

        # Check output content
        output_content = output_file.read_text()
        self.assertIn("Test result changes from main branch:", output_content)
        self.assertIn("TOTAL: +2", output_content)
        self.assertIn("PASSED: -2", output_content)
        self.assertIn("FAILED: +3", output_content)
        self.assertIn("New test failures (2):", output_content)
        self.assertIn("- test2", output_content)
        self.assertIn("- new_test", output_content)
        self.assertIn("Test improvements (1):", output_content)
        self.assertIn("+ old_failure", output_content)

    def test_compare_results_with_ignore_list(self):
        """Test comparison with ignored intermittent failures."""
        current_file = self.temp_path / "current.json"
        reference_file = self.temp_path / "reference.json"
        ignore_file = self.temp_path / "ignore.txt"
        output_file = self.temp_path / "output.txt"

        current_file.write_text(json.dumps(self.current_data))
        reference_file.write_text(json.dumps(self.reference_data))
        ignore_file.write_text("test2\nnew_test")  # Ignore both new failures

        result = compare_results(
            str(current_file), str(reference_file), str(ignore_file), str(output_file)
        )

        # Should return 0 because all new failures are ignored
        self.assertEqual(result, 0)

        # Check that intermittent failures are marked
        output_content = output_file.read_text()
        self.assertIn("- test2 (intermittent)", output_content)
        self.assertIn("- new_test (intermittent)", output_content)

    def test_compare_results_no_changes(self):
        """Test comparison with identical results."""
        current_file = self.temp_path / "current.json"
        reference_file = self.temp_path / "reference.json"
        output_file = self.temp_path / "output.txt"

        # Use same data for both files
        current_file.write_text(json.dumps(self.reference_data))
        reference_file.write_text(json.dumps(self.reference_data))

        result = compare_results(
            str(current_file), str(reference_file), output_file=str(output_file)
        )

        # Should return 0 for no new failures
        self.assertEqual(result, 0)

        # Output should be minimal or empty
        output_content = output_file.read_text()
        self.assertNotIn("New test failures", output_content)
        self.assertNotIn("Test improvements", output_content)

    def test_compare_results_only_improvements(self):
        """Test comparison with only improvements (no new failures)."""
        current_file = self.temp_path / "current.json"
        reference_file = self.temp_path / "reference.json"
        output_file = self.temp_path / "output.txt"

        # Create data where current has fewer failures than reference
        improved_data = {
            "summary": {"total": 48, "passed": 47, "failed": 0, "skipped": 1},
            "tests": [
                {"name": "test1", "status": "PASS"},
                {"name": "test3", "status": "PASS"},
                {"name": "old_failure", "status": "PASS"},  # Fixed the old failure!
            ],
        }

        current_file.write_text(json.dumps(improved_data))
        reference_file.write_text(json.dumps(self.reference_data))

        result = compare_results(
            str(current_file), str(reference_file), output_file=str(output_file)
        )

        # Should return 0 for no new failures
        self.assertEqual(result, 0)

        output_content = output_file.read_text()
        self.assertIn("Test improvements", output_content)
        # Should show improvement from old_failure
        self.assertIn("+ old_failure", output_content)
        # Should have no new failures
        self.assertNotIn("New test failures", output_content)

    def test_compare_results_invalid_current_file(self):
        """Test comparison with invalid current file."""
        reference_file = self.temp_path / "reference.json"
        reference_file.write_text(json.dumps(self.reference_data))

        result = compare_results("nonexistent_current.json", str(reference_file))

        # Should return 1 for error
        self.assertEqual(result, 1)

    def test_compare_results_invalid_reference_file(self):
        """Test comparison with invalid reference file."""
        current_file = self.temp_path / "current.json"
        current_file.write_text(json.dumps(self.current_data))

        result = compare_results(str(current_file), "nonexistent_reference.json")

        # Should return 1 for error
        self.assertEqual(result, 1)

    def test_compare_results_malformed_json(self):
        """Test comparison with malformed JSON files."""
        current_file = self.temp_path / "current.json"
        reference_file = self.temp_path / "reference.json"

        current_file.write_text("{ invalid json")
        reference_file.write_text(json.dumps(self.reference_data))

        result = compare_results(str(current_file), str(reference_file))

        # Should return 1 for error
        self.assertEqual(result, 1)


if __name__ == "__main__":
    unittest.main()
