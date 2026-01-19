#!/bin/bash
# Script to run GNU sed testsuite tests against the Rust sed implementation
#
# This script extracts and runs individual sed commands from the GNU sed testsuite
# to test compatibility between GNU sed and the Rust sed implementation.
#
# Usage: ./util/run-gnu-testsuite.sh [options] [test-pattern]
#
# Options:
#   -h, --help     Show this help message
#   -v, --verbose  Run tests with verbose output
#   -q, --quiet    Run tests quietly (only show failures)
#
# Examples:
#   ./util/run-gnu-testsuite.sh                    # Run basic functionality tests
#   ./util/run-gnu-testsuite.sh -v                 # Run with verbose output

# Don't exit on failure since test failures are expected
set -o pipefail

# Configuration
RUST_SED_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GNU_TESTSUITE_DIR_ORIG="${GNU_TESTSUITE_DIR:-}"
GNU_TESTSUITE_DIR="${GNU_TESTSUITE_DIR:-${RUST_SED_DIR}/../gnu.sed/testsuite}"
VERBOSE=false
QUIET=false
JSON_OUTPUT_FILE=""
DETAILED_RESULTS=()

# No colors for cleaner output

# Statistics
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

usage() {
    echo "Usage: $0 [options]"
    echo
    echo "Options:"
    echo "  -h, --help                Show this help message"
    echo "  -v, --verbose             Run tests with verbose output"
    echo "  -q, --quiet               Run tests quietly (only show failures)"
    echo "  --json-output FILE        Output results to JSON file"
    echo
    echo "Examples:"
    echo "  $0                        # Run basic functionality tests"
    echo "  $0 -v                     # Run with verbose output"
    echo "  $0 --json-output out.json # Output results to JSON file"
    echo
    echo "Environment variables:"
    echo "  GNU_TESTSUITE_DIR         Path to GNU sed testsuite directory"
    echo "                            (default: ../gnu.sed/testsuite)"
    echo ""
    echo "Setup:"
    echo "  To get the GNU sed testsuite for comprehensive testing:"
    echo "    git clone https://github.com/mirror/sed.git ../gnu.sed"
}

log_info() {
    if [[ "$QUIET" != "true" ]]; then
        echo "[INFO] $1"
    fi
}

log_success() {
    if [[ "$QUIET" != "true" ]]; then
        echo "[PASS] $1"
    fi
}

log_warning() {
    echo "[WARN] $1"
}

log_error() {
    echo "[FAIL] $1"
}

log_verbose() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo "[VERBOSE] $1"
    fi
}

# Function to generate JSON output
generate_json_output() {
    # Change back to the original directory to create the JSON file there
    cd "$RUST_SED_DIR"

    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local rust_version=$(cargo metadata --no-deps --format-version 1 2>/dev/null | jq -r '.packages[0].version // "unknown"')

    # Build tests array safely using jq
    local tests_json="[]"
    if [[ ${#DETAILED_RESULTS[@]} -gt 0 ]]; then
        # Create a temporary file with one JSON object per line
        local temp_file=$(mktemp)
        printf "%s\n" "${DETAILED_RESULTS[@]}" > "$temp_file"


        tests_json=$(jq -s '.' < "$temp_file" 2>/dev/null)
        if [[ $? -ne 0 ]]; then
            echo "ERROR: Failed to parse JSON from temp file"
            tests_json="[]"
        fi
        rm -f "$temp_file"
    fi

    # Generate JSON output using jq for safety
    jq -n \
        --arg timestamp "$timestamp" \
        --argjson total "$TOTAL_TESTS" \
        --argjson passed "$PASSED_TESTS" \
        --argjson failed "$FAILED_TESTS" \
        --argjson skipped "$SKIPPED_TESTS" \
        --argjson duration "$duration" \
        --arg rust_version "$rust_version" \
        --arg gnu_testsuite_dir "$GNU_TESTSUITE_DIR" \
        --argjson tests "$tests_json" \
        '{
            timestamp: $timestamp,
            summary: {
                total: $total,
                passed: $passed,
                failed: $failed,
                skipped: $skipped,
                duration_seconds: $duration
            },
            environment: {
                rust_sed_version: $rust_version,
                gnu_testsuite_dir: $gnu_testsuite_dir
            },
            tests: $tests
        }' > "$JSON_OUTPUT_FILE"

    log_info "JSON results written to: $JSON_OUTPUT_FILE"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -q|--quiet)
            QUIET=true
            shift
            ;;
        --json-output)
            JSON_OUTPUT_FILE="$2"
            shift 2
            ;;
        -*)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
        *)
            echo "Unknown argument: $1"
            usage
            exit 1
            ;;
    esac
done

# Validate environment
GNU_TESTSUITE_AVAILABLE=true

# Convert GNU_TESTSUITE_DIR to absolute path if it's relative
# This is critical because the script changes directories during test execution
if [[ -d "$GNU_TESTSUITE_DIR" ]]; then
    GNU_TESTSUITE_DIR="$(cd "$GNU_TESTSUITE_DIR" && pwd)"
fi

if [[ ! -d "$GNU_TESTSUITE_DIR" ]]; then
    log_warning "GNU sed testsuite not found at: $GNU_TESTSUITE_DIR"
    log_warning "To get the full GNU sed testsuite, clone it with:"
    log_warning "  git clone https://github.com/mirror/sed.git ${RUST_SED_DIR}/../gnu.sed"
    log_warning "Will run basic functionality tests only"
    GNU_TESTSUITE_AVAILABLE=false
fi

if [[ ! -f "$RUST_SED_DIR/Cargo.toml" ]]; then
    log_error "Not in a Rust project directory: $RUST_SED_DIR"
    exit 1
fi

# Build the Rust sed implementation
log_info "Building Rust sed implementation..."
cd "$RUST_SED_DIR"
if ! cargo build --release --quiet 2>/dev/null; then
    log_error "Failed to build Rust sed implementation"
    exit 1
fi

RUST_SED_BIN="$RUST_SED_DIR/target/release/sed"
if [[ ! -x "$RUST_SED_BIN" ]]; then
    log_error "Built sed binary not found at: $RUST_SED_BIN"
    exit 1
fi

log_info "Using Rust sed binary: $RUST_SED_BIN"

# Create temporary directory for test execution
TEST_WORK_DIR=$(mktemp -d)
trap 'rm -rf "$TEST_WORK_DIR"' EXIT

log_info "Test working directory: $TEST_WORK_DIR"

# Function to run a basic sed test
run_sed_test() {
    local test_name="$1"
    local sed_script="$2"
    local input_text="$3"
    local expected_output="$4"
    local flags="$5"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    log_verbose "Running test: $test_name"

    # Create test-specific directory
    local test_dir="$TEST_WORK_DIR/test_$TOTAL_TESTS"
    mkdir -p "$test_dir"
    cd "$test_dir"

    # Write input to file
    echo -n "$input_text" > input.txt
    echo -n "$expected_output" > expected.txt

    # Run Rust sed
    local rust_exit_code=0
    local rust_output=""
    if [[ -n "$flags" ]]; then
        rust_output=$("$RUST_SED_BIN" "$flags" "$sed_script" input.txt 2>/dev/null) || rust_exit_code=$?
    else
        rust_output=$(echo -n "$input_text" | "$RUST_SED_BIN" "$sed_script" 2>/dev/null) || rust_exit_code=$?
    fi

    local test_result=""
    local test_status=""
    local error_message=""

    # Compare with expected output
    if [[ "$rust_output" == "$expected_output" && $rust_exit_code -eq 0 ]]; then
        log_success "$test_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        test_status="PASS"
    else
        log_error "$test_name"
        if [[ "$VERBOSE" == "true" ]]; then
            echo "  | Expected: '$expected_output'"
            echo "  | Got: '$rust_output' (exit: $rust_exit_code)"
        fi
        FAILED_TESTS=$((FAILED_TESTS + 1))
        test_status="FAIL"
        error_message="Expected: '$expected_output', Got: '$rust_output' (exit: $rust_exit_code)"
    fi

    # Store detailed results for JSON output
    if [[ -n "$JSON_OUTPUT_FILE" ]]; then
        # Create JSON object using jq to ensure proper escaping and structure
        local json_obj
        json_obj=$(jq -n \
            --arg name "$test_name" \
            --arg status "$test_status" \
            --arg script "$sed_script" \
            --arg error "$error_message" \
            '{name: $name, status: $status, script: $script, error: $error}')

        DETAILED_RESULTS+=("$json_obj")
    fi

    # Cleanup test directory
    cd "$TEST_WORK_DIR"
    rm -rf "$test_dir"
}


# Define basic functionality tests
run_basic_tests() {
    log_info "Running basic sed functionality tests..."

    # Basic substitution
    run_sed_test "basic_substitution" "s/hello/world/" "hello there" "world there"
    run_sed_test "global_substitution" "s/a/X/g" "banana" "bXnXnX"

    # Line addressing
    run_sed_test "line_address" "2s/test/TEST/" $'line1\ntest\nline3' $'line1\nTEST\nline3'
    run_sed_test "range_address" "2,3s/x/X/" $'x1\nx2\nx3\nx4' $'x1\nX2\nX3\nx4'

    # Delete command
    run_sed_test "delete_line" "2d" $'line1\nline2\nline3' $'line1\nline3'
    run_sed_test "delete_range" "2,3d" $'line1\nline2\nline3\nline4' $'line1\nline4'

    # Print command
    run_sed_test "print_line" "-n 2p" $'line1\nline2\nline3' "line2" "-n"

    # Append and insert
    run_sed_test "append" "2a\\inserted" $'line1\nline2\nline3' $'line1\nline2\ninserted\nline3'
    run_sed_test "insert" "2i\\inserted" $'line1\nline2\nline3' $'line1\ninserted\nline2\nline3'

    # Character classes
    run_sed_test "digit_class" "s/[0-9]/X/g" "abc123def" "abcXXXdef"
    run_sed_test "word_class" "s/[a-z]/X/g" "Hello123" "HXXXX123"
}

# Run tests from specific GNU testsuite files that have .inp/.good/.sed triplets
run_gnu_testsuite_tests() {
    log_info "Extracting and running tests from GNU testsuite..."

    local tests_found=0

    # 1. Handle complete triplets (inp + good + sed)
    for inp_file in "$GNU_TESTSUITE_DIR"/*.inp; do
        if [[ -f "$inp_file" ]]; then
            local basename
            basename=$(basename "$inp_file" .inp)
            local good_file="$GNU_TESTSUITE_DIR/${basename}.good"
            local sed_file="$GNU_TESTSUITE_DIR/${basename}.sed"

            if [[ -f "$good_file" && -f "$sed_file" ]]; then
                local input_content
                local expected_content

                input_content=$(cat "$inp_file")
                expected_content=$(cat "$good_file")

                log_verbose "Found complete triplet: $basename"
                run_sed_test "${basename}_triplet" "$sed_file" "$input_content" "$expected_content" "-f"
                tests_found=$((tests_found + 1))
            fi
        fi
    done

    # 2. Handle partial triplets (inp + good, no sed script)
    for inp_file in "$GNU_TESTSUITE_DIR"/*.inp; do
        if [[ -f "$inp_file" ]]; then
            local basename
            basename=$(basename "$inp_file" .inp)
            local good_file="$GNU_TESTSUITE_DIR/${basename}.good"
            local sed_file="$GNU_TESTSUITE_DIR/${basename}.sed"

            if [[ -f "$good_file" && ! -f "$sed_file" ]]; then
                # Look for sed commands in the corresponding shell script
                local shell_file="$GNU_TESTSUITE_DIR/${basename}.sh"
                if [[ -f "$shell_file" ]]; then
                    extract_sed_commands_from_script "$shell_file" "$inp_file" "$good_file"
                    tests_found=$((tests_found + 1))
                fi
            fi
        fi
    done

    # 3. Extract simple tests from shell scripts
    local shell_count=0

    # Use array to collect shell files via find (more reliable than glob)
    local shell_files=()
    # Use temporary file approach to avoid process substitution issues in CI
    local temp_file_list
    temp_file_list=$(mktemp)
    trap 'rm -f "$temp_file_list"' EXIT

    # Find shell files and store in temporary file
    # GNU_TESTSUITE_DIR is already an absolute path, so use it directly
    find "$GNU_TESTSUITE_DIR" -name "*.sh" 2>/dev/null > "$temp_file_list"


    # Read the file list into array
    while IFS= read -r shell_file; do
        [[ -n "$shell_file" ]] && shell_files+=("$shell_file")
    done < "$temp_file_list"

    rm -f "$temp_file_list"

    for shell_file in "${shell_files[@]}"; do
        if [[ -f "$shell_file" && $shell_count -lt 50 ]]; then  # Process more test files
            local basename
            basename=$(basename "$shell_file" .sh)

            # Skip files we already processed and skip complex/problematic tests
            case "$basename" in
                "help-version"|"compile-"*|"panic-"*|"debug"|"*wrapper*"|"no-perl")
                    continue
                    ;;
            esac

            if [[ ! -f "$GNU_TESTSUITE_DIR/${basename}.inp" ]]; then
                extract_simple_tests_from_script "$shell_file"
                shell_count=$((shell_count + 1))
            fi
        fi
    done

    if [[ $tests_found -eq 0 && $shell_count -eq 0 ]]; then
        log_warning "No GNU testsuite tests found"
    else
        log_info "Extracted tests from GNU testsuite: $tests_found triplets + $shell_count shell scripts"
    fi
}

# Extract sed commands from shell scripts when we have input/output files
extract_sed_commands_from_script() {
    local script_file="$1"
    local input_file="$2"
    local expected_file="$3"
    local basename
    basename=$(basename "$script_file" .sh)

    log_verbose "Extracting from script: $basename"

    # Look for simple sed patterns in the script
    while IFS= read -r line; do
        # Match: sed 'script' < input > output
        if [[ $line =~ sed[[:space:]]+[\'\"](.*)[\'\"] ]] && [[ $line == *"<"* || $line == *"|"* ]]; then
            local sed_script="${BASH_REMATCH[1]}"
            if [[ -n "$sed_script" && ${#sed_script} -lt 100 ]]; then  # Avoid very complex scripts
                local input_content
                local expected_content
                input_content=$(cat "$input_file")
                expected_content=$(cat "$expected_file")
                run_sed_test "${basename}_extracted" "$sed_script" "$input_content" "$expected_content"
            fi
        fi
    done < "$script_file"
}

# Extract simple tests from shell scripts (without input/output files)
extract_simple_tests_from_script() {
    local script_file="$1"
    local basename
    basename=$(basename "$script_file" .sh)
    local extracted_count=0

    log_verbose "Scanning script: $basename"

    # Look for various sed command patterns in the GNU testsuite
    while IFS= read -r line; do
        if [[ $extracted_count -ge 5 ]]; then  # Limit extractions per file
            break
        fi

        # Skip comments and empty lines
        [[ $line =~ ^[[:space:]]*# ]] && continue
        [[ -z "${line// }" ]] && continue

        # Pattern 1: echo "text" | sed 'script'
        if [[ $line =~ echo[[:space:]]+[\'\"]([^\'\"]+)[\'\"].*\|.*sed[[:space:]]+[\'\"]([^\'\"]+)[\'\"] ]]; then
            local input_text="${BASH_REMATCH[1]}"
            local sed_script="${BASH_REMATCH[2]}"
            if [[ -n "$input_text" && -n "$sed_script" && ${#sed_script} -lt 80 ]]; then
                run_sed_test "${basename}_echo_${extracted_count}" "$sed_script" "$input_text" "" ""
                extracted_count=$((extracted_count + 1))
                continue
            fi
        fi

        # Pattern 2: sed -e 'script' file
        if [[ $line =~ sed[[:space:]]+-e[[:space:]]+[\'\"]([^\'\"]+)[\'\"] ]] && [[ $extracted_count -lt 2 ]]; then
            local sed_script="${BASH_REMATCH[1]}"
            if [[ -n "$sed_script" && ${#sed_script} -lt 80 ]]; then
                run_sed_test "${basename}_dash_e_${extracted_count}" "$sed_script" "line1\nline2\nline3" "" ""
                extracted_count=$((extracted_count + 1))
                continue
            fi
        fi

        # Pattern 3: sed 's/pattern/replacement/' file
        if [[ $line =~ sed[[:space:]]+[\'\"]s/([^/]+)/([^/]*)/[^\'\"]*[\'\"] ]] && [[ $extracted_count -lt 2 ]]; then
            local pattern="${BASH_REMATCH[1]}"
            local replacement="${BASH_REMATCH[2]}"
            if [[ -n "$pattern" && ${#pattern} -lt 30 ]]; then
                local input_text="This is $pattern in text"
                local sed_script="s/$pattern/$replacement/"
                run_sed_test "${basename}_subst_${extracted_count}" "$sed_script" "$input_text" "" ""
                extracted_count=$((extracted_count + 1))
                continue
            fi
        fi

        # Pattern 4: Simple sed 'command' patterns
        if [[ $line =~ sed[[:space:]]+[\'\"]([^\'\"]+)[\'\"] ]] && [[ $extracted_count -lt 1 ]]; then
            local sed_script="${BASH_REMATCH[1]}"
            # Only take simple, short commands
            if [[ -n "$sed_script" && ${#sed_script} -lt 50 && ! $sed_script =~ \$\{ ]]; then
                # Avoid complex scripts with variables or complex syntax
                case "$sed_script" in
                    *"\${"*|*'`'*|*"\$(")
                        continue
                        ;;
                    *)
                        run_sed_test "${basename}_cmd_${extracted_count}" "$sed_script" "test\ndata\nline" "" ""
                        extracted_count=$((extracted_count + 1))
                        ;;
                esac
            fi
        fi

        # Pattern 5: printf | sed patterns
        if [[ $line =~ printf[[:space:]]+[\'\"]([^\'\"]+)[\'\"].*\|.*sed[[:space:]]+[\'\"]([^\'\"]+)[\'\"] ]]; then
            local input_text="${BASH_REMATCH[1]}"
            local sed_script="${BASH_REMATCH[2]}"
            if [[ -n "$input_text" && -n "$sed_script" && ${#sed_script} -lt 60 ]]; then
                # Convert \n to actual newlines
                input_text=$(echo -e "$input_text")
                run_sed_test "${basename}_printf_${extracted_count}" "$sed_script" "$input_text" "" ""
                extracted_count=$((extracted_count + 1))
                continue
            fi
        fi

        # Pattern 6: sed 'script' input_file > output (file-based format common in GNU tests)
        if [[ $line =~ ^[[:space:]]*sed[[:space:]]+[\'\"]([^\'\"]+)[\'\"][[:space:]]+[a-zA-Z0-9_-]+[[:space:]]*\> ]]; then
            local sed_script="${BASH_REMATCH[1]}"
            if [[ -n "$sed_script" && ${#sed_script} -lt 80 && ! $sed_script =~ \$\{ ]]; then
                # Use generic test input for file-based tests
                run_sed_test "${basename}_file_${extracted_count}" "$sed_script" "line1\nline2\nline3\ntest data" "" ""
                extracted_count=$((extracted_count + 1))
                continue
            fi
        fi

        # Pattern 7: sed -n 'script' (with -n flag)
        if [[ $line =~ sed[[:space:]]+-n[[:space:]]+[\'\"]([^\'\"]+)[\'\"] ]]; then
            local sed_script="${BASH_REMATCH[1]}"
            if [[ -n "$sed_script" && ${#sed_script} -lt 80 ]]; then
                run_sed_test "${basename}_n_${extracted_count}" "$sed_script" "line1\nline2\nline3" "" "-n"
                extracted_count=$((extracted_count + 1))
                continue
            fi
        fi

    done < "$script_file"

    if [[ $extracted_count -gt 0 ]]; then
        log_verbose "Extracted $extracted_count tests from $basename"
    fi
}

# Main test execution
log_info "Starting test execution..."
echo

start_time=$(date +%s)

# Run basic functionality tests
run_basic_tests

# Try to run some GNU testsuite tests if available
if [[ "$GNU_TESTSUITE_AVAILABLE" == "true" ]]; then
    run_gnu_testsuite_tests
else
    log_warning "Skipping GNU testsuite-specific tests"
fi

end_time=$(date +%s)
duration=$((end_time - start_time))

# Print summary
echo
echo "========================================="
echo "TEST RESULTS SUMMARY"
echo "========================================="
echo "Total tests:   $TOTAL_TESTS"
echo "Passed:        $PASSED_TESTS"
echo "Failed:        $FAILED_TESTS"
echo "Skipped:       $SKIPPED_TESTS"
echo "Duration:      ${duration}s"

# Generate JSON output if requested
if [[ -n "$JSON_OUTPUT_FILE" ]]; then
    generate_json_output
fi

if [[ $FAILED_TESTS -eq 0 ]]; then
    echo "Result:        ALL TESTS PASSED"
    final_exit_code=0
else
    if [[ $PASSED_TESTS -gt 0 ]]; then
        pass_rate=$(( (PASSED_TESTS * 100) / (PASSED_TESTS + FAILED_TESTS) ))
        echo "Pass rate:     ${pass_rate}%"
    fi
    echo "Result:        SOME TESTS FAILED"
    final_exit_code=1
fi

exit $final_exit_code
