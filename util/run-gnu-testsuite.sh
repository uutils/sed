#!/bin/bash
# Script to run GNU sed testsuite tests against the Rust sed implementation
#
# This script runs the GNU sed testsuite shell scripts by providing a
# lightweight shim for the gnulib test framework (init.sh) and injecting
# our Rust sed binary via PATH.
#
# Usage: ./util/run-gnu-testsuite.sh [options]
#
# Options:
#   -h, --help                Show this help message
#   -v, --verbose             Run tests with verbose output
#   -q, --quiet               Run tests quietly (only show failures)
#   --json-output FILE        Output results to JSON file
#
# Examples:
#   ./util/run-gnu-testsuite.sh                    # Run all tests
#   ./util/run-gnu-testsuite.sh -v                 # Run with verbose output

# Don't exit on failure since test failures are expected
set -o pipefail

# Configuration
RUST_SED_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GNU_SED_DIR="${GNU_SED_DIR:-${RUST_SED_DIR}/../gnu.sed}"
GNU_TESTSUITE_DIR=""
VERBOSE=false
QUIET=false
JSON_OUTPUT_FILE=""
DETAILED_RESULTS=()

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
    echo "  $0                        # Run all tests"
    echo "  $0 -v                     # Run with verbose output"
    echo "  $0 --json-output out.json # Output results to JSON file"
    echo
    echo "Environment variables:"
    echo "  GNU_SED_DIR               Path to GNU sed source directory"
    echo "                            (default: ../gnu.sed)"
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

log_skip() {
    if [[ "$QUIET" != "true" ]]; then
        echo "[SKIP] $1"
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
    cd "$RUST_SED_DIR"

    local timestamp
    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local rust_version
    rust_version=$(cargo metadata --no-deps --format-version 1 2>/dev/null | jq -r '.packages[0].version // "unknown"')

    # Build tests array safely using jq
    local tests_json="[]"
    if [[ ${#DETAILED_RESULTS[@]} -gt 0 ]]; then
        local temp_file
        temp_file=$(mktemp)
        printf "%s\n" "${DETAILED_RESULTS[@]}" > "$temp_file"

        tests_json=$(jq -s '.' < "$temp_file" 2>/dev/null)
        if [[ $? -ne 0 ]]; then
            echo "ERROR: Failed to parse JSON from temp file"
            tests_json="[]"
        fi
        rm -f "$temp_file"
    fi

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

# Resolve GNU_SED_DIR to absolute path, then derive testsuite dir
if [[ -d "$GNU_SED_DIR" ]]; then
    GNU_SED_DIR="$(cd "$GNU_SED_DIR" && pwd)"
    GNU_TESTSUITE_DIR="$GNU_SED_DIR/testsuite"
fi

if [[ ! -d "$GNU_TESTSUITE_DIR" ]]; then
    log_warning "GNU sed testsuite not found at: $GNU_SED_DIR"
    log_warning "To get the full GNU sed testsuite, clone it with:"
    log_warning "  git clone https://github.com/mirror/sed.git ${RUST_SED_DIR}/../gnu.sed"
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

# Record a test result (for JSON output)
record_result() {
    local test_name="$1"
    local test_status="$2"
    local error_message="$3"

    if [[ -n "$JSON_OUTPUT_FILE" ]]; then
        local json_obj
        json_obj=$(jq -n \
            --arg name "$test_name" \
            --arg status "$test_status" \
            --arg error "$error_message" \
            '{name: $name, status: $status, error: $error}')
        DETAILED_RESULTS+=("$json_obj")
    fi
}

# Create the shim srcdir with init.sh and symlinks to real test data
create_test_shim() {
    local shim_srcdir="$1"
    local real_testsuite_dir="$2"

    # The test scripts do: . "${srcdir=.}/testsuite/init.sh"
    # We create a srcdir that has:
    #   testsuite/init.sh  -> our shim
    #   testsuite/*        -> symlinks to real GNU testsuite data files
    mkdir -p "$shim_srcdir/testsuite"

    # Symlink all real testsuite files (data, scripts, sed programs)
    for f in "$real_testsuite_dir"/*; do
        local base
        base=$(basename "$f")
        # Don't symlink init.sh if it exists (we provide our own)
        [[ "$base" == "init.sh" ]] && continue
        ln -sf "$f" "$shim_srcdir/testsuite/$base"
    done

    cat > "$shim_srcdir/testsuite/init.sh" << 'SHIM_EOF'
# Lightweight shim for gnulib test framework init.sh
# Provides just enough for GNU sed testsuite scripts to run

# Exit codes
EXIT_PASS=0
EXIT_SKIP=77
EXIT_FRAMEWORK_FAILURE=99

fail=0

# abs_top_srcdir is used by some tests to find data files (.inp, .sed, .good)
abs_top_srcdir="$srcdir"
export abs_top_srcdir

# Create a temp directory for the test and cd into it
test_dir_=$(mktemp -d)
trap 'rm -rf "$test_dir_"' EXIT
cd "$test_dir_" || exit $EXIT_FRAMEWORK_FAILURE

framework_failure_() {
    echo "FRAMEWORK FAILURE: $*" >&2
    exit $EXIT_FRAMEWORK_FAILURE
}

skip_() {
    echo "SKIP: $*" >&2
    exit $EXIT_SKIP
}

compare_() {
    diff "$1" "$2" > /dev/null 2>&1
}

compare() {
    diff "$1" "$2" > /dev/null 2>&1
}

# Run a command and check that it returns the expected exit code
returns_() {
    local expected_rc="$1"
    shift
    "$@"
    local actual_rc=$?
    if [[ $actual_rc -ne $expected_rc ]]; then
        return 1
    fi
    return 0
}

path_prepend_() {
    # The test scripts call: path_prepend_ ./sed
    # We ignore this since we already have our sed in PATH
    :
}

print_ver_() {
    :
}

Exit() {
    exit "$1"
}

# require_ functions: skip tests that need capabilities we can't provide
require_valgrind_() {
    skip_ "valgrind not available in this test harness"
}

require_selinux_() {
    skip_ "SELinux not available in this test harness"
}

very_expensive_() {
    skip_ "very expensive tests disabled"
}

expensive_() {
    skip_ "expensive tests disabled"
}

require_en_utf8_locale_() {
    # Check if en_US.UTF-8 locale is available
    if locale -a 2>/dev/null | grep -qi 'en_US\.utf-\?8'; then
        return 0
    fi
    skip_ "en_US.UTF-8 locale not available"
}

require_ru_utf8_locale_() {
    if locale -a 2>/dev/null | grep -qi 'ru_RU\.utf-\?8'; then
        return 0
    fi
    skip_ "ru_RU.UTF-8 locale not available"
}

require_el_iso88597_locale_() {
    skip_ "el_GR.iso88597 locale not available"
}

require_ja_shiftjis_locale_() {
    LOCALE_JA_SJIS=""
    for l in shiftjis sjis SJIS; do
        if locale -a 2>/dev/null | grep -qi "ja_JP\.$l"; then
            LOCALE_JA_SJIS="ja_JP.$l"
            return 0
        fi
    done
    skip_ "ja_JP shift-jis locale not available"
}

require_valid_ja_shiftjis_locale_() {
    skip_ "ja_JP shift-jis locale validation not available"
}

require_valid_ja_eucjp_locale_() {
    skip_ "ja_JP.eucJP locale validation not available"
}

remove_cr_inplace() {
    sed -i'' -e "s/\r//g" "$@" || framework_failure_
}
SHIM_EOF
}

# Run a single GNU testsuite shell script with our shims
run_gnu_shell_test() {
    local test_script="$1"
    local test_name
    test_name=$(basename "$test_script" .sh)

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    log_verbose "Running test: $test_name"

    # Run the test script with a per-test timeout to avoid hangs.
    # We write output to a file (not a pipe) and run timeout in the
    # foreground so there are no orphaned processes or signal issues.
    local test_output_file="$TEST_WORK_DIR/test_output_$$"
    local test_exit_code=0

    # When the script is not the process group leader (e.g. in CI),
    # GNU timeout falls back to "foreground" mode and sends SIGTERM
    # to the entire process group when a test hangs. Ignore SIGTERM
    # during the test so the parent script survives.
    trap '' TERM

    PATH="$SED_WRAPPER_DIR:$PATH" \
    srcdir="$SHIM_SRCDIR" \
    timeout --kill-after=5 10 \
        /bin/sh "$test_script" </dev/null >"$test_output_file" 2>&1 \
    || test_exit_code=$?

    trap - TERM

    local test_output=""
    [[ -f "$test_output_file" ]] && test_output=$(cat "$test_output_file")
    rm -f "$test_output_file"

    # Detect timeout: 124 = GNU coreutils timeout, 125 = uutils timeout,
    # >=128 = killed by signal (137=SIGKILL, 143=SIGTERM in foreground mode)
    if [[ $test_exit_code -eq 124 || $test_exit_code -eq 125 || $test_exit_code -ge 128 ]]; then
        log_error "$test_name (timeout)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        record_result "$test_name" "FAIL" "Test timed out after 10s"
        return
    fi

    local error_message=""

    case $test_exit_code in
        0)
            log_success "$test_name"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            record_result "$test_name" "PASS" ""
            ;;
        77)
            log_skip "$test_name"
            SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
            if [[ "$VERBOSE" == "true" ]]; then
                echo "  | $test_output" | head -3
            fi
            record_result "$test_name" "SKIP" "$test_output"
            ;;
        99)
            log_error "$test_name (framework failure)"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            error_message="Framework failure: $test_output"
            if [[ "$VERBOSE" == "true" ]]; then
                echo "  | Framework failure"
                echo "  | $test_output" | head -5
            fi
            record_result "$test_name" "FAIL" "$error_message"
            ;;
        *)
            log_error "$test_name"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            error_message="Exit code: $test_exit_code"
            if [[ "$VERBOSE" == "true" ]]; then
                echo "  | Exit code: $test_exit_code"
                echo "  | $test_output" | head -5
            fi
            record_result "$test_name" "FAIL" "$error_message"
            ;;
    esac
}

# Run all GNU testsuite shell script tests
run_gnu_shell_tests() {
    log_info "Running GNU testsuite shell script tests..."

    # Collect all .sh test files
    local shell_files=()
    local temp_file_list
    temp_file_list=$(mktemp)

    find "$GNU_TESTSUITE_DIR" -maxdepth 1 -name "*.sh" 2>/dev/null | sort > "$temp_file_list"

    while IFS= read -r shell_file; do
        [[ -n "$shell_file" ]] && shell_files+=("$shell_file")
    done < "$temp_file_list"
    rm -f "$temp_file_list"

    local shell_count=0
    for shell_file in "${shell_files[@]}"; do
        [[ -f "$shell_file" ]] || continue
        local base
        base=$(basename "$shell_file" .sh)

        # Skip scripts that aren't actual tests
        case "$base" in
            "help-version"|"panic-"*|"debug"|"Makefile"|"init")
                continue
                ;;
        esac

        run_gnu_shell_test "$shell_file"
        shell_count=$((shell_count + 1))
    done

    log_info "Ran $shell_count shell script tests"
}

# Set up sed wrapper and test shim (once, before all tests)
SED_WRAPPER_DIR="$TEST_WORK_DIR/bin"
mkdir -p "$SED_WRAPPER_DIR"
cat > "$SED_WRAPPER_DIR/sed" << WRAPPER_EOF
#!/bin/sh
exec "$RUST_SED_BIN" "\$@"
WRAPPER_EOF
chmod +x "$SED_WRAPPER_DIR/sed"

SHIM_SRCDIR="$TEST_WORK_DIR/srcdir"
if [[ "$GNU_TESTSUITE_AVAILABLE" == "true" ]]; then
    create_test_shim "$SHIM_SRCDIR" "$GNU_TESTSUITE_DIR"
fi

# Main test execution
log_info "Starting test execution..."

start_time=$(date +%s)

if [[ "$GNU_TESTSUITE_AVAILABLE" == "true" ]]; then
    run_gnu_shell_tests
else
    log_warning "Skipping GNU testsuite tests (testsuite not found)"
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
