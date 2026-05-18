# _kd_apply_version_labels(name)
#
# Internal helper called by add_kdtest and add_kdtest_omp after every add_test call.
# Parses the test name to derive version labels and attaches them to the CTest test.
#
# Naming convention: Testv{A}{B}{C}_{REST}
#   A = major version digit, B = minor version digit, C = patch version digit
#   e.g. Testv021_BUFFER_SIZE -> major=0, minor=2, patch=1 -> v0.2.1
#
# Each matched test receives two labels:
#   "v{A}"           - major-only label (e.g. "v0"), matches all tests in that major line
#   "v{A}.{B}.{C}"   - exact version label (e.g. "v0.2.1"), matches that release only
#
# Usage with ctest:
#   ctest --test-dir build -L "^v0$"        # all v0.x.y tests
#   ctest --test-dir build -L "^v0\.2\.1$"  # only v0.2.1 tests
#
# Tests whose names do not match the pattern are silently skipped (no labels assigned).
function(_kd_apply_version_labels name)
    if(NOT name MATCHES "^Testv([0-9])([0-9])([0-9])_")
        return()
    endif()
    set(major "${CMAKE_MATCH_1}")
    set(minor "${CMAKE_MATCH_2}")
    set(patch "${CMAKE_MATCH_3}")
    set_tests_properties(${name} PROPERTIES
        LABELS "v${major};v${major}.${minor}.${patch}")
endfunction()
