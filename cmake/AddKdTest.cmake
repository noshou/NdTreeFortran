# add_kdtest(name [WILL_FAIL])
#
# Registers a single-threaded CTest test backed by a Fortran executable.
#
# Arguments:
#   name       - Test name and stem of the .f90 source file. Must follow the
#                Testv{version}_{WHAT} naming convention.
#   WILL_FAIL  - Optional flag. When present, CTest inverts pass/fail: the test
#                passes only when the executable exits with a non-zero status.
#                Used for tests that exercise error guards (error stop paths).
#
# Behaviour:
#   - Builds ${name} from ${name}.f90 and links it against kdtreefortran.
#   - Registers the test name in the KD_ALL_TESTS global property so that
#     finalize_skip_kd_tests() can validate SKIP_TESTS at configure time.
#   - Applies version labels (e.g. "v0" and "v0.2.1") derived from the test name
#     so that ctest -L can filter by major version or exact release.
#   - If the name appears in SKIP_TESTS, no executable is built; the test is
#     registered with "cmake -E echo TEST SKIPPED" so it still appears in the
#     CTest run (preserving test numbers) and always passes.
function(add_kdtest name)
    cmake_parse_arguments(ARG "WILL_FAIL" "" "" ${ARGN})
    set_property(GLOBAL APPEND PROPERTY KD_ALL_TESTS "${name}")
    _kd_is_skipped(${name} _skip)
    if(_skip)
        add_test(NAME ${name} COMMAND ${CMAKE_COMMAND} -E echo "TEST SKIPPED: ${name}")
        _kd_apply_version_labels(${name})
        return()
    endif()
    add_executable(${name} ${name}.f90)
    target_link_libraries(${name} PRIVATE kdtreefortran)
    add_test(NAME ${name} COMMAND ${name})
    _kd_apply_version_labels(${name})
    if(ARG_WILL_FAIL)
        set_tests_properties(${name} PROPERTIES WILL_FAIL TRUE)
    endif()
endfunction()
