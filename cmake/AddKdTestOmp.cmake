# add_kdtest_omp(name [WILL_FAIL])
#
# Registers a CTest test for a Fortran executable that contains !$OMP directives.
#
# Arguments:
#   name       - Test name and stem of the .f90 source file. Must follow the
#                Testv{version}_{WHAT} naming convention.
#   WILL_FAIL  - Optional flag. When present, CTest inverts pass/fail: the test
#                passes only when the executable exits with a non-zero status.
#                Used for tests that exercise error guards (error stop paths).
#
# Behaviour:
#   - Builds ${name} from ${name}.f90 and links it against both ndtreefortran
#     and OpenMP::OpenMP_Fortran. The ndtreefortran library links OpenMP
#     privately, so test executables that contain their own !$OMP directives
#     must link it explicitly via this macro.
#   - Sets the OMP_NUM_THREADS=4 environment variable for the test run. Tests
#     that only call the library (without their own directives) should use
#     add_kdtest instead.
#   - Registers the test name in the KD_ALL_TESTS global property so that
#     finalize_skip_kd_tests() can validate SKIP_TESTS at configure time.
#   - Applies version labels (e.g. "v0" and "v0.6.0") derived from the test name
#     so that ctest -L can filter by major version or exact release.
#   - If the name appears in SKIP_TESTS, no executable is built; the test is
#     registered with "cmake -E echo TEST SKIPPED" so it still appears in the
#     CTest run (preserving test numbers) and always passes.
function(add_kdtest_omp name)
    cmake_parse_arguments(ARG "WILL_FAIL" "" "" ${ARGN})
    set_property(GLOBAL APPEND PROPERTY KD_ALL_TESTS "${name}")
    _kd_is_skipped(${name} _skip)
    if(_skip)
        add_test(NAME ${name} COMMAND ${CMAKE_COMMAND} -E echo "TEST SKIPPED: ${name}")
        _kd_apply_version_labels(${name})
        return()
    endif()
    add_executable(${name} ${name}.f90)
    target_link_libraries(${name} PRIVATE ndtreefortran OpenMP::OpenMP_Fortran)
    add_test(NAME ${name} COMMAND ${name})
    _kd_apply_version_labels(${name})
    set_tests_properties(${name} PROPERTIES ENVIRONMENT "OMP_NUM_THREADS=4")
    if(ARG_WILL_FAIL)
        set_tests_properties(${name} PROPERTIES WILL_FAIL TRUE)
    endif()
endfunction()
