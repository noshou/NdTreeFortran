set(SKIP_TESTS "" CACHE STRING
    "Semicolon-separated list of test names to skip (validated at configure time)")

# Internal global property accumulates every name registered by add_kdtest / add_kdtest_omp.
define_property(GLOBAL PROPERTY KD_ALL_TESTS
    BRIEF_DOCS "All registered KdTree test names"
    FULL_DOCS  "Populated by add_kdtest and add_kdtest_omp during configure")

# Sets out_var to TRUE in the caller's scope if name appears in SKIP_TESTS.
function(_kd_is_skipped name out_var)
    if(SKIP_TESTS AND "${name}" IN_LIST SKIP_TESTS)
        set(${out_var} TRUE  PARENT_SCOPE)
    else()
        set(${out_var} FALSE PARENT_SCOPE)
    endif()
endfunction()

# Call once after all add_subdirectory(tests) calls.
# Errors at configure time if any name in SKIP_TESTS is not a known test.
function(finalize_skip_kd_tests)
    if(NOT SKIP_TESTS)
        return()
    endif()

    get_property(all_tests GLOBAL PROPERTY KD_ALL_TESTS)

    foreach(skip_name IN LISTS SKIP_TESTS)
        if(NOT "${skip_name}" IN_LIST all_tests)
            list(JOIN all_tests "\n  " tests_str)
            message(FATAL_ERROR
                "SKIP_TESTS: unknown test name '${skip_name}'.\n"
                "Known tests:\n  ${tests_str}")
        endif()
    endforeach()

    list(JOIN SKIP_TESTS ", " skipped_str)
    message(STATUS "Skipping tests: ${skipped_str}")
endfunction()
