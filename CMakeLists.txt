cmake_minimum_required(VERSION 3.20)

project(KdTree LANGUAGES Fortran)
find_package(OpenMP REQUIRED)

add_compile_options(-O3)
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION ON)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include(SkipKdTests)
include(KdTestVersion)
include(AddKdTest)
include(AddKdTestOmp)

add_subdirectory(src)

option(BUILD_TESTS "Build tests" OFF)
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
    finalize_skip_kd_tests()
endif()

