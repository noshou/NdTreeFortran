# **Tests**

Tests are organized by version. Every version directory is an independent CMake subdirectory added in `tests/CMakeLists.txt`. All tests from all versions run together under a single CTest call.

## **Build and run**

Tests are not built by default. Pass `-DBUILD_TESTS=ON` to enable them:

```bash
cmake -B build -DBUILD_TESTS=ON
cmake --build build
ctest --test-dir build --output-on-failure
```

Note: toggling `BUILD_TESTS` on an existing build directory leaves stale `CTestTestfile.cmake` files; do a clean configure (`rm -rf build`) when switching from ON to OFF. \
\
**IT IS HIGHLY RECOMMENDED TO SKIP Testv060_DBSCAN_1M_ALL_NOISE**

```bash
cmake -B build -DBUILD_TESTS=ON \
    -DSKIP_TESTS="Testv060_DBSCAN_1M_ALL_NOISE;Testv060_DBSCAN_1M_SINGLE_CLUSTER"
cmake --build build
ctest --test-dir build --output-on-failure
```

`Testv060_DBSCAN_1M_SINGLE_CLUSTER` builds a 1M-point single-cluster DBSCAN case and can take well over 2 hours to complete on a performant system.
`Testv060_DBSCAN_1M_NOISE` takes a few minutes, but can cause bottlenecks on slower systems.

Unless you are specifically testing that code path, skip it. For more on skipping individual tests, see the relevant section.

## **Skipping and Selecting Tests**

### **Selecting Multithreaded Tests**

To run only the multithreaded tests:

```bash
ctest --test-dir build -R "MULTITHREAD" --output-on-failure
```

Multithreaded tests hardcode `NUM_THREADS(4)` in their `!$OMP PARALLEL DO` directives. The `OMP_NUM_THREADS` environment variable is ignored for those loops; however, OpenMP's thread pool is still initialised from the environment before the directives run, so setting `OMP_NUM_THREADS=4` (or higher) in advance avoids pool-resize overhead:

```bash
OMP_NUM_THREADS=4 ctest --test-dir build -R "MULTITHREAD" --output-on-failure
```

On machines with fewer than 4 physical threads the tests still pass -> OpenMP over-subscribes transparently -> but runtime will be higher.

### **Selecting by Version**

Each test is labelled with its major version line and its exact release version at configure time. Use `ctest -L` to filter by label:

```bash
# All v0 tests
ctest --test-dir build -L "^v0$" --output-on-failure

# Only v0.2.1 tests
ctest --test-dir build -L "^v0\.2\.1$" --output-on-failure

# Only v0.6.0 tests
ctest --test-dir build -L "^v0\.6\.0$" --output-on-failure
```

Labels are derived automatically from the test name (`Testv{A}{B}{C}_*` -> `v{A}` and `v{A}.{B}.{C}`). Every test carries both labels, so `-L "^v0$"` matches all v0.x.y tests and `-L "^v0\.2\.1$"` matches only that release.

### **Skipping tests**

Pass `-DSKIP_TESTS` at configure time with a semicolon-separated list of test names.

Skipped tests still appear in the CTest run, but will not be compiled and treated as "passed".

If any name in `SKIP_TESTS` does not match a registered test, CMake exits with a fatal error at configure time listing all known test names.

## **CMake framework**

### **Registering tests in a subdirectory**

Tests are registered with macros defined in `cmake/AddKdTest.cmake`, `cmake/AddKdTestOmp.cmake`, `cmake/SkipKdTests.cmake`, and `cmake/KdTestVersion.cmake`:

```cmake
add_kdtest(TestName)              # single-threaded test: passes on exit 0
add_kdtest(TestName WILL_FAIL)    # inverted test: passes on non-zero exit
add_kdtest_omp(TestName)          # OpenMP test: links OpenMP, sets OMP_NUM_THREADS=4
add_kdtest_omp(TestName WILL_FAIL)
```

Each call creates a standalone executable from `TestName.f90`, links it against `kdtree`, and registers it with CTest. The executable name and the CTest test name are both `TestName`. Version labels (`v{A}` and `v{A}.{B}.{C}`) are applied automatically from the test name.

Use `add_kdtest_omp` when the test program itself contains `!$OMP` directives. The `kdtree` library links OpenMP privately (it is not propagated to dependents), so test executables that use OpenMP must link it explicitly via this macro. Tests that only call the library -> even if the library uses OpenMP internally -> do not need `add_kdtest_omp` and should use `add_kdtest` instead.

`WILL_FAIL` is used for tests that verify error guards, where the program is expected to call `error stop`. CTest inverts the pass/fail logic: the test passes only when the program exits non-zero.

**Compilation can take a long time; build once and update only when needed.**

### Conventions

#### **One assertion per file**

Each test file has exactly one `stop 1` path (or one `error stop` for WILL_FAIL tests).

#### **Naming**

* Concurrent tests
  * `Testv{VERSION}_{WHAT_IS_TESTED}.f90`
* Multithreaded Tests
  * `Testv{VERSION}_MULTITHREAD_{WHAT_IS_TESTED}.f90`

##### **Error output**

On failure, print `'--- Testv{VERSION}_{WHAT_IS_TESTED} ---'` then the expected vs. actual values before `stop 1`.

For error guards, programs are expected to fail at some point, so the program should print stdout at the end of successful execution.

##### **Order Matters**

add_kdtest and add_subdirectory execute/open the respective tests/directories sequentially.

### Versions

Each release has a directory, and each version has its own set subdirectory. Each version contains subdirectories, one per unit under test.

Each subdirectory has its own `CMakeLists.txt` and holds all the `.f90` files for that unit. The subdirectory name matches the common prefix of all test files within it. New versions must pass all regresison tests.
