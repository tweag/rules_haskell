# `rules_haskell` integration testing

This package provides set of rules for writing test scenarios for `rules_haskell`'s rules testing. It supports:
    * Creating test workspaces inside `rules_haskell` codebase
    * Writing test scenarios in Haskell and provides small testing library with helper functions
    * Running test against multiple bazel verions including binary bazel distributions and nixpkgs bazel packages
    * Reusing bazel cache between test runs to significantly speedup test process

## How to write an integration test

There is a `rules_haskell_integration_test` rule which allows you to create a test workspace and
write a test-scenario in Haskell.

### Create test workspace

First you need to create directory for test workspace:

```
some-tests-dir
|
+-- some_test_workspace
|   |
|   +-- WORKSPACE
|   +-- BUILD.bazel
|   +-- # some other stuff needed in your test
|
+-- BUILD.bazel
|
+-- SomeTest.hs
...
```
If you want to refer  `rules_haskell` as a dependency you should put this in your test workspace:
```
local_repository(
    name = "rules_haskell",
    path = "%RULES_HASKELL_PATH%"
)
```
Exact path will be set by the test suite

### Write a test scenario

Test scenario in this example is described in `SomeTest.hs` file. It is supposed to be a runable binary which could fail or success with it exit status.
There is an `IntegrationTesting` library which provides some useful function to setup test environment, run bazel commands and test output conditions.
The most important of them are:

  1. setupWorkspace - prepares a directory structure for the test: directory for test workspace, for bazel output and for `rules_haskell`
                      returns a tuple of path to test workspace and path to bazel output
  2. bazelCmd workspaceDir bazelOutputDir - returns a function which creates process from bazel arguments with respect to test workspace, bazel output and platform configurations
  3. setupTestBazel is a combination of two functions above which prepares the workspace and returns bazel-function applied to this workspace

Also there is a bunch of functions for asserting bazel process and it's output on various conditions.
So for example we can create the simplest scenario for SomeTest.hs

```SomeTest.hs
import IntegrationTesting

main = do
    bazel <- setupTestBazel
    assertSuccess $ bazel ["test", "//..."]
```

### Describe test rule

Next you need to put an instance of `rules_haskell_integration_test` in `some-tests-dir/BUILD.bazel`

```BUILD.bazel
load("//tests/integration_testing:rules_haskell_integration_test.bzl", "rules_haskell_integration_test")

rules_haskell_integration_test(
    name = "some_test",
    srcs = ["SomeTest.hs"],
    workspace_path = "some_test",
)
```
By default it will create a test for every bazel version specified in `//:bazel_versions.bzl` `SUPPORTED_BAZEL_VERSIONS`-variable, and for every bazel nixpkgs package specified in `SUPPORTED_BAZEL_NIXPKGS_VERSIONS`-variable. To limit the list of bazel versions applied to a test one can use `bindist_bazel_versions` and `nixpkgs_bazel_packages` parameters.

### Update deleted packages

Since test workspaces contains `WORKSPACE` files it should be ignored by bazel. In order to achieve that one should run
```
bazel run @contrib_rules_bazel_integration_test//tools:update_deleted_packages
```
which will update .bazelrc with new `deleted_packages`. One should push this bazelrc changes with the test. Otherwise new test will fail with something like
```
Error in fail: Can not find specified file in [
...
]
```

### Run test

`rules_haskell_integration_test` will create a target for every bazel version it uses with specific name pattern. For example the instance of macro
```
rules_haskell_integration_test(
    name = "some_test",
    srcs = ["SomeTest.hs"],
    workspace_path = "some_test",
    bindist_bazel_versions = ["4.1.0"],
    nixpkgs_bazel_package = ["bazel_4"],
)
```
will create two test targets: `some_test_bindist_4.1.0` and `some_test_nixpkgs_bazel_4`
Also every integration tests gets the `integration` tag by default, so one can either refer test by name or just run
```
bazel test --test_tag_filters=integration //...
```
