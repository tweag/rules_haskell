# `rules_haskell` integration testing

This package provides a set of rules for writing test scenarios for `rules_haskell`'s rules testing. It supports:
    * Creating test workspaces inside the `rules_haskell` codebase
    * Writing test scenarios in Haskell and provides a small testing library with helper functions
    * Running tests against multiple bazel versions including binary bazel distributions and nixpkgs bazel packages
    * Reusing the bazel cache between test runs to significantly speedup test process

## How to write an integration test

There is a `rules_haskell_integration_test` rule which allows you to create a test workspace and
write a test-scenario in Haskell.

### Create test workspace

First you need to create a directory for the test workspace:

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
If you want to refer to `rules_haskell` as a dependency you should put this into your test workspace:
```
local_repository(
    name = "rules_haskell",
    path = "%RULES_HASKELL_PATH%"
)
```
The exact path will be set by the test suite.

### Write a test scenario

The test scenario in this example is described in the `SomeTest.hs` file. It is supposed to be a runnable binary which indicates failure or success with its exit status.
There is an `IntegrationTesting` library which provides some useful function to setup the test environment, run bazel commands, and test the output conditions.
The most important of them are:

  1. `setupWorkspace` - prepares a directory structure for the test: directory for test workspace, for bazel output and for `rules_haskell`
                      returns a tuple of path to test workspace and path to bazel output
  2. `bazelCmd workspaceDir bazelOutputDir` - returns a function which creates a process from bazel arguments with respect to test workspace, bazel output and platform configurations
  3. `setupTestBazel` - is a combination of the two functions above which prepares the workspace and returns the bazel-function applied to this workspace

Also there is a bunch of functions for asserting bazel process and it's output on various conditions.
So for example we can create the simplest scenario for `SomeTest.hs`:

`SomeTest.hs`
```haskell
import Test.Hspec (hspec, it)
import IntegrationTesting

main = hspec $ do
  it "bazel test" $ do
    bazel <- setupTestBazel
    assertSuccess $ bazel ["test", "//..."]
```

### Describe test rule

Next you need to put an instance of `rules_haskell_integration_test` in `some-tests-dir/BUILD.bazel`

`BUILD.bazel`
```bzl
load("//tests/integration_testing:rules_haskell_integration_test.bzl", "rules_haskell_integration_test")

rules_haskell_integration_test(
    name = "some_test",
    srcs = ["SomeTest.hs"],
    workspace_path = "some_test",
)
```
By default it will create a test for every bazel version specified in the `SUPPORTED_BAZEL_VERSIONS`-variable defined in `//:bazel_versions.bzl`, and for every bazel nixpkgs package specified in the `SUPPORTED_BAZEL_NIXPKGS_VERSIONS`-variable. To limit the list of bazel versions applied to a test one can use the `bindist_bazel_versions` and `nixpkgs_bazel_packages` parameters.

### Update deleted packages

Since test workspaces contains `WORKSPACE` files it should be ignored by bazel. In order to achieve that one should run
```
bazel run @rules_bazel_integration_test//tools:update_deleted_packages
```
which will update `.bazelrc` with new `deleted_packages` flags. One should push these `.bazelrc` changes with the test. Otherwise, the new test will fail with something like
```
Error in fail: Can not find specified file in [
...
]
```

### Run test

`rules_haskell_integration_test` will create a target for every bazel version it uses with a specific name pattern. For example the following instance of the macro
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
Also, every integration test gets the `integration` tag by default, so one can either refer to the test by name or just run
```
bazel test --test_tag_filters=integration //...
```
