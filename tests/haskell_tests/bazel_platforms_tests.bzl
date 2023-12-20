"""Tests for `bazel_platforms` module."""

load("@bazel_skylib//lib:unittest.bzl", "asserts", "unittest")
load("//haskell:private/bazel_platforms.bzl", "bazel_platforms")

def _get_os_test(ctx):
    env = unittest.begin(ctx)

    tests = [
        struct(
            msg = "darwin (legacy MacOS on x86_64)",
            os_cpu = "darwin",
            exp = "osx",
        ),
        struct(
            os_cpu = "darwin_x86_64",
            exp = "osx",
        ),
        struct(
            os_cpu = "darwin_arm64",
            exp = "osx",
        ),
        struct(
            os_cpu = "arm64_windows",
            exp = "windows",
        ),
        struct(
            os_cpu = "x64_windows",
            exp = "windows",
        ),
        struct(
            os_cpu = "freebsd",
            exp = "linux",
        ),
        struct(
            os_cpu = "openbsd",
            exp = "linux",
        ),
    ]
    for t in tests:
        actual = bazel_platforms.get_os(t.os_cpu)
        msg = getattr(t, "msg", t.os_cpu)
        asserts.equals(env, t.exp, actual, msg)

    return unittest.end(env)

get_os_test = unittest.make(_get_os_test)

def _get_cpu_test(ctx):
    env = unittest.begin(ctx)

    tests = [
        struct(
            msg = "darwin (legacy MacOS on x86_64)",
            os_cpu = "darwin",
            exp = "x86_64",
        ),
        struct(
            os_cpu = "darwin_x86_64",
            exp = "x86_64",
        ),
        struct(
            os_cpu = "darwin_arm64",
            exp = "arm64",
        ),
        struct(
            os_cpu = "arm64_windows",
            exp = "arm64",
        ),
        struct(
            os_cpu = "x64_windows",
            exp = "x86_64",
        ),
        struct(
            os_cpu = "freebsd",
            exp = "x86_64",
        ),
        struct(
            os_cpu = "openbsd",
            exp = "x86_64",
        ),
    ]
    for t in tests:
        actual = bazel_platforms.get_cpu(t.os_cpu)
        msg = getattr(t, "msg", t.os_cpu)
        asserts.equals(env, t.exp, actual, msg)

    return unittest.end(env)

get_cpu_test = unittest.make(_get_cpu_test)

def bazel_platforms_test_suite(name = "bazel_platforms_tests"):
    return unittest.suite(
        name,
        get_os_test,
        get_cpu_test,
    )
