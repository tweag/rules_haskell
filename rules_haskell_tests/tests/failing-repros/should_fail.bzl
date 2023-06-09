load("@rules_haskell//tests:inline_tests.bzl", "sh_inline_test")

def should_fail(name, exitcode, target, **kwargs):
    """Wrap a test target that is expected to fail.
    This is particularly helpful for repros which reproduce an existing bug.
    If the bug is fixed by a code change (deliberately or not), `should_fail`
    tests start to fail and test can go into the normal test suite.
    """
    sh_inline_test(
        name = name,
        size = "small",
        args = ["$(rootpaths {})".format(target)],
        data = [target],
        script = """
# not sure why -e is set in the first place,
# but disable it because $1 is expected to fail.
set +e
"$1"
ret=$?
set -e
if [ $ret -ne {exitcode} ]; then
  echo "ERROR: expected $1 to fail with exitcode {exitcode}, failed with $ret" 2>&1
  exit 1
fi
exit 0
        """.format(
            exitcode = exitcode,
        ),
        **kwargs
    )
