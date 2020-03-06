load("//tests:inline_tests.bzl", "sh_inline_test")

def shellcheck(name, args, data, sh_flavor = "sh", excludes = [], visibility = None):
    excludes_arg = "--exclude=" + ",".join(excludes) if excludes else ""
    shell_arg = "--shell " + sh_flavor if sh_flavor else ""
    sh_inline_test(
        name = name,
        visibility = visibility,
        size = "small",
        args = args,
        data = data,
        script = """\
shellcheck --color=always {} {} $1
""".format(shell_arg, excludes_arg),
        tags = ["requires_shellcheck"],
    )
