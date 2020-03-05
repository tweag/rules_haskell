load("//tests:inline_tests.bzl", "sh_inline_test")

def shellcheck(name, args, data, sh_flavor = None, excludes = [], visibility = None):
    excludes_arg = "" if len(excludes) == 0 else ("--exclude=" + ",".join(excludes))
    shell_arg = "" if (sh_flavor == None) else ("--shell " + sh_flavor)
    sh_inline_test(
        name = name,
        visibility = visibility,
        size = "small",
        args = args,
        data = data,
        script = """\
shellcheck --color=always %s %s $1
""" % (shell_arg, excludes_arg),
        tags = ["requires_shellcheck"],
    )
