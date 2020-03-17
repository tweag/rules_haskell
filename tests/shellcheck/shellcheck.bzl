load("//tests:inline_tests.bzl", "sh_inline_test")
load("@bazel_skylib//lib:shell.bzl", "shell")

def shellcheck(name, args, data, sh_flavor = "sh", visibility = None):
    cmd = ["shellcheck", "--color=always"]
    if sh_flavor != None:
        cmd += ["--shell", sh_flavor]
    sh_inline_test(
        name = name,
        visibility = visibility,
        size = "small",
        args = args,
        data = data,
        script = """\
{} "$1"
""".format(" ".join([shell.quote(x) for x in cmd])),
        tags = ["requires_shellcheck"],
    )
