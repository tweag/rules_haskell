load("//tests:inline_tests.bzl", "sh_inline_test")

def shellcheck(name, args, data, sh_flavor = "sh", visibility = None):
    fst_args = ["--color=always"]
    if sh_flavor != None:
        fst_args += ["--shell", sh_flavor]
    sh_inline_test(
        name = name,
        visibility = visibility,
        size = "small",
        args = fst_args + args,
        data = data,
        script = """\
shellcheck "$@"
""",
        tags = ["requires_shellcheck"],
    )
