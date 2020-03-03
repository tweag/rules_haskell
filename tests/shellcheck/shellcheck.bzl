load("//tests:inline_tests.bzl", "sh_inline_test")

def shellcheck(name, args, data, sh_flavor="bash", visibility=None):
  sh_inline_test(
    name = "shellcheck-%s" % name,
    visibility = visibility,
    size = "small",
    args = args,
    data = data,
    script = """\
shellcheck --color=always "--shell=%s" $1
""" % sh_flavor,
    tags = ["requires_shellcheck"]
  )
