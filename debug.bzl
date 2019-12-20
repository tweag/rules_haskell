def _debug(repository_ctx):
    print("BAZEL_SH", repository_ctx.os.environ.get("BAZEL_SH", "UNDEFINED"))
    print("WINDIR", repository_ctx.os.environ.get("WINDIR", "UNDEFINED"))
    print("PATH", repository_ctx.os.environ.get("PATH", "UNDEFINED"))
    print("bash.exe", str(repository_ctx.which("bash.exe")))
    repository_ctx.file("BUILD.bazel")

debug = repository_rule(_debug)
