"""Interop with Java."""

load("@bazel_skylib//:lib.bzl", "collections")

JavaInteropInfo = provider(
    doc = "Information needed for interop with Java rules.",
    fields = {
        "inputs": "Files needed during build.",
        "env": "Dict with env variables that should be set during build.",
    },
)

def java_interop_info(ctx):
    """Gather information from any Java dependencies.

    Args:
      ctx: Rule context.

    Returns:
      JavaInteropInfo: Information needed for Java interop.
    """

    inputs = []
    for dep in ctx.attr.deps:
        if JavaInfo in dep:
            for f in dep.files.to_list():
                # Crashes!
                # print(dep[JavaInfo].transitive_compile_time_jars)
                if f.extension == "jar":
                    inputs.append(f)

    env_dict = dict()
    uniq_classpath = collections.uniq([
        f.path
        for f in inputs
    ])

    if len(uniq_classpath) > 0:
        env_dict["CLASSPATH"] = ":".join(uniq_classpath)

    return JavaInteropInfo(
        inputs = depset(inputs),
        env = env_dict,
    )
