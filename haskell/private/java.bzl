"""Interop with Java."""

load("@bazel_skylib//lib:collections.bzl", "collections")

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

    inputs = depset(
        transitive = [
            # We only expose direct dependencies, though we could
            # expose transitive ones as well. Only exposing the direct
            # ones corresponds to Bazel's "strict Java dependencies"
            # mode. See
            # https://github.com/tweag/rules_haskell/issues/96.
            dep[JavaInfo].compile_jars
            for dep in ctx.attr.deps
            if JavaInfo in dep
        ],
    )

    env_dict = dict()
    uniq_classpath = collections.uniq([
        f.path
        for f in inputs
    ])

    if len(uniq_classpath) > 0:
        env_dict["CLASSPATH"] = ":".join(uniq_classpath)

    return JavaInteropInfo(
        inputs = inputs,
        env = env_dict,
    )
