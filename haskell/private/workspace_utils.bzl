load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")

def execute_or_fail_loudly(
        repository_ctx,
        arguments,
        environment = {},
        working_directory = ""):
    """Execute the given command

    Fails if the command does not exit with exit-code 0.

    Args:
      arguments: List, the command line to execute.

    Returns:
      exec_result: The output of the command.

    """
    exec_result = repository_ctx.execute(
        arguments,
        environment = environment,
        quiet = True,
        working_directory = working_directory,
    )
    if exec_result.return_code != 0:
        arguments = [_as_string(x) for x in arguments]
        fail("\n".join(["Command failed: " + " ".join(arguments), exec_result.stderr]))
    return exec_result

def _as_string(v):
    if type(v) == "string":
        return v
    else:
        return repr(v)

def find_python(repository_ctx):
    python = repository_ctx.which("python3")
    if not python:
        python = repository_ctx.which("python")
        if not python:
            fail("There is no Python in PATH. Please install Python >= 3.3.")
        result = execute_or_fail_loudly(repository_ctx, [python, "--version"])
        if not result.stdout.startswith("Python 3"):
            fail("rules_haskell requires Python >= 3.3. (found {})".format(result.stdout))
    return python

def resolve_labels(repository_ctx, labels):
    """
    Avoid rule restart by resolving these labels early. See
    https://github.com/bazelbuild/bazel/blob/master/tools/cpp/lib_cc_configure.bzl#L17.

    Args:
      repository_ctx: The context with which to resolve the labels.
      labels: Labels to be resolved expressed as a list of strings.

    Returns:
      A dictionary with the labels as keys and their paths as values.
    """
    return dict([(label, repository_ctx.path(Label(label))) for label in labels])

def define_rule(rule_type, name, **kwargs):
    """Generate a string representing a rule definition.

    Take care to escape string values using repr().

    ### Examples

      ```bzl
      define_rule("myrule",
          name = "foo",
          myattr1 = repr("bar"),
          myattr2 = ["baz"],
      )
      ```
    """
    attrs = ["{} = {},".format(k, v) for k, v in kwargs.items() if v != None]
    skeleton = """\
{rule_type}(
    name = {name},
    {attrs}
)
"""
    return skeleton.format(
        rule_type = rule_type,
        name = repr(name),
        attrs = "\n    ".join(attrs),
    )
