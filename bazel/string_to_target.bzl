load("@bazel_skylib//lib:shell.bzl", "shell")

def quote_make_variables(s):
    """Quote all genrule “Make” Variables in a string."""
    return s.replace("$", "$$")

def string_to_target(name, content):
    """Write a skylark string to a target.

    Example:
      ```
      string_to_target(
        name = "mytarget.txt",
        content = "abc",
      )
      ```
      creates a target `mytarget.txt` and a file with the same name
      which has the content `abc`.
    """
    native.genrule(
        # the genrule has to have a different name
        name = name + "-genrule",
        # but the actual file can use the name we pass from the outside
        outs = [name],
        # this is exceptionally ugly.
        cmd = """echo -n {quoted} > $(@)""".format(
            # but should at least be quoted right
            quoted = shell.quote(quote_make_variables(content)),
        ),
    )
