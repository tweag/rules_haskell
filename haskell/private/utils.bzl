"""Misc utils."""

load("@bazel_skylib//:lib.bzl",
     "paths",
)

def get_lib_name(lib):
  """Return name of library by dropping extension and \"lib\" prefix.

  Args:
    lib: The library File.

  Returns:
    String: name of library.
  """

  base = lib.basename[3:] if lib.basename[:3] == "lib" else lib.basename
  n = base.find(".so.")
  end = paths.replace_extension(base, "") if n == -1 else base[:n]
  return end
