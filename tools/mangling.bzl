def hazel_library(package_name):
  """Returns the label of the haskell_library rule for the given package."""
  return "@{}//:{}".format(hazel_workspace(package_name), package_name)


def hazel_binary(package_name):
  """Returns the label of the haskell_binary rule for the given package."""
  return "@{}//:{}_bin".format(hazel_workspace(package_name), package_name)


def hazel_workspace(package_name):
  """Convert a package name to a valid and unambiguous workspace name.

  Makes the name unambiguous to case-insensitive file-systems and
  converts the package name into a valid Bazel workspace name.

  Args:
    package_name: string: Package name.

  Returns:
    string: Workspace name.
  """
  return "haskell_{}".format(
    fixup_package_name(case_insensitive_name(package_name))
  )

def fixup_package_name(package_name):
  """Convert a package name to a valid workspace name.

  Replaces dashes with underscores.

  Args:
    package_name: string: Package name.

  Returns:
    string: A valid workspace name.
  """
  return package_name.replace("-", "_")


def case_insensitive_name(package_name):
  """Convert a package name to a case-insensitive name.

  Appends the hash of the input string, and converts the whole string to
  lower case. Note, the appended hash value is represented in decimal, and
  may be negative.

  Args:
    name: string: A potentially case-sensitive package name.

  Returns:
    string: A case-insensitive package name.
  """
  return "{lower}_{hash}".format(
    lower = package_name.lower(),
    hash = hash(package_name)
  )
