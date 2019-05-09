def hazel_library(package_name):
    """Returns the label of the haskell_library rule for the given package."""
    return "@{}//:lib".format(hazel_workspace(package_name))

def hazel_binary(package_name):
    """Returns the label of the haskell_binary rule for the given package."""
    return "@{}//:bin".format(hazel_workspace(package_name))

def hazel_cbits(package_name):
    """Returns the label of the cc_library rule for the given package."""
    return "@{}//:{}-cbits".format(hazel_workspace(package_name), package_name)

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
        fixup_package_name(case_insensitive_name(package_name)),
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

    An upper case character is converted to its lowercase equivalent followed by
    an underscore.

    A hyphen is converted to two underscores.

    Digits and lowercase characters and left as-is.

    >>> case_insensitive_name("Cabal")
    'c_abal'
    >>> case_insensitive_name("conduit")
    'conduit'
    >>> case_insensitive_name("AERN-Net")
    'a_e_r_n___n_et'
    >>> case_insensitive_name("c2hs-extra")
    'c2hs__extra'

    Args:
      name: string: A potentially case-sensitive package name.

    Returns:
      string: A case-insensitive package name.
    """

    chars = package_name.elems()
    out = []

    for c in chars:
        if c.islower() or c.isdigit():
            out += [c]
        elif c.isupper():
            out += [c.lower(), "_"]
        elif c == "-":
            out += ["_", "_"]
        else:
            fail("Don't know how to handle char %s in %s" % ([c], package_name))

    return "".join(out)
