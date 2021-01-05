#!/usr/bin/env python3

import collections
import types

def unfold_fields(lines):
    """Unfold fields that were split over multiple lines.

    Returns:
        A list of strings. Each string represents one field (a name/value pair
        separated by a colon).

    >>> unfold_fields("foo  \n   bar  \n   baz  \nbiz   \nboz   ")
    ['foo     bar     baz  ', 'biz   ', 'boz   ']
    """
    fields = []
    for line in lines:
        if line.startswith(" "):
            fields[-1] += line
        elif line.strip():
            fields.append(line)
    return fields

PackageConfiguration = collections.namedtuple("PackageConfiguration", [
    "name",
    "version",
    "id",
    "include_dirs",
    "library_dirs",
    "dynamic_library_dirs",
    "hs_libraries",
    "depends",
    "ld_options",
    "extra_libraries",
    "haddock_interfaces",
    "haddock_html",
])

def parse_package_configuration(lines):
    """Parses a single package configuration.

    Returns:
      An instance of `PackageConfiguration`.
    """
    pkg = types.SimpleNamespace(
        include_dirs = [],
        library_dirs = [],
        dynamic_library_dirs = [],
        depends = [],
        hs_libraries = [],
        ld_options = [],
        extra_libraries = [],
        haddock_interfaces = [],
        haddock_html = None,
    )
    for field in unfold_fields(lines):
        key, value = field.split(":", 1)
        value = value.strip()
        if key == "name":
            pkg.name = value
        elif key == "version":
            pkg.version = value
        elif key == "id":
            pkg.id = value
        elif key == "include-dirs":
            pkg.include_dirs += value.split()
        elif key == "library-dirs":
            pkg.library_dirs += value.split()
        elif key == "dynamic-library-dirs":
            pkg.dynamic_library_dirs += value.split()
        elif key == "hs-libraries":
            pkg.hs_libraries += value.split()
        elif key == "depends":
            pkg.depends += value.split()
        elif key == "ld-options":
            pkg.ld_options += [opt.strip('"') for opt in value.split()]
        elif key == "extra-libraries":
            pkg.extra_libraries += value.split()
        elif key == "haddock-interfaces":
            pkg.haddock_interfaces += value.split()
        elif key == "haddock-html":
            pkg.haddock_html = value

    return PackageConfiguration(**pkg.__dict__)

def split_records(lines):
    """Iterator over lists of lines separated by `---`.

    Skips empty records.
    """
    separator = "---"
    record = []
    for line in lines:
        if line.rstrip() == separator:
            yield record
            record = []
        else:
            record.append(line)
    if record:
        yield record

def parse_package_database_dump(lines):
    """Parse the output of ghc-pkg dump.

    Assumes that records are separated by `---`.

    Returns:
      Iterable of `PackageConfiguration`.
    """
    return (
        parse_package_configuration(record)
        for record in split_records(lines)
    )
