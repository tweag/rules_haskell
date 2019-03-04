load(
    "//:tests/inline_tests.bzl",
    "py_inline_test",
)

#
def ldd_test(name, elf_binary, script, current_workspace = None, tags = []):
    """Test with imported linking_utils.ldd library.
    The path to the `elf_binary` is passed in sys.argv[1].
    """
    py_inline_test(
        name,
        deps = ["@io_tweag_rules_haskell//debug/linking_utils"],
        data = [elf_binary],
        args = ["{}/$(rootpath {})".format(current_workspace, elf_binary)] if current_workspace else ["$(rootpath {})".format(elf_binary)],
        script = """
from io_tweag_rules_haskell.debug.linking_utils.ldd import \\
        dict_remove_empty, identity, const, \\
        LDD_MISSING, LDD_UNKNOWN, LDD_ERRORS, \\
        ldd, \\
        remove_matching_needed, remove_matching_runpaths, \\
        non_existing_runpaths, unused_runpaths, \\
        collect_unused_runpaths
""" + script,
        tags = tags,
    )
