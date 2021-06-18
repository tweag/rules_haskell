load("//haskell:private/pkg_id.bzl", "pkg_id")

def _package_name_out_transition_impl(settings, attr):
    package_name = getattr(attr, "package_name", None)
    version = getattr(attr, "version", None)

    # we don't have direct access to ctx.label here, so we've recreated it in
    # "attr.label". See //haskell:defs.bzl
    label = Label(attr.label_string)
    my_pkg_id = pkg_id.new(label, package_name, version)
    return {"//haskell/experimental:package_name_setting": pkg_id.to_string(my_pkg_id)}

package_name_out_transition = transition(
    implementation = _package_name_out_transition_impl,
    inputs = [],
    outputs = ["//haskell/experimental:package_name_setting"],
)
