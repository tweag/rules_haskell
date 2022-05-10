def expand_make_variables(name, ctx, strings, extra_label_attrs):
    # All labels in all attributes should be location-expandable.
    label_attrs = [
        ctx.attr.deps,
    ] + extra_label_attrs

    # Deduplicate targets. Targets could be duplicated between attributes, e.g.
    # srcs and extra_srcs. ctx.expand_location fails if any target occurs
    # multiple times.
    targets = {
        target.label: target
        for attr in label_attrs
        for target in attr
        # expand_location expects a list of targets, but haskell_proto_aspect
        # can inject lists of files instead.
        if hasattr(target, "label")
    }.values()
    strings = [ctx.expand_location(str, targets = targets) for str in strings]
    strings = [ctx.expand_make_variables(name, str, {}) for str in strings]
    return strings

def haskell_library_expand_make_variables(name, ctx, strings):
    extra_attrs = [
        ctx.attr.srcs,
        ctx.attr.extra_srcs,
        ctx.attr.data,
        ctx.attr.plugins,
        ctx.attr.tools,
    ]
    return expand_make_variables(name, ctx, strings, extra_attrs)
