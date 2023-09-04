"""Utilities for GHC plugins."""

def resolve_plugin_tools(ctx, plugin_info):
    """Convert a plugin provider to a struct with tools resolved to inputs."""
    (tool_inputs, tool_input_manifests) = ctx.resolve_tools(tools = plugin_info.tools)
    return struct(
        module = plugin_info.module,
        deps = plugin_info.deps,
        args = plugin_info.args,
        tool_inputs = tool_inputs,
        tool_input_manifests = tool_input_manifests,
    )
