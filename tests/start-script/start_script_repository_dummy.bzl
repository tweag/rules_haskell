# use the start script to construct a workspace
def _start_script_repository_dummy_impl(rep_ctx):
    script_path = rep_ctx.path(Label("//:start"))
    res = rep_ctx.execute([script_path])

start_script_repository_dummy = repository_rule(
    _start_script_repository_dummy_impl,
    local = True,
)
