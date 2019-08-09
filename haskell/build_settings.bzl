UseWorkerProvider = provider(fields = ['use_worker'])

def _impl(ctx):
    print(ctx.build_setting_value) # prints False even though I pass --//haskell:use_worker_setting=True
    return UseWorkerProvider(use_worker = ctx.build_setting_value)

use_worker_setting = rule(
    implementation = _impl,
    build_setting = config.bool(flag = True)
)
