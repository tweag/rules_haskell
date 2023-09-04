This directory contains an example of how to cross-compile to arm
with `rules_haskell`.

Build with
```
cd examples/arm
nix-shell --pure --run "bazel build //... --platforms=//:linux_aarch64 --config linux-nixpkgs"
```

Run with
```
cd examples/arm
nix-shell --pure qemu-shell.nix --run "qemu-aarch64 bazel-bin/example"
nix-shell --pure qemu-shell.nix --run "qemu-aarch64 bazel-bin/example2"
```

## Configuration remarks

Cross compilation of TH is accomplished by running the external
interpreter with qemu. This is arranged in arm-cross.nix by using
[haskell.nix][haskell-nix].

The cross-compiler provided by haskell.nix provides static core
libraries only, therefore it is needed to configure the cross
toolchain with `static_runtime = True`.

Note that `examples/arm/.bazelrc` contains
```
build --incompatible_enable_cc_toolchain_resolution
```
This is necessary for bazel to supply the CC cross toolchain to haskell
rules at a few places.

[haskell-nix]: https://github.com/input-output-hk/haskell.nix/
