# Go SDK

We cannot use the Go SDK bindist on NixOS so one is packaged in `shell.nix` and
we tell the Go tools to use the "host" SDK. There is no `shell.nix` on Windows
so we would like `rules_go` to download an SDK for us.

In order to achieve those different behaviors in the same codebase we generate
the import code based on the platform:

* Linux, darwin: use the host-provided (`shell.nix`) SDK.
* Windows: download an official distribution.
