
To reproduce issue 1365:

- Enter the env from Dockerfile in this dir
- bazel build //tests/repro-1365/...

Note this test passes in CI (which does not use Centos).
