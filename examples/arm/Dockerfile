FROM ubuntu@sha256:0f744430d9643a0ec647a4addcac14b1fbb11424be434165c15e2cc7269f70f8 AS nix

RUN adduser builder \
    && install -d -m755 -o builder -g builder /nix \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
         ca-certificates=20211016 curl=7.81.0-1ubuntu1.3 git=1:2.34.1-1ubuntu1.2 xz-utils=5.2.5-2ubuntu1 \
    && rm -rf /var/lib/apt/lists/*

USER builder
ENV USER=builder

RUN curl -L https://nixos.org/nix/install | sh

# ----------------------------------------------------------------------

FROM nix AS builder

COPY . /build/

WORKDIR /build/example/arm

RUN nix-shell --pure --run "bazel build //... --config linux-nixpkgs"

RUN nix-shell --pure --run "bazel-bin/example"
RUN nix-shell --pure --run "bazel-bin/example2"
