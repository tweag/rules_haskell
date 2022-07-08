FROM ubuntu@sha256:0f744430d9643a0ec647a4addcac14b1fbb11424be434165c15e2cc7269f70f8 AS nix

ADD https://github.com/bazelbuild/bazelisk/releases/download/v1.12.0/bazelisk-linux-arm64 /usr/bin/bazelisk

RUN adduser builder \
    && install -d -m755 -o builder -g builder /nix \
    && chmod +x /usr/bin/bazelisk \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
         ca-certificates=20211016 curl=7.81.0-1ubuntu1.3 git=1:2.34.1-1ubuntu1.2 xz-utils=5.2.5-2ubuntu1 \
    && rm -rf /var/lib/apt/lists/*

# TODO: add extra substituters
#    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
#    extra-substituters = https://hydra.iohk.io


USER builder
ENV USER=builder

RUN curl -L https://nixos.org/nix/install | sh

# ----------------------------------------------------------------------

FROM nix AS builder

COPY . /build/

WORKDIR /build/examples/arm

# running from qemu only works in Bazel 5.2+, see https://github.com/bazelbuild/bazel/pull/15223
ENV USE_BAZEL_VERSION 5.2.0

RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
    && nix-shell --pure --keep USE_BAZEL_VERSION --run "bazelisk build //... --config linux-nixpkgs"

RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
    && nix-shell --pure --run "bazel-bin/example"

RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
    && nix-shell --pure --run "bazel-bin/example2"
