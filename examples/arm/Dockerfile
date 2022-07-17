FROM ubuntu:focal AS nix

ADD https://github.com/bazelbuild/bazelisk/releases/download/v1.12.0/bazelisk-linux-arm64 /usr/bin/bazelisk
ADD https://github.com/commercialhaskell/stack/releases/download/v2.7.5/stack-2.7.5-linux-x86_64-static.tar.gz /tmp/stack.tgz

RUN adduser builder \
    && install -d -m755 -o builder -g builder /nix \
    && chmod +x /usr/bin/bazelisk \
    && mkdir -p /opt/stack \
    && tar xaf /tmp/stack.tgz --strip-components=1 -C /opt/stack \
    && /opt/stack/stack --version \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
         ca-certificates curl git xz-utils \
    && rm -rf /var/lib/apt/lists/*

# TODO: add extra substituters
#    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
#    extra-substituters = https://hydra.iohk.io


USER builder
ENV USER=builder
SHELL ["/bin/bash", "-c"]

RUN \
    echo $'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=\nextra-substituters = https://hydra.iohk.io' >> /home/builder/extra.conf \
    && bash <( curl -L https://nixos.org/nix/install ) --no-channel-add --nix-extra-conf-file "/home/builder/extra.conf"

# ----------------------------------------------------------------------

COPY --chown=builder . /build/

WORKDIR /build/examples/arm

# running from qemu only works in Bazel 5.2+, see https://github.com/bazelbuild/bazel/pull/15223
ENV USE_BAZEL_VERSION 5.2.0

RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
    && nix-shell --pure --keep USE_BAZEL_VERSION --run "/usr/bin/bazelisk build //... --config linux-nixpkgs"

RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
    && nix-shell --pure --run "bazel-bin/example"

RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
    && nix-shell --pure --run "bazel-bin/example2"
