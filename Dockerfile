# syntax=docker/dockerfile:1

# Toolchain layout follows blinklabs-io/docker-haskell, but pins GHC 9.6:
# the version cardano-node 11.1.3 (and so the pinned ledger, consensus and
# network libraries) supports. That image ships GHC 9.12.
FROM debian:bookworm-slim AS toolchain

ARG GHC_VERSION=9.6.7
ARG CABAL_VERSION=3.14.2.0
ARG GO_VERSION=1.27.4
ARG LIBSODIUM_REF=dbb48cce
ARG SECP256K1_REF=v0.3.2
ARG BLST_REF=v0.3.14

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    DEBIAN_FRONTEND=noninteractive \
    LD_LIBRARY_PATH=/usr/local/lib \
    PKG_CONFIG_PATH=/usr/local/lib/pkgconfig \
    PATH=/usr/local/go/bin:/root/.local/bin:/root/.cabal/bin:/usr/local/bin:/usr/bin:/bin

RUN apt-get update -y && apt-get install -y --no-install-recommends \
      autoconf automake autotools-dev build-essential ca-certificates curl g++ git jq \
      libffi-dev libgmp-dev liblmdb-dev libncursesw5 libnuma-dev \
      libsnappy-dev libssl-dev libsystemd-dev libtinfo-dev libtool \
      liburing-dev llvm-dev make pkg-config wget xz-utils zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/build

# GHC 9.6 publishes no deb12 bindist; the deb10 (aarch64) and deb11
# (x86_64) builds run on bookworm.
RUN case "$(uname -m)" in aarch64) dist=deb10 ;; *) dist=deb11 ;; esac \
    && wget -q https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-$(uname -m)-${dist}-linux.tar.xz \
    && tar -xf ghc-${GHC_VERSION}-$(uname -m)-${dist}-linux.tar.xz \
    && cd ghc-${GHC_VERSION}-$(uname -m)-unknown-linux \
    && ./configure && make install \
    && cd /opt/build && rm -rf ghc-${GHC_VERSION}-*

RUN wget -q https://downloads.haskell.org/~cabal/cabal-install-${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-$(uname -m)-linux-deb12.tar.xz \
    && tar -xf cabal-install-${CABAL_VERSION}-$(uname -m)-linux-deb12.tar.xz \
    && mkdir -p /root/.local/bin && mv cabal /root/.local/bin/ \
    && rm cabal-install-${CABAL_VERSION}-*.tar.xz && cabal update

RUN wget -q https://go.dev/dl/go${GO_VERSION}.linux-$(dpkg --print-architecture).tar.gz \
    && tar -C /usr/local -xzf go${GO_VERSION}.linux-$(dpkg --print-architecture).tar.gz \
    && rm go${GO_VERSION}.linux-$(dpkg --print-architecture).tar.gz

# Cardano crypto C libraries, as in blinklabs-io/docker-haskell. libsodium's
# autogen.sh fetches config.sub at build time; the fetched copy does not run
# here, so Debian's copies replace it.
RUN git clone https://github.com/input-output-hk/libsodium && cd libsodium \
    && git checkout ${LIBSODIUM_REF} && ./autogen.sh \
    && cp /usr/share/misc/config.guess /usr/share/misc/config.sub build-aux/ \
    && ./configure && make && make install \
    && cd /opt/build && rm -rf libsodium
RUN git clone https://github.com/bitcoin-core/secp256k1 && cd secp256k1 \
    && git checkout ${SECP256K1_REF} && ./autogen.sh \
    && ./configure --enable-module-schnorrsig --enable-experimental && make && make install \
    && cd /opt/build && rm -rf secp256k1
COPY docker/libblst.pc /usr/local/lib/pkgconfig/
RUN git clone https://github.com/supranational/blst && cd blst \
    && git checkout ${BLST_REF} && ./build.sh \
    && cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp /usr/local/include/ \
    && cp libblst.a /usr/local/lib/ && cd /opt/build && rm -rf blst

WORKDIR /src

# hs-tools is the dev image for `make docker-test-hs`: the toolchain plus
# proto-lens-protoc and a buf binary, without project dependencies (those
# live in the dev container's cabal store volume).
FROM toolchain AS hs-tools
ARG BUF_VERSION=v1.74.0
COPY haskell/cabal.project haskell/
COPY haskell/dxnode-hs/dxnode-hs.cabal haskell/dxnode-hs/
COPY Makefile ./
RUN --mount=type=cache,id=dxdingo-cabal-store,target=/root/.local/state/cabal \
    cd haskell && cabal update && cd .. && make tools-hs
RUN GOBIN=/usr/local/bin go install github.com/bufbuild/buf/cmd/buf@${BUF_VERSION}

# The cabal store is a BuildKit cache mount, so a dependency change rebuilds
# only what changed instead of the whole store.
FROM hs-tools AS build
ARG REVISIONS=
COPY . .
RUN --mount=type=cache,id=dxdingo-cabal-store,target=/root/.local/state/cabal \
    --mount=type=cache,id=dxdingo-go-mod,target=/root/go/pkg/mod \
    make build test smoke REVISIONS=${REVISIONS}

FROM debian:bookworm-slim AS runtime
RUN apt-get update -y && apt-get install -y --no-install-recommends \
      ca-certificates libffi8 libgmp10 libnuma1 \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /usr/local/lib/libsodium.so* /usr/local/lib/libsecp256k1.so* /usr/local/lib/
COPY --from=build /src/bin/dxnode-go /src/bin/dxnode-hs /src/bin/dxfuzz /usr/local/bin/
COPY corpus /corpus
ENV LD_LIBRARY_PATH=/usr/local/lib
ENTRYPOINT ["dxfuzz", "-a", "dxnode-go", "-b", "dxnode-hs"]
CMD ["replay", "/corpus"]
