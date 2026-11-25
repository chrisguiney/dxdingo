GO    ?= go
CABAL ?= cabal
BUF   := $(GO) tool buf

# Submodule pins stamped into dxnode-go's meta info, e.g. dingo@ae74b09219.
# Override when .git is unavailable (Docker builds pass it as a build arg).
REVISIONS ?= $(shell git submodule status 2>/dev/null | awk '{sub(/^[-+U ]/,"",$$1); n=$$2; sub(/.*\//,"",n); printf "%s%s@%s", s, n, substr($$1,1,10); s=","}')

HS_DIR := haskell
HS_GEN := haskell/dxnode-hs/gen
GO_GEN := gen/go

.PHONY: all proto proto-go proto-hs lint-proto fmt-proto tools-hs \
        build build-go build-hs test test-go test-hs smoke \
        submodules print-revisions docker docker-shell hs-dev-image docker-test-hs clean

all: build

## Protobuf

bin/protoc-gen-go: go.mod go.sum
	$(GO) build -o $@ google.golang.org/protobuf/cmd/protoc-gen-go

# proto-lens-protoc must come from the same index state as the proto-lens
# library the node links, so it is built from the project's cabal.project.
bin/proto-lens-protoc:
	mkdir -p bin
	cd $(HS_DIR) && $(CABAL) install proto-lens-protoc \
	  --installdir=$(CURDIR)/bin --install-method=copy --overwrite-policy=always

tools-hs: bin/proto-lens-protoc

proto: proto-go proto-hs

proto-go: bin/protoc-gen-go
	rm -rf gen/go
	$(BUF) generate --template buf.gen.go.yaml

proto-hs: bin/proto-lens-protoc
	rm -rf haskell/dxnode-hs/gen
	$(BUF) generate --template buf.gen.hs.yaml

lint-proto:
	$(BUF) lint
	$(BUF) format -d --exit-code

fmt-proto:
	$(BUF) format -w

## Build

build: build-go build-hs

build-go: proto-go
	$(GO) build -ldflags "-X main.revisions=$(REVISIONS)" -o bin/ ./cmd/...

build-hs: proto-hs
	cd $(HS_DIR) && $(CABAL) build exe:dxnode-hs
	install -m 0755 "$$(cd $(HS_DIR) && $(CABAL) list-bin exe:dxnode-hs)" bin/dxnode-hs

## Test

test: test-go test-hs

test-go: proto-go
	$(GO) vet ./...
	$(GO) test ./...

test-hs: proto-hs
	cd $(HS_DIR) && $(CABAL) test all

# Both nodes up, meta info from each, then replay the seed corpus.
smoke: build
	bin/dxfuzz -a bin/dxnode-go -b bin/dxnode-hs info
	bin/dxfuzz -a bin/dxnode-go -b bin/dxnode-hs replay corpus

## Workspace

submodules:
	git submodule update --init

print-revisions:
	@echo $(REVISIONS)

docker:
	docker build --build-arg REVISIONS=$(REVISIONS) -t dxdingo .

docker-shell:
	docker build --target toolchain -t dxdingo-toolchain .
	docker run --rm -it -v $(CURDIR):/src -w /src dxdingo-toolchain bash

# Fast Haskell loop: build and test dxnode-hs from the working tree in the
# deps image. Named volumes keep the cabal store (seeded from the image),
# build tree, Go module cache and Linux tools out of the host checkout.
HS_DEV_RUN = docker run --rm \
	-v $(CURDIR):/work -w /work \
	-v dxdingo-cabal-store:/root/.local/state/cabal \
	-v dxdingo-hs-dist:/work/haskell/dist-newstyle \
	-v dxdingo-hs-bin:/work/bin \
	-v dxdingo-go-mod:/root/go \
	dxdingo-hsdeps

hs-dev-image:
	docker build -q --target hs-deps -t dxdingo-hsdeps . >/dev/null

docker-test-hs: hs-dev-image
	$(HS_DEV_RUN) sh -c 'cp -n /src/bin/proto-lens-protoc bin/ && make test-hs'

clean:
	rm -rf bin gen/go haskell/dxnode-hs/gen haskell/dist-newstyle
