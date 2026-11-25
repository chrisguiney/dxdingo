# dxdingo

Differential testing of the Go Cardano implementation (dingo, gouroboros)
against the Haskell reference (cardano-ledger, ouroboros-consensus,
ouroboros-network, cardano-base).

| Binary | Role |
|---|---|
| `dxnode-go` | Go node under test; calls dingo and gouroboros |
| `dxnode-hs` | Haskell reference node; calls the IntersectMBO libraries |
| `dxfuzz` | Driver; starts both nodes, sends each the same calls, compares replies |

## Transport

Nodes read calls on stdin and write replies on stdout, one call at a time.
Each frame is a 4-byte big-endian length followed by a serialized
`dxdingo.v1.Call` (driver to node) or `dxdingo.v1.Reply` (node to driver).

`proto/dxdingo/v1/service.proto` lists every function as a `NodeService`
method. `envelope.proto` carries the same request and response messages in
the `Call` and `Reply` oneofs. A function's request and response share one
field number, blocked by area (codec 100, rule 200, ...). Add new functions
at the end of their block and never renumber. The `internal/envelope` tests
keep the service, `Call` and `Reply` in step.

A reply is one of:

- a response: the function ran. `rejection` (field 1 of every response) is
  set when it rejected its input. Failures are Haskell constructor names
  and compare as a set; `detail` is ignored.
- `error`: the call was not evaluated. `UNIMPLEMENTED` and `UNSUPPORTED`
  are skipped; `BAD_REQUEST` and `INTERNAL` are harness bugs.

Repeated fields documented as sets must be sorted by the node adapter
before replying; the comparator compares repeated fields in order.

## Layout

| Path | Contents |
|---|---|
| `proto/dxdingo/v1/` | protobuf API |
| `cmd/` | `dxnode-go`, `dxfuzz` |
| `internal/` | framing, dispatch, comparison, driver, subprocess client |
| `haskell/dxnode-hs/` | Haskell node (proto-lens) |
| `corpus/` | seed calls: `*.txtpb` (text) or `*.binpb` (binary) `Call`s |
| `third_party/` | submodules, each on the `dxdingo` branch of its fork |

The Go module replaces `github.com/blinklabs-io/dingo` and `gouroboros`
with the submodules. The Haskell build resolves the IntersectMBO libraries
from CHaP at the index states cardano-node 11.1.3 used; the submodules pin
the matching sources for reading and patching.

| Submodule | Base |
|---|---|
| dingo | main at setup (ae74b09) |
| gouroboros | v0.220.4, the version dingo pins |
| cardano-ledger | f649f97 (cardano-node 11.1.3) |
| ouroboros-consensus | 82ecba3 (4.2.1.0) |
| ouroboros-network | c45735a (1.2.0.0) |
| cardano-base | 060819b (cardano-crypto-class 2.5.1.0) |

## Build

```sh
make submodules
make build-go test-go   # Go only; needs Go 1.27
make docker             # everything, with GHC 9.6 and the Haskell node
make smoke              # both nodes: meta info, then replay corpus/
```

`make proto` regenerates both bindings with buf, run through `go tool`.
The Haskell plugin, `proto-lens-protoc`, is built by `make tools-hs`.

## License

Apache License 2.0; see [LICENSE](LICENSE).
