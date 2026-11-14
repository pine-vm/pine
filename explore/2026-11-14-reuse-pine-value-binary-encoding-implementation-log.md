# Elm AST binary encoding: implementation log

Date: 2026-11-14.

This implements the strategy in the November 13 and 14 exploration notes,
replacing the specialized AST codec introduced in
`772152dba7aee711da8a7ce69bf09df7042931ad`.
The [concise reusable spec](../guide/binary-tree-encoding-via-pine.md) describes
the contract; this file records implementation choices and actual allocations.

## Implementation decisions

The shared `TreeEncoder<TItem>` emits the existing deterministic Pine tags and
relative references. `TreeDecoder<TItem>.Read<TNode, TAdapter>` statically
dispatches struct adapters. Pine's public entry points keep their original wire
layout, factories, reusable instances and ordinary Eval callback semantics.

Syntax uses `TItem = object`: strings, qualified names, syntax nodes and cached
Pine payloads can all be referenced without wrappers. Scalars are read directly
from inline blobs into value types, never boxed into the internal dictionary.
Constructor environments are shallow; arrays are allocated directly in their
final element types. Optional signatures reuse a retained signature or a static
absence sentinel.

The external-Eval specialization consumes a whitelisted expression hash and its
fields, without instantiating an encoded expression or environment. All 64
constructor hashes are generated into static compiler data. The canonical Pine
expressions are built only when the ordinary-Pine reference resolver is used.
No AST version ID is emitted. Cached Pine values are preserved, but omitted from
the Elm semantic projection, which matches the frontend's abstract model.

An initial all-node decoder index allocated **222,144 B** on the first wide AST
decode, versus **139,800 B** in the specialized codec. The final reader first scans
the valid wire prefix for reference targets. Only those targets need index
entries; the scan never invokes resolvers or evaluators. This removes the
unreferenced-wide-tree scratch allocation without changing callback order,
reference completion rules, malformed-input errors or embedding.

## Measurement method

The [reproducible harness](2026-11-14-binary-tree-allocation/Program.cs) compiles
the original specialized source alongside the shared implementation in the
**same process**, both using the same Pine factories and runtime. The baseline is
`00b5a1ad5c23be8222d31578ae95eaa9a8fb9218`, the last specialized implementation
before this replacement, including its subsequent rational-float fixes.
No production copy of the old codecs is retained; the preparation script extracts
them into git-ignored `obj/baseline`.

Environment: Linux x86-64, .NET SDK 10.0.403, runtime 10.0.14, Release builds.
The workspace is shared. There were no embedded prebuilt reuse/declaration
bundles; both routes use the same factories with that same configuration.

Inputs, encoded arrays, sink delegates and measurement delegates are constructed
outside the measured regions. Each warmed operation has 64 warm-up invocations,
then 256 individually sampled invocations using
`GC.GetAllocatedBytesForCurrentThread`. Means are exact decimal divisions of the
integer totals, not rounded estimates. Cold observations use one invocation in
a fresh process **per route**, avoiding pool priming by the competing codec.
Decode first-use costs exclude the preceding preparation of encoded inputs.

These are allocated bytes, not live heap size or object counts. Incidental
runtime/weak-interner allocations are included, not subtracted. No throughput or
CPU-neutrality claim is made.

## Warmed allocation results

All entries are **allocated bytes per operation**, specialized -> shared.
Except for the large-payload shared decode noted below, every invocation in this
snapshot had the displayed exact allocation: minimum = mean = maximum.
The raw CSV preserves totals, repetitions, ranges and wire sizes.

| Pine fixture | Encode B/op | Decode B/op |
| --- | ---: | ---: |
| Empty list | 384 -> 304 | 272 -> 24 |
| 1,000,000-byte blob | 384 -> 304 | 312 -> 64 |
| Mixed list/blob tree | 520 -> 520 | 952 -> 320 |
| 1,024 distinct blobs | 119,032 -> 119,032 | 151,608 -> 49,264 |
| 128 nested lists | 10,360 -> 10,280 | 29,816 -> 12,312 |
| 1,024 references to a mixed subtree | 33,688 -> 33,688 | 9,272 -> 8,600 |
| 1,024 distinct blobs, each referenced again | 135,440 -> 135,440 | 159,800 -> 57,456 |

| Elm AST fixture | Encode B/op | Decode B/op |
| --- | ---: | ---: |
| Unit | 304 -> 56 | 88 -> 0 |
| Qualified application with three integer arguments | 768 -> 56 | 800 -> 536 |
| Two-field record with Unicode string | 768 -> 56 | 784 -> 520 |
| 1,000,000-character shader + 80,001-bit integer | 520 -> 56 | 2,010,408 -> 2,010,280.375 |
| 1,024 distinct integer literals | 204,520 -> 56 | 139,776 -> 90,168 |
| 1,024 references to the application | 768 -> 56 | 9,048 -> 8,784 |
| 1,024 distinct literals, each referenced again | 204,520 -> 56 | 147,968 -> 98,360 |
| 128 negations | 10,280 -> 56 | 7,336 -> 3,072 |

For the shared large-payload decode, the exact total was **514,631,776 B / 256**,
with **2,010,232 B minimum** and **2,016,600 B maximum**. Its specialized total was
**514,664,448 B / 256**, all at **2,010,408 B**. Thus even including the observed
incidental allocations, the shared total was **32,672 B lower**.

For the wide AST decode, the exact totals were **35,782,656 -> 23,083,008 B / 256**:
**49,608 B less per operation**. Its encode totals were
**52,357,120 -> 14,336 B / 256**, saving **204,464 B/op**. The remaining 56 B
encode allocation is the public encoding-session object, not a per-node wrapper.

The harness also checks 441 blob/list dictionary-growth boundary combinations;
none increased Pine encoding allocations. Pine byte sequences were equal, and
the AST routes returned equal native values.

## Cold costs and internal dictionaries

**Internal dictionaries/indexes do allocate, and those allocations are accepted
tradeoffs, not hidden by the warmed results.**

The encoder uses two reusable structural dictionaries. The table-only benchmark
inserts 1,025 expression keys and 1,024 cached-blob keys, without constructing
nodes, writing payloads or allocating a session object:

| Internal bookkeeping operation | First-use allocated B | Warmed total B / 256 |
| --- | ---: | ---: |
| Encoder dictionaries, wide AST | 204,520 | 0 |
| Decoder index, unique wide AST (no reference targets) | 0 | 0 |
| Decoder index, repeated application (at most 12 distinct targets) | 0 | 0 |
| Decoder index, 1,024 distinct referenced literals | 66,416 | 0 |

For that last index, the final pooled array has 2,048 slots of 16 B:
**32,792 B including the array header**. First growth allocates seven arrays
(32 through 2,048 slots), totaling **65,192 B**; the measured first-use total
includes **1,224 B** of additional pool/runtime initialization. Disposing clears
references and returns the arrays; warmed zero does **not** mean no heap storage
is retained by the pool.

The encoder retains at most two eligible dictionaries per item type, rejects
tables containing more than 4,096 entries from reuse, and clears keys on return.
Concurrent or larger scopes may allocate new tables. Final result arrays are
never returned to a pool.

| Elm AST fixture, first invocation | Encode B, specialized -> shared | Decode B, specialized -> shared |
| --- | ---: | ---: |
| Unit | 4,424 -> 360 | 112 -> 0 |
| Application | 4,984 -> 1,352 | 824 -> 624 |
| Unique wide tree | 208,640 -> 204,576 | 139,800 -> 90,168 |
| Large streaming payload | 21,144 -> 17,232 | 2,010,432 -> 2,010,344 |
| Distinct referenced literals | 208,640 -> 204,576 | 147,992 -> 164,776 |

The referenced-literal cold decode is **16,784 B more** than the old codec.
Its **164,776 B** is exactly **98,360 B** of warmed decoding allocations plus
**66,416 B** of first-use index allocations. This is the disclosed, accepted
dictionary/index overhead; it is not constructor-expression or environment
allocation. The corresponding cold Pine fixture still improves:
**159,824 -> 123,872 B** for decode and **135,824 -> 135,440 B** for encode.

Keeping a retained subobject as its own constructor enables object/child reuse
but can require an index slot. Flattening raw scalar fields removes boxing and
temporary entries but deliberately gives up referencing those scalar entries.
The chosen boundaries therefore preserve qualified names and typed collections
where they are retained objects, while flattening environments and scalar fields.
The reference-target scan adds a read-only wire pass, another explicit tradeoff.

## Wire-size tradeoff

Heap reduction does not imply smaller encoded data. The Eval + external hash +
environment envelope is 48 B before its fields; retained references occupy 8 B.
Strings use canonical UTF-32 rather than the old UTF-8 syntax format.

| AST fixture | Specialized wire B | Shared wire B |
| --- | ---: | ---: |
| Unit | 5 | 48 |
| Application | 74 | 564 |
| Record | 209 | 664 |
| Large payload | 1,020,027 | 4,020,224 |
| Unique wide tree | 17,417 | 73,776 |
| Repeated application | 5,194 | 8,796 |
| Distinct referenced literals | 22,537 | 81,968 |
| 128 negations | 133 | 6,192 |

Old `EAS` blobs and dependent content addresses must be regenerated. The ordinary
Pine wire format remains unchanged.

## Validation and reproduction

The automated Elm oracle encodes the original and specialized-decoded models to
Elm values independently of the wire codec. It asserts equality with ordinary
Pine decoding, real expression parsing and `DirectInterpreter` evaluation, then
`ElmValueEncoding.PineValueAsElmValue`. Concrete cases cover every expression,
pattern, annotation, declaration and let variant, empty/multiple components,
qualified names, Unicode, large/negative integers and rational floats.
Separate tests preserve unusual cached Pine values, exact sharing, embedding,
determinism, rejection behavior, target-index rehashing and large streaming
allocation budgets. All **92 Pine + 11 AST + 5 SCC tests passed**.

From the repository root:

```sh
bash explore/2026-11-14-binary-tree-allocation/prepare-baseline.sh
dotnet run -c Release --project explore/2026-11-14-binary-tree-allocation/measure.csproj
dotnet run -c Release --project explore/2026-11-14-binary-tree-allocation/measure.csproj -- --cold --route specialized distinct-shared-1024
dotnet run -c Release --project explore/2026-11-14-binary-tree-allocation/measure.csproj -- --cold --route shared distinct-shared-1024
dotnet run -c Release --project explore/2026-11-14-binary-tree-allocation/measure.csproj -- --encoder-tables
dotnet run -c Release --project explore/2026-11-14-binary-tree-allocation/measure.csproj -- --index-only distinct-shared-1024
```

The [warmed CSV](2026-11-14-binary-tree-allocation/measurements.csv),
[cold CSV](2026-11-14-binary-tree-allocation/cold-measurements.csv), and
[bookkeeping CSV](2026-11-14-binary-tree-allocation/bookkeeping-measurements.csv)
contain the exact observations. Encode rows use zero in `wire_bytes` because
decode inputs are prepared after first-use encoding is measured; decode rows
record the actual wire sizes.

To regenerate the compiler hash data after changing canonical expressions:

```sh
dotnet build -c Release explore/2026-11-14-binary-tree-allocation/measure.csproj
dotnet run --no-build -c Release --project explore/2026-11-14-binary-tree-allocation/measure.csproj -- --emit-hashes \
  | sed -n '/^using System;/,$p' \
  > implement/Pine.Core/Elm/Syntax/Abstract/ElmSyntaxAbstractBinary.ConstructorHashes.cs
```

The equivalence tests also verify the generated hashes against all canonical
constructor expressions. C# changes were formatted with `dotnet format`.
