# 2026-05-09 Elm Record Encoding Migration

Motivation is to improve runtime efficiency for applications using Elm records.

To achieve this, we plan to switch to a representation that is not as deeply nested when encoded as Pine value.

This only pertains the canonical representation which must be used at the entry points (parameters and return values) of compiled Elm programs.

Specifications of the new encoding:

+ The new encoding is a flat Pine list, containing an uneven number of items.
+ The first item in this list is tag which is the UTF32 encoding of `<Record_Type>`
+ Following the tag, we have two items for each field: The field name followed by the field value. The field name is encoded the same way as with the old encoding.

We want to continue parsing Pine values that remain persisted in the old format, since not all producers and stored representations will switch at the same time:

+ On decoding, `ElmValueEncoding` must continue to support the old encoding, and the corresponding tests must also remain in place for now. However, the variants for the old encoding must be changed to contain `_2025` in their name.
+ On encoding, `ElmValueEncoding` must encode records in the new format.
+ Add tests for extensive coverage of `ElmValueEncoding` with the new format.
+ The Elm compiler in `Pine.Core.Elm.ElmCompilerInDotnet` must be switched to emit the new encoding for records.
+ The Elm compiler in `Pine.Core.Elm.ElmCompilerInDotnet` must emit record access and record update to support the new encoding. If possible (all tests passing), only support the new format here.

## Usages of the old tag value

The old tag value is the UTF-32 encoded string `Elm_Record`, exposed in C# as `Pine.Core.Elm.ElmValue.ElmRecordTypeTagName` / `ElmRecordTypeTagNameAsValue` and in the self-hosting Elm compiler as `ElmCompiler.elmRecordTypeTagName` / `elmRecordTypeTagNameAsValue`. The following inventory groups every observed reference of either the literal string `"Elm_Record"`, the C# symbols `ElmRecordTypeTagName(AsValue)`, or the Elm symbols `elmRecordTypeTagName(AsValue)` by file path.

### Production code – tag definition

+ `implement/Pine.Core/Elm/ElmValue.cs`
  + Line 53: `public const string ElmRecordTypeTagName = "Elm_Record";`
  + Line 91: doc comment `Represents the 'Elm_Record' tag name as a PineValue …`.
  + Lines 93–94: `public static readonly PineValue ElmRecordTypeTagNameAsValue = StringEncoding.ValueFromString(ElmRecordTypeTagName);`
+ `implement/Pine.Core/Elm/elm-in-elm/src/ElmCompiler.elm`
  + Line 20–21: module export of `elmRecordTypeTagName`, `elmRecordTypeTagNameAsValue`.
  + Lines 151–158: the two top-level definitions (`elmRecordTypeTagName = "Elm_Record"` and `elmRecordTypeTagNameAsValue = Pine.valueFromString …`).
  + Lines 1840, 3689, 3700, 3847, 4889, 4916: six `LiteralExpression elmRecordTypeTagNameAsValue` construction sites that emit the record tag in the compiled Pine expression.

### Production code – encoding/decoding (.NET)

+ `implement/Pine.Core/Elm/ElmValueEncoding.cs`
  + Line 213: tag check inside the optimized `PineValueAsElmValue` fast path that branches into the record decoder.
  + Line 484: doc comment describing the expected structure `[Elm_Record, [ [ [fieldName, fieldValue], ... ] ]]`.
  + Line 501: `if (tagNameValue != ElmValue.ElmRecordTypeTagNameAsValue)` inside `ParsePineValueAsRecordTagged`.
  + Line 774: tag emission inside `ElmValueAsPineValue` (record encoder used during full conversion).
  + Line 807: tag emission inside `ElmRecordAsPineValue` (helper for already-encoded field values).
+ `implement/Pine.Core/Elm/ElmCompilerInDotnet/ExpressionCompiler.cs`
  + Line 407: emission of the record tag when compiling a record literal (one of the choice-tag branches).
  + Line 792: emission inside `CompileRecordExpression` (general path).
  + Line 1945: emission inside `EmitRecordConstructorFunctionValue` (record type alias used as a function).
+ `implement/Pine.Core/PopularValues.cs`
  + Line 51: the literal `"Elm_Record"` is part of the static list of strings whose `PineValue` representations are pre-built and reused.

### Production code – encoding/decoding (Elm self-host)

+ `implement/Pine.Core/Elm/elm-in-elm/src/ElmInteractive.elm`
  + Line 14: re-export `elmRecordTypeTagNameAsValue`.
  + Line 334: `else if tagValue == elmRecordTypeTagNameAsValue then …` in the value classifier.
+ `implement/Pine.Core/Elm/elm-in-elm/src/EncodeElmSyntaxAsPineValue.elm`
  + Line 995: tag emission for an encoded record value.

### Pre-built Pine value caches (must be regenerated)

+ `implement/Pine.Core/PineVM/PopularValue/danfishgold.Base64.Decode.fromBytes.aggregate-env-funcs.json`
  + 14 occurrences (lines 1740, 5155, 6106, 6947, 7785, 10076, 13683, 15148, 16165, 19237, 20055, 21821, 22284, 23102) embedded as `{ "BlobAsString": "Elm_Record" }` inside the serialized aggregate environment function. This file is a frozen snapshot of an already-compiled Elm program and will need to be regenerated against the new compiler.

### Tests (.NET)

+ `implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ElmCompilerTests/SimpleRecordPatternTests.cs`
  + Line 69: comment describing the on-the-wire layout `["Elm_Record", [[["alfa", alfaValue], …]]]`.
+ `implement/Pine.IntegrationTests/CodeAnalysis/ParserFastTests.cs`
  + Line 388: a large expected-tree snapshot that contains literal `Elm_Record` tag values (see the embedded `Literal (Elm_Record)` occurrences).

(Other test files such as `ElmValueRenderAsElmExpressionTests`, `RecordExpressionTests`, `RecordAccessTests`, `RecordConstructorTests`, `RecordRuntimeTests`, `KernelJsonFunctionTests`, `KernelParserFunctionTests`, `KernelUrlFunctionTests`, `ElmParserExpressionTests`, `ElmValueJsonValueEncodingTests`, `ElmValueTests`, `CompileElmCompilerTests`, `ParseElmModuleTextToPineValueTests` exercise records via the `ElmValueEncoding` API and therefore depend on the tag indirectly, but do not mention the literal string. They are listed here as transitive dependencies, not as direct references.)

### Tests (Elm self-host)

+ `implement/Pine.Core/Elm/elm-in-elm/tests/ElmInteractiveTests.elm`
  + Lines 647, 679, 717, 760: four expected-value constructions using `ElmCompiler.elmRecordTypeTagNameAsValue`.

### Scenarios shipped with the test corpus

+ `implement/test-and-train/elm-interactive-scenarios-core/regression-101-recursive-function-in-let-block/steps-disabled/100/submission.txt`
  + Line 26: `Elm_Record recordFields ->` — uses `Elm_Record` as an Elm **case-of pattern constructor**. This is the only place where the tag value participates in user-level Elm syntax. (See *Challenges* below.)

### Documentation

+ `implement/CHANGELOG.md`
  + Line 19: prose describing the existing record wrapping (`"…we use wrap record fields in the \"Elm_Record\" tag"`).

## Detailed implementation plan

The migration touches three layers (canonical encoding helpers, the .NET-hosted Elm compiler, and the Elm self-hosting compiler) plus their respective test suites. The plan below is ordered so that each step can be validated by running the existing test suites before moving on.

### Progress at a glance

- [x] **Step 1** — Tag constants in `ElmValue.cs` + `PopularValues.cs` (commit `f54b00cf`).
- [x] **Step 2** — Encoder helpers (`ElmRecordAsPineValue`, legacy renamed `_2025`).
- [x] **Step 3** — Decoder helpers (`ParsePineValueAsRecordTagged` for flat layout; `_2025` for legacy; fast path branches on both tags).
- [x] **Step 4** — `.NET` Elm compiler emit sites + record-pattern + record-access static path.
- [x] **Step 5** — `RecordRuntime.cs` walkers reworked for the flat layout.
- [~] **Step 6** — Elm self-hosting compiler — **out of scope** (see below).
- [~] **Step 7** — Regenerate cached/baked artifacts — **out of scope** (see below).
- [x] **Step 8** — Test additions for the flat layout / rename legacy decoder tests / update `SimpleRecordPatternTests.cs` comment.
- [x] **Step 9** — `regression-101…submission.txt` left in `steps-disabled/` (folder is already inactive and there are no in-tree code references that exercise it).
- [x] **Step 10** — CHANGELOG entry added; `elm-compiler-implementation-guide.md` updated with the flat-layout description.
- [x] **Step 11** — Final validation: full `Pine.Core.Tests` run = 3172 passed / 0 failed / 10 skipped (~13 min). `Pine.IntegrationTests/ElmValueTests` = 6/6 pass (including the 2 new flat-layout / legacy-decoder tests).

Performance-counter snapshots in 16 tests across `ElmParserExpressionTests`, `KernelJsonFunctionTests`, `ElmParserFileTests`, and `ElmLanguageServiceTests` were rebaselined alongside step 5 because the flat layout removes one level of `PineValue.List` nesting per record (mostly visible as a drop in `BuildListCount`).

### 1. Introduce the new tag constant alongside the renamed old one — done

+ In `Pine.Core/Elm/ElmValue.cs`, rename the existing `ElmRecordTypeTagName` and `ElmRecordTypeTagNameAsValue` to `ElmRecordTypeTagName_2025` / `ElmRecordTypeTagNameAsValue_2025`, keeping the literal `"Elm_Record"`.
+ Add new fields `ElmRecordTypeTagName` and `ElmRecordTypeTagNameAsValue` carrying the new literal `"<Record_Type>"`. Keeping the unsuffixed names for the new tag minimizes churn at call sites that should adopt the new format.
+ Add `"<Record_Type>"` to `Pine.Core/PopularValues.cs` so the `PineValue` is reused. Keep `"Elm_Record"` in the same list for as long as the old format is still decodable.

### 2. Adapt encoder helpers in `ElmValueEncoding.cs` — done

+ Rename the existing helper that produces the legacy `[Elm_Record, [[ pairs ]]]` shape to `ElmRecordAsPineValue_2025` (and the inner record arm of `ElmValueAsPineValue` similarly). Keep it accessible to tests but no longer used from production encoders.
+ Implement a new `ElmRecordAsPineValue` that returns the flat odd-length list `[tag, name0, value0, name1, value1, …]`. Field order remains ordinal-by-name as today.
+ Switch `ElmValueAsPineValue` to call the new helper for `ElmRecord` values.
+ Add inline doc comments documenting both shapes.

### 3. Adapt decoder helpers in `ElmValueEncoding.cs` — done

+ Keep the existing `ParsePineValueAsRecordTagged` and `ParsePineValueAsRecord`, but rename them to `…_2025` and update internal call sites that need the legacy shape (mainly tests and the `PineValueAsElmValue` fast path during the transition).
+ Add new `ParsePineValueAsRecordTagged` and `ParsePineValueAsRecord` that accept the flat odd-length form: validate that the first element is `ElmRecordTypeTagNameAsValue` and that the remaining length is even; pair-decode the rest.
+ In the optimized `PineValueAsElmValue` fast path (around line 213), branch on both tag values during the migration window. Direction for follow-up: drop the legacy branch once all stored values have migrated.

### 4. Update the .NET Elm compiler emit sites — done

+ `Pine.Core/Elm/ElmCompilerInDotnet/ExpressionCompiler.cs` lines 407, 792, 1945: replace the current `[tag, [[ pair, … ]]]` construction with the flat `[tag, name, value, …]` construction. Field ordering remains ordinal-by-name.
+ Update record-pattern destructuring (look up record-pattern compilation in `ExpressionCompiler` / `BuiltinHelpers`) so that for a record of statically known shape, the value of the *i*-th sorted field is read by `head (skip (2*i + 2) record)` instead of `head (skip i (head (skip 1 record)))`.

### 5. Update `RecordRuntime.cs` — done

`BuildPineFunctionForRecordUpdate` and `BuildPineFunctionForRecordAccess` currently expect the layout `[ElmRecordTag, [[field1, field2, …]]]`. Rework both:

+ The record's field stream is `skip 1 record` (a list of alternating name, value items). The recursive helpers must be reshaped to walk in pairs (`skip 2`) instead of single steps.
+ The reconstruction in `BuildPineFunctionForRecordUpdate` becomes `concat [[tag], updatedPairsFlat]` rather than wrapping into the doubly nested list. Take care that the helper walks `processedFlat` (a flat list of name, value, name, value, …) and emits the new value as `[name, newValue]` appended to the flat sequence.
+ The recursion key (the field-name comparison) still happens on every other element. The merge invariant (both updates and fields sorted by name) is unchanged.
+ Update the doc comments at lines 39, 52, 85, 90, 226, 239 to reflect the flat layout.

### 6. Update the Elm self-hosting compiler — **out of scope**

We will **not** plan or implement changes to the Elm self-hosting compiler (`Pine.Core/Elm/elm-in-elm/src/ElmCompiler.elm`, `ElmInteractive.elm`, `EncodeElmSyntaxAsPineValue.elm`, `tests/ElmInteractiveTests.elm`) as part of this migration. The Elm-based compiler may be replaced entirely in the future, so investing in it now is not justified. The legacy decoder support added in step 3 means values produced by the unchanged elm-in-elm compiler remain readable.

### 7. Regenerate cached/baked artifacts — **out of scope**

We will **not** regenerate cached/baked artifacts as part of this migration. The cached `PopularValue` JSON snapshot and the `ParserFastTests.cs` expected-tree snapshot continue to embed the legacy `Elm_Record` tag and the legacy nested layout. They keep working because:

+ The `PopularValue/danfishgold…aggregate-env-funcs.json` cache stores `PineValue`s by structural identity — the legacy decoder added in step 3 still parses any record values reached through these cached environments.
+ The `ParserFastTests.cs` snapshot tests an *expression tree* produced by the static-program parser, not a `PineValue` round-tripped through the new encoder, so it does not change as long as the cached input does not change.

### 8. Update tests

+ Tests that drive the **decoder** with the legacy shape are renamed (`_2025` suffix) and continue to assert that the legacy decoder works. Add a new mirrored test file (or per-test counterpart) for the flat layout.
+ Tests that drive the **encoder** assert the new flat layout. Tests that round-trip through Elm code (e.g. `RecordRuntimeTests`, `RecordAccessTests`, `RecordExpressionTests`, `RecordConstructorTests`, `KernelJsonFunctionTests`, `KernelParserFunctionTests`, `KernelUrlFunctionTests`, `ElmValueRenderAsElmExpressionTests`, `ElmValueJsonValueEncodingTests`) only need the new encoder to be in place; their assertions are about the visible Elm value, not the wire shape.
+ Update the comment in `SimpleRecordPatternTests.cs:69` to describe the flat layout.

### 9. Migrate or quarantine `regression-101…submission.txt`

This is the single user-level Elm source location that pattern-matches on `Elm_Record`. The new tag cannot be expressed as an Elm constructor (see *Challenges*). Options:

+ Rewrite the snippet to perform the destructuring through `Pine_kernel.head` / `Pine_kernel.skip` (the same primitives the runtime uses) and assert the same observable behavior.
+ Or, if the scenario's purpose is unrelated to records and only used `Elm_Record` as a convenience, rewrite it to avoid touching the internal record encoding altogether.
+ The folder is already named `steps-disabled`, suggesting this scenario is currently inactive; confirm before deciding whether to delete or rewrite it.

### 10. Update documentation

+ `implement/CHANGELOG.md`: add an entry describing the encoding change, the rationale (flatter encoding ⇒ less list nesting at runtime, fewer `Pine_kernel.head`/`skip` operations per access), the rename to `_2025`, and the back-compat policy for stored values.
+ Update `elm-compiler-implementation-guide.md` (referenced from `RecordRuntime.cs:12`) to describe the new layout.

### 11. Validation

+ Run the .NET test suites (`Pine.Core.Tests`, `Pine.IntegrationTests`) — these exercise both encoder and decoder paths and provide end-to-end coverage through the .NET-hosted compiler.
+ Run the Elm self-hosting compiler tests (`elm-in-elm/tests/ElmInteractiveTests.elm`).
+ Run any regenerated-artifact verification (the `danfishgold` JSON, snapshot tests).
+ Spot-check by serializing and re-reading a persisted state of one of the shipped web services / language services to confirm the legacy decoder still parses old values.

## Challenges of switching to a tag value that cannot be expressed in Elm syntax

The old tag value `Elm_Record` is *coincidentally* a syntactically valid Elm custom-type constructor name: it begins with an uppercase letter and contains only ASCII letters, digits, and underscores. The new tag value `<Record_Type>` contains the characters `<` and `>`, which are not legal in any Elm identifier. This asymmetry has several consequences.

### Where the asymmetry actually matters

The vast majority of references to the old tag value are not Elm-syntax constructors — they are *string values* (or `Pine.Value`s derived from a string). All of these are unaffected:

+ The C# constants `ElmRecordTypeTagName` / `ElmRecordTypeTagNameAsValue` are plain strings/`PineValue`s — angle brackets are just bytes in their UTF-32 encoding.
+ The Elm definition `elmRecordTypeTagName = "Elm_Record"` is a string literal, and Elm string literals can contain any Unicode character. `"<Record_Type>"` is a perfectly valid Elm string.
+ `LiteralExpression elmRecordTypeTagNameAsValue` in the self-host compiler embeds a `Pine.Value` blob into the compiled output — there is no Elm-syntax constraint on its contents.
+ Pattern matching in the *generated* Pine code (kernel `head` / `skip` / `equal`) compares `PineValue`s structurally; the tag string is opaque.
+ Cached JSON snapshots (`PopularValue/…json`) store the bytes; they will work as long as the string is regenerated consistently.

### Where the asymmetry causes a real challenge

There is **one** kind of location where the change is not transparent: Elm source code that destructures a record value at the Pine level using a custom-type pattern, e.g.

```elm
case record of
    Elm_Record recordFields -> …
```

This idiom appears today in `regression-101-recursive-function-in-let-block/steps-disabled/100/submission.txt`. It works because

1. `Elm_Record` parses as a constructor pattern (uppercase first letter, only valid identifier characters), and
2. the .NET-hosted Elm compiler's pattern matcher compiles a one-arm constructor pattern to the same kernel-level `head`/`skip`/`equal` checks the tag would be matched against.

After the migration, the corresponding pattern would need to be `<Record_Type> recordFields ->`, which is not parsable by Elm's grammar. There is no way to recover this idiom.

#### Mitigations

The challenge is bounded but real. Practical options for any user code (or test code) that relies on destructuring records via the tag pattern:

+ **Use kernel primitives directly.** Replace the `case` with:

  ```elm
  let
      recordFields = Pine_kernel.skip [ 1, record ]
  in
      …
  ```

  This is the same operation the compiled output of the legacy `Elm_Record fields ->` arm performs, modulo the new flat layout (so `fields` is now a flat name/value list, not a list of `[name, value]` pairs). This is the recommended replacement and matches what `RecordRuntime.cs` already does.
+ **Use record-syntax patterns.** Where the record's fields are statically known, Elm's native `{ a, b }` destructuring works regardless of the wire tag. This is by far the most common case in real user code; the `Elm_Record` pattern is essentially never written outside compiler-internal experiments.
+ **Decode through a runtime helper.** A small helper in user code, e.g. `recordFields : a -> List (String, b)`, can wrap the kernel primitives and hide the layout from callers.

#### Other considerations

+ **Rendering / debugging output.** `ElmValueRenderAsElmExpression` renders records using `{ … }` syntax, never the constructor name, so output is unaffected.
+ **Diagnostic strings, error messages, and logs** that mention the literal `Elm_Record` should be updated, but this is a textual search-and-replace, not a semantic challenge.
+ **Reflection-style introspection from Elm.** Code that pattern-matches every Elm value variant (Int / String / List / Record / …) by tag — there is one such function in `ElmInteractive.elm` (line 334) — only ever compares the tag value as a `Pine.Value`. It is not affected by the change.
+ **The choice of `<Record_Type>` is in fact a *feature*, not a wart.** Because the new tag is unparsable as a constructor, it is impossible for user-written code to accidentally collide with the internal tag — something that was technically possible (though unlikely) with `Elm_Record`. This eliminates a small class of latent bugs.

In short, the only concrete porting work that this asymmetry forces is rewriting any Elm source that uses `Elm_Record` as a constructor pattern, and we have located exactly one such place in the repository (`regression-101…submission.txt`). Everywhere else the change is purely a content-of-a-string change.

## Brainstorm: making record updates more efficient in the intermediate VM

The new flat layout `[tag, name0, value0, name1, value1, …]` already cuts list-nesting depth roughly in half compared to the current `[tag, [[ [name, value], … ]]]` layout, which alone reduces per-access overhead. Beyond that, record *updates* (which produce a new record value differing from an existing one in a small number of fields) are still asymptotically `O(N)` because the canonical `PineValue` representation is an immutable, hash-consed list, and any update requires constructing a fresh `ListValue`. Below are several directions for making them cheaper, ranging from cheap and local to deep architectural changes. They are not mutually exclusive; the most attractive combination is probably (A)+(B)+(D).

### A. Add specialized IR instructions for "list with one element replaced"

The simplest and most surgical addition. Today the Pine expression for a single-field update looks like

```text
concat [ take(2*i+1, record), [newName, newValue], skip(2*i+3, record) ]
```

which lowers to several stack instructions and at least one fresh allocation per `take`/`skip`/`concat`. Two new instructions cover this pattern:

+ `Replace_List_Item_Const(int index)` — takes `[record, newItem]` from the stack and produces a new `ListValue` whose backing array is a copy of `record.Items` with index `index` replaced by `newItem`. One allocation, one `Array.Copy`, no kernel-level `take`/`skip`/`concat` round trip.
+ `Replace_List_Slice_Const(int index, int sliceLength)` — generalization that replaces `sliceLength` consecutive items starting at `index`. Useful for the flat layout because each "field update" is exactly `Replace_List_Slice_Const(2*i + 1, 2)`.

**Pros:** a few hundred lines of code, no change to `PineValue`, easy to lower from existing IR; `Build_List_Tagged_Const` already shows the pattern of bundling a known-tag head with a runtime-built tail, so this fits the existing IR vocabulary.
**Cons:** still `O(N)` because a fresh array is copied. For large records, a single update is fast; chained updates of the same record degrade linearly in the number of updates × record width.

### B. Compile-time fusion of statically known updates

Where the compiler can see *all* fields of the record (which is true for any record whose type is statically known — i.e. the common case), an update `{ r | a = expr1, b = expr2 }` can be emitted as a `Build_List` that re-uses unchanged fields directly from the source record by index, rather than calling the runtime walker:

```text
Build_List(2 * N + 1):
    push tag
    push (Local_Get/extract field 0 of r)            // unchanged
    push expr1                                        // changed
    push (Local_Get/extract field 2 of r)            // unchanged
    push expr2                                        // changed
    …
```

This is a compile-time transformation. Combined with (A), it reduces a typical update of *k* out of *N* fields from `O(N)` allocations spread across a recursive walker to a single `Build_List` of `2*N + 1` items. The runtime walker `BuildPineFunctionForRecordUpdate` is then needed only for genuinely dynamic updates (e.g. `Dict`-driven field assignment) which are rare in real Elm.

**Pros:** zero additional runtime machinery; pure compiler optimization; aligns with the type-known fast path that already exists for record *access*.
**Cons:** requires the compiler to be confident about the field set; falls back to (A) or the runtime walker otherwise.

### C. Persistent tree-shaped record values (deferred materialization in the value rep)

A more ambitious option: extend `PineValue` with an internal-only "view" variant that represents a record as `(base : ListValue) ⊕ (patches : map<index, PineValue>)`. The view exposes the same `Items` interface as `ListValue` (head/skip/length pretend the patch has been applied) but does not allocate a new array on update — only the patch map grows.

+ Reads via `head (skip k v)` walk the patch map first (cheap `Dictionary` / sorted-array lookup), falling back to `base.Items[k]` otherwise.
+ Writes (further updates) just extend the patch map.
+ Materialization to a real `ListValue` is forced on hash, structural-equality probe, marshalling out of the VM, or when the patch map gets larger than some fraction of the base.

**Pros:** turns chained updates into amortized `O(1)` per field; especially good for hot loops in The Elm Architecture-style state updates where the same record gets many small writes.
**Cons:** every place in the codebase that pattern-matches on `PineValue.ListValue` must also handle the new variant — this is a wide blast radius. Hash-consing assumptions break unless we either (a) materialize before hashing or (b) define a canonical hash that ignores the patch shape. Equality semantics must be carefully preserved (two views with different patch sets but the same logical contents must compare equal). The repo's `PineValue` is currently a closed two-case hierarchy (`ListValue` / `BlobValue`); any change here will reverberate through interpreters, JIT, and serialization paths. Recommend behind a feature flag.

### D. Shared-array `ListValue` slicing

A lighter version of (C) that does not need a new `PineValue` case. Today `ListValue.Items` is a `ReadOnlyMemory<PineValue>` over a freshly allocated array. Allow `ListValue` to also be constructed from `(sourceArray, offset, length)` — i.e. a window over an existing array — and let the IR `Skip_Const` and `Take_Const` produce such windows without copying.

A `Replace_List_Item_Const` (from A) implemented on top of this could share the prefix and suffix arrays with the original record and only allocate one tiny array for the changed cell, then concatenate three windows. With reference-counted shared backing arrays, the steady-state allocation per update collapses to *O(1)*. This is essentially "ropes for `ListValue`", and it leaves the `PineValue` case structure untouched.

**Pros:** one new internal constructor on `ListValue`, transparent to consumers, big asymptotic win for `take`/`skip`-heavy code (which is most of compiled Elm).
**Cons:** complicates GC behavior — sliced views keep the entire source array alive. Mitigation: materialize when the slice is much smaller than the source (e.g. < 1/8).

### E. Record-update batching IR

A lower-priority idea: a `Record_Update_Multi_Const(int[] indices)` instruction that takes a record plus *m* fresh values from the stack and produces a new record with all *m* slots replaced in a single allocation. The Elm-level expression `{ r | a = …, b = …, c = … }` currently lowers to nested calls of the runtime walker; with this IR plus the compile-time fusion in (B), a multi-field update becomes one `Build_List` (when the type is known) or one `Record_Update_Multi_Const` (when it is not). This is essentially an `O(N + m)` update with `O(1)` overhead per extra field.

### F. PGO-driven specialization of `BuildPineFunctionForRecordUpdate`

The PGO pipeline already specializes hot recursive callees against the constants they are called with. The runtime record-update walker is parameterized by the record and the update list; when both have known shapes (e.g. always a single update at a known field name), PGO can monomorphize the walker into a straight-line sequence equivalent to (A). This is more of a "make the existing architecture pay off" play than a new feature.

### Recommendation

For the scope of the migration in this document, options **(A)** (a `Replace_List_Slice_Const` IR primitive) and **(B)** (compile-time fusion against the known field set) provide the largest practical speedup at the lowest risk: they directly capitalize on the new flat layout, leave `PineValue` semantics untouched, and avoid the wide blast radius of (C). Option **(D)** is the natural follow-up if profiling shows that `take`/`skip` allocation cost dominates after (A) and (B) land. Option **(C)** should be revisited only after measuring whether chained record-update workloads remain a hotspot once the simpler optimizations are in place.
