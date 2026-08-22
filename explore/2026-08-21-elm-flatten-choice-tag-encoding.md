# 2026-08-21 Elm Flatten Choice Tag Encoding

We switch the representation of Elm choice-type tagged values (e.g., Maybe.Just, Result.Err) to a flattened encoding.

The motivation is to improve efficiency in program execution and in representing Elm values and derivations from Elm values, such as hashes.

## Target Encoding

Following the specifics of the representation of Elm choice type tagged values going forward:

+ List value.
+ The number of items is the number of arguments plus 2.
+ For a tag with zero arguments (e.g. `Nothing`), the list contains two items.
+ The first item is always the value `<Choice_Type>`, analogous to how we already use `<Record_Type>` for records.
+ The second item contains the value for the tag name, encoded as before.

## Implementation

+ The emission part of the Elm compiler should assume that the type checker will have rejected invalid patterns.
  + It should not check for the presence of the `<Choice_Type>` label.
  + Example: A pattern that only checks a tag name: The emitted code should only test equality on the second item, which contains the tag name.
  + A length-check in pattern compilation should never be necessary, since Elm semantics mean the tag already determines length.
+ Parsers like `ElmValueEncoding.PineValueAsElmValue` must maintain backward compatibility.
+ Tests like in `ElmValueEncoding.cs` must test the new encoding, and place tests for backward compatibility with the older choice tag representation with a `2025` in the test name. (In C#, at least test the method name, test class name, namespace name, or a combination)
+ Further backward compatibility must be maintained to avoid breaking any functionality that depends on persisted artifacts using the old encoding. (Inventory cases as part of plan so we can remove obsolete before starting migration)

## Analysis before implementation

The current generic choice-tag encoding is:

```text
[ tagName, [ arg0, arg1, ... ] ]
```

The target encoding is:

```text
[ <Choice_Type>, tagName, arg0, arg1, ... ]
```

Consequently, the tag name moves from index `0` to index `1`, argument `i` moves
from nested path `[1, i]` to root index `2 + i`, and a zero-argument
constructor changes from `[tagName, []]` to `[<Choice_Type>, tagName]`.

The marker removes the old decoder's ambiguity between an ordinary two-item
list and a choice value. However, retaining the old decoder necessarily retains
that ambiguity for legacy values during the compatibility period.

## Further Clarifications

+ The special wrappers `String`, `Elm_Bytes` and `Elm_Float` will also switch to this format, because Elm code in the core library implementation uses pattern matching syntax with these.
+ The Elm-based compiler implementation is out of sync already since the change in Pine expression encoding earlier this year, and not maintained anymore. Therefore no changes in `implement\Pine.Core\Elm\elm-in-elm\src\ElmCompiler.elm`. We will implement the Elm compiler in Elm in the future.
+ Unsuffixed writers should always emit the new format.
+ Unsuffixed parsers should accept both old and new format.
+ If a list begins with `<Choice_Type>` but has fewer than two items, the parser should treat it as an ordinary list/tuple.
+ Pattern emission in compilation: The existing optimization that omits the name comparison for a single-constructor type remains valid.
+ Pattern emission in compilation: We continue to use an equality check over the whole tagged value, if all arguments are given and fixed (e.g. `Just 4`)
+ Sequential IR: No need to adapt instruction forms like `Build_List_Tagged_Const`. These might become list important with this change and might be removed in the future.
+ Precompiled leaves must be updated to match the new format.
+ Runtime backward compatibility: Newly compiled Elm code should not accept the old format. Consequently, producers like the Elm languager server must be updated as part of the migration.

## Implementation Log

### 2026-08-21 — Initial implementation inventory

+ Confirmed the clarified scope: all canonical Elm choice values, including the
  `String`, `Elm_Bytes`, and `Elm_Float` wrappers, move to
  `[<Choice_Type>, tagName, arg0, ...]`.
+ Unsuffixed encoders will emit only the flat format. Unsuffixed host-side
  parsers will accept flat and 2025 nested formats.
+ Compiled Elm pattern matching and the in-process interpreter will consume only
  the flat format. They will compare constructor names at index `1`, access
  arguments at indices starting with `2`, and will not emit marker or arity
  checks.
+ `Build_List_Tagged_Const` and its existing `PineValueInProcess.CreateTagged`
  specialization remain unchanged because they implement a general two-item
  list instruction, not the new Elm choice ABI. A separate lazy flat-choice
  representation/helper will be used by the Elm interpreter.
+ The retired Elm-in-Elm compiler implementation is excluded. Shared C# boundary
  encoders and maintained host producers remain in scope.
+ Existing persistent raw application state cannot be made compatible merely by
  dual-format host parsing because compiled patterns intentionally reject the
  legacy representation. Compiler/cache identities that derive from emitted
  expressions will naturally change; explicit cache versioning will be reviewed
  before completion.
+ Planned implementation order:
  1. Canonical constants, writers, dual-format readers, and wire-layout tests.
  2. Compiler emission and named-pattern offsets.
  3. In-process interpreter representation and builtins.
  4. Core-library expression builders and precompiled leaves.
  5. Host producers, documentation, snapshots, and full validation.

### 2026-08-21 — Canonical encoding and first consumers

+ Added the `<Choice_Type>` constant and popular-string entry.
+ Changed all unsuffixed `ElmValueEncoding` writers, including `String`,
  `Elm_Bytes`, and `Elm_Float`, to emit the flat format.
+ Added `TagAsPineValue_2025` and `StringAsPineValue_2025`; the recursive
  `ElmValueAsPineValue_2025` path continues to emit the complete legacy format.
+ Consolidated choice decoding so `PineValueAsElmValue` accepts both layouts,
  including special wrappers, and updated `ParseAsTag` to return the flat
  argument suffix without copying.
+ Preserved the clarified malformed-marker behavior: `[<Choice_Type>]` falls
  through to ordinary list decoding.
+ Added direct canonical, wrapper, recursive, and 2025 compatibility tests.
+ Updated .NET compiler constructor emission and named-pattern offsets.
  Constant patterns still compare the whole canonical value; nonconstant
  patterns compare only index `1` and bind arguments from index `2`.
+ Added `PineValueInProcess.CreateChoice` as a lazy ordinary-list construction,
  leaving `CreateTagged` and `Build_List_Tagged_Const` unchanged.
+ Started migrating interpreter constructors, named patterns, strings, floats,
  bytes, JSON values, and Dict operations to the flat layout.
+ Validation: focused `ElmValueTests` filters build successfully and pass. The
  first run exposed a static-initialization cycle and an ordinary-list fallback
  bug; both were corrected before proceeding.

### 2026-08-21 — Core library, precompiled leaves, and host producers

+ Migrated generated `Basics` expressions for strings, floats, `Dict`, and `Set`
  to read constructor names at index `1`, arguments from index `2`, and build
  canonical flat choices.
+ Updated `Debug.toString` choice recognition and extraction for flat strings and
  floats.
+ Migrated maintained precompiled leaves for `Basics`, `Dict`, `String`, `Bytes`,
  Base64, the concrete-syntax parser, concrete-to-abstract conversion, and
  language-service helpers. These optimized paths now consume and return the
  same flat values as their compiled Elm counterparts.
+ Updated language-service file-tree requests (`BlobNode` and `TreeNode`) and the
  web-service `Result` encoder so newly compiled Elm patterns receive canonical
  values at host boundaries.
+ Kept generic environment lists, tuples, records, and expression encodings
  unchanged; only Elm choice encodings were flattened.
+ Validation: `Pine.Core` builds successfully after the core-library and
  precompiled-leaf migration. The next phase is broader tests and snapshot/cache
  review.

### 2026-08-21 — Cache boundary and documentation

+ Added an Elm-choice ABI version to compiled-module file-cache hashes. Existing
  cache files remain intact but are no longer selected for new compilations, so
  old and new compiled environments cannot be combined.
+ Raw persisted application state is intentionally not rewritten heuristically:
  the legacy layout is ambiguous with ordinary two-item lists. Compatibility is
  provided at explicit host decoding and application migration boundaries,
  while newly compiled patterns continue to accept only canonical values as
  required.
+ Documented the canonical layout, index rules, legacy host-decoding window, and
  excluded Elm-in-Elm and sequential-IR surfaces in the compiler guide and
  changelog.

### 2026-08-21 — Compatibility validation

+ Regenerated the local bundled declaration keys with the repository prebuild
  project and verified that freshly built precompiled-leaf values match the
  bundle.
+ Updated the Elm test-result reader to use the dual-format tag parser, corrected
  direct Dict builtin fixtures to construct canonical choices, and kept special
  string, float, and bytes wrappers out of generic constructor parenthesization
  in `Debug.toString`.
+ Corrected the final unsuffixed `AsElmBytesBytes` helper to emit the canonical
  layout and added an explicit `_2025` legacy counterpart.
+ Focused Dict, Elm testing, Debug, core String, and interpreter Bytes/Base64
  validation passes. Broader validation still exposes failures in the compiled
  Base64 decoder path and expected snapshot/performance changes; these remain
  unresolved at this checkpoint.
