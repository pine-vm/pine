# Versioned Elm testing over LSP

## Scope and architecture

Pine offers test discovery, source navigation and execution through an opt-in LSP
extension. Protocol version **1** is independent of the Pine executable version,
LSP version and source version being tested. The native VS Code integration
requires **VS Code 1.93 or later**.

Discovery evaluates suite construction with the Elm syntax interpreter, retaining
groups, skips, focus selection and original construction locations. It does not run
assertion or fuzz callbacks. Selected callbacks are subsequently compiled to Pine
expression/environment pairs and executed on PineVM, using the same execution,
expectation and fuzz machinery as the CLI. `Debug.log` is currently silent.

The language-server host owns immutable source snapshots. Neither the compiler nor
ordinary document requests acquire a new mutable "current test version." Separate
versions can be discovered, navigated and run independently, including concurrently.
Run events and terminal responses always identify their particular version.

The implementation is in
[`LanguageServerTestingService`](../implement/Pine.Core/Elm/LanguageServer/LanguageServerTestingService.cs);
wire records are in
[`PineTesting.cs`](../implement/Pine.Core/LanguageServerProtocol/PineTesting.cs).
JSON property names use camel case; enum strings are case-sensitive.

## Negotiation

The client opts in during `initialize`:

```json
{
  "capabilities": {
    "experimental": {
      "pineTests": { "protocolVersion": 1 }
    }
  }
}
```

An implementing server replies under `capabilities.experimental.pineTests`:

```json
{
  "protocolVersion": 1,
  "versionKinds": ["workspace", "file-tree", "commit"],
  "changeNotifications": true,
  "sourceText": true,
  "fuzzReplay": true,
  "retainedVersions": 16
}
```

`workspace` means capture current inputs, not a mutable version identifier.
`commit` is omitted by hosts without a commit-source adapter. Missing or
unsupported protocol versions do not negotiate testing. Clients must not send
testing requests to older servers; servers reject requests without negotiation.
This capability does not advertise debugging or coverage.

## Source versions and inventories

### Version identifiers

* `commit-<sha>` uses the full, lowercase Git **commit object** ID: 40 hexadecimal
  characters for SHA-1 repositories or 64 for SHA-256 repositories. Branch names,
  abbreviated IDs and Git tree IDs are not accepted.
* `file-tree-<sha>` uses 64 lowercase hexadecimal characters identifying the
  canonical Pine FileTree value described below.

An omitted discovery version captures current workspace inputs and returns a
`file-tree-` identifier even inside a Git repository. An explicit version must
never silently resolve to the current workspace.

For protocol v1, the file-tree hash is exactly:

```csharp
PineValueHashTree.ComputeHash(
    FileTreeEncoding2026.Encode(FileTree.Sort(tree)))
```

Use the **2026 encoding**, not whichever FileTree encoding a future Pine release
selects by default. Files are Pine blobs. Directories are flat alternating lists
`[name0, child0, name1, child1, ...]`, with canonically sorted sibling names encoded
as UTF-32 big-endian blobs. Apply Pine's value hash tree algorithm, not a hash of
JSON, concatenated file contents, a filesystem archive or a Git tree.

The hash covers every supplied file's path and bytes relative to `sourceRootUri`.
Project URIs, document versions and timestamps are not hashed. A workspace capture
includes `.elm` files and `elm.json` manifests below its source root, excluding
`.git`, `elm-stuff` and `node_modules`. An explicitly supplied tree may additionally
contain binary files; these contribute to its hash.

### Workspace capture and commits

The host begins with the containing workspace folder, initialized root or project
directory, and expands the root if necessary to contain declared parent source
directories. Open document contents accepted by the server override backing files,
including unsaved manifests and new files. `documentVersions` reports the accepted
versions under the original client document URIs. A concurrent accepted source
change rejects capture with `workspaceChanged`; the client should retry.

Filesystem enumeration is not an atomic filesystem transaction. File-change
notifications trigger rediscovery after external changes. Once captured, subsequent
discovery and execution read immutable bytes, not the live filesystem.

The CLI host's Git adapter reads exact local objects without checkout, hooks,
network fetches, branch changes or working-tree mutation. It captures Elm files
and manifests from the repository root and rejects relevant symbolic links instead
of following mutable external files. Locating commits or browsing history is not
part of this protocol.

### Dependency and inventory identity

A source version does not by itself identify external package resolution or the
compiler implementation. Successful discovery returns `resolutionFingerprint`
and an opaque `discoveryId`. The retained inventory freezes its resolved build;
runs with different seeds reuse that environment rather than re-resolving package
constraints.

Selections require the `discoveryId` from the corresponding inventory. A mismatch
returns `staleDiscovery`, never execution against different meanings for the same
IDs. After snapshot eviction or server restart, rediscover: package constraints or
compiler identity might now yield a different inventory token.

Test IDs are unique **within an inventory**, not durable semantic IDs across
edits. Current IDs include structural ordinals: insertion or reordering can change
them. A future diff client can discover a commit and current file tree independently
and compare module/declaration identity, labels, hierarchy and source provenance.
It must handle duplicates and ambiguity rather than equating raw IDs across
versions. Neither discovery changes the other version's inventory.

## Requests and notifications

All project and source-root URIs are absolute directory URIs without query or
fragment. The server returns canonical URIs; clients should retain them for
correlation. Windows file URI drive letters are normalized: `file:///k%3A/`
and `file:///K:/` refer to the same source root, including for unsaved document
overlays and retained versions. Escaped filename characters remain escaped.
Source paths are relative, slash-separated logical paths; uploaded
paths use arrays of individual segments. Locations use LSP zero-based,
end-exclusive **UTF-16** ranges, converted from Elm's Unicode-scalar columns.

### `pine/tests/discover` - request

Parameters:

```typescript
{
  projectUri: string;
  version?: string;
  sourceRootUri?: string;
  files?: { path: string[]; contentBase64: string }[];
}
```

Current workspace discovery:

```json
{ "projectUri": "file:///work/project/" }
```

Exact commit discovery:

```json
{
  "projectUri": "file:///work/project/",
  "version": "commit-0123456789abcdef0123456789abcdef01234567"
}
```

For an explicit file tree, compute its hash first and send its `file-tree-` version,
`files`, and optionally `sourceRootUri` (default: project directory). The project
must be contained in that root and have an `elm.json` there. The server verifies the
hash before accepting the bytes. Alternatively, omit `files` to reuse a retained
tree, including for another contained project. `sourceRootUri` requires `files`;
uploaded files cannot claim a commit ID.

Response:

```typescript
{
  projectUri: string; version: string; sourceRootUri: string; treeHash: string;
  complete: boolean;
  items?: TestItem[];
  hasOnly: boolean; hasSkipped: boolean;
  documentVersions: { [documentUri: string]: number };
  resolutionFingerprint?: string;
  discoveryId?: string;
  error?: { code: string; message: string };
}
```

`complete: true` is a complete replacement for that project/version. An empty
`items` array is valid when no tests exist; it does not assert that every project
declaration has been type-checked. Discovery failure after source capture returns
`complete: false`, an error and no inventory, retaining the known version.
Clients must not interpret this as successful empty discovery.

Items are parent-before-child:

```typescript
{
  id: string; parentId?: string; label: string;
  filePath: string; moduleName: string; declarationName: string;
  descriptionPath: string[];
  kind: "Group" | "Unit" | "Fuzz" | "Todo";
  selection: "Included" | "Skipped" | "ExcludedByOnly";
  isOnly: boolean; isEmptyGroup: boolean; fuzzRuns?: number;
  sourcePath: string;
  location: { uri: string; range: Range };
}
```

All groups and skipped/only-excluded tests remain in the inventory. `filePath`
identifies the discovery root's source file; `sourcePath` and required `location`
identify the actual construction expression, possibly in a helper or dependency.
Generated tests can share a construction location. It is not an assertion stack
frame. Paths can include the project prefix within a larger source tree.

### `pine/tests/run` - request

```typescript
{
  projectUri: string;
  version: string;
  runId: string;
  discoveryId?: string;
  include?: string[];
  exclude?: string[];
  seed?: number;
  fuzzRuns?: number; // default 100
}
```

`version` is mandatory. `runId` is a nonempty client-chosen correlation ID, at most
200 characters, unique among active runs. IDs in `include` and `exclude` refer to
that version and inventory. `discoveryId` is required when `include` is supplied
or `exclude` is nonempty; an unselected full run may omit it.

Omitted/null `include` selects all entries; `[]` selects none. Selecting a group
selects its descendants. Exclusions expand similarly and win over inclusions.
Unknown IDs are errors. Selection never overrides `Test.skip` or `Test.only`.
TODOs report skipped; included empty groups fail. Ordinary nonempty groups
aggregate their descendants and do not produce separate execution results.

`seed` is an unsigned 32-bit integer; if omitted, one effective seed is chosen for
the run. `fuzzRuns` must be a positive unsigned 32-bit integer. The request remains
outstanding until completion. Standard LSP `$/cancelRequest` cancels that request;
there is no separate stop method. Cancellation is cooperative during preparation
and VM execution. Different versions are independently runnable; runs for the
same project/version are serialized.

Response:

```typescript
{
  projectUri: string; version: string; runId: string;
  status: "completed" | "cancelled" | "errored";
  settings: { seed: number; fuzzRuns: number };
  incomplete: boolean;
  results: TestResult[];
  error?: { code: string; message: string };
  resolutionFingerprint?: string;
  discoveryId?: string;
}
```

`completed` means processing completed, not that every test passed. `incomplete`
records suite skip/focus flags, selected TODOs, cancellation or preparation errors;
it is not a coverage measurement. Explicit include/exclude selection alone does
not set it.

```typescript
type TestResult = {
  testId: string;
  state: "passed" | "failed" | "errored" | "skipped";
  durationMilliseconds: number;
  messages: {
    message: string; expected?: string; actual?: string; location?: Location;
  }[];
  fuzz?: ElmFuzzResult;
  reproductionCommand?: string;
};
```

Expectation failures and empty groups are `failed`; VM evaluation errors are
`errored`. Results are in reporting order, not necessarily inventory order.
Skipped/TODO/empty entries can finish before runnable callback preparation.
Preparation failure marks unfinished selections errored, rather than passing or
silently dropping them. Cancellation marks unfinished selections skipped.

`fuzz` retains the core runner's structured `ElmFuzzResult`: effective settings,
requested/elapsed runs, failing iteration, original/shrunk inputs, random state,
choice tapes, shrinking completion, generator/evaluation failures and distribution
report. Reusing the exact version, inventory token, test selection and settings
supports deterministic replay. `reproductionCommand` is a POSIX-shell-quoted CLI
command; run it from the matching project with matching source/dependencies. It
does not check out historical sources. Protocol replay avoids that ambiguity.

### `pine/tests/event` - server notification

```typescript
{
  projectUri: string; version: string; runId: string;
  sequence: number;
  kind: "phase" | "test" | "end";
  testId?: string;
  state?: string;
  result?: TestResult;
  message?: string;
}
```

Sequence numbers start at 1 and strictly increase within a run. Phases are
`waiting`, `discovery`, `runner-compilation`, `running`. `runner-compilation` covers
selected callback/capture export and compilation to Pine, including reseeding when settings change;
it replaces the former, less specific `preparation` phase label. Test states are `queued`,
`started`, `passed`, `failed`, `errored`, `skipped`; terminal test events contain
`result`. Nonexecuting selections need not emit `started`. An accepted run ends
with an `end` event whose state matches its terminal status.

Clients correlate the full project/version/run tuple, reject duplicate or stale
sequence numbers and reconcile terminal response results if notifications were
missed. A client library can reject an outstanding request locally on cancellation
before the terminal response arrives; the UI must still finish pending items.
Requests rejected before run acceptance can instead produce RPC errors.

### `pine/tests/changed` - server notification

```json
{ "workspaceRevision": 42 }
```

This is invalidation, not an inventory patch or content-addressed source ID.
Revisions increase within the server session. Clients interested in current state
debounce changes and rediscover without a version. Retained historical inventories
and active runs remain unchanged.

### `pine/tests/source` - request

```typescript
{ projectUri: string; version: string; path: string }
```

Returns `{ projectUri, version, path, text }` with exact original retained text,
including resolved dependency/substitution sources rather than rewritten compiler
identities. Unknown/evicted versions or absent paths are explicit errors.

Workspace locations use logical workspace URIs. Such a URI does not promise that
today's disk contains the historical bytes. Historical clients should retrieve
the requested version's text and display a read-only document. Dependency
construction sites use:

```text
pine-test-source:/<version>/<escaped/source/path>?project=<escaped-project-uri>
```

The VS Code extension registers a content provider for that scheme.
Decode each escaped component exactly once: editor URI objects can already expose
decoded paths and queries. Late historical failures open version-qualified source
documents rather than highlighting an old range in newer workspace text.

## Errors, retention and recovery

Rejected source/version requests use JSON-RPC code `-32001`, with
`{ code, message }` error data. Run preparation/execution failures use the
structured terminal result described above. Categories include `unsupportedProtocol`,
`invalidUri`, `invalidVersion`, `invalidSource`, `invalidManifest`,
`versionMismatch`, `versionUnavailable`, `projectUnavailable`,
`workspaceUnavailable`, `workspaceChanged`, `sourceUnavailable`,
`snapshotTooLarge`, `snapshotCapacity`, `commitUnavailable`, `invalidRunId`,
`duplicateRunId`, `invalidSettings`, `unknownTest`, `discoveryIdRequired`,
`staleDiscovery`, `discoveryFailed` and `executionFailed`.

The server retains up to **16 source versions per source root**, shared by projects
under that root. In-flight operations pin versions. Least-recently-used inactive
versions are evicted together with their project inventories. If all slots are
pinned, the server returns `snapshotCapacity`; retry after work finishes.
Successful discovery is cached; cancellation/failure does not poison it.
Testing has four shared work slots and does not occupy the ordinary LSP ingress
lane while interpreting, compiling, initializing execution caches or running tests.

A snapshot's combined file bytes cannot exceed **64 MiB**. Uploaded paths reject
duplicates, file/directory conflicts, empty segments, `.`, `..`, separators inside
segments and NUL. Elm files/manifests must be valid UTF-8. These checks occur before
discovery. Clients cannot upload bytes under a falsely claimed version.

Retention is session-local, not persistent history storage. An evicted commit can
be rediscovered from local Git; an evicted file tree must be supplied again, or
recaptured if it is still current. Never substitute newer workspace bytes.

## Initial VS Code experience

The extension uses the native Testing API, not a webview or terminal-output parser.
The Test Explorer groups projects, source modules, `describe` groups and tests;
source ranges provide editor navigation and gutter integration. Kind/selection
tags and descriptions distinguish fuzz tests, TODOs, skips, focus exclusions and
empty groups. Run lifecycle, durations, expected/actual comparisons and structured
failure locations feed native test results.

Only the **current workspace** is displayed initially. Every manual run/replay
refreshes current discovery; superseded refreshes are cancelled and ignored.
Successful inventories replace removed tests. Discovery errors preserve the
previous tree for context but mark it unavailable for execution. Version-qualified
item identities and captured per-run maps prevent late old-run events from updating
newer items.

Run, group/file selection, exclusions, cancellation and scoped continuous runs are
supported. Continuous runs debounce edits, cancel obsolete work, coalesce refreshes
and avoid overlapping executions per subscription. Cross-edit scope remapping is
best-effort using discovery-root metadata and labels, not a protocol guarantee of
durable IDs. If duplicate labels make a selected item's mapping ambiguous across
inventories, execution is rejected until the user selects it again.

`pine.testing.seed` and `pine.testing.fuzzRuns` configure fuzz runs. "Pine: Replay Elm
Fuzz Test" reuses recorded settings only while source version and inventory match;
"Pine: Copy Elm Fuzz Reproduction Command" copies the matching CLI command. There is
no Debug/Coverage profile, Git-history browser or cross-version diff UI in this
initial extension. Those clients can be added without changing the protocol's
independently addressable source versions.

See the [design analysis](../explore/2026-11-01-elm-editor-test-discovery-and-execution/analysis.md)
for UX/protocol alternatives and the
[CLI guide](elm-test-command-line-interface.md) for shared runner semantics.
