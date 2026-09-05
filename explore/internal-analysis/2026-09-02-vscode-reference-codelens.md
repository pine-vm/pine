# Reference-count CodeLens for the Pine VS Code extension

Date: 2026-09-02

Last reviewed against the implementation: 2026-09-05

## Goal

Add the reference-count UI illustrated in the
[provided screenshot](https://github.com/user-attachments/assets/effcd52f-3114-4ac9-982b-a054c512663b):

- show `0 references`, `1 reference`, or `N references` above declarations;
- offer it only for declarations whose binding is at Elm module scope;
- open VS Code's references peek UI when the summary is selected;
- count usages in the summary, while showing one declaration location plus the
  usages in the peek list;
- never add a second declaration entry for an Elm type-annotation name;
- keep the updated VS Code extension compatible with older Pine language
  servers: CodeLens is absent, but activation and all capabilities offered by
  the older server continue to work.

The recommended implementation is the standard LSP CodeLens request plus lazy
resolution, combined with a small VS Code-specific client command. This is the
same broad design used by the current Roslyn-based C# extension.

## Executive summary

1. The .NET server reuses the Elm service's existing document-symbol request as
   the cheap candidate query. Each root `DocumentSymbol` already has a complete
   declaration `range` and one canonical `selectionRange`.
2. The .NET server advertises `codeLensProvider.resolveProvider`, maps those
   root symbols to unresolved LSP `CodeLens` values, and stores the URI,
   canonical position, and document identity in opaque `data`.
3. When VS Code resolves a visible lens, the .NET server asks the Elm service
   for references with `includeDeclaration = False`, counts those locations,
   and attaches a `pine.client.peekReferences` command titled with that count.
4. When the user selects the lens, the extension command invokes
   `vscode.executeReferenceProvider`. VS Code invokes the Pine reference
   provider with `includeDeclaration = true`; Pine returns one canonical
   declaration location and every usage. The command passes those locations to
   `editor.action.showReferences`.
5. Compatibility is capability-negotiated. The extension does not register its
   own CodeLens provider or issue CodeLens requests during activation. When an
   older server omits `codeLensProvider`, `vscode-languageclient` registers no
   provider, no CodeLens requests are sent, and the internal navigation command
   remains dormant.

This keeps initial CodeLens discovery cheap, avoids embedding a potentially
large and stale location list in every lens, and gives the requested
`2 references`/three-location behavior.

## Implementation progress

Two prerequisites identified during the original analysis are implemented:

- `DeclarationOccurrence` now stores a raw structural
  `scopePath : Path`; `[]` means module scope and nonempty paths are local
  visibility roots
  (`implement/Pine.Core/Elm/elm-in-elm/src/LanguageServiceAnalysis.elm:45-58`);
- `declarationOccurrencesForArguments` localizes every binding in a function
  argument pattern to the function implementation path
  (`LanguageServiceAnalysis.elm:362-389`);
- `References_resolution_limits_argument_patterns_to_their_function` proves
  that simple and tuple-destructured arguments do not resolve across sibling
  module declarations
  (`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs:919-966`).

The language server has also acquired infrastructure which the CodeLens work
should now use rather than duplicate:

- client document versions, document generations, accepted language-service
  versions, and pending updates are tracked separately
  (`implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs:50-109`);
- document updates are cancellable and are applied through the scheduled
  language-service session (`LanguageServer.cs:643-699,712-821`);
- the session runs revision-aware requests on a bounded worker pool whose
  workers share an invocation cache
  (`ScheduledLanguageServiceSession.cs:12-72` and
  `LanguageServiceSessionFactory.cs:51-102`);
- RPC request handlers now yield the ingress lane, log their work, and
  propagate client cancellation, including the current references handler
  (`implement/pine/Elm/LanguageServerRpcTarget.cs:425-458`).

The CodeLens feature is now implemented with protocol records, advertised
capability, RPC handlers, client command, refresh publisher, and
`includeDeclaration` support. Candidate discovery reuses the existing root
document symbols.

## Why C# says “2 references” but shows “Locations (3)”

This is normal in the current Roslyn LSP mode. The two numbers come from
different operations:

1. Roslyn resolves the lens by independently computing a usage-reference count
   and sets the command title to that count
   ([`CodeLensResolveHandler.cs`](https://github.com/dotnet/roslyn/blob/4cac4334c3ed532aea57169ebb11db0934a01ea8/src/LanguageServer/Protocol/Handler/CodeLens/CodeLensResolveHandler.cs#L25-L89)).
2. The command is `roslyn.client.peekReferences`, which invokes
   `vscode.executeReferenceProvider` only when clicked
   ([`serverCommands.ts`](https://github.com/dotnet/vscode-csharp/blob/dc82460e8ff06c9d764b593633577720cb19196c/src/lsptoolshost/server/serverCommands.ts#L40-L62)).
3. VS Code calls reference providers with `{ includeDeclaration: true }`
   ([`goToSymbol.ts`](https://github.com/microsoft/vscode/blob/1f625adb84abf41cdff31f40f66e58a222f033f6/src/vs/editor/contrib/gotoSymbol/browser/goToSymbol.ts#L77-L89)).
4. Roslyn honors that value
   ([`FindAllReferencesHandler.cs`](https://github.com/dotnet/roslyn/blob/4cac4334c3ed532aea57169ebb11db0934a01ea8/src/LanguageServer/Protocol/Handler/References/FindAllReferencesHandler.cs#L51-L80)).

Therefore, for the screenshot:

```text
lens title = 2 usage references
peek list  = 1 declaration + 2 usage references = 3 locations
```

VS Code does **not** derive `2` by inspecting and tagging the three popup
entries. The server supplied the title earlier.

The standard items are not tagged. The LSP references result is `Location[]`,
and a `Location` has only `uri` and `range`. The included declaration is
structurally indistinguishable from a usage. Proprietary clients can define
richer reference items, but Pine should not depend on such an extension.

Legacy OmniSharp is a useful caveat: its CodeLens request sets
`ExcludeDefinition: true` and passes those locations directly to the popup
([`codeLensProvider.ts`](https://github.com/dotnet/vscode-csharp/blob/dc82460e8ff06c9d764b593633577720cb19196c/src/omnisharp/features/codeLensProvider.ts#L108-L148)).
It need not show the same count/list difference.

## Relevant official contracts

The links below use the LSP 3.18 specification. These parts are unchanged in
the 3.17 contract currently linked throughout Pine's protocol model.

### CodeLens discovery, capability, and shape

The [Code Lens Request](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_codeLens)
states:

> “The code lens request is sent from the client to the server to compute code
> lenses for a given text document.”

The client capability is `textDocument.codeLens`; the server capability is
`codeLensProvider: CodeLensOptions`. `CodeLensOptions` contains:

> “Code lens has a resolve provider as well.”

The request method is `textDocument/codeLens`, its params contain a
`TextDocumentIdentifier`, and its result is `CodeLens[] | null`.

The same section defines a lens:

> “A code lens represents a command that should be shown along with source
> text, like the number of references, a way to run tests, etc.”

> “A code lens is *unresolved* when no command is associated to it. For
> performance reasons the creation of a code lens and resolving should be done
> in two stages.”

Its `range`:

> “Should only span a single line.”

Its optional `data` is:

> “preserved on a code lens item between a code lens and a code lens resolve
> request.”

These requirements imply that Pine should return declaration ranges and opaque
identity data first, not eagerly scan the whole workspace for every
declaration.

### Lazy resolution

The [Code Lens Resolve Request](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#codeLens_resolve)
states:

> “The code lens resolve request is sent from the client to the server to
> resolve the command for a given code lens item.”

The method is `codeLens/resolve`; both its parameter and result are `CodeLens`.

The official [VS Code `CodeLensProvider` API](https://code.visualstudio.com/api/references/vscode-api#CodeLensProvider)
gives the corresponding performance guidance:

> “This call should return as fast as possible and if computing the commands is
> expensive implementors should only return code lens objects with the range
> set and implement resolve.”

It also says resolution:

> “will be called for each visible code lens, usually when scrolling.”

The `vscode-languageclient` package already converts an advertised LSP
CodeLens capability into a registered VS Code provider and forwards both
requests
([`client/src/common/codeLens.ts`](https://github.com/microsoft/vscode-languageserver-node/blob/main/client/src/common/codeLens.ts#L36-L106)).
The Pine extension must not register a second `CodeLensProvider`. This is also
the backward-compatibility boundary: a server which does not advertise
`codeLensProvider` does not activate the language client's CodeLens feature.

### Refresh after project-wide changes

The [Code Lens Refresh Request](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#codeLens_refresh)
states:

> “The `workspace/codeLens/refresh` request is sent from the server to the
> client.”

> “As a result the client should ask the server to recompute the code lenses for
> these editors.”

It is a server-to-client **request**, not a notification. It is valid only when
the client advertises `workspace.codeLens.refreshSupport`. The specification
also warns that it is global and:

> “should be used with absolute care.”

Pine needs this for cross-file correctness: editing `B.elm` can change a count
displayed in an unchanged `A.elm`. Refresh requests should therefore be
debounced and coalesced after accepted language-service state changes, not sent
for every file during initial workspace loading.

### References and declaration inclusion

The [Find References Request](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_references)
states:

> “The references request is sent from the client to the server to resolve
> project-wide references for the symbol denoted by the given text document
> position.”

Its params are `ReferenceParams`, not merely
`TextDocumentPositionParams`. `ReferenceParams.context` is a
`ReferenceContext` containing:

> “Include the declaration of the current symbol.”

```typescript
interface ReferenceContext {
    includeDeclaration: boolean;
}
```

Its result is `Location[] | null`. Pine must explicitly deserialize and honor
this flag. VS Code cannot infer or add Pine's declaration range by itself.

The equivalent [VS Code `ReferenceContext`](https://code.visualstudio.com/api/references/vscode-api#ReferenceContext)
uses the same wording, while
[`ReferenceProvider`](https://code.visualstudio.com/api/references/vscode-api#ReferenceProvider)
defines project-wide location results.

### Locations are untagged

The official [LSP `Location`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#location)
definition says:

> “Represents a location inside a resource, such as a line inside a text file.”

It contains only:

```typescript
interface Location {
    uri: DocumentUri;
    range: Range;
}
```

Even
[`LocationLink`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#locationLink)
has no declaration/reference role, and `textDocument/references` does not
return `LocationLink` in any case. No standard tag configuration is missing.

### Commands and the VS Code-specific bridge

The official [LSP `Command`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#command)
contract says:

> “Commands are identified by a string identifier.”

and, importantly:

> “The protocol currently doesn't specify a set of well-known commands.”

It recommends that either a server execute a command through negotiated
capabilities or “the tool extension code could handle the command.” A command
has a `title`, identifier, and optional arguments.

Consequently, `editor.action.showReferences` is not portable LSP. The robust
boundary is a Pine-owned command registered by the extension. The official
[VS Code command reference](https://code.visualstudio.com/api/references/commands#_commands)
documents:

> “`vscode.executeReferenceProvider` - Execute all reference providers.”

It takes a URI and position and returns `Location[]`. The same reference
documents `vscode.executeCodeLensProvider`, including its optional resolved-item
count, which is useful for end-to-end tests.

`editor.action.showReferences` is not in that documented API-command subset,
although current VS Code registers it as an alias of
`editor.action.peekLocations`
([`goToCommands.ts`](https://github.com/microsoft/vscode/blob/1f625adb84abf41cdff31f40f66e58a222f033f6/src/vs/editor/contrib/gotoSymbol/browser/goToCommands.ts#L812-L859)).
It is a deliberate VS Code compatibility dependency and should be isolated in
the extension command, not emitted as though it were an LSP-standard command.

## Current Pine implementation

### Extension

`implement/vscode/extension/pine/client/src/extension.ts` creates one
`LanguageClient` for Elm files and starts it. It currently registers only the
`pine.showLanguageServerClientLog` command
(`extension.ts:67-168`). Since `vscode-languageclient` installs standard feature
providers from server capabilities, there is no CodeLens provider code in the
extension today and Pine should not add one.

The client package uses `vscode-languageclient` `^10.1.0`
(`implement/vscode/extension/pine/client/package.json:15-20`), while the
extension supports VS Code 1.75 or newer
(`implement/vscode/extension/pine/package.json:23-29`). That language client
already contains CodeLens discovery, resolution, and refresh plumbing.

The extension test harness is still inherited from the generic LSP sample: its
helper activates `vscode-samples.lsp-sample`, and its fixtures and assertions
exercise `.txt` completion/diagnostics rather than Pine Elm behavior
(`client/src/test/helper.ts:14-25`,
`client/src/test/completion.test.ts:10-35`, and
`client/src/test/diagnostics.test.ts:10-31`). CodeLens end-to-end coverage first
needs that harness aligned with the actual `Pine.pine` extension and an Elm
workspace fixture.

### Older language-server compatibility

The extension and server can be installed or selected independently because
`pineLanguageServer.pathToPineExecutableFile` can point to any Pine executable
(`implement/vscode/extension/pine/package.json:79-84`). Therefore, a newly
updated extension must treat CodeLens as optional.

Use normal LSP server-capability negotiation as the only feature gate:

- register `pine.client.peekReferences` as a local, internal command, but do not
  call it or send CodeLens requests during activation;
- do not manually register a VS Code `CodeLensProvider`;
- do not require `codeLensProvider` to exist in the initialize response and do
  not reject an older server based on its version string;
- do not send a custom request to probe for CodeLens support;
- keep the navigation command defensive: missing/malformed arguments or an
  unavailable reference result should log and return without throwing;
- do not add a manifest activation event or configuration requirement that
  depends on the new server.

With an older server, the language client sees no `codeLensProvider`, so the
editor shows no Pine reference lenses. Existing formatting, hover, completion,
definition, symbols, references, and rename capabilities continue to be
negotiated exactly as before. The internal command is not normally reachable
because only a newer server emits it in a resolved lens.

### .NET language server and protocol model

- `LanguageServer.Initialize` advertises formatting, hover, completion,
  definition, document symbols, references, and rename, but no CodeLens
  (`implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs:270-338`).
- `ServerCapabilities` has no `CodeLensProvider` member
  (`implement/Pine.Core/LanguageServerProtocol/ServerCapabilities.cs:8-19`).
- There are no `CodeLens`, `CodeLensParams`, `CodeLensOptions`, or LSP
  `Command` protocol records. `ClientCapabilities` also has no
  `textDocument.codeLens` or `workspace.codeLens.refreshSupport` model
  (`ClientCapabilities.cs:6-34`).
- `LanguageServerRpcTarget` exposes `textDocument/references` but no CodeLens
  methods (`implement/pine/Elm/LanguageServerRpcTarget.cs:425-458`).
- More importantly, the references RPC and server methods accept
  `TextDocumentPositionParams`, so the required `context.includeDeclaration`
  is discarded (`LanguageServerRpcTarget.cs:428-445` and
  `LanguageServer.cs:2182-2237`).
- The RPC target's `JsonRpc` property currently connects only the diagnostics
  publisher (`LanguageServerRpcTarget.cs:17-35`). CodeLens refresh needs a
  parallel server-to-client request publisher.
- The server already tracks both the latest client version and the version
  accepted by the language-service session
  (`LanguageServer.cs:50-67,888-916`). CodeLens discovery and resolution must
  use that distinction because an accepted `didChange` can still be pending
  when a later request enters.

### Elm language service

The service already contains all candidate metadata and most of the reference
semantics:

- `topLevelDeclarations` filters analyzed declaration occurrences by
  `scopePath == []` (`LanguageService.elm:660-685`);
- `textDocumentSymbol` emits only root module declarations, with custom-type
  constructors nested as children rather than root symbols
  (`LanguageService.elm:1469-1508`);
- each emitted `DocumentSymbol` already carries the complete declaration range
  and a canonical selection range. For an annotated function, the latter comes
  from the implementation declaration path rather than from the annotation
  (`LanguageService.elm:1511-1557`);
- `textDocumentReferencesGroupedByFilePath` resolves either a reference or a
  declaration under the cursor and returns both the target declaration range
  and grouped usage ranges (`LanguageService.elm:1691-1746`);
- `findReferences` searches workspace, core, and package modules
  (`LanguageService.elm:1808-1836`);
- `textDocumentReferences` currently throws away the declaration half and
  returns usages only (`LanguageService.elm:1640-1654`);
- rename deliberately uses every own-name range, including annotation and
  implementation names (`LanguageService.elm:2073-2125`).

That last distinction is important. Rename should continue editing both names,
but reference navigation should select exactly one canonical declaration name.

The original filtering trap for function arguments has been corrected, and the
scope representation now uses a raw `scopePath : Path`: `[]` means module-level
and a nonempty path identifies local visibility. Pattern occurrences start at
`[]`, and `declarationOccurrencesForArguments` assigns the function
implementation path before returning them. Consequently, parameters of
module-level functions are local to their function body rather than being
module declarations. Existing `let` processing subsequently applies its legacy
enclosing-let path to all occurrences in a `let` declaration; this change
deliberately does not alter that broader behavior.

For this feature, the existing document-symbol root list is a stronger
presentation boundary than selecting every occurrence with `scopePath == []`:
it also removes custom-type constructor children and already chooses one symbol
for an annotated value. Existing analysis omits ports and infix declarations
entirely (`LanguageServiceAnalysis.elm:244-248`).

Scope representation is analyzed separately in
[`2026-09-02-language-service-scope-and-binding-representation.md`](./2026-09-02-language-service-scope-and-binding-representation.md).
That document also evaluates whether Elm's no-shadowing rule makes
`ModuleLevelScope | LocalScope String` sufficient and covers imports, packages,
invalid editor states, typing, and runtime costs.

## Proposed behavior

### Eligible declarations

Return one lens per supported binding introduced at module scope:

- top-level functions and values;
- type aliases;
- choice type names;

Do not return lenses for:

- the module header or imports;
- function parameters;
- lambda parameters;
- `let` declarations or pattern binders inside expressions;
- individual choice-type constructors, which are children of the top-level
  custom-type document symbol rather than top-level syntax declarations;
- the annotation copy of an annotated function/value name.

Ports and infix operators should join the list only after their declaration and
reference occurrences are represented by `LanguageServiceAnalysis`; silently
claiming a zero count for symbols the reference engine cannot analyze would be
misleading.

Use the root list returned by the current `textDocumentSymbol` implementation.
That list is already based on `scopePath == []`, excludes constructor
occurrences from its root level, and emits one symbol for an annotated value.
If candidate construction is later moved directly into
`LanguageServiceAnalysis`, retain both the empty-scope check and the supported
declaration-kind check.

### Display range versus query position

Each existing root `DocumentSymbol` supplies the two source coordinates:

1. a one-line `range` controlling where VS Code displays the lens;
2. a canonical name `position` used to resolve references.

Construct the display range as a zero-width range at
`DocumentSymbol.range.start`, so an annotated value's lens appears above the
annotation. Use `DocumentSymbol.selectionRange.start` as the query position;
the current symbol builder selects the implementation name. There is still only
one lens and one declaration location.

For declaration navigation, return exactly that canonical name range. Do not
reuse `DeclarationRange`'s full `declNamesRanges`: that list intentionally
contains both annotation and implementation names for rename.

### Zero references

Show `0 references`, matching the C# convention. Keep its command active:
clicking it should show the single declaration location. Use singular only for
`1 reference`.

### Request flow

```text
VS Code
  -> textDocument/codeLens { textDocument }
.NET
  -> existing Elm TextDocumentSymbolRequest fileLocation
Elm
  -> root DocumentSymbol values [{ range, selectionRange, ... }]
.NET
  -> unresolved CodeLens[] with data {
       uri, position, clientVersion, documentGeneration
     }

VS Code (visible lens)
  -> codeLens/resolve CodeLens
.NET
  -> Elm TextDocumentReferencesRequest { position, includeDeclaration = False }
Elm
  -> usage locations
.NET
  -> resolved CodeLens command:
       title = "N references"
       command = "pine.client.peekReferences"
       arguments = [uriString, position]

User clicks
  -> pine.client.peekReferences(uriString, protocolPosition)
Extension
  -> vscode.executeReferenceProvider(uri, vscodePosition)
VS Code / language client
  -> textDocument/references {
       textDocument, position,
       context: { includeDeclaration: true }
     }
.NET -> Elm
  -> one canonical declaration location + usage locations
Extension
  -> editor.action.showReferences(uri, position, locations)
```

## Required changes

### 1. VS Code extension

In `implement/vscode/extension/pine/client/src/extension.ts`:

1. Import `Uri`, `Position`, and the LSP position type as needed.
2. Register `pine.client.peekReferences` in `context.subscriptions`.
3. Accept the URI as a string and the position as a plain protocol object.
   Reconstruct real VS Code `Uri` and `Position` instances; server command
   arguments arrived through JSON and do not have VS Code class prototypes.
4. Invoke `vscode.executeReferenceProvider`.
5. If an array is returned, invoke `editor.action.showReferences` with the URI,
   position, and returned `Location[]`.
6. Handle `undefined`, cancellation, and command failure without an unhandled
   rejection; log failures to `pineOutputChannel`.
7. Keep command registration independent of server capabilities, but leave all
   provider registration to `vscode-languageclient`. Do not make extension
   activation await or probe a CodeLens endpoint.

No manual `languages.registerCodeLensProvider` call is needed. No
`contributes.commands` entry is needed for this internal command unless it
should also appear in the command palette.

Keep the undocumented `editor.action.showReferences` dependency in this one
function so it can later be replaced with a documented UI mechanism if VS Code
changes it.

### 2. Shared LSP protocol records

Under `implement/Pine.Core/LanguageServerProtocol/`, add protocol records for:

- `CodeLensParams`;
- `CodeLensOptions`;
- `CodeLens`;
- `Command`;
- `ReferenceParams`;
- `ReferenceContext`;
- `CodeLensClientCapabilities` under `TextDocumentClientCapabilities`;
- `CodeLensWorkspaceClientCapabilities` under
  `ClientCapabilitiesWorkspace`, including `refreshSupport`.

Add `CodeLensOptions? CodeLensProvider` to `ServerCapabilities`. Keep property
names compatible with the existing camel-case JSON serializer.

Represent `CodeLens.Data` in a form that survives a JSON round trip through
StreamJsonRpc, and explicitly decode it into a typed resolve payload. The
payload should contain the normalized URI, canonical position, client version,
and document generation. The current server tracks those identities separately
from the accepted language-service version; do not stamp a lens as current
while that document still has a newer pending update.

### 3. .NET language server

In `implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs`:

1. Advertise `CodeLensProvider = new CodeLensOptions(ResolveProvider: true)`.
2. Add `TextDocument_codeLens`:
   - normalize/map the document URI;
   - capture the current document identity and compare the client version with
     the language-service-accepted version;
   - if a newer update is pending, return no candidates for that pass rather
     than blocking or querying stale syntax; the post-acceptance refresh will
     request them again;
   - reuse `TextDocumentSymbolRequest`/`TextDocument_documentSymbol` and map
     each root document symbol to one unresolved lens;
   - use a zero-width range at the symbol's range start for display and its
     selection-range start as the canonical query position;
   - return unresolved lenses with opaque resolve data.
3. Add `CodeLens_resolve`:
   - validate and decode `data`;
   - compare both the current client document identity and the accepted
     language-service version;
   - reject stale coordinates with LSP `ContentModified`, or leave the lens
     unresolved and request a refresh if the RPC layer cannot yet express that
     error;
   - request usages with `includeDeclaration = false`;
   - set the singular/plural title and `pine.client.peekReferences` command.
4. Change `TextDocument_references` to accept `ReferenceParams` and forward
   `Context.IncludeDeclaration`.
5. Add a debounced CodeLens refresh publisher, negotiated through
   `workspace.codeLens.refreshSupport`, after accepted document/package/file
   changes that can alter project-wide counts.

In `implement/pine/Elm/LanguageServerRpcTarget.cs`:

1. Expose `textDocument/codeLens`.
2. Expose `codeLens/resolve`.
3. Pass cancellation tokens through both methods.
4. Send `workspace/codeLens/refresh` as an awaited JSON-RPC request through the
   existing client connection, connected from the `JsonRpc` property alongside
   diagnostics publication. Use the existing dynamic-registration request as
   the `Invoke...Async` precedent; diagnostics itself is a notification.
5. Update `textDocument/references` to deserialize `ReferenceParams`.

Follow the current request-handler pattern: increment the RPC sequence, log,
`await Task.Yield()` to release the ordered ingress lane, pass the cancellation
token into the server/session, and log cancellation separately. The existing
diagnostics callback wiring in `JsonRpc` is the model for connecting the server
core to the client channel.

### 4. C# ↔ Elm language-service ABI

In
`implement/Pine.Core/Elm/LanguageServer/LanguageServiceInterface/LanguageServiceRequest.cs`:

- introduce a dedicated references request record with
  `IncludeDeclaration`, instead of continuing to use
  `ProvideHoverRequestStruct`;
- encode that additional field.

The existing `TextDocumentSymbolRequest` and `TextDocumentSymbolResponse`
already carry the CodeLens candidate's complete range and canonical selection
range, so no CodeLens-specific C#↔Elm request, response, encoder, or decoder is
needed. `TextDocumentReferencesResponse` can remain a flat location list.
Update the mirrored request comments so the Elm and C# declarations remain
visibly aligned.

### 5. Elm language service

In
`implement/Pine.Core/Elm/elm-in-elm/src/LanguageServiceInterface.elm`:

- replace the `ProvideReferencesRequestStruct = ProvideHoverRequestStruct`
  alias with a record that adds `includeDeclaration : Bool`.

In `implement/Pine.Core/Elm/elm-in-elm/src/LanguageService.elm`:

1. Keep `textDocumentSymbol` as the CodeLens candidate source: root symbols are
   module declarations, `range` starts at the display line, and
   `selectionRange` identifies the canonical declaration name.
2. Keep `findReferences` returning usages only, preserving its usefulness for
   the lens count and rename.
3. When `includeDeclaration` is true, prepend exactly one canonical declaration
   `LocationInFile` to the flattened usage locations.
4. Derive that canonical range from the declaration occurrence's
   `declarationPath` with `SelectName`; do not use the full declaration range or
   every `namePaths` entry. Handle `ResolvedModuleTarget` separately by selecting
   the module-definition name.
5. Keep rename based on all declaration-name ranges, so an annotation and
   implementation still rename together.

No CodeLens-specific Elm request is needed. Prefer extracting a shared
canonical-declaration-range helper from the current document-symbol and
reference code rather than adding a second declaration resolver. Honoring
`includeDeclaration` intentionally changes the existing Find References result
for every caller, not only CodeLens clicks, so cover module, value, type, and
constructor targets as applicable.

## Performance and consistency

`findReferences` currently scans every parsed workspace, core, and package
module for one target (`LanguageService.elm:1808-1836`). Eagerly calling it for
every declaration in `textDocument/codeLens` would multiply that cost by the
number of declarations and delay opening every editor. Reusing
`textDocumentSymbol` keeps discovery to a single-document analysis lookup;
unresolved lenses defer project-wide scans until lenses become visible.

Lazy resolve limits work to visible lenses, but scrolling a declaration-heavy
file can still cause many full scans. The current
`ScheduledLanguageServiceSession` can run read-only requests speculatively on a
bounded worker pool (four workers by default), so several visible-lens resolves
may scan concurrently rather than serially. This improves responsiveness but
does not reduce total work. After correctness is established:

- cache counts by accepted language-service revision plus target identity; or
- build a state-level reverse-reference index and derive all counts from one
  traversal.

The first option overlaps with, but is not equivalent to, the Pine engine's
existing in-memory invocation cache. The current language-service session gives
each worker a buffered cache backed by one shared `ConcurrentInvocationCache`
(`LanguageServiceSessionFactory.cs:59-77` and
`ScheduledLanguageServiceSession.cs:19-65`). At the VM level, an entry is keyed
by the evaluated expression and its complete `StackFrameInput`
(`InvocationCache.cs:12-20`). Because a language-service call evaluates
`handleRequestInCurrentWorkspace` with the encoded request and immutable
language-service state as arguments (`LanguageServiceState.cs:370-388`), a
repeated byte-for-byte-equivalent reference request against the same state can
indeed benefit from that cache.

A count cache at the .NET language-server layer can still avoid work that the
VM cache does not:

- VM cache admission is an execution optimization governed by cost thresholds,
  not a guarantee that every reference request result is retained
  (`PineVM.cs:942-951`).
- A VM hit still goes through language-service scheduling, request encoding,
  Pine function dispatch, response decoding, location mapping/filtering, and
  counting. A semantic cache can return the already mapped integer before
  crossing the C#–Elm ABI.
- The VM retains the Elm response containing all internal reference locations,
  not the final count after .NET has mapped and omitted locations that cannot be
  represented by client URIs. The semantic cache stores that post-mapping
  integer.
- A revision/target cache has an explicit CodeLens lifetime and can be bounded
  independently. Advancing the scheduled language-service state invalidates
  its entries without depending on the admission or retention policy of the
  general-purpose VM cache.

It would nevertheless be wasteful to add a second cache merely on assumption.
First measure repeated CodeLens resolution with the existing shared VM cache.
Add the state/target count cache only if encoding, scheduling, decoding/mapping,
or non-admitted scans remain material; otherwise rely on the Pine cache. A
state-level reverse-reference index is not redundant with invocation caching:
it changes one-scan-per-target into one traversal for all targets and is the
better option if many distinct visible declarations dominate the cost.

Do not cache popup locations in the CodeLens command. Re-running references on
click produces current navigation results after edits. Document/version checks
and `workspace/codeLens/refresh` prevent stale titles.

The current document pipeline records client changes before their cancellable
language-service updates necessarily finish. Trigger refresh only after an
update has been accepted by the session and confirmed as the latest update for
that document. Suppress refresh during initial workspace loading and coalesce
bursts; otherwise opening a project can cause a global refresh storm.

Package locations already map through `LanguageServer.MapLocations`; preserve
the existing behavior of omitting locations that cannot be represented by a
client URI, and count the mapped locations shown to the user rather than
unmappable internal locations.

## Test plan

### Elm language service

Extend
`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs`
with scenarios proving:

- existing document-symbol output gives one root symbol for annotated and
  unannotated module-level values;
- parameters and `let` declarations produce no root symbols;
- type aliases and custom types produce root symbols, while constructors remain
  children;
- `includeDeclaration = false` returns usages only;
- `includeDeclaration = true` returns one canonical declaration plus usages;
- the annotation name is not a second navigation entry;
- zero, one, same-file, cross-file, package, and shadowed-name cases are
  correct.

The existing complex document-symbol test and cross-module reference/rename
tests already provide useful fixtures
(`ElmLanguageServiceTests.cs:758-966,1131-1373,1378-1537`). Extend those
scenarios rather than introducing a second candidate protocol. Update
performance snapshots only where the changed references request is
deliberately exercised; do not weaken unrelated snapshots.

### ABI and .NET server

Extend `implement/Pine.Core.Tests/Elm/LanguageServer/LanguageServiceTests.cs`
for the changed references-request encoding, and add protocol JSON
round-trip coverage for CodeLens `data`, commands, and `ReferenceParams`.

Add focused language-server tests for:

- `codeLensProvider.resolveProvider == true`;
- candidate discovery reusing root document symbols;
- one-based Elm to zero-based LSP range conversion;
- unresolved lens `data` round-tripping;
- pending-update synchronization and stale-version/generation rejection;
- `0`, `1`, and plural titles;
- command identifier and JSON-safe arguments;
- `ReferenceParams.context.includeDeclaration` forwarding;
- refresh negotiation/debouncing;
- cancellation reaching Elm evaluation.

Place these beside the current focused server tests under
`implement/Pine.Core.Tests/Elm/LanguageServer/`, using the existing session
factory and test-double infrastructure in `LanguageServerTestDoubles.cs`.

Extend `implement/Pine.IntegrationTests/ElmLanguageServerTests.cs` to exercise
the serialized `initialize`, `textDocument/codeLens`, `codeLens/resolve`, and
`textDocument/references` messages.

### VS Code extension

First replace the remaining sample extension ID, `.txt` fixtures, and sample
completion/diagnostics assumptions with a Pine Elm workspace that contains
`elm.json`; otherwise the extension's `workspaceContains:**/elm.json`
activation event and Elm document selector are not exercised.

Then add an end-to-end test that calls the documented
`vscode.executeCodeLensProvider` command with a resolve count, then assert:

- lenses exist only at module-level declarations;
- an annotated declaration has one lens;
- title pluralization/count is correct;
- the command is `pine.client.peekReferences`;
- invoking it causes a reference-provider request whose result contains the
  declaration and usages.

The peek widget itself is difficult to assert reliably. Unit-test the command's
argument conversion separately if a VS Code API seam is introduced; otherwise
the returned reference list and command invocation are the stable assertions.

Add a backward-compatibility scenario using an older or stub server whose
initialize result omits `codeLensProvider`. Verify that:

- extension activation and the language-client connection succeed;
- no `textDocument/codeLens` or `codeLens/resolve` request is sent;
- capabilities that the older server does advertise remain usable;
- the internal navigation command safely returns when invoked without valid
  arguments;
- no uncaught rejection or user-facing compatibility error is produced.

This scenario is required release coverage for the extension. It protects the
supported pairing “new extension + old server”; it must not be replaced by a
server-version comparison test.

### Validation commands during implementation

Follow the repository's Microsoft.Testing.Platform rule:

```text
cd implement/Pine.Core.Tests
dotnet run -- --filter-method="*RelevantTestName*"
```

Run affected broader .NET test projects with `dotnet run`, not `dotnet test`.
For the extension, run its existing `npm run compile`, `npm run lint`, and
`npm test` scripts. Format changed C# with `dotnet format`.

## Implementation sequence

0. **Completed:** Migrate declaration scopes to raw structural paths, correct
   function-argument pattern scopes, and cover simple and destructured
   arguments with reference-resolution tests.
1. Add the LSP CodeLens, command, references, and client-capability records with
   serialization tests.
2. Make references honor `includeDeclaration`, selecting exactly one canonical
   declaration range, and add semantic tests.
3. Add .NET CodeLens discovery by reusing root document symbols, then implement
   lazy resolve, document-identity checks, capabilities, and RPC handlers.
4. Register the Pine client navigation command and add both new-server CodeLens
   tests and old-server graceful-degradation tests.
5. Add negotiated, debounced refresh after accepted language-service state
   changes.
6. Run focused tests, then broader server/extension validation and C# format.
7. Manually verify the screenshot scenario:
   `2 references` above the declaration and three peek locations.

## Implementation log

### 2026-09-05: Implementation started

- Rechecked the plan against the current language-service scheduler, document
  synchronization, LSP protocol records, RPC target, and VS Code extension.
- Updated the screenshot to the latest image supplied with the implementation
  request.
- Started with protocol and references semantics so later CodeLens handlers can
  build on tested contracts.
- Significant pre-implementation discovery: `textDocumentSymbol` already
  provides exactly one root item per eligible declaration plus both the display
  and canonical-name ranges. The implementation will reuse it rather than add
  the CodeLens-specific Elm request proposed in the earliest version of this
  analysis.
- Significant compatibility constraint: the extension and server can be
  upgraded independently. The extension implementation therefore leaves
  CodeLens provider registration entirely to `vscode-languageclient` capability
  negotiation and does not probe or version-check the server.
- Added the LSP CodeLens, command, references, and CodeLens capability records,
  with JSON round-trip tests for opaque resolve data and
  `context.includeDeclaration`.
- Discovery: the C# record member for the LSP command identifier cannot simply
  be named after its enclosing `Command` type. It uses an explicit
  `JsonPropertyName("command")` so the wire shape remains the standard LSP
  shape.
- Implemented `includeDeclaration` end to end in the Elm request ABI, the .NET
  encoder, the LSP references handler, and the Elm reference result.
- The language service now retains one canonical declaration name range in
  addition to the complete declaration range and all rename ranges. For
  annotated functions, the canonical range is the implementation name, while
  rename still updates both the annotation and implementation.
- Added a semantic test proving that `includeDeclaration = true` prepends
  exactly one implementation declaration and that `false` preserves the
  existing usage-only response.
- Discovery: adding the Boolean request field and canonical range increases the
  deterministic VM performance counters for the existing reference scenarios.
  The expected snapshots were refreshed only after confirming that all semantic
  reference assertions still passed.
- Minor backtracking: the first filtered-test invocation ran from the repository
  root and therefore found no project. Subsequent invocations follow the
  repository rule and run from `implement/Pine.Core.Tests`.
- Implemented server capability advertisement, module-level CodeLens discovery,
  lazy reference-count resolution, and both LSP RPC endpoints.
- Discovery reuses only the root list returned by `textDocument/documentSymbol`;
  it deliberately does not flatten `children`, which makes the module-level-only
  rule structural rather than dependent on symbol kinds.
- Resolve data captures the canonical selection position plus the client
  document version and generation. Discovery and resolution both suppress
  results when an update is pending, and resolution leaves stale lenses
  unresolved rather than relying on an unverified StreamJsonRpc mapping for the
  LSP `ContentModified` error.
- Added server tests covering capability advertisement, zero-width display
  ranges, exclusion of child symbols, usage-only counts, navigation arguments,
  and suppression after an intervening edit.
- Minor test surprise: the mixed string/position command-argument expectation
  needed an explicit `object[]`; collection-expression inference could not
  select the generic assertion overload.
- Added capability-negotiated `workspace/codeLens/refresh` requests. Accepted
  document updates and applied workspace changes schedule a single global
  refresh after a short debounce; unsupported clients and initial workspace
  loading do not receive the request.
- The refresh publisher is connected at the RPC boundary, keeping StreamJsonRpc
  out of the implementation-neutral language-server class. Publisher failures
  are logged and contained because refresh is advisory.
- Added tests proving that a burst of accepted updates is coalesced and that a
  client which does not advertise `workspace.codeLens.refreshSupport` receives
  no refresh request.
- Minor backtracking: the first refresh-test build exposed a missing
  `System.Threading` import for `Interlocked`; no production design changed.
- Registered the internal `pine.client.peekReferences` command. It validates
  opaque server arguments, asks VS Code's reference provider for locations, and
  opens the built-in reference peek only when locations exist; malformed
  arguments and provider failures are contained.
- Replaced the inherited sample-extension test assumptions with a real Elm
  workspace and `Pine.pine` activation. Added end-to-end assertions for
  module-level lenses, the `2 references` count, and the three navigation
  locations, plus focused command-conversion tests.
- Added a minimal old-server LSP fixture which advertises references but omits
  CodeLens. Its separate extension-host scenario verifies activation, existing
  reference-provider use, safe direct invocation of the internal command, and
  the absence of CodeLens requests.
- Discovery: the checked-in `vscode-languageclient` 10.1 types use package
  exports that TypeScript's previous default module resolution could not read.
  The client test project now uses the Node 16 module and resolver, and its
  CommonJS Mocha import was adjusted accordingly.
- Validation: TypeScript compilation and ESLint pass. The extension-host test
  command built the language server but stopped while the VS Code test harness
  was resolving a downloadable VS Code version, before any tests ran; this
  environment did not provide the required download.
- Review found a compatibility gap for clients that support CodeLens but not
  refresh: an initial CodeLens request could race the yielded `didOpen` update,
  receive an empty result, and never be prompted to retry. Discovery now waits
  cancellably for the latest pending document update before reading symbols, so
  refresh support is an optimization rather than a prerequisite.
- Added a regression test which blocks document acceptance, starts discovery
  without refresh support, and verifies that discovery resumes with the lens
  after acceptance. This was the first material design correction prompted by
  implementation review.
