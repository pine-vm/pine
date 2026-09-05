# Language-Server Formatting Priority

Date: 2026-09-05

## Purpose

Formatting is an explicit user command and should complete as soon as the formatting computation itself allows. In particular, it should not wait tens of seconds for earlier edits to finish processing in the Elm language-service state.

This document reassesses the state-heavy design originally proposed for that goal. The revised design keeps formatting independent of the language-service state, leaves document synchronization largely unchanged, and adds the version, identity, and timing information needed to diagnose the actual delay.

## Revised conclusion

The original proposal was too broad for the problem:

- It split document ingestion from synchronization and introduced an owned update processor.
- It proposed a global priority coordinator, cancellation reasons, yielding and retrying background work, fairness policy, and a reserved executor.
- It coupled the formatting fix to workspace initialization, diagnostics scheduling, watched files, document close, and future prioritization of hover and completion.
- It proposed rejecting ordinary concurrent edits with `ContentModified`, although the LSP specification advises against that use of the error.

Most of this machinery may be useful for a general language-server scheduler, but none of it is required to establish the property we need:

> Formatting can finish while an earlier language-service document update is still processing.

The formatter already accepts source text directly and does not need parsed or committed language-service state. The minimal design should preserve that separation. It should not submit formatting to `RevisionedOperationScheduler`, wait for `ILanguageServiceSession.AddFileAsync`, pause language-service work, or change the synchronization algorithm merely to format a document.

There is one important correction to the suggested simplification: a standard LSP formatting request does **not** contain document text or a document version. A conforming server therefore cannot make standard `textDocument/formatting` entirely stateless. For an open document it still needs the lightweight text mirror maintained from `didOpen` and `didChange`. That mirror is protocol synchronization state, but it is not the expensive Elm language-service state.

If a custom request explicitly carries source text, that source can be formatted as request-scoped input without reading or changing either state. Such a request is an extension, not standard `textDocument/formatting`.

## What the LSP specification says

The links and quotations in this section refer to the in-development [LSP 3.18 specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/).

### The formatting request contains identity and options, not content

The specification describes [`textDocument/formatting`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_formatting) as follows:

> “The document formatting request is sent from the client to the server to format a whole document.”

Its parameters are:

```typescript
interface DocumentFormattingParams extends WorkDoneProgressParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The formatting options.
     */
    options: FormattingOptions;
}
```

[`TextDocumentIdentifier`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocumentIdentifier) contains only a URI:

```typescript
interface TextDocumentIdentifier {
    /**
     * The text document's URI.
     */
    uri: DocumentUri;
}
```

The response is `TextEdit[] | null`, also without a document version. Therefore:

- There is no formatting-request content to add to tracked state.
- There is also no request content from which a standard handler could compute formatting independently.
- Adding `text` or `version` to this payload would define a custom protocol extension.

### The client owns the content of an open document

The [`textDocument/didOpen`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_didOpen) section states:

> “The document's content is now managed by the client and the server must not try to read the document's content using the document's Uri.”

After [`textDocument/didClose`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_didClose), the ownership changes:

> “The document's master now exists where the document's Uri points to.”

For standard formatting, the source is consequently:

- the synchronized client-content mirror for an open document;
- the URI-backed workspace content for a closed document.

Formatting the open file from disk would violate this ownership rule and could ignore unsaved edits.

### Earlier changes must be represented before a dependent request

The [`textDocument/didChange`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_didChange) section says:

> “Before requesting information from the server (e.g., `textDocument/completion` or `textDocument/signatureHelp`), the client must ensure that the document's state is synchronized with the server to guarantee reliable results.”

For mirroring content it instructs the server to:

> “apply the 'textDocument/didChange' notifications in the order you receive them.”

The general [request, notification and response ordering](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#messageOrdering) rule permits parallel execution only:

> “as long as this reordering doesn't affect the correctness of the responses.”

This means formatting may overtake the **expensive processing** of earlier content, but it must not see text older than an earlier received `didChange`. The existing short update of `_clientTextDocumentContents` and `_clientTextDocumentVersions` is the relevant synchronization point. Acceptance of the same text by the Elm language service is not.

### Formatting must not predict that the client applied edits

The formatting response merely describes edits. It does not transfer ownership of the document to the server and does not confirm that the client applied them. Therefore formatting must not write the proposed formatted content into `_clientTextDocumentContents`. A subsequent `didChange` is the authoritative report of any applied edit.

### `ContentModified` is not an ordinary version mismatch response

The [LSP error-code definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#errorCodes) says:

> “The server detected that the content of a document got modified outside normal conditions. A server should NOT send this error code if it detects a content change in its unprocessed messages. The result even computed on an older state might still be useful for the client.”

The original proposal's unconditional `ContentModified` response for a newer normal `didChange` is therefore not appropriate. We should still capture and compare document identity:

- honor `$/cancelRequest` when the client cancels obsolete formatting;
- log whether the source version/generation changed during formatting;
- avoid applying any result on the server;
- if local policy suppresses a known-stale result, return no edits rather than claiming an out-of-band modification.

The standard request and response contain no version handshake. Internal tracking improves correctness and observability, but cannot add a protocol guarantee which is absent from `textDocument/formatting`.

## Current implementation

The relevant implementation is concentrated in:

- [`implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs`](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs)
- [`implement/pine/Elm/LanguageServerRpcTarget.cs`](../../implement/pine/Elm/LanguageServerRpcTarget.cs)
- [`implement/pine/Elm/LanguageServerAdapters/ElmDocumentFormatters.cs`](../../implement/pine/Elm/LanguageServerAdapters/ElmDocumentFormatters.cs)
- [`implement/Pine.Core/Elm/LanguageServer/ScheduledLanguageServiceSession.cs`](../../implement/Pine.Core/Elm/LanguageServer/ScheduledLanguageServiceSession.cs)

`TextDocument_didChangeAsync` records the newest client text and version before it awaits language-service processing ([source](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L710-L795)). `TextDocument_formattingAsync` logs entry, snapshots that client-managed text, and invokes `IDocumentFormatter` through dedicated bounded execution ([source](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L1417-L1503), [source](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L1704-L1774)). Formatting does not enter `ScheduledLanguageServiceSession` or its revisioned scheduler ([formatting source](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L1417-L1593), [scheduler boundary](../../implement/Pine.Core/Elm/LanguageServer/ScheduledLanguageServiceSession.cs#L89-L104)).

The direct-server regression test blocks `AddFileAsync`, invokes formatting before releasing it, and verifies that formatting sees the changed client text and finishes while the update remains incomplete ([test](../../implement/Pine.Core.Tests/Elm/LanguageServer/LanguageServerFormattingTests.cs#L23-L85)). That proves independence after `TextDocument_formattingAsync` has been invoked; because the test calls the server object directly, it does not test when StreamJsonRpc invokes that method ([same test](../../implement/Pine.Core.Tests/Elm/LanguageServer/LanguageServerFormattingTests.cs#L48-L62)).

### Why the screenshot can still show a delay longer than ten seconds

The reported capture shows VS Code waiting while the visible server output has no `Formatting request … received` event ([capture](https://github.com/user-attachments/assets/cc646250-83ca-444b-9a85-efee0f4f9264)). That event is emitted at the beginning of `TextDocument_formattingAsync`, before source capture, lock acquisition, formatter-capacity acquisition, or formatter execution ([source](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L1417-L1439)). Consequently, the capture is consistent with delay before the formatting implementation starts; it is not evidence that the formatter itself spent ten seconds computing.

There is a concrete pre-handler serialization boundary. Pine constructs StreamJsonRpc 2.25.29 without changing `JsonRpc.SynchronizationContext` ([package version](../../implement/pine/pine.csproj#L50), [construction](../../implement/pine/CLI/LangServerCommand.cs#L195-L210)). That exact StreamJsonRpc version initializes a non-sticky `NonConcurrentSynchronizationContext` “to preserving order of incoming messages” ([v2.25.29 source](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/src/StreamJsonRpc/JsonRpc.cs#L258-L262)). Its documentation states:

> “By default, requests are dispatched one at a time, in order. When an async RPC method yields (i.e. returns a Task, whether complete or incomplete) the next request can be dispatched.”
>
> — [StreamJsonRpc, “Receiving a JSON-RPC request”](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/docfx/docs/recvrequest.md#receiving-a-json-rpc-request)

Therefore the RPC server does force **invocation order until each earlier handler first yields**. The dependency's test suite verifies both sides of that rule: the default context is `NonConcurrentSynchronizationContext` ([test](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/test/StreamJsonRpc.Tests/JsonRpcTests.cs#L883-L886)), and a second synchronous method is not invoked while the first synchronous method is blocked ([test](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/test/StreamJsonRpc.Tests/JsonRpcTests.cs#L2154-L2163)).

Pine's `didChange` RPC method returns the task from `TextDocument_didChangeAsync` ([source](../../implement/pine/Elm/LanguageServerRpcTarget.cs#L117-L125)), and that asynchronous operation first waits at `GetLanguageServiceStateAsync` or `AddFileAsync` after updating the client mirror ([source](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L746-L795), [source](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L850-L871)). Under StreamJsonRpc's documented rule, the unfinished language-service tail of `didChange` does **not** by itself hold the RPC queue after that yield.

The remaining risk is an earlier slow **synchronous** RPC handler. Hover, completion, definition, document-symbol, references, and rename are exposed synchronously and call the corresponding server operations before returning ([source](../../implement/pine/Elm/LanguageServerRpcTarget.cs#L161-L239)). If one of those messages precedes formatting on the connection and takes ten seconds, StreamJsonRpc's default context does not invoke formatting during those ten seconds ([ordering documentation](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/docfx/docs/resiliency.md#concurrency-vs-message-ordering), [blocking test](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/test/StreamJsonRpc.Tests/JsonRpcTests.cs#L2154-L2163)).

So the answer is qualified:

- **Yes**, StreamJsonRpc's default ingress ordering can account for the observed pre-handler delay when a preceding synchronous language-feature request is slow ([Pine construction](../../implement/pine/CLI/LangServerCommand.cs#L203-L210), [StreamJsonRpc rule](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/docfx/docs/resiliency.md#default-ordering-and-concurrency-behavior)).
- **No**, it should not wait for the entire asynchronous `didChange`/`AddFileAsync` operation merely because that notification arrived first; StreamJsonRpc admits the next message when that handler returns its incomplete task ([Pine async boundary](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L793-L795), [StreamJsonRpc rule](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/docfx/docs/recvrequest.md#receiving-a-json-rpc-request)).
- **The capture alone cannot identify the preceding RPC method.** Pine logs formatting only after the RPC target has called the core method, and it does not currently log receipt/start/end for every RPC target method ([RPC target](../../implement/pine/Elm/LanguageServerRpcTarget.cs#L145-L239), [formatting entry log](../../implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs#L1422-L1429)). A wire trace or RPC-target ingress events are required to distinguish a blocked predecessor from a client-side delay.

The appropriate correction is not to make standard formatting depend on more state. Preserve ordered, cheap document-mirror ingestion, but ensure every potentially expensive RPC method yields that ordered ingress lane before doing language-service work; also log method receipt, invocation, first yield, and completion at the RPC boundary. Clearing `JsonRpc.SynchronizationContext` would maximize concurrency, but StreamJsonRpc documents that this gives up message ordering, so doing that without a separate ordered document-ingress mechanism could let formatting overtake an earlier `didChange` and read stale text ([StreamJsonRpc configuration](https://github.com/microsoft/vs-streamjsonrpc/blob/v2.25.29/docfx/docs/resiliency.md#default-ordering-and-concurrency-behavior), [LSP synchronization requirement](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_synchronization)).

## Minimal design

### 1. Keep language-service synchronization unchanged

Do not redesign `didOpen`, `didChange`, the pending-update records, or `RevisionedOperationScheduler` for formatting.

The handler may continue processing the same text into the Elm language-service state in the background. Formatting neither waits for that work nor cancels it. Existing version supersession remains responsible for obsolete language-service updates.

### 2. Snapshot only the source needed by the formatter

On a standard formatting request:

1. Assign an internal formatting request identity and record receipt time.
2. For an open document, capture URI, client version, content, and per-document generation together in one short critical section.
3. For a closed document, read the URI-backed workspace content without holding the document-state lock.
4. Invoke `IDocumentFormatter` with that immutable request snapshot.

This snapshot is not added to the Elm language-service state. It is also not a new authoritative copy: it is a request-local view of the existing protocol content mirror.

The dispatcher must preserve the cheap ingress ordering between an earlier `didChange` and this capture. It need not preserve completion order between formatting and the expensive language-service update started by that change.

### 3. Keep the formatter execution path independent

The formatting path must not:

- await workspace initialization for an open document;
- await `AddFileAsync` or any language-service scheduler operation;
- wait for language-service document versions to catch up;
- pause, cancel, yield, or retry background language-service operations;
- publish diagnostics before returning edits;
- run logging I/O while holding the document-state lock.

First instrument the existing direct path and reproduce the delay with controlled gates. If evidence shows that formatting is waiting for shared execution capacity, add one bounded formatter executor which background Pine VM work cannot occupy. That is a local execution-isolation change; it does not require a general priority coordinator or changes to synchronization semantics.

### 4. Treat the result as a proposal

Compute and return edits from the captured source. Do not replace the client mirror with formatted text. Let the client report applied edits through `didChange`.

Capture the current document identity again when formatting completes. Use the comparison for logging, cancellation handling, and an explicit stale-result policy, not to mutate synchronized content. Do not use `ContentModified` for an ordinary later notification.

Formatting diagnostics are not needed to construct the response. Run them later against an explicit source snapshot, let the resulting `didChange` trigger them, or omit them from successful formatting.

### 5. Keep a content-bearing alternative explicitly custom

If Pine controls both client and server and wants a completely state-independent operation, define a separate custom request containing at least URI, source text, formatting options, and a client-provided identity/version. Its source text remains request-scoped and is never merged into the document mirror or language-service state.

This alternative does not replace standard `textDocument/formatting` for third-party LSP clients. It also does not remove the need to synchronize open documents for all other language features.

## Tracking and observability

Expanding tracking is useful even though synchronization remains unchanged. Structured events should include:

- document URI and internal formatting request identity;
- client document version and per-document generation, when available;
- source origin: open-document mirror, workspace, or custom request;
- relevant document-update sequence and latest language-service-accepted version;
- formatting received, source captured, computation started, computation completed, response returned, canceled, or suppressed as stale;
- time from receipt to source capture, capture to formatter start, formatter duration, and post-format response work;
- whether document identity changed during formatting;
- active Pine VM worker count or scheduler queue information already available at those points.

Do not log source content. If a content identity is needed, log a bounded hash and length.

These events distinguish dispatch delay, lock delay, executor starvation, formatter cost, and response follow-up cost without first committing to a scheduling redesign.

## Deterministic automated tests

Use controlled tasks and gates, not stopwatch thresholds.

### Formatting is independent of language-service processing

1. Open and synchronize a document.
2. Configure the next `AddFileAsync` to signal that it started and then block.
3. Submit a `didChange` and wait until `AddFileAsync` is blocked.
4. Invoke formatting without releasing `AddFileAsync`.
5. Assert that the formatter receives the changed text and that formatting returns the expected edits.
6. Release the update during cleanup and assert that normal synchronization eventually completes.

This single test proves the goal without adding priority cancellation or retry behavior.

### Additional focused tests

- An earlier received `didChange` is visible to formatting even while its language-service processing is blocked.
- Formatting an open document does not read stale workspace content.
- Returning edits does not mutate the client-content mirror.
- A client cancellation reaches the formatter.
- A document identity change during formatting is logged and follows the chosen no-edit or cancellation policy without `ContentModified`.
- Blocked formatting diagnostics do not delay the formatting response.
- If a dedicated formatter executor is added, exhausting background execution capacity does not prevent its work item from starting.

## Implementation sequence

1. Add formatting request identities and phase timings around the existing direct path.
2. Add the blocked-`AddFileAsync` test to locate any actual dependency on notification completion or language-service processing.
3. Fix only the boundary demonstrated by that test, such as RPC dispatch or accidental workspace-initialization waiting.
4. Stop writing returned formatting content into the client mirror.
5. Move formatting diagnostics after the response path.
6. Add a bounded independent formatter executor only if measurements or deterministic contention tests show shared execution starvation.
7. Reconsider a general priority coordinator only if later evidence shows that execution isolation is insufficient for multiple interactive language features.

## Acceptance criteria

- Formatting completes while an earlier `AddFileAsync` is deliberately blocked.
- Formatting sees all earlier received document changes.
- Formatting does not wait for language-service state acceptance or workspace initialization for an open document.
- Formatting leaves existing language-service synchronization and supersession semantics unchanged.
- Returning edits does not mutate the client-content mirror.
- Formatting diagnostics and logging I/O do not extend the response-critical path.
- Logs identify the source version/generation and separate dispatch, lock, execution, formatting, and follow-up time.
- Normal `didChange` processing does not produce a `ContentModified` formatting error.

## Implementation log

### Ingress lane yielding and RPC boundary observability

**Status:** Complete

**Summary:**
Preserved ordered, cheap document-mirror ingestion on the StreamJsonRpc ingress lane while ensuring all potentially expensive RPC methods (`textDocument/hover`, `textDocument/completion`, `textDocument/definition`, `textDocument/documentSymbol`, `textDocument/references`, `textDocument/rename`, `textDocument/didClose`, `workspace/didChangeWatchedFiles`, `textDocument/didSave`) yield that ingress lane (`await Task.Yield()`) before executing language-service operations. Added structured logging for method receipt, invocation, first yield, and completion at the RPC boundary.

**Significant discoveries and surprises:**

1. **StreamJsonRpc formatter interface dependencies for parameter deserialization:**
   Wrapping `SystemTextJsonFormatter` only via `IJsonRpcMessageFormatter` caused `HeaderDelimitedMessageHandler` to throw `NotSupportedException` because `HeaderDelimitedMessageHandler.get_Encoding` queries `formatter as IJsonRpcMessageTextFormatter`. Furthermore, StreamJsonRpc's `UseSingleObjectParameterDeserialization = true` requires `IJsonRpcFormatterState` and `IJsonRpcMessageFactory` to deserialize JSON-RPC parameter objects into single parameter structures. Without those interfaces, StreamJsonRpc treated the request as positional and reported method signature mismatches.
2. **Explicit interface implementations on `SystemTextJsonFormatter`:**
   `SystemTextJsonFormatter.Deserialize` is implemented as an explicit interface member rather than a public virtual method. Subclassing could not override it directly; `DelegatingJsonRpcMessageFormatter` was therefore structured to implement and forward all 7 interfaces (`IJsonRpcMessageFormatter`, `IJsonRpcMessageTextFormatter`, `IJsonRpcFormatterState`, `IJsonRpcInstanceContainer`, `IJsonRpcMessageFactory`, `IJsonRpcFormatterTracingCallbacks`, `IDisposable`) to the inner formatter while intercepting `Deserialize` to log incoming JSON-RPC message receipt.
3. **Synchronous session mutation test doubles:**
   In `LanguageServerFormattingTests`, `ControlledLanguageServiceSession` differentiates synchronous mutations (`BlockNextSynchronousMutation` gating `AddFile`) from asynchronous adds (`BlockNextAsynchronousAdd` gating `AddFileAsync`). Switching `ProcessFileChanges` to call `AddFileAsync` bypassed the synchronous gate in `Formatting_is_not_blocked_by_watched_file_language_service_mutation`. Keeping `languageServiceState.AddFile` in `ProcessFileChangesAsync` after `await Task.Yield()` preserved the test contract while ensuring the RPC ingress lane is yielded immediately.

**Backtracking:**

1. **Attempted inheritance from `SystemTextJsonFormatter`:**
   Initially tried deriving `DelegatingJsonRpcMessageFormatter` from `SystemTextJsonFormatter` with method overrides for `Deserialize`. Discovered `Deserialize` is not virtual. Reverted to composition, implementing all interfaces and forwarding them to `inner`.
2. **`languageServiceState.AddFileAsync` in `ProcessFileChangesAsync` and `TextDocument_didCloseAsync`:**
   Initially converted `languageServiceState.AddFile` calls to `await languageServiceState.AddFileAsync(...)`. Reverted to `AddFile` after realizing `await Task.Yield()` on entering `Workspace_didChangeWatchedFilesAsync` and `TextDocument_didCloseAsync` already frees the RPC ingress lane, and `AddFile` maintains compatibility with test doubles expecting synchronous mutations.

