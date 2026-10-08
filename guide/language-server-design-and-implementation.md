# Language Server Design and Implementation

## Compiler-Backed Analysis and Queries

The [2026-10-07 frontend design contract](../explore/internal-analysis/2026-10-07-frontend-compiler-design.md#agreed-implementation-contract) defines compiler integration. The default host now uses `ElmCompilerDiagnosticsProvider` rather than invoking `elm make` for source diagnostics; the external adapter remains available to callers that explicitly select it.

`IApplicationDiagnosticsProvider` supplies a project URI to the host scheduler. This is needed to replace one application's previous diagnostics regardless of which document triggered analysis, preventing stale contributions from other documents. It is not an entry point, compilation unit, or compiler state identity.

Application analysis selects no entry points and emits or executes no code. It returns source-located diagnostics across application declarations, including unused declarations, unused missing imports, and disconnected source modules. Independent declarations must still be analyzed after errors. Unrelated package declarations need not be diagnosed, but unavailable dependencies required by application code must be reported.

This change retains today's type-checking coverage and regression behavior. Complete Elm-compatible type checking is a future project; an empty diagnostic list must not be advertised as proof that all types were checked.

Definitions, inferred types, and other requested source information come from source queries rather than an application-analysis payload containing all semantic facts. The compiler's `QueryDeclaration` demands the information needed to answer a declaration query and returns reliable definition information despite analysis errors, with relevant diagnostics. Existing language-service query implementations remain available; this change does not replace all of them. Explain when an answer cannot be established rather than presenting an unavailable answer as "not found".

Modules are source-level resolution contexts, not compilation units. Explorer markings aggregate source diagnostics; they do not require module compilation. The compiler processes complete demanded top-level declaration bodies, including unused local bindings.

The language server supplies current source contents, including unsaved editor text, and handles document version tracking, supersession, dependent-file updates, and publication. When diagnostics disappear, it clears prior markings; incomplete or failed analysis must not be presented as a clean result. None of this introduces a "frontend snapshot" or "program revision" abstraction, API field, or result identity into the compiler.

Project diagnostics run when a workspace root contains `elm.json`, after accepted document updates, when a document closes, and after backing-file changes. Each successful result covers selected project files, including empty diagnostic lists for files whose errors disappeared. Package preparation supplies original import locations and rewritten syntax retaining source ranges. Document formatting retains the independent syntax diagnostics provider.

Compiler diagnostics name the failing declaration and explain the resolution failure. Related information links source declarations, reference sites, relevant imports, and private-member definitions when the workspace provides real document URIs. Logical package-source paths remain visible in the diagnostic text; the server does not fabricate local file links for bundled or virtual package sources. Errors that prevent checking a dependent application declaration identify the failing dependency and its location. Application-wide diagnostics do not display a compilation-root chain, because no entry points were selected.

Compilation is a separate operation with explicit declaration roots and only those roots exported. Callers evaluate compiled code separately when needed. Compilation, application analysis, and queries share compiler logic and can reuse unchanged work, but application-wide diagnostics must not block compilation of unrelated roots.

## Optimizations for Response Times

The Pine language server implements various optimizations for response times. For one, it benefits from the general memoization infrastructure available in Pine, which helps both efficiency and response times. Beyond that, the language server optimizes response times specifically by distributing work across multiple threads.

The ways we employ concurrent processing of the requests from the language clients are broadly grouped into three categories:

+ Parallel computation for read-only requests ('queries')
+ Partial concurrent execution of mutating requests ('transactions')
+ Lenient evaluation

> Note: Another approach to optimizing response times is the parallel execution of work inside of a single request (think `List.map`), but we don't cover that here.

### Maintaining Simplicity for Application Programmers

Crucially, none of these optimizations depend on the program code that implements the language services. It's still plain Elm code without any notion of concurrency.

Identifying parallelizable work, enabling concurrency, and choosing synchronization strategies, etc., are automated by the compiler and virtual machine.

This is also important because the same approach to improving response times through concurrency can be applied to other Elm applications where optimizing for faster responses or higher throughput is desirable.

### Parallel Processing Read-Only Requests

Some requests from language clients will not change the application state/database.

One way to prove this is to have the Elm app process the request, then check whether the returned state is the same as the previous state.

How can we use this knowledge to improve response times? One way to do that is through speculative execution: instead of waiting for each request to finish processing, we could start processing it as soon as it arrives. As long as none of these requests executed concurrently produces a new state, the results are the same as with a serial execution. When one request's processing produces a new state, this can invalidate the results from processes started for a previous state.

These invalidated results are then discarded, and processing of the corresponding requests is restarted from the latest state.

### Partial Concurrent Execution of Mutating Requests

For some combinations of requests, we need to go a bit further to capitalize on concurrent execution. 

For example, a client might change the contents of multiple documents, sending a [`DidChangeTextDocument` Notification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_didChange) for each changed document.

When opening a workspace, we are in a similar situation: the new document content does not arrive via a `textDocument_didChange` request, but we need to process many new document contents to support queries like `Go to Definition` or `Hover`.

The processing of new document content also entails parsing from a string into a syntax tree. The language service then produces a new state to make a parsed representation of the document available in a dictionary for future queries.

Since the overall handling of new document content updates the Elm application state, we must eventually run all of these requests in sequence.

Meanwhile, the part parsing the syntax tree is a great candidate for concurrent processing:

+ It does not depend on the previous state, only on the new document content.
+ It typically makes up more than 90 percent of the total cost of processing the request.

Can we offload the parsing to a separate thread, even though the overall request requires serial processing?

It turns out, we can, and it's not even complicated. Since the caching functionality enables reuse of results across threads, we implement this concurrent execution in two stages as follows:

+ First, we process the new document content event on a separate thread. While processing the event, the interpreter creates cache entries for the computationally expensive parts, as usual. These cache entries are merged into a shared dictionary, where they will be available for future event processing.
+ Second, we process the new document content event again, based on the Elm app's latest state. In this second pass, the interpreter will encounter the same function application again and pick up the previously computed result from the cache.

There are multiple ways to implement the merging of cache entries:

+ One is to use fine-grained synchronization on every cache write.
+ Another approach is to direct all new cache entries to a thread-owned collection, then merge them once processing of the entire event is complete.

In any case, this approach means that some work, such as building a new dictionary in the overall app state, is done multiple times, causing overhead in CPU cycles.

#### Superseding Obsolete Document Updates

[`textDocument/didChange`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange) is an LSP notification, not a request. A [notification message](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage) has no request ID, so the client cannot target a document update with [`$/cancelRequest`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#cancelRequest).

The server therefore performs version-based supersession itself:

+ It records the newest content and version before starting expensive language-service work.
+ A newer version cancels pending or in-flight processing of the older version.
+ Pine VM evaluation observes that cancellation cooperatively.
+ Only the update that still matches the newest client version is accepted.

This keeps intermediate versions produced during rapid typing from accumulating in the scheduler while preserving the LSP ordering requirement for document synchronization notifications.

> Note: This approach to internal cancellation might become obsolete with the introduction of lenient evaluation, in which the expensive parts are not evaluated immediately, and their thunks can be discarded before evaluation.

#### Request Cancellation

For LSP requests such as hover, completion, definition, references, rename, document symbols, formatting, and CodeLens (including resolve), the RPC boundary accepts a cancellation token. StreamJsonRpc connects that token to the base protocol's [`$/cancelRequest`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#cancelRequest) notification, and cancellation flows through the language-service scheduler into Pine VM evaluation.

The LSP base protocol defines no general server capability flag for request cancellation. It is therefore not added to `ServerCapabilities`; the server continues to announce each implemented request provider and its [`TextDocumentSyncOptions`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncOptions) during initialization. The similarly named `serverCancelSupport` field is specific to [semantic tokens](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens) and does not apply to document synchronization or general requests.

The server logs document URI, version, internal update sequence, pending count, supersession, scheduler cancellation, accepted or discarded completion, elapsed time, and observed client request cancellation. These events distinguish client-issued `$/cancelRequest` from the server's own cancellation of obsolete document versions.

### Lenient Evaluation

The tool of lenient evaluation can help improve both response times and efficiency.

TODO
