# 2026-08-26 PineVM Cache Configurability

## Purpose

Adapt the intermediate Pine VM so callers can choose how invocation-cache entries
are read, written, and merged. This is a prerequisite for the speculative and
partially concurrent request processing described in
[`guide/language-server-design-and-implementation.md`](../../guide/language-server-design-and-implementation.md).

The design must support at least:

- immediate writes to a shared cache with fine-grained synchronization;
- writes to an evaluation- or worker-owned cache followed by an explicit batch
  merge into a shared cache;
- configurable in-memory and persistent-cache candidate reporting;
- future cache organizations without another change to the VM evaluation loop;
- deterministic tests that do not need expensive expressions merely to cross
  hardcoded cache-admission thresholds.

The plan also covers a reusable scheduler for speculative processing and
serialized state transitions, followed by its integration into the language
server. The scheduling component must not depend on language-server protocols
or types so other stateful applications can adopt it in future projects.

## Resolved Decisions

- `IPineVM` and its `EvaluateExpression` method remain unchanged.
- A `PineVM` instance is single-consumer. Concurrent evaluations use multiple
  warmed VM instances which may share invocation-cache infrastructure.
- Compilation-cache configuration is deferred to future work. This plan must
  not make per-VM compilation state thread-safe.
- Preserve source compatibility for callers currently passing
  `IDictionary<EvalCacheEntryKey, PineValue>`.
- The cache-admission configuration is a numeric form of the current weighted
  formula; an arbitrary caller policy is not required.
- The numeric thresholds decide whether the VM reports a completed application
  to a caller-supplied cache delegate at all. The delegate controls in-memory
  publication and any additional persistent lookup or storage decisions.
- The implementation scope includes the generic scheduler and the complete
  language-server integration: configurable concurrency, speculative first
  passes, serialized replay, stale-result invalidation, and multiple warmed
  workers.
- The language server uses read-through/write-local caching. Each worker reads
  its local buffer before the shared cache, writes new entries only to its local
  buffer, and merges that buffer into the shared cache after finishing each
  event-processing attempt.
- Initial buffer merging is deliberately unconditional and simple. Entries
  accumulated by completed, canceled, failed, or invalidated attempts follow the
  same merge path; branch outcome does not select merge versus discard.
- The first production language-server configuration uses this fixed buffering
  strategy without profiling or runtime selection between synchronization
  variants.
- The language server publishes a response as soon as the scheduler establishes
  that the result has not been invalidated. It never waits for profiling or
  strategy-selection work.
- `LanguageServerOptions.MaxConcurrencyCount` defaults to `4`.
- Web-service adoption and web-service integration tests are deferred to a
  future project. This plan only keeps the generic scheduler free of
  language-server dependencies.
- Lenient evaluation remains outside this plan unless its requirements are
  specified separately.

## Current State

The relevant implementation is concentrated in:

- `implement/Pine.Core/Interpreter/IntermediateVM/PineVM.cs`
- `implement/Pine.Core/Interpreter/IntermediateVM/InvocationCache.cs`
- `implement/Pine.Core/PineVM/IPineVM.cs`
- `implement/Pine.Core/PineVM/LockingPineVM.cs`
- `implement/Pine.Core/IntermediateVM/SetupVM.cs`
- `implement/Pine.Core/Elm/LanguageServer/LanguageServer.cs`
- `implement/Pine.Core/Elm/LanguageServer/LanguageServiceState.cs`
- `implement/Pine.Core/Elm/LanguageServer/LanguageServiceSessionFactory.cs`
- `implement/pine/IntermediateVM/SetupVM.cs`
- `implement/pine/Elm/LanguageServerAdapters/LanguageServerComposition.cs`
- `implement/pine/Pine/PineVM/PineVMResettingCache.cs`

`PineVM.CreateCustom` currently accepts an optional
`IDictionary<EvalCacheEntryKey, PineValue>`. The VM directly calls dictionary
operations and maintains its own expression index by observing the dictionary's
count and enumerating its keys. This couples evaluation to a mutable collection
and assumes that cache reads, writes, count checks, enumeration, and the VM's
side index cannot race.

The cache is consulted for nested function applications. Entries are added when
an eligible frame returns and when a persistent file-cache hit is promoted into
the in-memory cache.

Normal frame-return admission uses two hardcoded weighted-cost checks:

- frame instruction count plus frame stack-frame count multiplied by `100` must
  be greater than `700`;
- instruction count since the last successful insertion plus eval count since
  that insertion multiplied by `100` must be greater than `700`.

The per-evaluation "last insertion" counters advance only after a successful
cache insertion. These details are observable policy and should be preserved by
the default configuration.

`InvocationCache` is a `Dictionary` and is not thread-safe.
`PineVMResettingCache` can clear it after an evaluation and already accepts a
reset-entry threshold, although several compositions hardcode `10_000`.
`LockingPineVM` serializes complete evaluations; it does not provide concurrent
evaluation with fine-grained cache synchronization.

The language service currently combines a mutable state and one VM in
`LanguageServiceState`, protects evaluation with `lock (pineVM)`, and mutates
the state after each request. `LanguageServer` holds one session task; document
notifications call that session serially under document-state locks, and
ordinary requests synchronously use the same session. `LanguageServerOptions`
currently contains only the reported server version.

The cache is not the only mutable state in `PineVM`. Expression compilation,
expression encoding, reduction, input hashing, and related indexes also use
per-VM mutable collections. Therefore, replacing the invocation cache alone
must not imply that one `PineVM` instance is safe for concurrent calls.

## Target Design

### 1. Introduce an invocation-cache access abstraction

Replace direct dictionary use in the evaluation loop with a small interface
whose responsibilities are:

- report whether an expression can have cached inputs, preserving the current
  optimization that avoids constructing and hashing keys unnecessarily;
- look up an expression/input pair;
- atomically attempt to publish an expression/input/result entry;
- define duplicate-key behavior and whether a successful no-op counts as an
  insertion for admission spacing.

The VM must not infer cache state by reading `Count`, enumerate externally
mutable keys, maintain a second unsynchronized index, clear a caller-owned
cache, or know which synchronization strategy an implementation uses.

Keep lifecycle and management operations, such as count, clear, snapshot,
drain, and merge, outside the minimal hot-path interface. Concrete cache
implementations or a separate management interface can expose them to
composition code such as `PineVMResettingCache`.

Retain a dictionary-backed adapter so existing call sites can migrate without
changing cache semantics. Keep the existing `IDictionary` construction
signature source-compatible and route it through the adapter. Add the new
configuration through an overload or options parameter arrangement that does
not make existing positional or named calls ambiguous.

### 2. Support independent read and write destinations

Ensure the abstraction can express more than one backing collection:

- **Immediate shared cache:** lookups and atomic insertions use one synchronized
  shared store. Different VM instances can publish entries as their frames
  complete.
- **Buffered cache:** lookups use the worker-owned entries followed by the
  shared store, while new entries go only to the worker-owned store. The caller
  drains or snapshots the worker-owned entries and merges them into the shared
  store at the event boundary.
- **Custom composition:** callers can add lookup tiers, read-only caches,
  filtered writes, instrumentation, size limits, or remote/persistent adapters
  without modifying `PineVM`.

The VM should perform only lookup and publication. For the language server, the
scheduler runs one common finalization path after every event-processing attempt
and merges all entries from that worker-owned buffer into the shared store.
Canceled, failed, or invalidated attempts are not special cases. Batch merge
uses the shared store's atomic insertion operation so overlapping workers can
merge safely and duplicate keys remain harmless. The initial implementation
uses a straightforward entry-by-entry merge; batching, contention reduction,
and outcome-sensitive retention are future tuning. A speculative pass and a
later serial replay are separate event-processing attempts and each merges its
own buffer after it finishes; repeated same-key/same-value merges are
idempotent.

Specify ownership and immutability requirements for
`EvalCacheEntryKey`, `StackFrameInput`, and `PineValue` when an entry outlives
an evaluation or crosses a thread boundary. A cross-thread cache implementation
must fully materialize the evaluated frame arguments and establish stable
hash/equality state before publishing a key. No lazy `PineValueInProcess` data
or unsynchronized lazy key state may become visible to another VM. Preserve the
legacy dictionary adapter's same-thread behavior where possible, while the new
shared and buffered-merge implementations enforce this publication boundary.

### 3. Add a configurable cache-admission policy

Move frame-return cache admission behind a dedicated VM-level policy rather
than adding unrelated cache settings to the quota-oriented
`EvaluationConfig`.

Restrict the admission calculation to numeric configuration and the counters in
the current formula:

- completed-frame instruction and stack-frame counts;
- instruction and eval counts accumulated since the last successful entry.

The cache delegate may receive the candidate key, result, measured counters, and
source after admission so the application can make richer storage and
persistence decisions. Those details do not participate in the VM's admission
formula.

Provide a default weighted-threshold policy that exactly preserves both current
strict `> 700` comparisons and the `100` multipliers. Make its thresholds and
weights numeric and configurable, with validated nonnegative values and
sufficiently wide integer arithmetic. A zero threshold should let the smallest
nonzero-cost fixtures reach the delegate, while omitting the delegate disables
candidate reporting, without adding an arbitrary policy callback.

Keep the state tracking work since the previous successful insertion local to
one evaluation. Define whether a duplicate publication reported by the cache
updates that state; the default must match today's `TryAdd` behavior.

Treat cache admission and cache retention as separate concerns. Admission
chooses which completed applications become entries. Entry-count reset,
eviction, and memory bounds belong to cache implementations or wrappers.
Preserve the existing `PineVMResettingCache` behavior while adapting it to the
new management surface, and make host-level reset defaults explicit rather than
folding them into the admission policy.

Remove persistent storage decisions from the VM evaluation loop. Once the
numeric threshold is crossed, report the candidate key, result, metrics, and
source to the same caller-supplied cache interface/delegate used for ordinary
invocation-cache publication. That component decides whether to keep the entry
in memory, write it to a persistent store, or both.

Likewise, lookup through the cache abstraction may consult memory and then a
persistent tier. A persistent hit returned to the VM is promoted, rejected, or
republished only through that abstraction; the VM must not write directly to an
`IFileStore`. Preserve existing optimization-parameter naming/key derivation as
needed for source and cache compatibility, but place persistent entry selection
and storage in the caller-owned implementation. Migrate the current
`PersistentCachePredicate` behavior into that caller-owned tier, then remove its
special handling from the VM evaluation loop.

### 4. Wire configuration through VM construction

Add the cache-access and admission-policy configuration to the intermediate
VM's construction API and thread it through both `SetupVM` helpers.
`IPineVM.EvaluateExpression` and `IPineVM` remain unchanged.

Use immutable option objects or focused parameters with documented defaults so
the already large `CreateCustom` signature does not become the long-term public
configuration model. Preserve no-cache behavior when no cache access object is
configured.

Update the VM factories, cache-owning wrappers, and language-server composition
that opt into the new configuration. Verify source compatibility against direct
`CreateCustom`, `SetupVM.Create`, interactive, web-service, compiler, test, and
profiling call sites; web-service composition must continue compiling without
adopting the new scheduler or cache strategy.

### 5. State the concurrency boundary explicitly

Document that a `PineVM` instance does not support concurrent
`EvaluateExpression` calls. Each scheduler worker owns one VM and evaluates at
most one operation at a time. A pool reuses workers so compilation, reduction,
encoding, parsing, hashing, and other per-instance caches remain warmed.

Invocation-cache access may be shared safely between VM instances, or each
worker may buffer new entries before merging them into a shared store.
Compilation-cache sharing and configuration are explicitly deferred. Remove
unnecessary full-evaluation locking only where exclusive worker ownership makes
it redundant; keep `LockingPineVM` available for unrelated compatibility use.

### 6. Add a reusable speculative state-machine scheduler

Place the scheduling and serialization implementation in a common namespace
such as `Pine.Core.Concurrent`, independent of Elm, LSP, HTTP, and
language-server types. Give it generic request, state, result, and
worker/resource types so the language server can use it now and other stateful
applications can supply adapters in future projects.

The scheduler owns:

- a canonical immutable state and monotonically increasing revision;
- a FIFO serial lane which is the only component allowed to commit a new state;
- a bounded pool of exclusive, reusable, warmed workers;
- a configurable maximum concurrency count and cancellation/lifetime handling;
- bookkeeping for operation sequence, source revision, attempts, and completion;
- invalidation and replay when speculative work used an obsolete state.

For each submitted operation:

1. Capture the canonical state and revision.
2. Run a speculative first pass on an available worker without holding the
   serial commit lane.
3. After the event-processing attempt finishes, invoke one finalization path
   which merges every worker-local cache entry into the shared cache, regardless
   of success, cancellation, failure, or later invalidation.
4. If the result leaves state unchanged, its base revision is still current, and
   the application adapter classifies its effects as safe for speculative
   completion, complete it as a read-only operation.
5. If the result mutates state, or the base revision became stale, enqueue it in
   submission order on the serial lane.
6. On the serial lane, replay the operation against the latest canonical state.
   Commit the returned state, increment the revision only for an actual state
   change, and complete the caller with the serial result.
7. Mark speculative results based on older revisions stale. Read-only operations
   that have not completed are retried against the new state; queued mutating
   operations retain their original ordering and are replayed when they reach
   the serial lane.

Define cancellation separately from invalidation. Cancellation stops work whose
caller no longer needs a result when safe, while invalidation causes replay to
preserve serial semantics. Ensure worker leases return to the pool on success,
error, cancellation, and exceptions, after the common buffer-merge finalizer has
run. Avoid waiting synchronously while holding state or scheduler locks, and
expose structured lifecycle/reporting events so tests and hosts can observe
speculation, invalidation, replay, commit, and cache merge decisions.

The generic scheduler should not decide whether two application states are
equal, whether a state-equal result is safe to complete out of order, how to
execute an operation, or how to construct a worker. Supply these as application
adapters. Keep operation results separate from committed side effects so
commands, persistence, HTTP responses, diagnostics, and other effects are
emitted only from an adapter-approved read-only result or serialized replay,
never from a discarded speculative pass.

### 7. Integrate the scheduler into the language server

Refactor the language-service execution adapter so the compiled interface and
immutable Pine state can be used with multiple session workers. Each worker
owns a warmed `PineVM`; the scheduler supplies the current state explicitly for
an operation and receives both the response and resulting state. Do not share a
mutable `LanguageServiceState` instance across workers.

Extend `LanguageServerOptions` with scheduler configuration, including at least
`MaxConcurrencyCount`, validated as positive and defaulting to `4`.
Thread the options through `LanguageServerComposition`,
`LanguageServiceSessionFactory`, and test doubles. The factory should build the
worker pool around multiple warmed VM instances while reusing compiled language
service artifacts. Configure every production worker with a read-through,
write-local invocation cache backed by the same shared cache; do not add a
production switch for immediate synchronized writes.

Submit all stateful language-service operations through the scheduler:

- document open, change, close, save, and watched-file notifications;
- workspace-folder and package/workspace initialization changes;
- hover, completion, definition, references, rename, and symbol queries.

Preserve protocol ordering for mutations. Capture document versions, workspace
generation, and scheduler state revision on submission. Superseding document
versions or workspace reinitialization invalidate obsolete speculative work.
Return a query result only when it was computed from the still-current canonical
workspace state; otherwise replay it. Publish the response immediately after the
scheduler atomically verifies that its base revision is still current and marks
the result accepted, so a state commit cannot invalidate it between validation
and publication. Keep the existing diagnostics generation checks, but coordinate
them with scheduler revisions so diagnostics and other externally visible
effects from discarded passes are not published.

Initialization must create and warm the configured workers without compiling
the language service independently for each one. Workspace loading should use
the same scheduler semantics as later mutations, while allowing expensive file
processing to populate shared cache entries concurrently before serial replay.
Reuse each worker for later requests so its VM-local compilation and reduction
caches become progressively warmer. An optional startup warm-up may exercise
representative language-service functions on every worker, but sharing or
configuring those VM-local compilation caches remains future work.

### 8. Preserve reuse without adopting the scheduler elsewhere

Keep the scheduler API and namespace independent of Elm and language-server
types, and cover that independence with generic scheduler unit tests. Do not
change `MutatingWebServiceApp`, web-service hosts, HTTP request processing, or
their tests in this project. Applying the scheduler to occasional expensive web
requests, such as parsing an uploaded PDF, belongs to a future project.

## Implementation Sequence

1. Characterize current cache behavior with focused tests, including the two
   admission boundaries, insertion-spacing updates, duplicate insertions,
   persistent-cache promotion, and no-cache operation.
2. Define the minimal cache-access contract, duplicate semantics, ownership
   rules, persistent-tier delegation, and optional management contract.
3. Implement the legacy dictionary adapter and migrate `PineVM` lookups and all
   publication sites to the contract without changing default behavior or
   breaking existing `IDictionary` call sites.
4. Extract the numeric weighted admission configuration, preserve current
   defaults, and apply it before invoking the caller-supplied cache delegate.
5. Implement the buffered read-through/write-local cache, its straightforward
   event-boundary merge into a synchronized shared cache, and a caller-owned
   persistent tier matching the current file-cache behavior. Keep alternative
   publication and merge implementations available to other callers, but do not
   add language-server strategy selection.
6. Adapt both `SetupVM` layers, `PineVMResettingCache`, and all direct cache
   owners and constructors.
7. Implement and test the generic revisioned state-machine scheduler, serial
   commit lane, worker pool, invalidation, replay, cancellation, and deferred
   effect publication in the language-neutral `Pine.Core.Concurrent` namespace.
8. Refactor language-service evaluation into immutable state transitions on
   exclusive warmed workers, then adapt the session factory to construct the
   worker pool without repeated compilation.
9. Extend `LanguageServerOptions` and composition APIs with
   `MaxConcurrencyCount`; route initialization, notifications, requests, and
   diagnostics through the scheduler.
10. Document the supported concurrency boundary and update the language-server
    design guide with the implemented scheduling, invalidation, and cache merge
    semantics.
11. Record deferred tuning opportunities, such as merge batching, alternative
    synchronization, outcome-sensitive retention, and profiling. None gates the
    first production language-server configuration.

## Test Plan

Add focused tests under
`implement/Pine.Core.Tests/Interpreter/IntermediateVM`, using injected policies
and instrumented fake caches rather than relying on costly expressions or
timing.

### Contract and compatibility

- The default policy admits and spaces entries at the same strict boundaries as
  the current formulas, including exact-threshold non-admission.
- Configured thresholds and weights independently alter both checks.
- A zero threshold lets small nonzero-cost fixtures reach the delegate, while
  an absent delegate provides deterministic never-report behavior.
- A disabled cache performs no cache lookups, key hashing, or writes.
- A dictionary-backed cache produces the same result and reuse behavior as the
  current `InvocationCache`.
- Existing source code passing an `IDictionary` to `CreateCustom` or
  `SetupVM.Create` still compiles and receives the legacy behavior.
- Duplicate keys do not overwrite an existing value, and only the documented
  successful-insertion result advances spacing state.
- The VM does not call the candidate delegate below the configured threshold.
- Above the threshold, the delegate receives enough key, result, metric, and
  source data to choose memory-only, persistent-only, both, or neither.
- Persistent hits, promotion into memory, and additional persistent writes
  occur through the same abstraction and preserve result/key compatibility.
- A cache delegate can reject all persistence without changing evaluation.
- Invalid policy values are rejected at construction.

### Cache implementations and merging

- An entry published by one VM through the synchronized shared cache is visible
  to a second VM and avoids recomputation.
- Concurrent publication of distinct keys through multiple VM instances loses
  no entries and corrupts no index.
- Concurrent publication of the same key has deterministic first-entry and
  result-consistency behavior when values agree.
- A key materialized for cross-thread publication has stable hash and equality
  behavior under concurrent lookup, without evaluating lazy frame data.
- A buffered cache reads existing shared entries but keeps new entries private
  before merge.
- Before event processing finishes, another VM cannot observe the worker-local
  entries.
- After event-boundary merge, another VM can reuse the worker-local entries.
- Successful, canceled, failed, and invalidated attempts all invoke the same
  merge finalizer and retain every entry accumulated before the attempt stopped.
- Concurrent batch merges with overlapping keys are safe and idempotent.
- A same-key/different-value fixture verifies the collision behavior selected
  when resolving the corresponding open question.
- Read-only, write-filtered, and instrumented fake implementations demonstrate
  that future policies do not require changes to the VM.
- Every worker is leased exclusively and the same warmed VM is reused across
  sequential leases; no test invokes one VM concurrently.

Use barriers and explicit observations instead of sleeps or throughput
assertions so concurrency tests remain deterministic.

### Evaluation outcomes and lifecycle

- Successful results are identical with caching disabled, immediate shared
  caching, and buffered caching.
- Cancellation, quota exhaustion, evaluation errors, and exceptions have the
  documented effect on entries already buffered or published.
- Per-evaluation spacing state does not leak between sequential evaluations on
  one VM or concurrent evaluations on distinct VMs.
- Reset/clear behavior does not leave a stale expression-presence index.
- Cache reset at, below, and above the configured entry threshold preserves the
  existing wrapper contract.

### Generic scheduler

- Up to `MaxConcurrencyCount` speculative operations run at once, while one
  worker never handles two operations concurrently.
- Read-only operations computed from the current revision complete without
  entering the serial replay lane.
- A speculative state mutation is replayed against the latest state and only
  the replay result is committed.
- Concurrent mutations commit in defined submission order even when their
  speculative passes finish out of order.
- A state commit invalidates unfinished work from older revisions; stale
  read-only results are never returned and are retried on the latest state.
- Multiple successive invalidations cannot lose, duplicate, or reorder an
  operation.
- State-equal replay does not increment the canonical state revision.
- Cancellation before start, during speculation, while queued for replay, and
  during disposal has defined outcomes, merges entries accumulated by each
  completed attempt, and always returns worker leases.
- Exceptions and operation errors do not corrupt canonical state or stop the
  serial lane.
- Effects from discarded speculative passes are never emitted; accepted query
  effects and serialized replay effects are emitted exactly once.
- The same scheduler tests run with a fake state machine that has no Pine, Elm,
  LSP, or web-service dependency.

### Language-server integration

- `MaxConcurrencyCount` is validated and the configured number of workers is
  created, warmed, bounded, and disposed correctly; omitting it creates four
  workers.
- Workspace initialization compiles/loads the language-service program once and
  prepares equivalent worker execution contexts.
- An expensive document-content update performs a speculative first pass,
  merges all cache entries when that attempt finishes, and then replays serially
  against the latest workspace state with observable cache reuse.
- Concurrent read-only hover/completion/definition requests execute in parallel
  when based on the same current revision.
- A document change arriving during a query invalidates its stale response and
  causes replay against the new document version.
- A response is published immediately after atomic acceptance against the
  current revision, and a result invalidated before acceptance is never
  published.
- Rapid changes to one document, changes to different documents, watched-file
  events, and workspace-folder reinitialization preserve notification order and
  final state.
- A speculative operation that unexpectedly mutates state is automatically
  routed through serial replay rather than being returned as a query.
- Superseded speculative diagnostics, commands, logs intended as effects, and
  protocol responses are not published.
- Existing synchronous public entry points either await the scheduler safely or
  gain asynchronous counterparts without blocking while holding document-state
  locks.
- `MaxConcurrencyCount = 1` preserves serial behavior and provides a
  compatibility/debugging configuration.
- The production composition always uses read-through/write-local caches and
  event-boundary merging; no profile or runtime condition selects a different
  synchronization strategy.
- Generic scheduler tests compile and run without references to Elm,
  language-server, HTTP, or web-service types.
- Existing web-service production code and tests remain unchanged.

Run changed C# tests from the `Pine.Core.Tests` project with `dotnet run`, using
method filters for focused iterations. Format changed C# with `dotnet format`,
then run the broader affected test project before submission.

## Questions to Resolve Before Implementation

1. Same-key/same-value merges are first-wins and idempotent. If two workers
   produce different values for the same key, should the cache keep the first
   value, assert value equality, report a diagnostic, or fail the merge? A
   mismatch would indicate a serious determinism or keying defect.
2. Which cache retention policies are required initially: the existing
   entry-count reset, bounded eviction, generation-based replacement, or only
   caller-managed clearing?
3. What queue bound and overload behavior should the language-server API use?
4. Should externally requested cancellation remove an operation entirely, or
   must already accepted mutating notifications still reach the serial lane to
   preserve protocol ordering?
5. How should warmed workers receive a new canonical state: pass immutable state
   into each operation, reset a worker-local session wrapper, or introduce a
   stateless language-service program adapter?
6. Which language-server side effects besides responses and diagnostics must be
   represented explicitly so speculative passes cannot publish them?
