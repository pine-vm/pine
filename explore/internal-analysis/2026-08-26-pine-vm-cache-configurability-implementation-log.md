# 2026-08-26 PineVM Cache Configurability Implementation Log

This log tracks implementation of
[`2026-08-26-pine-vm-cache-configurability.md`](./2026-08-26-pine-vm-cache-configurability.md).

## Current Status

- **Phase:** Validation
- **Last completed increment:** Concurrent workspace initialization submission
- **In progress:** Final security and repository checks
- **Next:** None
- **Validation:** Cache, PineVM, scheduler, language-service, and language-server tests pass

## Progress

- [x] Source-compatible PineVM cache-access abstraction
- [x] Configurable numeric cache-admission thresholds
- [x] Stable cross-thread cache keys
- [x] Read-through/write-local worker cache and merge
- [x] Caller-owned persistent invocation-cache behavior
- [x] Generic revisioned speculative scheduler
- [x] Stateless language-service transition adapter
- [x] Warmed language-service worker pool
- [x] Language-server scheduling and invalidation
- [x] Concurrent workspace initialization submission
- [x] Documentation and full validation

## Implementation Increments

### Increment 1 — Cache access and admission configuration

**Status:** Complete

Planned result:

- Preserve existing `IDictionary<EvalCacheEntryKey, PineValue>` call sites.
- Route VM cache operations through a focused access interface.
- Replace hardcoded `700` and `100` values with validated numeric configuration
  whose defaults preserve current behavior.
- Add deterministic threshold and compatibility tests.

Implemented:

- Added `IInvocationCacheAccess` and the source-compatible dictionary adapter.
- Added validated numeric weights and thresholds preserving strict historical
  comparisons.
- Routed lookup, persistent promotion, and frame-return publication through the
  access interface.
- Threaded optional access/configuration through the platform `SetupVM`.
- Added seven focused contract and threshold tests.

Validation:

- `Pine.Core` builds.
- `pine` builds.
- All seven `InvocationCacheTests` pass.

### Increment 2 — Stable shared keys and buffered cache

**Status:** Complete

Planned result:

- Materialize cache keys before cross-thread publication.
- Add a synchronized shared cache.
- Add worker-local read-through/write-local buffering with unconditional drain
  and merge.
- Verify visibility boundaries, duplicate behavior, and concurrent merging.

Implemented:

- Added an explicit `StackFrameInput.Materialize` publication boundary that
  eagerly evaluates arguments and establishes hash state.
- Added a straightforward lock-protected shared invocation cache.
- Added a worker-owned buffer that reads local then shared, writes local only,
  and merges every buffered entry into shared storage before clearing.
- Kept merge behavior first-writer-wins for same-key conflicts.

Validation:

- All eleven `InvocationCacheTests` pass, including parallel publication,
  pre/post-merge visibility, read-through, and overlapping merge coverage.

### Increment 3 — Caller-owned persistent cache tier

**Status:** Complete

Implemented:

- Moved persistent-cache predicate evaluation, stable file-key derivation,
  encoding, storage, decoding, and memory promotion into
  `PersistentInvocationCacheAccess`.
- Removed persistent file reads and writes from the VM evaluation loop and
  removed the now-unused per-frame file-name state.
- Preserved the existing `cacheFileStore` construction parameter by composing
  the persistent tier at VM construction.
- Persistent writes now follow the same numeric admission path as memory
  publication, as specified by the plan.
- Replaced checked `long` admission arithmetic with `Int128` arithmetic so large
  counters cannot overflow.

Validation:

- Fourteen `InvocationCacheTests` pass, including persistent round-trip,
  promotion, configuration filtering, and large-counter coverage.
- Sixteen combined `InvocationCacheTests` and `PineVMTests` pass.
- `pine` builds with pre-existing warnings and no errors.

### Increment 4 — Generic revisioned speculative scheduler

**Status:** Complete

Implemented:

- Added a language-neutral generic scheduler in `Pine.Core.Concurrent`.
- Added eagerly constructed, bounded, exclusively leased reusable workers.
- Added speculative execution, FIFO acceptance, stale-result replay, canonical
  immutable state, and monotonically increasing revisions.
- Added common per-attempt finalization before worker return for success,
  cancellation, and exceptions.
- Added structured lifecycle events and asynchronous lifetime disposal.

Validation:

- Six focused scheduler tests pass for ordered mutation replay, direct read-only
  acceptance, stale read replay, concurrency bounds, finalization, failures, and
  preservation of the FIFO chain after a failure.

### Increment 5 — Stateless language-service transition adapter

**Status:** Complete

Implemented:

- Separated one-time language-service compilation/initialization into an
  immutable program value containing the parsed interface and initial state.
- Extracted request evaluation into a transition function that accepts an
  explicit Pine state and VM and returns the response plus next state.
- Retained the existing mutable `LanguageServiceState` API as a compatibility
  wrapper over the transition function.

Validation:

- All ten `LanguageServiceTests` pass.

### Increment 6 — Language-server worker-pool integration

**Status:** Complete

Implemented:

- Added an eagerly constructed language-service worker pool reusing the compiled
  program and immutable initial state across distinct VM instances.
- Configured every production worker with read-through/write-local invocation
  caching over one synchronized shared cache.
- Routed every `ILanguageServiceSession` operation through the revisioned
  scheduler, with state-equal query responses accepted only at the current
  revision and stale work replayed.
- Merged each worker buffer in the scheduler's common attempt finalizer,
  including failed, canceled, and invalidated attempts.
- Added validated `LanguageServerOptions.MaxConcurrencyCount`, defaulting to
  four, and threaded the options through session creation.
- Kept web-service composition unchanged.

Validation:

- The `pine` host builds with pre-existing warnings and no errors.
- Thirty-seven language-server and language-service tests pass.

### Increment 7 — Concurrent workspace initialization submission

**Status:** Complete

Implemented:

- Added non-blocking language-service request and file-submission methods while
  preserving the synchronous session API for existing callers.
- Changed workspace initialization to submit all discovered files to the
  scheduler before awaiting completion, allowing the configured worker pool to
  run speculative file-processing attempts concurrently.
- Preserved deterministic submission and commit order through the scheduler's
  existing FIFO replay lane.
- Kept per-file latency reporting by recording elapsed time when each
  asynchronous submission completes.
- Added a regression test that blocks two file submissions and verifies both
  are active concurrently during initialization.

Validation:

- The focused concurrent-initialization regression test passes.
- `Pine.Core` builds, and all fifteen combined scheduler and language-server
  document synchronization tests pass.

## Surprises and Plan Changes

- The first build command was run from the repository root with a project path
  relative to `implement`. It failed before compilation because the project was
  not found. Subsequent .NET commands use `implement` as their working
  directory or an absolute project path.
- The first filtered test command also assumed the shell working directory
  could be set implicitly. It failed before running tests. Test commands now use
  `dotnet run --project <absolute-csproj> -- ...`.

## Backtracking

- Corrected two command invocations that assumed an `implement` working
  directory. Neither failure compiled or executed repository code.
- The first cache-test compilation exposed that this project does not enable
  implicit `System` imports for the new test file. Added the explicit import and
  reran the same filtered tests.
- The plan referred to "both `SetupVM` layers", but the current
  `Pine.Core.IntermediateVM.SetupVM` only supplies precompiled leaves and does
  not construct VMs. Only `implement/pine/IntermediateVM/SetupVM.cs` needs the
  new construction parameters.
- The first increment-2 build used `System.Threading.Lock`, but `Pine.Core`
  targets `net8.0` as well as newer frameworks where that type is unavailable.
  Replaced it with an ordinary private lock object before any tests could run.
  This was a compatibility correction, not a design change.
- While removing persistent I/O from `PineVM`, found that its readonly
  `_cacheFileStore` field was never assigned by the constructor. The old
  persistent lookup/write branches were therefore dormant. The new tier both
  restores the intended behavior and makes it directly testable.
- Independent scheduler review found that an operation failing during its
  speculative pass could signal its ordering completion before its predecessor
  had completed. That would let the next operation bypass FIFO ordering and
  could make disposal race active workers. Changed all failure/cancellation
  paths to await the predecessor before advancing the chain and added a
  regression test.
- Initial language-service integration used the host namespace without a
  `global::` qualifier, which resolved against `Pine.Pine`; corrected the
  qualification after the first host build.
- Final integration review found that a synchronous VM delegate could begin
  executing inline while `SubmitAsync` still held the scheduler state lock,
  serializing submissions. Scheduler processing now starts asynchronously
  after linking the FIFO chain.
- The worker pool was present, but workspace initialization called the
  synchronous `AddFile` API and waited for each file before submitting the next.
  Therefore only one scheduler operation existed at a time and no speculative
  work could overlap. The fix adds an asynchronous submission path rather than
  changing worker ownership or cache merging.

## Validation History

- `dotnet build implement/Pine.Core/Pine.Core.csproj --no-restore` — passed
  with pre-existing warnings after the cache-access refactor.
- First `InvocationCacheTests` run — build failed because the new test file
  omitted `using System`; fixed immediately.
- `InvocationCacheTests` — 7 passed after adding the explicit import.
- Repeated `InvocationCacheTests` after formatting — 7 passed.
- `dotnet build implement/pine/pine.csproj --no-restore` — passed with 43
  pre-existing warnings and no errors.
- `InvocationCacheTests` after the increment-2 compatibility correction — 11
  passed.
- First increment-3 build failed after removing the `CommonEncodings` import
  because unrelated expression/string encoding helpers later in `PineVM` still
  need it. Restored that import; no implementation was reverted.
- `InvocationCacheTests` after persistent-tier implementation — 14 passed.
- Combined `InvocationCacheTests` and `PineVMTests` — 16 passed.
- `dotnet build implement/pine/pine.csproj --no-restore` after persistent-tier
  composition — passed with 43 pre-existing warnings and no errors.
- First scheduler-test compilation used xUnit's `Assert`, which is not available
  in this test project's compatibility surface. Replaced it with the existing
  AwesomeAssertions style.
- `RevisionedOperationSchedulerTests` after initial implementation — 5 passed.
- `RevisionedOperationSchedulerTests` after the ordering-chain correction — 6
  passed.
- First stateless-adapter build exposed a C# pattern-variable name collision
  after removing the former enclosing lock scope. Renamed the request error
  binding and reran the same tests.
- `LanguageServiceTests` after extracting the transition adapter — 10 passed.
- First worker-pool build omitted the explicit `System.Threading.Tasks` import;
  added it before the host integration build.
- First host build then exposed the namespace qualification issue described
  above; the repeated host build passed.
- Combined language-server and language-service filter — 37 passed.
- Running `dotnet format` over the complete test project removed test-framework
  imports from unrelated files and made the project uncompilable. Reverted all
  formatter-only test changes, retained only the targeted test file, restored
  its imports, and reran the focused test successfully.
