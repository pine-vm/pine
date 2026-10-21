# 2026-10-21 Invocation Policy for the Elm Language Service

+ PineVM implements various strategies to evaluate `Eval` expressions.
+ All of these strategies result in the same return value.
+ Different strategies can have vastly different runtime costs.
+ Some strategies (e.g., cache lookup, remote execution) can fail (e.g., no cache entry, degraded network connection), requiring the selection of a backup strategy.

Using an invocation policy, we configure how a PineVM:

+ Selects a strategy to implement an invocation.
+ Continues optimization related to this invocation, beyond the minimum necessary to continue the main processing thread.
  + If the `Eval` was initially only stored as a deferred thunk, do we immediately start evaluation on a separate thread?
  + When the result value is available, which caches to publish it to?


Note:

+ For tail-call optimization, we haven't seen trade-offs so far, so we haven't planned a toggle for it in the policy.
+ The transformation of recursive tail calls to loops looks more like it will be modeled as part of a compilation policy, rather than an invocation policy.

## Situation Today

Reference: <https://github.com/pine-vm/pine/blob/ad917b6166ec3b18ef1e9259a388f49a7f0d29d7/implement/Pine.Core/Interpreter/IntermediateVM/PineVM.cs>

See the parameter `optimizationParametersSerial` for an example of a part of the invocation policy already implemented. The `ExpressionConfig` in there already contains predicate models, but so far these offer almost no customization.

Policy for admission to caches (`invocationCache.TryAdd`) has so far been based only on work counters (`_invocationCacheConfiguration.ShouldOfferEntry`) and does not even support per-expression configuration.

## Ranking Model

### Predicate Model

So far, the ranking model was limited to simple predicates for individual strategies. The order of strategies was not configurable.

So far, predicate applications used the same data model at runtime as the configuration API. When compiling expressions to lower-level representations, we might bake part of the invocation policy into the compilation results to improve data locality and reduce page misses.

## Policy Learning and Layering

In the early days of computing, humans manually wrote policies to optimize runtime performance. A human would manually set up a profiling session, then use these observations to form a theory about which changes to program code are promising to improve efficiency.
Unfortunately, many projects did not clearly separate these specializations for runtime performance. Application code often mixed business logic with implementation details only meant to improve runtime efficiency. This made the resulting artifacts hard to read and understand.

Pine enables offloading more of that implementation work to the machine by contributing:

+ A model to clearly separate performance optimizations from business logic.
+ Automation of profiling and learning of optimization policies.

The API supports easy composition of a policy from different learning runs.

+ Some of the learning happens in a build pipeline, before release of the software, and resulting policy fragments can be bundled with a binary distribution.
+ Some of the learning happened on the same machine, but in an earlier process, days ago, and is loaded from a persisted cache.
+ Some of the learning happened earlier in the same process.

The API must support composing the outputs coming from these different learning runs, to be sure we can approximate the performance and efficiency we get from classic human-authored optimizations.

## Motivation Today

Today we are looking to improve runtime efficiency in the Elm language service for `textDocument/didChange` requests.

In this workload, the current generic policy for cache admission clearly adds significant runtime overhead.
See the implementation parsing Elm module text into syntax trees: <https://github.com/pine-vm/pine/blob/ad917b6166ec3b18ef1e9259a388f49a7f0d29d7/implement/Pine.Core/Elm/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Concrete/Parser/FromString.elm>

Remember that our current caching implementation uses the identity of the complete input to `Eval` as the cache key.

> We could expand the implementation to develop proofs that certain parts of the environment are not observable for a given expression, and therefore can be omitted when deriving the cache key. (A convenience feature similar to having a compiler rewrite functions to unlock tail call optimizations) We currently have not planned such an extension.

The Elm module parser is currently structured so that each expression parser receives the complete source text.

In parsing of nested expressions inside a declaration, the current policy results in many cache admissions that are both costly and useless:

+ A cache hit for the result of parsing a whole declaration means we don't utilize cache entries made for the contained expressions.
+ While it's conceivable that a use case will call for parsing individual declarations, we are unlikely ever to reach a point where we parse just an expression without the declaration.
+ A cache admission not only increases retained memory use, but also causes extra work by forcing materialization in inputs and return value.

By introducing a more selective cache-admission policy, we hope to improve runtime efficiency for this workload significantly.

### Learning Strategy for the Elm Language Service

We want to try changing the Elm Language Service to start with a zero-cache admission policy, and only enable admission for specific predicates after demonstrating that they actually lead to reuse.

For today, we keep caching in-process only; persistent caches will follow later. We start with a predicate that's only based on the invoked expression, since that might be specific enough already.

Profiling and learning will happen on a separate thread that reuses some subset of the requests processed by the server handling the requests from the language client. Tuning the training-data selection is not in scope for today.
The profiling process uses a different PineVM configuration to gather the information needed to estimate cost and benefit.

Does this in-process learning mean we have to live with sluggish startup in every new session? Not quite. There are multiple improvements we can layer on in separate implementation increments:

+ Adding a learning run into the release build and shipping that policy with the canonical distribution as default.
+ Persisting learned policies on the current machine and loading them again on startup.

To support manual review of the effectiveness of the policies configured in production instances, we begin implementation by adding a report that collects statistics on cache admissions and cache hits.

This report contains:

+ Cache admission count.
+ Cache lookup count.
+ Cache hit count.
+ Each of these is tracked separately for each expression in the cache key.

The language server should log these stats once every five minutes.

### Effectiveness-review APIs and reporting

`InvocationCacheStatistics.Observe` instruments an `IInvocationCacheAccess`. `GetSnapshot`
returns immutable cumulative counters keyed by the complete expression value, plus aggregate
totals equal to their sum. Instrumentation is opt-in outside the language service.

Language-service sessions share one collector across initialization and all worker attempts.
`ILanguageServiceSession.GetInvocationCacheStatistics` exposes its snapshot; the language server
also exposes `GetInvocationCacheStatistics` after workspace initialization. Implementations
without instrumentation return null.

Counters have these meanings:

+ **Admissions / admission attempts:** successful `TryAdd` calls / all completed `TryAdd` calls
  at the VM-facing boundary. In the language service this is admission to a worker's private
  buffer. Publication into shared storage is not a second admission; overlapping workers can
  each admit the same key. Failed attempts still contribute completed cache operations.
+ **Lookups / hits / misses / hit rate:** completed full-key `TryGet` calls, successes, failures,
  and the fraction of successful lookups (zero when no lookups occurred). A local hit and a shared
  hit each count once; internal cache-tier probes and promotions do not count again.
+ **Expression checks / skipped lookups:** `MayContainExpression` calls and negative results,
  respectively. These expose full-key lookups avoided before input materialization; they are
  not misses. They do not count invocations bypassing the cache altogether, such as inlined calls.
+ **Rejected admissions:** `TryAdd` calls returning false, including duplicate keys and buffer
  capacity refusals. Admission predicates that prevent a `TryAdd` call are not included.

Snapshots also include the shared cache's existing generation diagnostics: current unique and
physical entry counts, promotions, rotations and retired physical slots. These help distinguish
reuse from retention and generation churn. They are sampled separately from operation counters;
slot counts are not measurements of retained bytes, and retirements need not remove a unique key.

The language server logs a cumulative report every five minutes, including while idle.
Aggregate operation totals and shared-storage diagnostics precede all per-expression counters.
Expressions use their full deterministic value hash, ordered by decreasing hit count and then
hash, without logging invocation inputs or return values. Logging does not reset counters.
Shutdown or disposal stops the reporter, including when initialization is still in progress.

These APIs provide observations for effectiveness review and automated assertions.

## Reusable policy learning

`InvocationCachePolicy` is an immutable allow-list of complete encoded expression values for
both cache admission and lookup. Equality and `GetHashCode` compare set contents, independently
of input ordering or duplicate entries. `Id` is a deterministic content identifier suitable for
cross-process logs; it is not a substitute for full-value equality.

`Apply` wraps existing invocation storage. Changing the policy does not delete entries. The VM
checks expression admission before materializing inputs or return values for caching; numeric
admission thresholds remain independently configurable.

`InvocationCachePolicyLearner` accepts a caller-supplied PineVM factory and an ordered sequence
of `InvocationCacheLearningRequest` root expressions and environments. It is independent of Elm
and the language server. Public operations include:

+ `DiscoverExpressions`: replay without admission, so parent hits cannot hide child candidates.
+ `Evaluate`: replay one policy from empty invocation storage, returning deterministic work
  counters, cache statistics, retained entry count, root results, and estimated cost.
+ `EstimateCost`: score measurements using configurable execution, lookup, admission,
  materialization, and retained-entry weights.
+ `Learn`: measure the base policy on the current sequence, then replay bounded single-expression
  additions and removals. Accept lower estimated cost, or equal cost with a smaller expression set.
  Verify root results match the baseline before accepting any alternative.

All alternatives replay the same ordered requests. Entries are reused across requests within a
replay but never between alternatives. A fresh VM is created per request, matching request-scoped
hosts; callers may share compilation and parse caches independently. Configure the factory's
numeric admission thresholds consistently with production.

`InvocationCacheLearningState` carries the executable policy, known candidates, a rotating search
cursor, run count, and latest-run evidence. Measurements are recomputed for each new sequence:
costs from different workloads are not compared. A bounded run can return the same policy instance;
that indicates no tested neighbor was better, not proof of a global optimum. Removals allow a
previously useful child to be dropped when a parent cache entry shadows it. Further runs continue
the search using retained state. Discovery and neighbor budgets are configurable and cancellation
does not publish partially learned state.

The objective is an explicit heuristic, not a wall-clock prediction or retained-byte measurement.
Cold invocation storage makes policies comparable, but does not model every warm production cache
or generation retirement. Interacting changes can require multiple runs, and coordinate search can
settle at a local optimum. Keep production statistics and switch evidence when reviewing results.
Candidate-pool limits can exclude expressions in large workloads; increase the discovery budget
when using the learner for broader offline optimization.

## Continuous learning in the language server

The language server starts with the empty policy and zero numeric admission thresholds. Each
worker attempt captures one immutable policy for its entire execution, while keeping the session's
existing invocation and metadata caches. New attempts see the latest atomically published policy;
in-flight attempts finish using their original policy, including publication of completed entries.

The reusable `ContinuousInvocationCacheLearning` controller samples successful root evaluations
without waiting for training. Its pending queue and rolling sample are bounded, and only one
learning run is active. The language-server host retains eight recent evaluations and at most
sixteen pending samples, with a minimum thirty-second interval between rounds. Overloaded sampling
drops requests rather than delaying production.

Actual replays run on the controller's background task within the language-server process, not on
production request threads. PineVM provides evaluation isolation: learning uses fresh VM instances,
independent invocation storage for each alternative, and learning-only metadata caches shared across
replays. No operating-system process, IPC, or serialization is needed. Each round tests up to four
add/remove neighbors with a two-minute cooperative cancellation deadline; further rounds resume the
retained search cursor until an unchanged sweep completes. Cancellation and shutdown stop replay
through PineVM's cancellation support. Training errors leave the current production policy intact.

Logs identify policies by deterministic full content hashes and report every actual policy switch,
including added/removed expression hashes, old/new set sizes, run and switch counts, sample/drop
counts, elapsed learning time, time since the preceding switch, and estimated baseline/selected
costs. Equal policies do not cause a switch. No training environments, source contents, or return
values are logged. Session/server disposal stops sampling and cancels training. Policies are
currently learned in process lifetime only; persistence and bundled training
policies remain separate extensions.
