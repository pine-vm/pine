# elm-review warm rerun profiler

Reproduces productive rule edits after priming one long-lived test-cache session.
See the [analysis and baseline](../internal-analysis/2026-11-04-elm-review-tasktest-warm-rerun-profile.md)
for measured bottlenecks, limitations, and the proposed incremental cache design.
This project is intentionally outside the main solution and normal test runs.

## Prepare

Requires .NET 10, Git, and Python 3 for the optional speedscope analyzer.
From the Pine repository root:

```sh
(cd implement && dotnet run --project prebuild/prebuild.csproj --configuration Release)
dotnet build explore/2026-11-04-elm-review-warm-rerun/ElmReviewProfiling.csproj -c Release

git clone https://github.com/jfmengels/elm-review.git /tmp/elm-review-profile
git -C /tmp/elm-review-profile checkout 73dfa9a03352a19fde51ffe3e6ea02bdb0f7010f
```

The prebuild program's working directory must be `implement/`.
Do not include prebuild, restore, or compilation in timed runs.

Use a **dedicated disposable workload checkout**, not a working editor session.
The harness edits `tests/Simplify.elm`, restores its original bytes in `finally`,
and refuses to overwrite a concurrently edited file. Hard termination, machine
failure, or an external edit can still require manual restoration.

Package sources must be cached for the default offline run. On the first run,
add `--online`; dependency acquisition occurs during the excluded priming round.
Record this choice when comparing runs.

## Measure edited warm reruns

```sh
dotnet run --no-build -c Release \
  --project explore/2026-11-04-elm-review-warm-rerun/ElmReviewProfiling.csproj -- \
  run /tmp/elm-review-profile /tmp/pine-profile-baseline --rounds 3 --online
```

The output directory must be new or empty and outside the workload checkout.
Use a different output directory for each intervention.
The harness runs an excluded priming round, then distinct one-, two-, and
three-space diagnostic edits by default. Each measured round must run 79 tests
and fail only the specified `Task.sequence []` test. Increase `--rounds N` for
more distinct edits. Priming must pass all 79 tests.

`metadata.json` records revisions, dirty working trees, runtime, CPU count,
GC mode, core assembly hash/MVID, settings, and cache policy.
`harness-results.json` retains phase wall time, process CPU, allocation, GC,
inventory sizes, package versions, failure messages, and cache snapshots.
Select records with `"Measured": true`; do not include `"warmup"` in comparisons.
Completed rounds are saved as they finish, including when a later round fails.

The cache policy is the core testing service's generational defaults **without
the CLI watch learner**, with retained CLI-style source parsing and serial
execution. This is not an end-to-end LSP or CLI-watch benchmark. For the actual
watch learner, use `pine elm test --watch` and attach diagnostics after its first
passing run as described in the report.

`--dependencies` adds the baseline's declaration/reverse-dependency counts.
It uses explicit reflection into discovery's captured canonical result and is
optional so ordinary profiling survives internal layout changes. If that option
fails after a refactor, update its introspection instead of treating absent
counts as cache hits. Root enumeration also reuses the runner's internal exposure
predicate via a checked delegate; a changed API produces an explicit error.

## Measure the language-server path

Add `--language-server` to exercise the actual `LanguageServerTestingService`
instead of the core-only pipeline. Each round captures/discovers a new immutable
version, runs the same 79 selected tests, and requests an empty selection on that
retained inventory with a different seed. This records `lsp-discover`, `lsp-run`,
and `lsp-empty-selection` phases, including CPU, allocation, and GC measurements.
The empty selection checks overhead independently of callback execution.

The service and its execution caches are retained across productive edits.
Snapshot capture uses the disposable checkout; protocol calls and event handling
are in-process, without JSON-RPC transport or editor rendering. Dependency resolution
uses the service's normal configuration and already warm package caches.
`--online` and `--dependencies` are not applicable in this mode.
Compare this mode only with the same mode on another implementation, not with
core-only phase timings.

## Attach diagnostics only after priming

Add `--wait-for-profiler`. After printing `READY`, the harness waits until
`<output-directory>/continue` exists, before applying the first measured edit.
Use the printed process ID:

```sh
dotnet-trace collect --process-id <pid> \
  --profile dotnet-sampled-thread-time,gc-verbose \
  --format Speedscope --output /tmp/edited.nettrace
```

In another terminal, attach `dotnet-counters` if required, then release the gate:

```sh
dotnet-counters collect --process-id <pid> --counters System.Runtime \
  --format csv --output /tmp/edited-counters.csv

# Separate terminal:
touch /tmp/pine-profile-baseline/continue
```

The collectors stop when the harness exits; Ctrl+C also stops a collector.
They must be attached before creating the gate. Capture and unprofiled benchmark
runs separately: sampling/allocation instrumentation changes latency and GC.
Ctrl+C in the harness cooperatively cancels work and restores the fixture.

## Analyze captures

```sh
dotnet run --no-build -c Release \
  --project explore/2026-11-04-elm-review-warm-rerun/ElmReviewProfiling.csproj -- \
  trace /tmp/edited.nettrace > /tmp/allocations-and-suspensions.json

dotnet run --no-build -c Release \
  --project explore/2026-11-04-elm-review-warm-rerun/ElmReviewProfiling.csproj -- \
  trace /tmp/edited.nettrace --allocation-stacks > /tmp/allocation-stacks.json

python3 explore/2026-11-04-elm-review-warm-rerun/analyze_speedscope.py \
  /tmp/edited.speedscope.json /tmp/stack-coverage.json
```

Stack allocation analysis creates an `.etlx` index alongside the trace.
Allocation ticks estimate volume, not live retained size. Runtime EE suspension
events include non-GC reasons, including profiling effects; do not substitute
their sum for the GC-pause metric. Synthetic speedscope `CPU_TIME` coverage is
not process CPU accounting. The Python analyzer excludes finalizer-thread
coverage from rankings and retains it separately.

The allocation classifier has an explicitly documented baseline-specific
inlined-lambda fallback for inventory projection. Check raw stacks after source
changes before comparing that category; method renaming or inlining can move
samples into another category without reducing total work.
