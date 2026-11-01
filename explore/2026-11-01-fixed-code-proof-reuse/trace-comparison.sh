#!/usr/bin/env bash
set -euo pipefail

if [[ $# != 5 ]]; then
    echo "Usage: trace-comparison.sh <dotnet-trace> <baseline ProfileScenario.dll> <optimized ProfileScenario.dll> <project> <new output directory>" >&2
    exit 1
fi

TRACE="$1"
BASELINE="$2"
OPTIMIZED="$3"
PROJECT="$4"
OUT="$5"
for path in "$TRACE" "$BASELINE" "$OPTIMIZED" "$PROJECT" "$OUT"; do
    if [[ "$path" != /* ]]; then
        echo "All paths must be absolute." >&2
        exit 1
    fi
done
if [[ -e "$OUT" ]]; then
    echo "Use a new output directory." >&2
    exit 1
fi
mkdir -p "$OUT"
"$TRACE" --version > "$OUT/trace-version.txt"

for variant in baseline optimized; do
    DLL="$BASELINE"
    if [[ "$variant" == optimized ]]; then DLL="$OPTIMIZED"; fi
    "$TRACE" collect --profile dotnet-sampled-thread-time,gc-verbose \
        --format Speedscope --show-child-io --output "$OUT/$variant.nettrace" \
        -- dotnet "$DLL" run "$PROJECT" "$OUT/$variant-traced" baseline \
        > "$OUT/$variant-eventpipe.log" 2>&1
    dotnet "$OPTIMIZED" analyze "$OUT/$variant.nettrace" "$OUT/$variant-summary.json"
done

for variant in baseline optimized; do
    DLL="$BASELINE"
    if [[ "$variant" == optimized ]]; then DLL="$OPTIMIZED"; fi
    dotnet "$DLL" run "$PROJECT" "$OUT/$variant-kernel-run" baseline \
        > "$OUT/$variant-kernel-run.log" 2>&1 &
    TARGET_PID=$!
    trap 'kill "$TARGET_PID" 2>/dev/null || true' EXIT
    echo "$TARGET_PID" > "$OUT/$variant-target-pid.txt"
    # The runner's selection callback occurs after preparation, immediately before execution.
    until grep -q '^Running 559 tests; skipped 0.' "$OUT/$variant-kernel-run.log"; do
        if ! kill -0 "$TARGET_PID" 2>/dev/null; then
            wait "$TARGET_PID"
            echo "Runner exited before test execution." >&2
            exit 1
        fi
        sleep 0.1
    done
    "$TRACE" collect-linux --profile cpu-sampling --process-id "$TARGET_PID" \
        --duration 00:00:03:00 --output "$OUT/$variant-kernel.nettrace" \
        > "$OUT/$variant-kernel.log" 2>&1
    wait "$TARGET_PID"
    trap - EXIT
    dotnet "$OPTIMIZED" analyze "$OUT/$variant-kernel.nettrace" "$OUT/$variant-kernel-summary.json"
done
date --iso-8601=seconds > "$OUT/completed.txt"
echo "Completed paired EventPipe and execution-only target-process kernel traces: $OUT"
