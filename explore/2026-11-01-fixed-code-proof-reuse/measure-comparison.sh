#!/usr/bin/env bash
set -euo pipefail

if [[ $# != 4 && $# != 5 ]]; then
    echo "Usage: measure-comparison.sh <baseline ProfileScenario.dll> <optimized ProfileScenario.dll> <project> <new output directory> [repetitions=3]" >&2
    exit 1
fi

BASELINE="$1"
OPTIMIZED="$2"
PROJECT="$3"
OUT="$4"
REPETITIONS="${5:-3}"
if [[ ! "$REPETITIONS" =~ ^[1-9][0-9]*$ ]]; then
    echo "Repetitions must be a positive integer." >&2
    exit 1
fi
for path in "$BASELINE" "$OPTIMIZED" "$PROJECT" "$OUT"; do
    if [[ "$path" != /* ]]; then
        echo "All paths must be absolute." >&2
        exit 1
    fi
done
if [[ -e "$OUT" ]]; then
    echo "Use a new output directory to avoid mixing captures." >&2
    exit 1
fi
mkdir -p "$OUT"
dotnet --info > "$OUT/dotnet-info.txt"
sha256sum "$BASELINE" "$(dirname "$BASELINE")/Pine.Core.dll" \
    "$OPTIMIZED" "$(dirname "$OPTIMIZED")/Pine.Core.dll" > "$OUT/binary-sha256.txt"
date --iso-8601=seconds > "$OUT/started.txt"

# Interleave fresh processes; never share runtime/VM state or run benchmarks concurrently.
for ((run = 1; run <= REPETITIONS; ++run)); do
    dotnet "$BASELINE" run "$PROJECT" "$OUT/baseline-$run" baseline > "$OUT/baseline-$run.log" 2>&1
    dotnet "$OPTIMIZED" run "$PROJECT" "$OUT/optimized-$run" baseline > "$OUT/optimized-$run.log" 2>&1
done
dotnet "$BASELINE" run "$PROJECT" "$OUT/baseline-observed" sharing > "$OUT/baseline-observed.log" 2>&1
dotnet "$OPTIMIZED" run "$PROJECT" "$OUT/optimized-observed" sharing > "$OUT/optimized-observed.log" 2>&1
date --iso-8601=seconds > "$OUT/completed.txt"
echo "Completed interleaved baseline/optimized runs and both observers: $OUT"
