"""Validate and summarize fresh-process before/after captures; standard library only."""

import json
import hashlib
import importlib.util
import pathlib
import statistics
import sys


def read(path):
    return json.loads(path.read_text())


def dependency_inputs(report):
    configuration = {key: value for key, value in report["Configuration"].items()
                     if key != "CompilerImplementationId"}
    return {key: report[key] for key in (
        "ManifestPath", "ManifestText", "Manifest", "Requirements", "Packages", "Failures", "Succeeded"
    )} | {"Configuration": configuration}


def validate(run, source, resolution):
    assert run["Passed"] == 559 and run["Failed"] == 0 and run["IncompleteReason"] is None
    assert run["Seed"] == 1050281084 and run["Fuzz"] == 100 and run["Workers"] == 1
    assert run["SourceFingerprint"] == source and run["ResolutionFingerprint"] == resolution
    assert len(run["FuzzProperties"]) == 8
    assert all(prop["RunsRequested"] == prop["RunsElapsed"] == 100 for prop in run["FuzzProperties"])


def describe(values):
    return {
        "values": values,
        "median": statistics.median(values),
        "minimum": min(values),
        "maximum": max(values),
        "mean": statistics.mean(values),
        "populationStdev": statistics.pstdev(values),
    }


def main():
    if len(sys.argv) not in (3, 4):
        raise SystemExit("Usage: compare.py <capture directory> <comparison.json> [repetitions=3]")
    repetitions = int(sys.argv[3]) if len(sys.argv) == 4 else 3
    if repetitions < 1:
        raise SystemExit("Repetitions must be a positive integer.")
    indices = range(1, repetitions + 1)
    root = pathlib.Path(sys.argv[1])
    baseline = [read(root / f"baseline-{index}" / "summary.json") for index in indices]
    optimized = [read(root / f"optimized-{index}" / "summary.json") for index in indices]
    before = read(root / "baseline-observed" / "summary.json")
    after = read(root / "optimized-observed" / "summary.json")
    source = baseline[0]["SourceFingerprint"]
    baseline_resolution, optimized_resolution = (
        baseline[0]["ResolutionFingerprint"], optimized[0]["ResolutionFingerprint"]
    )
    for run in baseline + [before]:
        validate(run, source, baseline_resolution)
    for run in optimized + [after]:
        validate(run, source, optimized_resolution)
    reference_inputs = dependency_inputs(read(root / "baseline-1" / "dependencies.json"))
    for variant in ("baseline", "optimized"):
        for suffix in [str(index) for index in indices] + ["observed"]:
            inputs = dependency_inputs(read(root / f"{variant}-{suffix}" / "dependencies.json"))
            assert inputs == reference_inputs, f"Dependency inputs changed in {variant}-{suffix}"
    comparable_fingerprint = hashlib.sha256(
        json.dumps(reference_inputs, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()
    measurements = {}
    for field in (
        "DurationMilliseconds", "CompilationMilliseconds", "PreparationMilliseconds",
        "ExecutionMilliseconds", "CpuMilliseconds", "AllocatedBytes", "GcPauseMilliseconds",
        "PreparationVmMilliseconds", "ExecutionVmMilliseconds",
    ):
        b, a = describe([run[field] for run in baseline]), describe([run[field] for run in optimized])
        measurements[field] = {
            "baseline": b, "optimized": a,
            "medianReduction": b["median"] - a["median"],
            "medianReductionPercent": (b["median"] - a["median"]) * 100 / b["median"],
            "pairedReductionPercent": [
                (old[field] - new[field]) * 100 / old[field] for old, new in zip(baseline, optimized)
            ],
        }
    counters = {}
    for field in sorted(before["SiteCounters"].keys() | after["SiteCounters"].keys()):
        b, a = before["SiteCounters"].get(field), after["SiteCounters"].get(field)
        counters[field] = {"baseline": b, "optimized": a,
                           "difference": a - b if a is not None and b is not None else None}
    cache_fields = (
        "IrCompilationEntries", "ParseCacheEntries", "EncodedExpressionEntries",
        "ReducedExpressionEntries", "TemplatePlanEntries",
        "TemplateConstructorProofEntries", "TemplateBoundedValueProofEntries",
    )
    result = {
        "Repetitions": repetitions,
        "SourceFingerprint": source,
        "ResolutionFingerprints": {"baseline": baseline_resolution, "optimized": optimized_resolution},
        "ComparableDependencyFingerprint": comparable_fingerprint,
        "DependencyComparisonExcludedIdentity": "Configuration.CompilerImplementationId (Pine.Core MVID)",
        "UnobservedMeasurements": measurements,
        "ObservedSiteCounters": counters,
        "ObservedCacheEntries": {field: {"baseline": before.get(field), "optimized": after.get(field)}
                                for field in cache_fields},
        "ObservedDirectTemplates": {
            label: {key: value for key, value in run["DirectTemplates"].items() if key != "Templates"}
            for label, run in (("baseline", before), ("optimized", after))
        },
        "ObservedInvocationCacheStatistics": {
            "baseline": before["CacheStatistics"], "optimized": after["CacheStatistics"],
        },
        "ObservedWorkSummary": {"baseline": before["WorkSummary"], "optimized": after["WorkSummary"]},
        "UnchangedLogicalWork": {
            field: before["WorkSummary"][field] == after["WorkSummary"][field]
            for field in ("Counters", "CachedWorkCounters", "CountersByPhase")
        },
        "UnchangedInvocationCacheStatistics": before["CacheStatistics"] == after["CacheStatistics"],
    }
    helper_path = pathlib.Path(__file__).parent.parent / (
        "2026-11-01-elm-type-inference-test-performance/summarize.py"
    )
    spec = importlib.util.spec_from_file_location("pine_profile_summary", helper_path)
    assert spec is not None and spec.loader is not None
    helper = importlib.util.module_from_spec(spec)
    sys.dont_write_bytecode = True
    spec.loader.exec_module(helper)
    result["ObservedResourceWindows"] = {
        variant: helper.derive_work(root / f"{variant}-observed" / "work.ndjson")
        for variant in ("baseline", "optimized")
    }
    pathlib.Path(sys.argv[2]).write_text(json.dumps(result, indent=2) + "\n")
    for field, data in measurements.items():
        print(f'{field}: {data["baseline"]["median"]:.3f} -> {data["optimized"]["median"]:.3f}; '
              f'{data["medianReductionPercent"]:.2f}% reduction')


if __name__ == "__main__":
    main()
