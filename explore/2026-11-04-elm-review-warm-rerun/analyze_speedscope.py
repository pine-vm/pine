"""Summarize TraceEvent evented speedscope exports; coverage is not process CPU."""

import argparse
import collections
import json


def category(names):
    text = "\n".join(names)
    if "InvocationCachePolicyLearner" in text or "ContinuousInvocationCacheLearning" in text:
        return "background-learner"
    if "CompileApplicationsToPine" in text or "PineClosureCompiler" in text:
        return "compile-selected-closures"
    if "CanonicalizeDeclarations" in text:
        return "canonicalization"
    if "ElmTestDiscovery.DiscoverTests" in text:
        if "RunTrampoline" in text or "ElmSyntaxInterpreter.Interpret" in text:
            return "interpreter-construction-and-bridge"
        return "discovery-preparation-and-inventory"
    if "ElmResolvedBuildPreparation" in text or "ElmDependencyResolver" in text:
        return "load-resolve-parse"
    if "RunTest" in text or "ExecuteDiscoveredTest" in text:
        return "execute"
    return "other"


def summarize(data):
    names = [frame["name"] for frame in data["shared"]["frames"]]
    inclusive = collections.Counter()
    exclusive = collections.Counter()
    categories = collections.Counter()
    threads = {}
    excluded = {}
    ranges = {}
    markers = 0

    for profile in data["profiles"]:
        if profile["type"] != "evented" or profile["unit"] != "milliseconds":
            raise ValueError("Expected TraceEvent evented profiles with millisecond units.")
        stack = []
        previous = profile["startValue"]
        coverage = 0
        runtime_thread = "Finalizer" in profile["name"]

        for event in profile["events"]:
            delta = event["at"] - previous
            if delta < 0:
                raise ValueError("Profile event timestamps are not ordered.")
            if stack and delta > 0 and names[stack[-1]] == "CPU_TIME":
                markers += 1
                coverage += delta
                if not runtime_thread:
                    actual = [names[index] for index in stack[:-1]]
                    exclusive[actual[-1]] += delta
                    for name in set(actual):
                        inclusive[name] += delta
                    key = category(actual)
                    categories[key] += delta
                    bounds = ranges.setdefault(key, [previous, event["at"]])
                    bounds[0] = min(bounds[0], previous)
                    bounds[1] = max(bounds[1], event["at"])
            if event["type"] == "O":
                stack.append(event["frame"])
            elif event["type"] == "C":
                if not stack or stack.pop() != event["frame"]:
                    raise ValueError("Unbalanced profile stack.")
            else:
                raise ValueError("Unknown event type: " + event["type"])
            previous = event["at"]

        if stack:
            raise ValueError("Profile ends with an open stack.")
        (excluded if runtime_thread else threads)[profile["name"]] = coverage

    if not markers:
        raise ValueError("No TraceEvent CPU_TIME markers found; this export needs a different interpretation.")

    return {
        "Warning": "Managed-stack coverage is not process CPU. Finalizer threads are excluded from rankings.",
        "ThreadStackCoverageMs": threads,
        "ExcludedRuntimeThreadCoverageMs": excluded,
        "CategoryStackCoverageMs": categories,
        "CategoryRangesMs": ranges,
        "InclusiveMs": inclusive.most_common(),
        "ExclusiveMs": exclusive.most_common(),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", help="TraceEvent .speedscope.json export")
    parser.add_argument("output", help="JSON summary destination")
    args = parser.parse_args()
    with open(args.input, encoding="utf-8") as source:
        summary = summarize(json.load(source))
    with open(args.output, "w", encoding="utf-8") as output:
        json.dump(summary, output, indent=2)
    print(summary["Warning"])
    print("INCLUSIVE (nested rows must not be added)")
    for name, value in summary["InclusiveMs"][:55]:
        print(f"{value:10.1f} {name}")
    print("EXCLUSIVE")
    for name, value in summary["ExclusiveMs"][:35]:
        print(f"{value:10.1f} {name}")


if __name__ == "__main__":
    main()
