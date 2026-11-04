using Microsoft.Diagnostics.Tracing;
using Microsoft.Diagnostics.Tracing.Etlx;
using Microsoft.Diagnostics.Tracing.Parsers.Clr;
using System.Text.Json;

namespace ElmReviewProfiling;

internal static class TraceAnalysis
{
    private static readonly JsonSerializerOptions s_json = new() { WriteIndented = true };

    public static void Summarize(string path)
    {
        var types = new Dictionary<string, (long Bytes, long Samples)>();
        var collections = new Dictionary<int, int>();
        var suspensions = new Dictionary<string, (int Count, double Milliseconds)>();
        var events = new Dictionary<string, int>();
        double suspendStart = 0;
        double maxSuspension = 0;
        var suspendReason = "";
        var suspended = false;

        using var source = new EventPipeEventSource(path);
        source.Clr.GCAllocationTick += data =>
        {
            var name = data.TypeName ?? "<unknown>";
            var previous = types.GetValueOrDefault(name);
            types[name] = (previous.Bytes + data.AllocationAmount64, previous.Samples + 1);
        };
        source.Clr.GCStart += data => collections[data.Depth] = collections.GetValueOrDefault(data.Depth) + 1;
        source.Clr.GCSuspendEEStart += data =>
        {
            suspendStart = data.TimeStampRelativeMSec;
            suspendReason = data.Reason.ToString();
            suspended = true;
        };
        source.Clr.GCRestartEEStop += data =>
        {
            if (!suspended)
                return;

            var elapsed = data.TimeStampRelativeMSec - suspendStart;
            var previous = suspensions.GetValueOrDefault(suspendReason);
            suspensions[suspendReason] = (previous.Count + 1, previous.Milliseconds + elapsed);
            maxSuspension = Math.Max(maxSuspension, elapsed);
            suspended = false;
        };
        source.Dynamic.All += data =>
        {
            var name = data.ProviderName + "/" + data.EventName;
            events[name] = events.GetValueOrDefault(name) + 1;
        };
        source.Process();

        Console.WriteLine(JsonSerializer.Serialize(new
        {
            SampledAllocations = types.OrderByDescending(pair => pair.Value.Bytes).Take(40)
                .Select(pair => new { Type = pair.Key, pair.Value.Bytes, pair.Value.Samples }),
            EstimatedTotalAllocated = types.Sum(pair => pair.Value.Bytes),
            Collections = collections,
            RuntimeEeSuspensionMs = suspensions.Sum(pair => pair.Value.Milliseconds),
            MaxEeSuspensionMs = maxSuspension,
            EeSuspensionsByReason = suspensions.Select(pair => new
            {
                Reason = pair.Key,
                pair.Value.Count,
                pair.Value.Milliseconds
            }),
            Events = events.OrderByDescending(pair => pair.Value).Take(30),
            Warning = "EE suspensions include non-GC reasons and profiler effects; use runtime GC-pause counters."
        }, s_json));
    }

    public static void AllocationStacks(string path)
    {
        var etlx = TraceLog.CreateFromEventPipeDataFile(path);
        using var log = new TraceLog(etlx);
        var bytes = new Dictionary<string, long>();
        var groupedTypes = new Dictionary<string, long>();

        foreach (var item in log.Events)
        {
            if (item is not GCAllocationTickTraceData allocation)
                continue;

            var frames = new List<string>();

            for (var stack = item.CallStack(); stack is not null; stack = stack.Caller)
                frames.Add(stack.CodeAddress.FullMethodName);

            var text = string.Join('\n', frames);
            var category = Category(text);
            bytes[category] = bytes.GetValueOrDefault(category) + allocation.AllocationAmount64;
            var key = category + ": " + allocation.TypeName;
            groupedTypes[key] = groupedTypes.GetValueOrDefault(key) + allocation.AllocationAmount64;
        }

        Console.WriteLine(JsonSerializer.Serialize(new
        {
            AllocationBytesByStack = bytes,
            TopTypesByStack = groupedTypes.OrderByDescending(pair => pair.Value).Take(50),
            Warning = "Sampled estimates. Inlined inventory-lambda fallback is specific to the report's baseline."
        }, s_json));
    }

    private static string Category(string stack)
    {
        if (stack.Contains("InvocationCachePolicyLearner", StringComparison.Ordinal))
            return "learner";

        if (stack.Contains("PreparedDeclarationResolverIndex", StringComparison.Ordinal) ||
            stack.Contains("BuildResolvers", StringComparison.Ordinal))
        {
            return "resolver-setup";
        }

        // ExactSelector is inlined in the recorded baseline; recheck this fallback after source changes.
        if (stack.Contains("ExactSelector", StringComparison.Ordinal) ||
            stack.Contains("<DiscoverTests>b__21", StringComparison.Ordinal))
        {
            return "inventory-selectors";
        }

        if (stack.Contains("PineClosureCompiler", StringComparison.Ordinal))
            return "compile-closures";

        if (stack.Contains("ElmTestDiscovery.DiscoverTests", StringComparison.Ordinal))
            return "discovery-other";

        return stack.Contains("RunTest", StringComparison.Ordinal) ? "execute" : "other";
    }
}
