using Pine.Core.Elm.Testing;
using System.Diagnostics;
using System.Text.Json;

var count = args.Length > 0 ? int.Parse(args[0]) : 100;
var shape = args.Length > 1 ? args[1] : "direct";
var repetitions = args.Length > 2 ? int.Parse(args[2]) : 5;
var mode = args.Length > 3 ? args[3] : "both";
if (count < 1 || repetitions < 1 || shape is not ("direct" or "generated") || mode is not ("syntax" or "compiler" or "both"))
    throw new ArgumentException("Usage: benchmark [positive count] [direct|generated] [positive repetitions] [syntax|compiler|both]");

var directory = Path.Combine(Path.GetTempPath(), "pine-test-discovery-" + Guid.NewGuid().ToString("N"));
Directory.CreateDirectory(Path.Combine(directory, "tests"));
try
{
    File.WriteAllText(Path.Combine(directory, "elm.json"),
        """
        {
          "type": "application", "source-directories": ["src"], "elm-version": "0.19.1",
          "dependencies": { "direct": { "elm/core": "1.0.5" }, "indirect": {} },
          "test-dependencies": { "direct": { "elm-explorations/test": "2.2.1" },
            "indirect": { "elm/random": "1.0.0", "elm/bytes": "1.0.8" } }
        }
        """);
    var declarations = shape == "direct"
        ? "suite = Test.describe \"suite\"\n    [ " +
            string.Join("\n    , ", Enumerable.Range(1, count).Select(index =>
                $"Test.test \"case {index}\" (\\_ -> Expect.equal {index} {index})")) + "\n    ]\n"
        : "factory index = Test.test (\"case \" ++ String.fromInt index) (\\_ -> Expect.equal index index)\n" +
            $"suite = Test.describe \"suite\" (List.map factory (List.range 1 {count}))\n";
    File.WriteAllText(Path.Combine(directory, "tests", "Tests.elm"),
        "module Tests exposing (suite)\nimport Test\nimport Expect\n\n" + declarations);

    string[]? expected = null;
    if (mode is "both" or "syntax")
        Measure("syntax");
    if (mode is "both" or "compiler")
        Measure("compiler");

    void Measure(string engine)
    {
        var results = new List<object>();
        for (var iteration = 0; iteration < repetitions; iteration++)
        {
            GC.Collect();
            GC.WaitForPendingFinalizers();
            GC.Collect();
            var allocated = GC.GetTotalAllocatedBytes(precise: true);
            var watch = Stopwatch.StartNew();
            string[] paths;
            double? sourceMs = null;
            double? preparationMs = null;
            double? discoveryMs = null;
            if (engine == "syntax")
            {
                var discovery = ElmTestDiscovery.DiscoverTests(directory);
                paths = discovery.Tests.Select(test => test.ListedTest.FullPath).ToArray();
                foreach (var test in discovery.Tests)
                    if (discovery.GetSourceLocation(test.Id).FilePath != "tests/Tests.elm")
                        throw new InvalidOperationException("Incorrect test source location.");
                sourceMs = discovery.SourcePreparationDuration.TotalMilliseconds;
                preparationMs = discovery.InterpreterPreparationDuration.TotalMilliseconds;
                discoveryMs = discovery.DiscoveryDuration.TotalMilliseconds;
            }
            else
            {
                var run = ElmTestRunner.CompileAndRunTests(directory, listTests: true);
                if (run is not ElmTestRun.Listed listed)
                    throw new InvalidOperationException("Expected a listing, got " + run.GetType().Name);
                paths = listed.Tests.Select(test => test.FullPath).ToArray();
            }
            watch.Stop();
            var allocatedBytes = GC.GetTotalAllocatedBytes(precise: true) - allocated;
            if (paths.Length != count)
                throw new InvalidOperationException($"Expected {count} tests, got {paths.Length}.");
            var requiredPaths = Enumerable.Range(1, count).Select(index => $"tests/Tests.elm/suite/case {index}");
            if (!paths.SequenceEqual(requiredPaths) || (expected is not null && !paths.SequenceEqual(expected)))
                throw new InvalidOperationException("Discovery outputs differ in names or order.");
            expected = paths;
            results.Add(new { iteration, elapsedMs = watch.Elapsed.TotalMilliseconds, allocatedBytes, sourceMs, preparationMs, discoveryMs });
        }
        Console.WriteLine(JsonSerializer.Serialize(new
        {
            engine,
            shape,
            count,
            repetitions,
            framework = System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription,
            os = System.Runtime.InteropServices.RuntimeInformation.OSDescription,
            processors = Environment.ProcessorCount,
            results
        }));
    }
}
finally
{
    Directory.Delete(directory, recursive: true);
}
