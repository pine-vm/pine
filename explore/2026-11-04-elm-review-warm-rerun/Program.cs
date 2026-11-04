namespace ElmReviewProfiling;

internal static class Program
{
    public static int Main(string[] args)
    {
        if (args is ["run", var checkout, var output, .. var options])
        {
            var rounds = 3;
            var wait = false;
            var dependencies = false;
            var offline = true;

            for (var index = 0; index < options.Length; index++)
            {
                switch (options[index])
                {
                    case "--rounds" when index + 1 < options.Length &&
                        int.TryParse(options[index + 1], out var count) && count > 0:
                        rounds = count;
                        index++;
                        break;

                    case "--wait-for-profiler":
                        wait = true;
                        break;

                    case "--dependencies":
                        dependencies = true;
                        break;

                    case "--online":
                        offline = false;
                        break;

                    default:
                        throw new ArgumentException("Unknown or invalid option: " + options[index]);
                }
            }

            return WarmRerun.Run(checkout, output, rounds, wait, dependencies, offline);
        }

        if (args is ["trace", var trace])
        {
            TraceAnalysis.Summarize(trace);
            return 0;
        }

        if (args is ["trace", var allocationTrace, "--allocation-stacks"])
        {
            TraceAnalysis.AllocationStacks(allocationTrace);
            return 0;
        }

        Console.Error.WriteLine(
            """
            Usage:
              ElmReviewProfiling run <elm-review-checkout> <new-output-directory>
                [--rounds N] [--wait-for-profiler] [--dependencies] [--online]
              ElmReviewProfiling trace <file.nettrace> [--allocation-stacks]
            """);

        return args is ["--help"] or [] ? 0 : 1;
    }
}
