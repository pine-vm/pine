using Pine;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ElmTime.ElmInteractive;

public partial class TestElmInteractive
{
    /// <summary>
    /// Locates a repository scenario suite from a supplied directory or the test executable's
    /// base directory, independent of the process working directory and build configuration.
    /// </summary>
    public static string FindScenariosDirectory(string suiteName, string? startingDirectory = null)
    {
        if (string.IsNullOrWhiteSpace(suiteName) || Path.GetFileName(suiteName) != suiteName ||
            suiteName is "." or "..")
        {
            throw new ArgumentException("Expected a scenario suite directory name, not a path.", nameof(suiteName));
        }

        var start = Path.GetFullPath(startingDirectory ?? AppContext.BaseDirectory);

        for (var directory = new DirectoryInfo(start); directory is not null; directory = directory.Parent)
        {
            if (!File.Exists(Path.Combine(directory.FullName, "pine.slnx")))
                continue;

            var candidate = Path.Combine(directory.FullName, "implement", "test-and-train", suiteName);

            if (!Directory.Exists(candidate))
                throw new DirectoryNotFoundException("Scenario suite not found: " + candidate);

            return candidate;
        }

        throw new DirectoryNotFoundException("Could not locate pine.slnx above " + start + ".");
    }

    /// <summary>
    /// Loads fixtures without creating an interpreter or running submissions. Empty leftover
    /// directories are ignored; missing/empty suites and duplicate scenario names fail explicitly.
    /// The returned tree and scenario identities do not depend on filesystem enumeration order.
    /// </summary>
    public static ParsedScenarios LoadScenariosFromDirectories(
        IReadOnlyList<string> directories,
        IConsole console)
    {
        if (directories.Count is 0)
            throw new ArgumentException("Select at least one scenario suite.", nameof(directories));

        var scenarios = new SortedDictionary<string, FileTree>(StringComparer.Ordinal);

        foreach (var directory in directories)
        {
            var count = 0;

            foreach (var scenarioDirectory in Directory.EnumerateDirectories(directory).Order(StringComparer.Ordinal))
            {
                if (!Directory.EnumerateFiles(scenarioDirectory, "*", SearchOption.AllDirectories).Any())
                    continue;

                var name = Path.GetFileName(scenarioDirectory);

                if (!scenarios.TryAdd(name, LoadFromLocalFilesystem.LoadSortedTreeFromPath(scenarioDirectory)!))
                {
                    throw new ArgumentException(
                        "Duplicate scenario name '" + name + "' in " + directory + ".",
                        nameof(directories));
                }

                count++;
            }

            if (count is 0)
                throw new ArgumentException("No scenarios found in " + directory + ".", nameof(directories));
        }

        return
            ParseElmInteractiveScenarios(
                FileTree.SortedDirectory([.. scenarios.Select(item => (item.Key, item.Value))]),
                console);
    }
}
