using System.Collections.Generic;
using System.IO;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Pine.Core.Tests;

/// <summary>
/// Helper used to re-record snapshots (e.g. performance-counter snapshots) of tests.
/// </summary>
public static class SnapshotRecorder
{
    private static readonly Lock s_lock = new();

    private static readonly Dictionary<string, int> s_memberCounters = [];

    public static string LogString(
        string snapshot,
        [CallerMemberName] string memberName = "")
    {
        lock (s_lock)
        {
            s_memberCounters.TryGetValue(memberName, out var index);
            s_memberCounters[memberName] = index + 1;

            System.IO.File.AppendAllText(
                "snapshots.txt",
                "### " + memberName + " #" + index + "\n" + snapshot + "\n\n");
        }

        return snapshot;
    }

    public static string ReadEmbeddedTrace(string name)
    {
        using var stream =
            typeof(SnapshotRecorder).Assembly.GetManifestResourceStream(
                "Pine.Core.Tests.TestData.StackInstructionTraces." + name + ".txt")
            ?? throw new FileNotFoundException("Missing embedded instruction trace: " + name);

        using var reader = new StreamReader(stream);

        return reader.ReadToEnd();
    }
}
