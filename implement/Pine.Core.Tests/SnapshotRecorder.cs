using System.Collections.Generic;
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
}
