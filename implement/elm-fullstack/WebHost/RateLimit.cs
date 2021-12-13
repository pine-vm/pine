using System;
using System.Linq;

namespace ElmFullstack.WebHost;

public interface IRateLimit
{
    (IRateLimit newState, bool passed) AttemptPass(Int64 time);
}

public interface IMutableRateLimit
{
    bool AttemptPass(long time);
}

public class MutableRateLimitAlwaysPassing : IMutableRateLimit
{
    public bool AttemptPass(long time) => true;
}

public class RateLimitStateSingleWindow : IRateLimit
{
    public int limit;
    public int windowSize;

    public long[] passes;

    public (IRateLimit newState, bool passed) AttemptPass(long attemptTime)
    {
        var previousPassTime = passes?.Skip(limit - 1).Cast<long?>().FirstOrDefault();

        var previousPassAge = attemptTime - previousPassTime;

        if (previousPassAge < windowSize)
            return (this, false);

        return
            (new RateLimitStateSingleWindow
            {
                limit = limit,
                windowSize = windowSize,
                passes = new[] { attemptTime }.Concat(passes ?? Array.Empty<long>()).Take(limit).ToArray(),
            },
            true);
    }
}

public class RateLimitMutableContainer : IMutableRateLimit
{
    readonly object @lock = new();

    IRateLimit rateLimitState;

    public RateLimitMutableContainer(IRateLimit init)
    {
        rateLimitState = init;
    }

    public bool AttemptPass(long time)
    {
        lock (@lock)
        {
            var (newLimitState, passed) = rateLimitState.AttemptPass(time);

            rateLimitState = newLimitState;

            return passed;
        }
    }
}
