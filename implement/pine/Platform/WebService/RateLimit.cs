using System.Collections.Immutable;
using System.Linq;

namespace ElmTime.Platform.WebService;

public interface IRateLimit
{
    (IRateLimit newState, bool passed) AttemptPass(long time);
}

public interface IMutableRateLimit
{
    bool AttemptPass(long time);
}

public class MutableRateLimitAlwaysPassing : IMutableRateLimit
{
    public bool AttemptPass(long time) => true;
}

public record RateLimitStateSingleWindow(
    int limit,
    int windowSize,
    IImmutableQueue<long> passes) : IRateLimit
{
    public (IRateLimit newState, bool passed) AttemptPass(long attemptTime)
    {
        var previousPassTime = this.passes.Reverse().Skip(limit - 1).Cast<long?>().FirstOrDefault();

        var previousPassAge = attemptTime - previousPassTime;

        if (previousPassAge < windowSize)
            return (this, false);

        var passes = this.passes.Enqueue(attemptTime);

        while (limit < passes.Count())
            passes = passes.Dequeue();

        return
            (new RateLimitStateSingleWindow
            (
                limit: limit,
                windowSize: windowSize,
                passes: passes
            ),
            true);
    }
}

public class RateLimitMutableContainer(
    IRateLimit rateLimitState)
    : IMutableRateLimit
{
    private readonly object @lock = new();

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
