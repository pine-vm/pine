using System;
using System.Linq;

namespace ElmFullstack.WebHost
{
    public interface IRateLimit
    {
        (IRateLimit newState, bool passed) AttemptPass(Int64 time);
    }

    public interface IMutableRateLimit
    {
        bool AttemptPass(Int64 time);
    }

    public class RateLimitStateSingleWindow : IRateLimit
    {
        public int limit;
        public int windowSize;

        public Int64[] passes;

        public (IRateLimit newState, bool passed) AttemptPass(Int64 attemptTime)
        {
            var previousPassTime = passes?.Skip(limit - 1).Cast<Int64?>().FirstOrDefault();

            var previousPassAge = attemptTime - previousPassTime;

            if (previousPassAge < windowSize)
                return (this, false);

            return
                (new RateLimitStateSingleWindow
                {
                    limit = limit,
                    windowSize = windowSize,
                    passes = new[] { attemptTime }.Concat(passes ?? new Int64[0]).Take(limit).ToArray(),
                },
                true);
        }
    }

    public class RateLimitMutableContainer : IMutableRateLimit
    {
        readonly object @lock = new object();

        IRateLimit rateLimitState;

        public RateLimitMutableContainer(IRateLimit init)
        {
            rateLimitState = init;
        }

        public bool AttemptPass(Int64 time)
        {
            lock (@lock)
            {
                var (newLimitState, passed) = rateLimitState.AttemptPass(time);

                rateLimitState = newLimitState;

                return passed;
            }
        }
    }
}