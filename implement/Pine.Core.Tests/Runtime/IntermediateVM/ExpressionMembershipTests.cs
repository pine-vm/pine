using AwesomeAssertions;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class ExpressionMembershipTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Membership_reads_complete_while_the_result_gate_is_held(bool generational)
    {
        var cache = CreateCache(generational);
        var expression = Expression(1);
        cache.TryAdd(Key(expression), PineValue.EmptyList).Should().BeTrue();

        var gate =
            (Lock)cache.GetType().GetField("_lock", BindingFlags.Instance | BindingFlags.NonPublic)!
            .GetValue(cache)!;

        using var finished = new ManualResetEventSlim();
        var positive = false;
        var negative = true;

        var reader =
            new Thread(
                () =>
                {
                    positive = cache.MayContainExpression(expression);
                    negative = cache.MayContainExpression(Expression(2));
                    finished.Set();
                });

        bool completedWhileHeld;

        lock (gate)
        {
            reader.Start();
            completedWhileHeld = finished.Wait(TimeSpan.FromSeconds(5));
        }

        reader.Join(TimeSpan.FromSeconds(5)).Should().BeTrue();
        completedWhileHeld.Should().BeTrue();
        positive.Should().BeTrue();
        negative.Should().BeFalse();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Negatives_are_reused_until_completed_publication(bool generational)
    {
        var shared = CreateCache(generational);
        var counted = new CountedCache(shared);
        var buffer = new BufferedInvocationCacheAccess(counted);
        var expression = Expression(1);

        for (var index = 0; index < 100; ++index)
            buffer.MayContainExpression(Expression(1)).Should().BeFalse();

        counted.Probes.Should().Be(1);
        shared.Merge(new Dictionary<EvalCacheEntryKey, PineValue> { [Key(expression)] = PineValue.EmptyList });
        buffer.MayContainExpression(expression).Should().BeTrue();
        counted.Probes.Should().Be(2);
        buffer.TryGet(Key(expression), out var result).Should().BeTrue();
        result.Should().Be(PineValue.EmptyList);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Only_new_shared_membership_advances_the_version(bool generational)
    {
        var shared = CreateCache(generational);
        var expression = Expression(1);
        var initial = shared.ExpressionMembershipVersion;
        shared.TryAdd(Key(expression), PineValue.EmptyList).Should().BeTrue();
        var published = shared.ExpressionMembershipVersion;
        published.Should().NotBe(initial);
        shared.TryAdd(Key(expression), Expression(2)).Should().BeFalse();
        shared.TryAdd(Key(expression, 2), Expression(2)).Should().BeTrue();
        shared.ExpressionMembershipVersion.Should().Be(published);
        shared.TryGet(Key(expression), out var first).Should().BeTrue();
        first.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void A_miss_spanning_publication_is_not_retained_at_the_new_version()
    {
        var shared = new ConcurrentInvocationCache();
        var counted = new CountedCache(shared);
        var buffer = new BufferedInvocationCacheAccess(counted);
        var expression = Expression(1);
        counted.AfterProbe = () => shared.TryAdd(Key(expression), PineValue.EmptyList);

        buffer.MayContainExpression(expression).Should().BeFalse();
        counted.AfterProbe = null;
        buffer.MayContainExpression(expression).Should().BeTrue();
        counted.Probes.Should().Be(2);
    }

    [Theory]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(3)]
    public void Publication_between_version_reads_does_not_leave_a_stale_negative(int publicationRead)
    {
        var shared = new ConcurrentInvocationCache();
        var counted = new CountedCache(shared);
        var buffer = new BufferedInvocationCacheAccess(counted);
        var expression = Expression(1);

        counted.AfterVersionRead =
            read =>
            {
                if (read == publicationRead)
                    shared.TryAdd(Key(expression), PineValue.EmptyList);
            };

        buffer.MayContainExpression(expression).Should().Be(publicationRead == 1);

        if (publicationRead == 3)
            buffer.MayContainExpression(expression).Should().BeFalse();

        buffer.MayContainExpression(expression).Should().BeTrue();
    }

    [Fact]
    public void Local_publication_overrides_a_negative_and_completion_discards_the_memo()
    {
        var counted = new CountedCache(new ConcurrentInvocationCache());
        var buffer = new BufferedInvocationCacheAccess(counted);
        var expression = Expression(1);
        buffer.MayContainExpression(expression).Should().BeFalse();
        buffer.TryAdd(Key(expression), PineValue.EmptyList).Should().BeTrue();
        buffer.MayContainExpression(expression).Should().BeTrue();
        counted.Probes.Should().Be(1);
        buffer.MergeIntoShared();
        buffer.MayContainExpression(expression).Should().BeTrue();

        var absent = Expression(2);
        buffer.MayContainExpression(absent).Should().BeFalse();
        var probes = counted.Probes;
        buffer.MergeIntoShared();
        buffer.MayContainExpression(absent).Should().BeFalse();
        counted.Probes.Should().Be(probes + 1);
    }

    [Fact]
    public void Unsupported_adapters_keep_probing_and_observe_external_dictionary_additions()
    {
        var entries = new InvocationCache();
        var counted = new CountedCache(new InvocationCacheAccessFromDictionary(entries));
        var buffer = new BufferedInvocationCacheAccess(counted);
        var expression = Expression(1);
        buffer.MayContainExpression(expression).Should().BeFalse();
        buffer.MayContainExpression(expression).Should().BeFalse();
        counted.Probes.Should().Be(2);
        entries.Add(Key(expression), PineValue.EmptyList);
        buffer.MayContainExpression(expression).Should().BeTrue();
    }

    [Fact]
    public void Failed_completion_discards_negative_and_result_buffers()
    {
        var counted =
            new CountedCache(new ConcurrentInvocationCache())
            {
                MergeFailure = new InvalidOperationException("publication failed")
            };

        var buffer = new BufferedInvocationCacheAccess(counted);
        var expression = Expression(1);
        buffer.MayContainExpression(expression).Should().BeFalse();
        buffer.TryAdd(Key(Expression(2)), PineValue.EmptyList).Should().BeTrue();
        Action complete = buffer.MergeIntoShared;
        complete.Should().Throw<InvalidOperationException>().WithMessage("publication failed");
        buffer.BufferedEntryCount.Should().Be(0);
        buffer.MayContainExpression(expression).Should().BeFalse();
        counted.Probes.Should().Be(2);
    }

    [Fact]
    public void Negative_retention_is_bounded_without_changing_answers()
    {
        var counted = new CountedCache(new ConcurrentInvocationCache());
        var buffer = new BufferedInvocationCacheAccess(counted);

        for (var index = 0; index < 2048; ++index)
            buffer.MayContainExpression(Expression(index)).Should().BeFalse();

        for (var index = 0; index < 2048; ++index)
            buffer.MayContainExpression(Expression(index)).Should().BeFalse();

        counted.Probes.Should().Be(2048 + 1024);
    }

    [Fact]
    public async Task Generational_membership_tracks_promoted_copies_and_republication()
    {
        var shared = new GenerationalInvocationCache();
        var buffer = new BufferedInvocationCacheAccess(shared);
        var expression = Expression(1);
        shared.TryAdd(Key(expression), PineValue.EmptyList).Should().BeTrue();
        shared.TryAdd(Key(expression, 2), PineValue.EmptyList).Should().BeTrue();
        await shared.AdvanceGenerationAsync(2);
        shared.TryGet(Key(expression), out _).Should().BeTrue();
        await shared.AdvanceGenerationAsync(2);
        buffer.MayContainExpression(expression).Should().BeTrue();
        await shared.AdvanceGenerationAsync(2);
        buffer.MayContainExpression(expression).Should().BeFalse();
        shared.TryAdd(Key(expression), PineValue.EmptyList).Should().BeTrue();
        buffer.MayContainExpression(expression).Should().BeTrue();
    }

    [Fact]
    public void Immutable_policy_and_observer_preserve_publication_versions_and_logical_counts()
    {
        var shared = new ConcurrentInvocationCache();
        var expression = Expression(1);
        var counted = new CountedCache(new InvocationCachePolicy([expression]).Apply(shared));
        var statistics = new InvocationCacheStatistics();
        var sharedStatistics = new InvocationCacheStatistics();
        var buffer = new BufferedInvocationCacheAccess(sharedStatistics.Observe(counted));
        var observed = statistics.Observe(buffer);
        observed.MayContainExpression(expression).Should().BeFalse();
        observed.MayContainExpression(expression).Should().BeFalse();
        counted.Probes.Should().Be(1);
        shared.TryAdd(Key(expression), PineValue.EmptyList).Should().BeTrue();
        observed.MayContainExpression(expression).Should().BeTrue();
        counted.Probes.Should().Be(2);
        statistics.GetSnapshot().PerExpression[expression].ExpressionCheckCount.Should().Be(3);
        sharedStatistics.GetSnapshot().PerExpression[expression].ExpressionCheckCount.Should().Be(2);
    }

    [Fact]
    public void Equal_but_distinct_expression_values_share_a_negative()
    {
        var bytes = Guid.NewGuid().ToByteArray();
        var first = PineValue.Blob(bytes);
        var equal = PineValue.Blob([.. bytes]);
        equal.Should().NotBeSameAs(first);
        var counted = new CountedCache(new ConcurrentInvocationCache());
        var buffer = new BufferedInvocationCacheAccess(counted);
        buffer.MayContainExpression(first).Should().BeFalse();
        buffer.MayContainExpression(equal).Should().BeFalse();
        counted.Probes.Should().Be(1);
    }

    private static IInvocationCacheAccess CreateCache(bool generational) =>
        generational ? new GenerationalInvocationCache() : new ConcurrentInvocationCache();

    private static PineValue Expression(int index) => PineValue.Blob(BitConverter.GetBytes(index));

    private static EvalCacheEntryKey Key(PineValue expression, int input = 0) =>
        new(expression, StackFrameInput.GenericFromEnvironmentValue(Expression(input)));

    private sealed class CountedCache(IInvocationCacheAccess cache) : IInvocationCacheAccess
    {
        public int Probes { get; private set; }

        public Action? AfterProbe { get; set; }

        public Action<int>? AfterVersionRead { get; set; }

        public Exception? MergeFailure { get; set; }

        private int _versionReads;

        public long? ExpressionMembershipVersion
        {
            get
            {
                var version = cache.ExpressionMembershipVersion;
                AfterVersionRead?.Invoke(++_versionReads);
                return version;
            }
        }

        public bool MayContainExpression(PineValue expressionValue)
        {
            ++Probes;
            var result = cache.MayContainExpression(expressionValue);
            AfterProbe?.Invoke();
            return result;
        }

        public bool TryGet(EvalCacheEntryKey key, out PineValue value) => cache.TryGet(key, out value);

        public bool TryAdd(EvalCacheEntryKey key, PineValue value) => cache.TryAdd(key, value);

        public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries)
        {
            if (MergeFailure is { } failure)
                throw failure;

            cache.Merge(entries);
        }
    }
}
