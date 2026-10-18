using AwesomeAssertions;
using Pine.Core.Internal;
using System;
using System.Linq;
using System.Reflection;
using Xunit;

namespace Pine.Core.Tests.Internal;

public class CollectedTemplateEnvironmentsTests
{
    [Fact]
    public void Append_preserves_the_original_and_independent_branches()
    {
        var first = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(37)]);
        var prefix = CollectedTemplateEnvironments.Empty.Append(first);
        var left = prefix.Append(PineValueInProcess.EmptyBlob);
        var right = prefix.Append(PineValueInProcess.EmptyList);

        CollectedTemplateEnvironments.Empty.Count.Should().Be(0);
        prefix.Count.Should().Be(1);
        left.Count.Should().Be(2);
        right.Count.Should().Be(2);
        prefix[0].Should().BeSameAs(first);
        left[0].Should().BeSameAs(first);
        right[0].Should().BeSameAs(first);
        left[1].Should().BeSameAs(PineValueInProcess.EmptyBlob);
        right[1].Should().BeSameAs(PineValueInProcess.EmptyList);
        first.EvaluatedOrNull.Should().BeNull();
    }

    [Theory]
    [InlineData(0, -1)]
    [InlineData(0, 0)]
    [InlineData(1, -1)]
    [InlineData(1, 1)]
    [InlineData(2, 2)]
    [InlineData(2, int.MaxValue)]
    public void Indexer_rejects_indices_outside_the_history(int count, int index)
    {
        var environments = CollectedTemplateEnvironments.Empty;

        for (var item = 0; item < count; ++item)
            environments = environments.Append(PineValueInProcess.CreateInteger(item));

        Action access = () => _ = environments[index];
        access.Should().Throw<ArgumentOutOfRangeException>().WithParameterName("index");
    }

    [Fact]
    public void Indexed_observation_prepares_and_reuses_one_snapshot_without_copying_on_append()
    {
        var prefix =
            CollectedTemplateEnvironments.Empty
            .Append(PineValueInProcess.EmptyBlob)
            .Append(PineValueInProcess.EmptyList);

        var snapshotField =
            typeof(CollectedTemplateEnvironments).GetField("_indexed", BindingFlags.Instance | BindingFlags.NonPublic)!;

        snapshotField.GetValue(prefix).Should().BeNull();
        prefix[1].Should().BeSameAs(PineValueInProcess.EmptyList);
        snapshotField.GetValue(prefix).Should().BeNull();

        prefix[0].Should().BeSameAs(PineValueInProcess.EmptyBlob);
        var snapshot = snapshotField.GetValue(prefix);
        snapshot.Should().NotBeNull();

        prefix.ToArray().Should().Equal(
            new[] { PineValueInProcess.EmptyBlob, PineValueInProcess.EmptyList },
            (actual, expected) => ReferenceEquals(actual, expected));

        snapshotField.GetValue(prefix).Should().BeSameAs(snapshot);

        var extended = prefix.Append(PineValueInProcess.CreateInteger(37));
        snapshotField.GetValue(extended).Should().BeNull();
        snapshotField.GetValue(prefix).Should().BeSameAs(snapshot);
        extended[2].Should().BeSameAs(PineValueInProcess.CreateInteger(37));
        snapshotField.GetValue(extended).Should().BeNull();

        extended[0].Should().BeSameAs(PineValueInProcess.EmptyBlob);
        var extendedSnapshot = snapshotField.GetValue(extended);
        extendedSnapshot.Should().NotBeSameAs(snapshot);

        extended.ToArray().Should().Equal(
            new[] { PineValueInProcess.EmptyBlob, PineValueInProcess.EmptyList, PineValueInProcess.CreateInteger(37) },
            (actual, expected) => ReferenceEquals(actual, expected));

        snapshotField.GetValue(extended).Should().BeSameAs(extendedSnapshot);
        snapshotField.GetValue(prefix).Should().BeSameAs(snapshot);
    }
}
