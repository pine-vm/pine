using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Optimization;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Optimization;

public class ReusedInstancesTests
{
    [Fact]
    public void Ensure_reference_equality_between_mappings_between_reused_instances()
    {
        ReusedInstances.Instance.AssertReferenceEquality();
    }

    [Fact]
    public void Fallback_without_bundled_declarations_matches_expression_only_build()
    {
        Expression[] expressionRoots =
            [
            Expression.ListInst([Expression.EnvironmentInstance])
            ];

        var expected =
            ReusedInstances.BuildPineListValueReusedInstances(
                expressionRoots,
                additionalRoots: []);

        var actual =
            ReusedInstances.LoadPrecompiledFromEmbeddedOrDefault(
                () => expressionRoots,
                bundledDeclarations: null);

        AssertPineValueListDictsAreEquivalent(
            actual.PineValueLists,
            expected.PineValueLists);

        actual.ValuesExpectedInCompilerLists.SetEquals(expected.ValuesExpectedInCompilerLists).Should().BeTrue();
        actual.ValuesExpectedInCompilerBlobs.SetEquals(expected.ValuesExpectedInCompilerBlobs).Should().BeTrue();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Elm_reachability_preserves_nested_components_and_duplicate_representatives(bool knownRootCount)
    {
        var integer = ElmValue.Integer(42);
        var character = ElmValue.CharInstance('x');
        var text = ElmValue.StringInstance("reachability");
        var opaque = new ElmValue.ElmInternal("reachability");
        var shared = new ElmValue.ElmTag("ReachabilityShared", [integer, character]);
        var record = new ElmValue.ElmRecord([("a", shared), ("z", text)]);
        var list = new ElmValue.ElmList([record, opaque, shared]);
        var nullary = new ElmValue.ElmTag("ReachabilityNullary", []);
        var firstRoot = new ElmValue.ElmTag("ReachabilityRoot", [list, nullary]);
        var lastRoot = new ElmValue.ElmTag(firstRoot.TagName, firstRoot.Arguments);
        ElmValue[] roots = [firstRoot, lastRoot, shared];
        var enumerationCount = 0;

        IEnumerable<ElmValue> EnumerateOnce()
        {
            enumerationCount++;

            if (enumerationCount > 1)
                throw new InvalidOperationException("Root enumeration repeated");

            foreach (var root in roots)
                yield return root;
        }

        var actual =
            ReusedInstances.CollectAllComponentsFromRoots(knownRootCount ? roots : EnumerateOnce());

        actual.SetEquals([lastRoot, shared, record, list, nullary, integer, character, text, opaque])
            .Should().BeTrue();

        actual.Single(value => value.Equals(firstRoot)).Should().BeSameAs(lastRoot);
        actual.Single(value => value.Equals(shared)).Should().BeSameAs(shared);
        enumerationCount.Should().Be(knownRootCount ? 0 : 1);
    }

    [Fact]
    public void Elm_reachability_hashes_each_popped_node_once()
    {
        var first = new HashCountingInternal("first");
        var second = new HashCountingInternal("second");
        ElmValue[] roots = [first, second, second];
        HashCountingInternal.HashCodeCalls = 0;

        var actual = ReusedInstances.CollectAllComponentsFromRoots(roots);

        HashCountingInternal.HashCodeCalls.Should().Be(roots.Length);
        actual.Count.Should().Be(2);
    }

    [Fact]
    public void Elm_reachability_grows_beyond_root_count_and_handles_empty_roots()
    {
        const int depth = 2_048;
        ElmValue root = new ElmValue.ElmInternal("leaf");

        for (var i = 0; i < depth; i++)
            root = new ElmValue.ElmTag("ReachabilityNested", [root]);

        var actual = ReusedInstances.CollectAllComponentsFromRoots([root]);

        actual.Count.Should().Be(depth + 1);
        actual.Single(value => ReferenceEquals(value, root)).Should().BeSameAs(root);
        ReusedInstances.CollectAllComponentsFromRoots([]).Should().BeEmpty();
    }

    [Fact]
    public void Elm_reachability_preserves_unsupported_variant_errors()
    {
        var collect =
            () =>
            ReusedInstances.CollectAllComponentsFromRoots([new ElmValue.ElmBytes(new byte[] { 1, 2, 3 })]);

        collect.Should().Throw<NotImplementedException>().WithMessage("*ElmBytes");
    }

    private sealed record HashCountingInternal(string Name) : ElmValue.ElmInternal(Name)
    {
        public static int HashCodeCalls { get; set; }

        public override int GetHashCode()
        {
            HashCodeCalls++;
            return base.GetHashCode();
        }
    }

    public static void AssertPineValueListDictsAreEquivalent(
        IReadOnlyDictionary<PineValue.ListValue.ListValueStruct, PineValue.ListValue> a,
        IReadOnlyDictionary<PineValue.ListValue.ListValueStruct, PineValue.ListValue> b)
    {
        a.Count.Should().Be(b.Count, "Counts should be equal");

        foreach (var kv in a)
        {
            b.Should().Contain(kv, "dictionary should contain the key-value pair");
        }
    }
}
