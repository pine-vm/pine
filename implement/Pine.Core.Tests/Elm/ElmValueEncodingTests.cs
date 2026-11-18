using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Encodings;
using Pine.Core.Optimization;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm;

public class ElmValueEncodingTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Standard_encoding_cache_hits_allocate_nothing_and_do_not_report_new_encodings(
        bool additionalCache,
        bool observe)
    {
        var reused = ReusedInstances.Instance;
        var globalCache = reused.ElmValueEncoding!;
        var reusedTag = reused.ElmTagValues!.Values.First();
        var reusedList = reused.ElmValues!.OfType<ElmValue.ElmList>().First();

        ElmValue[] values =
            [
            new ElmValue.ElmInteger(42),
            new ElmValue.ElmTag(reusedTag.TagName, reusedTag.Arguments),
            new ElmValue.ElmList(reusedList.Items)
            ];

        var cache = additionalCache ? values.ToDictionary(value => value, _ => (PineValue)PineValue.EmptyList) : null;
        var reports = 0;
        Action<ElmValue, PineValue>? reporter = observe ? (_, _) => reports++ : null;

        foreach (var value in values)
        {
            var expected = additionalCache ? cache![value] : globalCache[value];

            for (var i = 0; i < 10_000; i++)
                ElmValueEncoding.ElmValueAsPineValue(value, cache, reporter);

            var before = GC.GetAllocatedBytesForCurrentThread();
            PineValue? actual = null;

            for (var i = 0; i < 1_000; i++)
                actual = ElmValueEncoding.ElmValueAsPineValue(value, cache, reporter);

            var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
            actual.Should().BeSameAs(expected);
            allocated.Should().Be(0);
        }

        reports.Should().Be(0);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Standard_tag_encoding_preserves_argument_order_nested_records_and_callback_memoization(bool memoize)
    {
        var cachedChild = ElmValue.Integer(42);
        var cachedEncoding = PineValue.Blob([17, 18, 19, 20, 21]);
        var shared = new ElmValue.ElmTag("EncoderRegressionShared", [ElmValue.Integer(7)]);
        var leaf = new ElmValue.ElmTag("EncoderRegressionLeaf", []);
        var record = new ElmValue.ElmRecord([("z", leaf), ("a", shared)]);
        var list = new ElmValue.ElmList([record, leaf]);
        var root = new ElmValue.ElmTag("EncoderRegressionRoot", [shared, cachedChild, shared, list]);
        var cache = new Dictionary<ElmValue, PineValue> { [cachedChild] = cachedEncoding };
        var reports = new List<(ElmValue value, PineValue encoding)>();

        var actual =
            ElmValueEncoding.ElmValueAsPineValue(
                root,
                cache,
                (value, encoding) =>
                {
                    reports.Add((value, encoding));

                    if (memoize)
                        cache.TryAdd(value, encoding);
                });

        var expectedShared = FlatTag(shared.TagName, IntegerEncoding.EncodeSignedInteger(7));
        var expectedLeaf = FlatTag(leaf.TagName);

        var expectedRecord =
            PineValue.List(
                [
                ElmValue.ElmRecordTypeTagNameAsValue,
                StringEncoding.ValueFromString("a"), expectedShared,
                StringEncoding.ValueFromString("z"), expectedLeaf
                ]);

        actual.Should().Be(
            FlatTag(
                root.TagName,
                expectedShared,
                cachedEncoding,
                expectedShared,
                PineValue.List([expectedRecord, expectedLeaf])));

        reports.Select(report => report.value).Should().Equal(
            memoize ? [shared, leaf, record, list, root] : [shared, shared, shared, leaf, record, leaf, list, root]);

        reports.Last().encoding.Should().BeSameAs(actual);

        if (memoize)
        {
            ElmValueEncoding.ElmValueAsPineValue(root, cache, (_, _) => throw new Exception("Unexpected cache miss"))
                .Should().BeSameAs(actual);
        }

        var encodedItems = actual.Should().BeOfType<PineValue.ListValue>().Which.Items.Span;
        encodedItems[3].Should().BeSameAs(cachedEncoding);

        if (memoize)
            encodedItems[2].Should().BeSameAs(encodedItems[4]);
    }

    [Theory]
    [InlineData("True", false)]
    [InlineData("False", false)]
    [InlineData("True", true)]
    [InlineData("False", true)]
    [InlineData("EncoderRegressionNullary", false)]
    public void Standard_tag_encoding_preserves_nullary_booleans_and_ordinary_tag_layout(string name, bool hasArgument)
    {
        var tag = new ElmValue.ElmTag(name, hasArgument ? [ElmValue.Integer(7)] : []);
        var actual = ElmValueEncoding.ElmValueAsPineValue(tag);

        var expected =
            !hasArgument && name is "True" or "False"
            ?
            PineValue.BlobSingleByte(name == "True" ? (byte)4 : (byte)2)
            :
            FlatTag(name, hasArgument ? [IntegerEncoding.EncodeSignedInteger(7)] : []);

        actual.Should().Be(expected);
    }

    [Fact]
    public void Flat_tag_builder_allocates_only_the_result_array_and_pine_list()
    {
        const string name = "Maybe";
        var choice = StringEncoding.ValueFromString(ElmValue.ElmChoiceTypeTagName);
        var tagName = StringEncoding.ValueFromString(name);
        PineValue[] arguments = [IntegerEncoding.EncodeSignedInteger(7), PineValue.EmptyList];

        for (var i = 0; i < 100; i++)
        {
            ElmValueEncoding.TagAsPineValue(name, arguments);
            PineValue.List([choice, tagName, arguments[0], arguments[1]]);
        }

        var before = GC.GetAllocatedBytesForCurrentThread();
        PineValue? expected = null;

        for (var i = 0; i < 1_000; i++)
            expected = PineValue.List([choice, tagName, arguments[0], arguments[1]]);

        var baseline = GC.GetAllocatedBytesForCurrentThread() - before;

        before = GC.GetAllocatedBytesForCurrentThread();
        PineValue? actual = null;

        for (var i = 0; i < 1_000; i++)
            actual = ElmValueEncoding.TagAsPineValue(name, arguments);

        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;

        actual.Should().Be(expected);
        allocated.Should().Be(baseline);
    }

    [Fact]
    public void Standard_tag_encoding_propagates_child_and_callback_errors_without_reporting_parent_success()
    {
        var child = new ElmValue.ElmTag("EncoderRegressionFirst", []);
        var unsupported = new ElmValue.ElmInternal("unsupported-encoding-regression");
        var root = new ElmValue.ElmTag("EncoderRegressionError", [child, unsupported]);
        var reports = new List<ElmValue>();
        var encode = () => ElmValueEncoding.ElmValueAsPineValue(root, null, (value, _) => reports.Add(value));

        encode.Should().Throw<NotImplementedException>().WithMessage("*ElmInternal");
        reports.Should().Equal(child);

        var failure = new InvalidOperationException("encoding observer failure");
        encode = () => ElmValueEncoding.ElmValueAsPineValue(root, null, (_, _) => throw failure);
        encode.Should().Throw<InvalidOperationException>().Which.Should().BeSameAs(failure);
    }

    private static PineValue FlatTag(string name, params PineValue[] arguments) =>
        PineValue.List(
            [
            StringEncoding.ValueFromString(ElmValue.ElmChoiceTypeTagName),
            StringEncoding.ValueFromString(name),
            .. arguments
            ]);
}
