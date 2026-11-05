using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.Testing;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestDiscoveryTagTests
{
    [Theory]
    [InlineData("ElmTestVariant__UnitTest")]
    [InlineData("ElmTestVariant__FuzzTest")]
    [InlineData("ElmTestVariant__Labeled")]
    [InlineData("ElmTestVariant__Batch")]
    [InlineData("ElmTestVariant__Skipped")]
    [InlineData("ElmTestVariant__Only")]
    [InlineData("PineTodo")]
    [InlineData("PineEmptyGroup")]
    [InlineData("PineFuzzCount")]
    [InlineData("ElmTestVariant__Future")]
    public void Discovery_tag_classifier_preserves_blob_and_legacy_test_tags(string tag)
    {
        var classifier = new ElmTestDiscovery.TestTagClassifier();

        classifier.Classify(Choice(StringEncoding.ValueFromString(tag))).Should().Be(tag);
        classifier.Classify(Choice(StringEncoding.ValueFromString_2024(tag))).Should().Be(tag);
    }

    [Theory]
    [InlineData("Just")]
    [InlineData("MagnitudeNotNormalised")]
    [InlineData("Other_\U0001F600")]
    [InlineData("ElmTestVariant_UnitTest")]
    [InlineData("PineTodoOther")]
    public void Discovery_tag_classifier_rejects_non_test_choices(string tag)
    {
        var classifier = new ElmTestDiscovery.TestTagClassifier();

        classifier.Classify(Choice(StringEncoding.ValueFromString(tag))).Should().BeNull();
        classifier.Classify(Choice(StringEncoding.ValueFromString_2024(tag))).Should().BeNull();
    }

    [Theory]
    [InlineData("ElmTestVariant__UnitTest", false)]
    [InlineData("ElmTestVariant__UnitTest", true)]
    [InlineData("ElmTestVariant__Future", false)]
    [InlineData("ElmTestVariant__Future", true)]
    [InlineData("MagnitudeNotNormalised", false)]
    [InlineData("MagnitudeNotNormalised", true)]
    public void Discovery_tag_classifier_does_not_allocate_for_repeated_tags(string tag, bool legacy)
    {
        var classifier = new ElmTestDiscovery.TestTagClassifier();
        PineValue Encode() => legacy ? StringEncoding.ValueFromString_2024(tag) : StringEncoding.ValueFromString(tag);
        PineValueInProcess[] values = [Choice(Encode()), Choice(Encode())];

        for (var index = 0; index < 100; index++)
            classifier.Classify(values[index % values.Length]);

        var before = GC.GetAllocatedBytesForCurrentThread();

        for (var index = 0; index < 10_000; index++)
            classifier.Classify(values[index % values.Length]);

        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        allocated.Should().BeLessThan(1024);
    }

    [Fact]
    public void Discovery_tag_classifier_preserves_malformed_tag_errors()
    {
        PineValue[] malformed =
            [
                PineValue.Blob([255]),
                PineValue.Blob([0, 0, 0xD8, 0]),
                PineValue.Blob([0, 0x11, 0, 0]),
                PineValue.List([PineValue.EmptyList])
            ];

        var classifier = new ElmTestDiscovery.TestTagClassifier();

        foreach (var tag in malformed)
        {
            Action classify = () => classifier.Classify(Choice(tag));
            classify.Should().Throw<InvalidOperationException>().WithMessage("Invalid choice tag: *");
            classify.Should().Throw<InvalidOperationException>().WithMessage("Invalid choice tag: *");
        }
    }

    [Fact]
    public void Discovery_tag_classifier_ignores_non_choices_and_does_not_force_payloads()
    {
        var classifier = new ElmTestDiscovery.TestTagClassifier();
        classifier.Classify(PineValueInProcess.EmptyList).Should().BeNull();
        classifier.Classify(PineValueInProcess.Create(PineValue.EmptyBlob)).Should().BeNull();

        classifier.Classify(
            PineValueInProcess.CreateList(
                [PineValueInProcess.Create(ElmValue.ElmChoiceTypeTagNameAsValue)])).Should().BeNull();

        classifier.Classify(
            PineValueInProcess.CreateList(
                [
                    PineValueInProcess.EmptyList,
                    PineValueInProcess.Create(PineValue.Blob([255]))
                ])).Should().BeNull();

        var payload = PineValueInProcess.CreateList([PineValueInProcess.EmptyList]);

        var value =
            PineValueInProcess.CreateList(
                [
                    PineValueInProcess.Create(ElmValue.ElmChoiceTypeTagNameAsValue),
                    PineValueInProcess.Create(StringEncoding.ValueFromString("ElmTestVariant__UnitTest")),
                    payload
                ]);

        classifier.Classify(value).Should().Be("ElmTestVariant__UnitTest");
        value.EvaluatedOrNull.Should().BeNull();
        payload.EvaluatedOrNull.Should().BeNull();
    }

    [Fact]
    public void Discovery_reports_unknown_test_variants_instead_of_silently_excluding_them()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                type Future = ElmTestVariant__Future
                suite = ElmTestVariant__Future
                """);

        Action discover = () => ElmTestDiscovery.DiscoverTests(project.Directory);

        discover.Should().Throw<InvalidOperationException>()
            .WithMessage("Unsupported Elm Test variant: ElmTestVariant__Future");
    }

    private static PineValueInProcess Choice(PineValue tag) =>
        PineValueInProcess.CreateList(
            [
                PineValueInProcess.Create(ElmValue.ElmChoiceTypeTagNameAsValue),
                PineValueInProcess.Create(tag),
                PineValueInProcess.EmptyList
            ]);
}
