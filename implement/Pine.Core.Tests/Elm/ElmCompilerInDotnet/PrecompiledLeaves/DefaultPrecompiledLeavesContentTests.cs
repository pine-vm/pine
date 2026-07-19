using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Pins the content of <see cref="IntermediateVM.SetupVM.DefaultPrecompiledLeaves"/>,
/// the aggregate introduced by commit
/// <c>c947c7c53215b058ec8c4f6c3cd3a908bd5c6d57</c>.
/// <para>
/// This aggregate is what <see cref="Pine.IntermediateVM.SetupVM.Create"/> now uses as
/// its default precompiled-leaves dictionary. The subject commit made it a
/// <em>replacement</em> for the much larger bundled dictionary rather than an addition
/// to it, which is the root cause of the non-terminating integration tests (see
/// <c>explore/2026-05-30.integration-tests-never-terminating-precompiled-leaves-regression.md</c>).
/// </para>
/// <para>
/// These assertions document exactly which leaves the aggregate currently carries so that
/// any future change to its membership (in particular the recommended union with the
/// bundled leaves) is caught.
/// </para>
/// </summary>
public class DefaultPrecompiledLeavesContentTests
{
    [Fact]
    public void Default_aggregate_contains_the_kernel_and_base64_leaves()
    {
        var aggregate =
            IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

        aggregate.Count.Should().Be(
            26,
            because:
            "the Pine.Core aggregate contributes the Basics.compare, Basics.eq, Basics.idiv " +
            "and Basics.gcd leaves, six Dict kernel leaves, the Json.Decode.parseValue leaf, " +
            "eight String kernel leaves, the LanguageService comment-unwrapping leaf, " +
            "two Bytes kernel leaves, the two record runtime leaves " +
            "(record access and record update), " +
            "plus the two Base64 conversion leaves (Base64.Encode.toBytes and " +
            "Base64.Decode.fromBytes)");

        aggregate.Keys.Should().Contain(
            CoreBasicsPrecompiledLeaves.CompareLeafKey,
            because: "the aggregate must expose the Basics.compare leaf");

        aggregate.Keys.Should().Contain(
            [
            CoreBasicsPrecompiledLeaves.EqLeafKey,
            CoreBasicsPrecompiledLeaves.IdivLeafKey,
            CoreBasicsPrecompiledLeaves.GcdLeafKey,
            ],
            because: "the aggregate must expose the Basics.eq, Basics.idiv and Basics.gcd leaves");

        aggregate.Keys.Should().Contain(
            CoreDictPrecompiledLeaves.DictGetLeafKey,
            because: "the aggregate must expose the Dict.get leaf");

        aggregate.Keys.Should().Contain(
            [
            CoreDictPrecompiledLeaves.DictToListLeafKey,
            CoreDictPrecompiledLeaves.DictSizeLeafKey,
            CoreDictPrecompiledLeaves.DictKeysLeafKey,
            CoreDictPrecompiledLeaves.DictValuesLeafKey,
            CoreDictPrecompiledLeaves.DictInsertLeafKey,
            ],
            because: "the aggregate must expose all optimized Dict leaves");

        aggregate.Keys.Should().Contain(
            KernelJsonDecodePrecompiledLeaves.ParseValueLeafKey,
            because: "the aggregate must expose the Json.Decode.parseValue leaf");

        aggregate.Keys.Should().Contain(
            [
            CoreStringPrecompiledLeaves.ToListRecursiveLeafKey,
            CoreStringPrecompiledLeaves.SplitHelperOnBlobLeafKey,
            CoreStringPrecompiledLeaves.LinesHelperLeafKey,
            CoreStringPrecompiledLeaves.ToFloatLeafKey,
            CoreStringPrecompiledLeaves.ToIntLeafKey,
            CoreStringPrecompiledLeaves.FromIntLeafKey,
            CoreStringPrecompiledLeaves.TrimLeftCountBytesTrimmedLeafKey,
            CoreStringPrecompiledLeaves.TrimRightCountBytesRemainingLeafKey,
            ],
            because: "the aggregate must expose all optimized String leaves");

        aggregate.Keys.Should().Contain(
            LanguageServicePrecompiledLeaves.RemoveWrappingFromMultilineCommentLeafKey,
            because: "the aggregate must expose the LanguageService comment-unwrapping leaf");

        aggregate.Keys.Should().Contain(
            [
            KernelBytesPrecompiledLeaves.DecodeBlobAsCharsRecLeafKey,
            KernelBytesPrecompiledLeaves.EncodeCharsAsBlobHelpLeafKey,
            ],
            because: "the aggregate must expose both recursive Bytes conversion leaves");

        aggregate.Keys.Should().Contain(
            CoreRecordPrecompiledLeaves.RecordAccessLeafKey,
            because: "the aggregate must expose the record access leaf");

        aggregate.Keys.Should().Contain(
            CoreRecordPrecompiledLeaves.RecordUpdateLeafKey,
            because: "the aggregate must expose the record update leaf");

        aggregate.Keys.Should().Contain(
            IntermediateVM.SetupVM.Base64ConversionPrecompiledLeaves.Keys,
            because: "the aggregate must expose the Base64 conversion leaves");
    }

    [Fact]
    public void Default_aggregate_is_the_union_of_the_per_area_dictionaries()
    {
        var aggregate =
            IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

        var perAreaKeys =
            new HashSet<PineValue>(
                IntermediateVM.SetupVM.BasicsPrecompiledLeaves.Keys
                .Concat(IntermediateVM.SetupVM.DictPrecompiledLeaves.Keys)
                .Concat(IntermediateVM.SetupVM.JsonDecodePrecompiledLeaves.Keys)
                .Concat(IntermediateVM.SetupVM.StringPrecompiledLeaves.Keys)
                .Concat(IntermediateVM.SetupVM.LanguageServicePrecompiledLeaves.Keys)
                .Concat(IntermediateVM.SetupVM.BytesPrecompiledLeaves.Keys)
                .Concat(IntermediateVM.SetupVM.RecordAccessAndUpdatePrecompiledLeaves.Keys)
                .Concat(IntermediateVM.SetupVM.Base64ConversionPrecompiledLeaves.Keys));

        aggregate.Keys.Should().BeEquivalentTo(
            perAreaKeys,
            because:
            "the aggregate must be exactly the union of the per-area precompiled-leaf " +
            "dictionaries it advertises");
    }

    [Fact]
    public void Per_area_dictionaries_each_expose_their_leaves()
    {
        IntermediateVM.SetupVM.BasicsPrecompiledLeaves.Count
            .Should().Be(
            4,
            because:
            "the Basics area currently exposes four leaves: compare, eq, idiv and gcd");

        IntermediateVM.SetupVM.DictPrecompiledLeaves.Count
            .Should().Be(6, because: "the Dict area exposes six optimized operations");

        IntermediateVM.SetupVM.JsonDecodePrecompiledLeaves.Count
            .Should().Be(1, because: "the Json.Decode area exposes the parseValue leaf");

        IntermediateVM.SetupVM.StringPrecompiledLeaves.Count
            .Should().Be(
            8,
            because:
            "the String area exposes five optimized recursive helpers and three numeric conversions");

        IntermediateVM.SetupVM.LanguageServicePrecompiledLeaves.Count
            .Should().Be(
            1,
            because: "the LanguageService area exposes the comment-unwrapping leaf");

        IntermediateVM.SetupVM.BytesPrecompiledLeaves.Count
            .Should().Be(
            2,
            because:
            "the Bytes area exposes recursive UTF-8 decoding and encoding leaves");

        IntermediateVM.SetupVM.RecordAccessAndUpdatePrecompiledLeaves.Count
            .Should().Be(
            2,
            because:
            "the record runtime area exposes the record access and record update leaves");

        IntermediateVM.SetupVM.Base64ConversionPrecompiledLeaves.Count
            .Should().Be(
            2,
            because:
            "the Base64 conversion area exposes the Base64.Encode.toBytes and " +
            "Base64.Decode.fromBytes leaves");
    }
}
