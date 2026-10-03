using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class FileTreeEncodingTests
{
    [Fact]
    public void Encode_delegates_to_2026()
    {
        var tree =
            FileTree.SortedDirectory(
                [
                ("a", FileTree.File(new byte[] { 1 })),
                ("b", FileTree.SortedDirectory([("c", FileTree.File(new byte[] { 2 }))]))
                ]);

        var expected =
            PineValue.List(
                [
                StringEncoding.ValueFromString("a"),
                PineValue.Blob([1]),
                StringEncoding.ValueFromString("b"),
                PineValue.List([StringEncoding.ValueFromString("c"), PineValue.Blob([2])])
                ]);

        FileTreeEncoding.Encode(tree).Should().Be(expected);
        FileTreeEncoding.Encode(tree).Should().Be(FileTreeEncoding2026.Encode(tree));
        FileTreeEncoding.Encode(tree).Should().NotBe(FileTreeEncoding2025.Encode(tree));
    }

    [Theory]
    [InlineData(2025)]
    [InlineData(2026)]
    public void Parse_accepts_both_formats(int year)
    {
        var trees =
            new FileTree[]
            {
                FileTree.File(new byte[] { 0, 1, 255 }),
                FileTree.File(new byte[] { }),
                FileTree.EmptyTree,
                FileTree.SortedDirectory([("file", FileTree.File(new byte[] { 1 }))]),
                FileTree.SortedDirectory(
                    [
                    ("a", FileTree.File(new byte[] { })),
                    ("b",
                        FileTree.SortedDirectory(
                            [
                            ("c",
                                FileTree.SortedDirectory(
                                    [("d", FileTree.File(new byte[] { 2, 3 }))])),
                            ("empty", FileTree.EmptyTree)
                            ])),
                    ("\u00e4\U0001f600", FileTree.File(new byte[] { 4 }))
                    ])
            };

        foreach (var tree in trees)
        {
            var encoded =
                year is 2025
                ? FileTreeEncoding2025.Encode(tree)
                : FileTreeEncoding2026.Encode(tree);

            FileTreeEncoding.Parse(encoded)
                .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
                .Which.Value.Should().Be(tree);
        }
    }

    [Fact]
    public void Parse_does_not_mistake_2025_entry_pairs_for_list_based_names()
    {
        var encoded =
            PineValue.List(
                [
                PineValue.List([StringEncoding.ValueFromString("a"), PineValue.Blob([1])]),
                PineValue.List([StringEncoding.ValueFromString("b"), PineValue.Blob([2])])
                ]);

        var expected =
            FileTree.SortedDirectory(
                [
                ("a", FileTree.File(new byte[] { 1 })),
                ("b", FileTree.File(new byte[] { 2 }))
                ]);

        FileTreeEncoding.Parse(encoded)
            .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
            .Which.Value.Should().Be(expected);
    }

    [Fact]
    public void Parse_accepts_2025_with_legacy_list_based_names()
    {
        var encoded =
            PineValue.List(
                [
                PineValue.List(
                    [
                    StringEncoding.ValueFromString_2024("directory"),
                    PineValue.List(
                        [
                        PineValue.List(
                            [
                            StringEncoding.ValueFromString_2024("file"),
                            PineValue.Blob([1, 2, 3])
                            ])
                        ])
                    ])
                ]);

        var expected =
            FileTree.SortedDirectory(
                [
                ("directory",
                    FileTree.SortedDirectory(
                        [("file", FileTree.File(new byte[] { 1, 2, 3 }))]))
                ]);

        FileTreeEncoding.Parse(encoded)
            .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
            .Which.Value.Should().Be(expected);
    }

    [Fact]
    public void Reencoding_2025_uses_2026()
    {
        var tree =
            FileTree.SortedDirectory(
                [("directory", FileTree.SortedDirectory([("file", FileTree.File(new byte[] { 1 }))]))]);

        var decoded =
            FileTreeEncoding.Parse(FileTreeEncoding2025.Encode(tree))
                .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
                .Which.Value;

        FileTreeEncoding.Encode(decoded).Should().Be(FileTreeEncoding2026.Encode(tree));
        FileTreeEncoding.Encode(decoded).Should().NotBe(FileTreeEncoding2025.Encode(tree));
    }

    [Fact]
    public void Parse_rejects_invalid_values_in_both_formats()
    {
        var name = StringEncoding.ValueFromString("file");
        var content = PineValue.Blob([1, 2, 3]);

        var invalidValues =
            new PineValue[]
            {
                PineValue.List([name]),
                PineValue.List([name, content, name]),
                PineValue.List([PineValue.Blob([1]), content]),
                PineValue.List([name, PineValue.List([name])]),
                PineValue.List([PineValue.List([name])]),
                PineValue.List([PineValue.List([name, content, content])]),
                PineValue.List([PineValue.List([PineValue.Blob([1]), content])]),
                PineValue.List([PineValue.List([name, PineValue.List([name])])])
            };

        foreach (var encoded in invalidValues)
        {
            FileTreeEncoding.Parse(encoded)
                .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Err>();
        }
    }

    [Fact]
    public void Hashing_uses_2026_and_preserves_file_hashes()
    {
        var file = FileTree.File(new byte[] { 1, 2, 3 });
        var directory = FileTree.SortedDirectory([("file", file)]);

        PineValueHashTree.ComputeHashNotSorted(file).ToArray().Should().Equal(
            PineValueHashTree.ComputeHash(FileTreeEncoding2025.Encode(file)).ToArray());

        PineValueHashTree.ComputeHashNotSorted(directory).ToArray().Should().Equal(
            PineValueHashTree.ComputeHash(FileTreeEncoding2026.Encode(directory)).ToArray());

        PineValueHashTree.ComputeHashNotSorted(directory).ToArray().Should().NotEqual(
            PineValueHashTree.ComputeHash(FileTreeEncoding2025.Encode(directory)).ToArray());
    }
}
