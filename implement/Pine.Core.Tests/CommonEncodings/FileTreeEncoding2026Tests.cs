using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class FileTreeEncoding2026Tests
{
    [Fact]
    public void Encode_uses_flat_directories_recursively()
    {
        var tree =
            FileTree.SortedDirectory(
                [
                ("a", FileTree.File(new byte[] { 0, 1, 2 })),
                ("b",
                    FileTree.SortedDirectory(
                        [
                        ("c", FileTree.File(new byte[] { 3, 4, 5, 6 })),
                        ("d", FileTree.File(new byte[] { 7, 8 }))
                        ]))
                ]);

        var expected =
            PineValue.List(
                [
                PineValue.Blob([0, 0, 0, 97]),
                PineValue.Blob([0, 1, 2]),
                PineValue.Blob([0, 0, 0, 98]),
                PineValue.List(
                    [
                    PineValue.Blob([0, 0, 0, 99]),
                    PineValue.Blob([3, 4, 5, 6]),
                    PineValue.Blob([0, 0, 0, 100]),
                    PineValue.Blob([7, 8])
                    ])
                ]);

        FileTreeEncoding2026.Encode(tree).Should().Be(expected);

        FileTreeEncoding2026.Parse(expected)
            .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
            .Which.Value.Should().Be(tree);
    }

    [Fact]
    public void Encode_uses_utf32_big_endian_names()
    {
        var tree =
            FileTree.SortedDirectory(
                [("A\u00e4\U0001f600", FileTree.File(new byte[] { 0, 255 }))]);

        var expected =
            PineValue.List(
                [
                PineValue.Blob([0, 0, 0, 65, 0, 0, 0, 228, 0, 1, 246, 0]),
                PineValue.Blob([0, 255])
                ]);

        FileTreeEncoding2026.Encode(tree).Should().Be(expected);

        FileTreeEncoding2026.Parse(expected)
            .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
            .Which.Value.Should().Be(tree);
    }

    [Fact]
    public void File_blobs_and_empty_directories_remain_distinct()
    {
        var testCases =
            new (FileTree tree, PineValue encoded)[]
            {
                (FileTree.File(new byte[] { 0, 1, 255 }), PineValue.Blob([0, 1, 255])),
                (FileTree.File(new byte[] { }), PineValue.EmptyBlob),
                (FileTree.EmptyTree, PineValue.EmptyList),
                (FileTree.SortedDirectory(
                    [
                    ("directory", FileTree.EmptyTree),
                    ("file", FileTree.File(new byte[] { }))
                    ]),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("directory"),
                    PineValue.EmptyList,
                    StringEncoding.ValueFromString("file"),
                    PineValue.EmptyBlob
                    ]))
            };

        foreach (var (tree, encoded) in testCases)
        {
            FileTreeEncoding2026.Encode(tree).Should().Be(encoded);

            FileTreeEncoding2026.Parse(encoded)
                .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
                .Which.Value.Should().Be(tree);
        }
    }

    [Fact]
    public void Encode_preserves_entry_order_and_parse_sorts_recursively()
    {
        var tree =
            FileTree.NonSortedDirectory(
                [
                ("z",
                    FileTree.NonSortedDirectory(
                        [
                        ("d", FileTree.File(new byte[] { 4 })),
                        ("c", FileTree.File(new byte[] { 3 }))
                        ])),
                ("a", FileTree.File(new byte[] { 1 }))
                ]);

        var expected =
            PineValue.List(
                [
                StringEncoding.ValueFromString("z"),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("d"),
                    PineValue.Blob([4]),
                    StringEncoding.ValueFromString("c"),
                    PineValue.Blob([3])
                    ]),
                StringEncoding.ValueFromString("a"),
                PineValue.Blob([1])
                ]);

        FileTreeEncoding2026.Encode(tree).Should().Be(expected);

        FileTreeEncoding2026.Parse(expected)
            .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Ok>()
            .Which.Value.Should().Be(FileTree.Sort(tree));
    }

    [Fact]
    public void Parse_rejects_malformed_directories()
    {
        var name = StringEncoding.ValueFromString("file");
        var content = PineValue.Blob([1, 2, 3]);

        var invalidValues =
            new PineValue[]
            {
                PineValue.List([name]),
                PineValue.List([name, content, name]),
                PineValue.List([PineValue.Blob([1]), content]),
                PineValue.List([PineValue.Blob([0, 17, 0, 0]), content]),
                PineValue.List([PineValue.Blob([0, 0, 216, 0]), content]),
                PineValue.List([PineValue.List([PineValue.EmptyList]), content]),
                PineValue.List([StringEncoding.ValueFromString_2024("file"), content]),
                PineValue.List([name, PineValue.List([name])]),
                PineValue.List([name, content, PineValue.Blob([1]), content]),
                PineValue.List([PineValue.List([name, content])]),
                PineValue.List(
                    [
                    PineValue.List([name, content]),
                    PineValue.List([name, content])
                    ]),
                PineValue.List(
                    [
                    PineValue.List([StringEncoding.ValueFromString("a"), PineValue.Blob([1])]),
                    PineValue.List([StringEncoding.ValueFromString("b"), PineValue.Blob([2])])
                    ])
            };

        foreach (var encoded in invalidValues)
        {
            FileTreeEncoding2026.Parse(encoded)
                .Should().BeOfType<Result<IReadOnlyList<(int index, string name)>, FileTree>.Err>();
        }
    }
}
