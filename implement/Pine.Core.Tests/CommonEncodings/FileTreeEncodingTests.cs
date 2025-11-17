using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class FileTreeEncodingTests
{
    [Fact]
    public void Composition_from_tree_with_string_path()
    {
        var testCases = new[]
        {
        new
        {
            input = FileTree.File(new byte[]{0,1,2}),
            expectedOutput = PineValue.Blob([0,1,2])
        },
        new
        {
            input = FileTree.SortedDirectory(
                [
                    (name: "ABC ä 😀",
                    component: FileTree.File(new byte[]{0,1,2,3}) ),
                ]),
            expectedOutput = (PineValue)PineValue.List(
                [
                    PineValue.List(
                        PineValue.Blob([0,0,0,65,0,0,0,66,0,0,0,67,0,0,0,32,0,0,0,228,0,0,0,32,0,1,246,0]),
                        PineValue.Blob([0,1,2,3])
                    )
                ])
            },
        };

        foreach (var testCase in testCases)
        {
            var asComposition = FileTreeEncoding.Encode(testCase.input);

            asComposition.Should().Be(testCase.expectedOutput);
        }
    }

    [Fact]
    public void Parse_as_tree_with_string_path()
    {
        var testCases = new[]
        {
        new
        {
            input =
            PineValue.Blob([0,1,2]),

            expectedOutput = Result<IReadOnlyList<(int index, string name)>, FileTree>.ok(
                FileTree.File(new byte[]{0,1,2}))
        },
        new
        {
            input =
            (PineValue)PineValue.List(
                PineValue.List(
                    PineValue.Blob([0,0,0,68,0,0,0,69,0,0,0,70,0,0,0,32,0,1,243,50]),
                    PineValue.Blob([0,1,2,3]))),
            expectedOutput = Result<IReadOnlyList<(int index, string name)>, FileTree>.ok(
                FileTree.SortedDirectory(
                    directoryContent:
                    [
                        (name: "DEF 🌲",
                        component: FileTree.File(new byte[]{0,1,2,3}) ),
                    ])
            )
        },
    };

        foreach (var testCase in testCases)
        {
            var parseResult = FileTreeEncoding.Parse(testCase.input);

            parseResult.Should().Be(testCase.expectedOutput);
        }
    }

    [Fact]
    public void Composition_from_file_tree()
    {
        var testCases = new[]
        {
        new
        {
            input = new []
            {
                (filePath: "a", fileContent: (ReadOnlyMemory<byte>)new byte[]{0,1,2}),
                (filePath: "b/c", fileContent: new byte[]{3,4,5,6}),
                (filePath: "b/d", fileContent: new byte[]{7,8}),
            },
            expectedOutput = PineValue.List(
                items:
                [
                    PineValue.List(
                        items:
                        [
                            PineValue.Blob([0,0,0,97]),
                            PineValue.Blob([0,1,2]),
                        ]),
                    PineValue.List(
                        PineValue.Blob([0,0,0,98]),
                        PineValue.List(
                            PineValue.List(
                                PineValue.Blob([0,0,0,99]),
                                PineValue.Blob([3,4,5,6])),

                            PineValue.List(
                                PineValue.Blob([0,0,0,100]),
                                PineValue.Blob([7,8]))
                            )
                    ),
                ])
        },
    };

        foreach (var testCase in testCases)
        {
            var asComposition =
                FileTreeEncoding.Encode(
                    FileTree.FromSetOfFilesWithCommonFilePath(testCase.input));

            asComposition.Should().Be(testCase.expectedOutput);
        }
    }
}
