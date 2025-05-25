using ElmTime;
using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace TestElmTime;

[TestClass]
public class ProgramCommandMakeTests
{
    [TestMethod]
    public void TestCommandMake_with_blobMain()
    {
        var projectFiles = new[]
        {
            new
            {
                path = ImmutableList.Create("elm.json"),
                content =
                """
                {
                    "type": "application",
                    "source-directories": [
                        "src"
                    ],
                    "elm-version": "0.19.1",
                    "dependencies": {
                        "direct": {
                            "TSFoster/elm-bytes-extra": "1.3.0",
                            "elm/bytes": "1.0.8",
                            "elm/core": "1.0.5",
                            "elm/json": "1.1.3"
                        },
                        "indirect": {
                        }
                    },
                    "test-dependencies": {
                        "direct": {},
                        "indirect": {}
                    }
                }
                """,
            },
            new
            {
                path = ImmutableList.Create("src","Build.elm"),
                content =
                """
                module Build exposing (..)

                import Bytes
                import Bytes.Extra


                blobMain : Bytes.Bytes
                blobMain =
                    Bytes.Extra.fromByteValues [ 0, 1, 3, 4, 71 ]

                """,
            },
        };

        var outputBlob =
            GetOutputFileContentForCommandMake(
                projectFiles:
                [.. projectFiles
                .Select(file => ((IReadOnlyList<string>)file.path, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.content)))],
                entryPointFilePath: ["src", "Build.elm"]);

        outputBlob.Span.ToArray().Should().Equal(new byte[] { 0, 1, 3, 4, 71 });
    }

    [TestMethod]
    public void TestCommandMake_with_blobMain_as_thunk()
    {
        var projectFiles = new[]
        {
            new
            {
                path = ImmutableList.Create("elm.json"),
                content =
                """
                {
                    "type": "application",
                    "source-directories": [
                        "src"
                    ],
                    "elm-version": "0.19.1",
                    "dependencies": {
                        "direct": {
                            "TSFoster/elm-bytes-extra": "1.3.0",
                            "elm/bytes": "1.0.8",
                            "elm/core": "1.0.5",
                            "elm/json": "1.1.3"
                        },
                        "indirect": {
                        }
                    },
                    "test-dependencies": {
                        "direct": {},
                        "indirect": {}
                    }
                }
                """,
            },
            new
            {
                path = ImmutableList.Create("src","Build.elm"),
                content =
                """
                module Build exposing (..)

                import Bytes
                import Bytes.Extra


                blobMain : () -> Bytes.Bytes
                blobMain () =
                    Bytes.Extra.fromByteValues [ 0, 13, 17, 31, 71 ]

                """,
            },
        };

        var outputBlob =
            GetOutputFileContentForCommandMake(
                projectFiles:
                [.. projectFiles
                .Select(file => ((IReadOnlyList<string>)file.path, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.content)))],
                entryPointFilePath: ["src", "Build.elm"]);

        outputBlob.Span.ToArray().Should().Equal(new byte[] { 0, 13, 17, 31, 71 });
    }

    private static ReadOnlyMemory<byte> GetOutputFileContentForCommandMake(
        IReadOnlyList<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> projectFiles,
        IReadOnlyList<string> entryPointFilePath) =>
        GetOutputFileContentForCommandMake(
            projectFiles.ToImmutableDictionary(
                entry => entry.path,
                entry => entry.content,
                keyComparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()),
            entryPointFilePath: entryPointFilePath);

    private static ReadOnlyMemory<byte> GetOutputFileContentForCommandMake(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> projectFiles,
        IReadOnlyList<string> entryPointFilePath)
    {
        var makeResult =
            Program.Make(
                sourceFiles: projectFiles,
                workingDirectoryRelative: null,
                pathToFileWithElmEntryPoint: entryPointFilePath,
                outputFileName: "should-not-matter",
                elmMakeCommandAppendix: null);

        if (makeResult.IsErrOrNull() is { } err)
        {
            throw new Exception("Failed make command:\n" + err);
        }

        if (makeResult.IsOkOrNull() is not { } makeOk)
        {
            throw new NotImplementedException(
                "MakeGuiHtml: Unexpected result type: " + makeResult.GetType());
        }

        if (makeOk.ProducedFiles is not TreeNodeWithStringPath.BlobNode blobNode)
        {
            throw new NotImplementedException(
                "MakeGuiHtml: Unexpected content in files produced by make: " + makeResult.GetType());
        }

        return blobNode.Bytes;
    }
}
