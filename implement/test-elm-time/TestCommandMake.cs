using ElmTime;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace TestElmTime;

[TestClass]
public class TestCommandMake
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
                            "agu-z/elm-zip": "3.0.1",
                            "danfishgold/base64-bytes": "1.1.0",
                            "elm/browser": "1.0.2",
                            "elm/bytes": "1.0.8",
                            "elm/core": "1.0.5",
                            "elm/html": "1.0.0",
                            "elm/json": "1.1.3",
                            "elm/time": "1.0.0",
                            "elm/url": "1.0.0",
                            "mdgriffith/elm-ui": "1.1.8"
                        },
                        "indirect": {
                            "elm/parser": "1.1.0",
                            "elm/virtual-dom": "1.0.3",
                            "elm-community/list-extra": "8.7.0",
                            "folkertdev/elm-flate": "2.0.5",
                            "justinmimbs/date": "4.0.1",
                            "justinmimbs/time-extra": "1.1.1"
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
                    Bytes.Extra.fromByteValues [ 0, 1, 3, 4 ]

                """,
            },
        };

        var outputBlob = GetOutputFileContentForCommandMake(
            projectFiles: projectFiles.Select(file => ((IReadOnlyList<string>)file.path, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.content))).ToImmutableList(),
            entryPointFilePath: ImmutableList.Create("src", "Build.elm"));

        CollectionAssert.AreEqual(new byte[] { 0, 1, 3, 4 }, outputBlob.Span.ToArray());
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
        var makeResult = Program.Make(
            sourceFiles: projectFiles,
            workingDirectoryRelative: null,
            pathToFileWithElmEntryPoint: entryPointFilePath,
            outputFileName: "should-not-matter",
            elmMakeCommandAppendix: null);

        return makeResult.Extract(fromErr: err => throw new Exception("Failed make command:\n" + err)).producedFile;
    }
}
