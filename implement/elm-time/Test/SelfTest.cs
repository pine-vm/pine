using System;
using System.Collections.Immutable;
using System.Linq;
using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;

namespace ElmTime.Test;

/// <summary>
/// The <see cref="SelfTest"/> class contains tests that do not depend on any functionality requiring a build different from the one shipped for production.
/// The self-tests were introduced following the observation of frequent problems with the integration of native dependencies which were specific to production environments and not detected in the previously existing test setups(`dotnet test`)
/// </summary>
public class SelfTest
{
    public static int RunAllTestsAndPrintToConsole()
    {
        var tests =
            new[]
            {
                RunWebGitCloneTest,
                RunWebServerTest
            };

        var testsResults = tests.Select(test => test()).ToImmutableArray();

        return testsResults.Max();
    }

    public static int RunWebGitCloneTest()
    {
        var expectedFilesNamesAndHashes = new[]
        {
            (fileName: "README.md", fileHash: "e80817b2aa00350dff8f00207083b3b21b0726166dd695475be512ce86507238"),
        };

        var loadFromGithubResult =
            LoadFromGitHubOrGitLab.LoadFromUrl(
                    "https://github.com/elm-time/elm-time/blob/30c482748f531899aac2b2d4895e5f0e52258be7/")
                .Extract(error => throw new Exception("Failed to load from GitHub: " + error));

        var loadedFilesNamesAndContents =
            loadFromGithubResult.tree.EnumerateBlobsTransitive()
                .Select(blobPathAndContent => (
                    fileName: string.Join("/", blobPathAndContent.path),
                    fileContent: blobPathAndContent.blobContent))
                .ToImmutableArray();

        var loadedFilesNamesAndHashes =
            loadedFilesNamesAndContents
                .Select(fileNameAndContent =>
                    (fileNameAndContent.fileName,
                        fileHash: CommonConversion.StringBase16(
                            CommonConversion.HashSHA256(fileNameAndContent.fileContent)).ToLowerInvariant()))
                .ToImmutableArray();

        foreach (var expectedFileNameAndHash in expectedFilesNamesAndHashes)
        {
            Assert.IsTrue(
                loadedFilesNamesAndHashes.Contains(expectedFileNameAndHash),
                "Collection of loaded files contains a file named '" + expectedFileNameAndHash.fileName +
                "' with hash " + expectedFileNameAndHash.fileHash + ".");
        }

        return 0;
    }

    public static int RunWebServerTest()
    {
        const int serverHttpPort = 17654;

        using var webHost =
            RunServer.BuildWebHostToRunServer(
                processStorePath: null,
                adminInterfaceUrls: null,
                adminPassword: null,
                publicAppUrls: ["http://localhost:" + serverHttpPort],
                elmEngineType: new ElmEngineType.JavaScript_V8(),
                deletePreviousProcess: false,
                copyProcess: null,
                deployApp: "https://github.com/elm-time/elm-time/tree/1b8d4ff50a3ae94921f9e0f3357abf2bc1a696f3/implement/example-apps/minimal-backend-hello-world");

        webHost.StartAsync().Wait();

        using var httpClient = new System.Net.Http.HttpClient();

        var httpResponse = httpClient.GetAsync("http://localhost:" + serverHttpPort + "/").Result;

        httpResponse.EnsureSuccessStatusCode();

        webHost.StopAsync().Wait();

        return 0;
    }
}
