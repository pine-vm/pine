using System;
using System.Collections.Immutable;
using System.Linq;
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
                "https://github.com/pine-vm/pine/blob/30c482748f531899aac2b2d4895e5f0e52258be7/")
            .Extract(error => throw new Exception("Failed to load from GitHub: " + error));

        var loadedFilesNamesAndContents =
            loadFromGithubResult.tree.EnumerateFilesTransitive()
                .Select(blobPathAndContent => (
                    fileName: string.Join("/", blobPathAndContent.path),
                    fileContent: blobPathAndContent.fileContent))
                .ToImmutableArray();

        var loadedFilesNamesAndHashes =
            loadedFilesNamesAndContents
                .Select(fileNameAndContent =>
                    (fileNameAndContent.fileName,
                        fileHash:
                        Convert.ToHexStringLower(
                            System.Security.Cryptography.SHA256.HashData(fileNameAndContent.fileContent.Span))))
                .ToImmutableArray();

        foreach (var expectedFileNameAndHash in expectedFilesNamesAndHashes)
        {
            if (!loadedFilesNamesAndHashes.Contains(expectedFileNameAndHash))
            {
                throw new Exception(
                    "Collection of loaded files contains a file named '" +
                    expectedFileNameAndHash.fileName +
                    "' with hash " +
                    expectedFileNameAndHash.fileHash + ".");
            }
        }

        return 0;
    }

    public static int RunWebServerTest()
    {
        const int serverHttpPort = 17654;

        using var webHost =
            RunServer.BuildWebHostToRunServer(
                processStorePath: null,
                processStoreReadonlyPath: null,
                adminInterfaceUrls: null,
                adminPassword: null,
                publicAppUrls: ["http://localhost:" + serverHttpPort],
                deletePreviousProcess: false,
                copyProcess: null,
                deployApp: "https://github.com/pine-vm/pine/tree/fd7f2100bff563538d1447064decde8b2a3bd07b/implement/example-apps/minimal-backend-hello-world");

        webHost.StartAsync().Wait();

        using var httpClient = new System.Net.Http.HttpClient();

        var httpResponse = httpClient.GetAsync("http://localhost:" + serverHttpPort + "/").Result;

        httpResponse.EnsureSuccessStatusCode();

        webHost.StopAsync().Wait();

        return 0;
    }
}
