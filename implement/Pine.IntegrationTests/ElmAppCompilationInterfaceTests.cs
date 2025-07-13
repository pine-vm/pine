using AwesomeAssertions;
using System.Collections.Immutable;
using System.Linq;
using System.Net;
using System;
using Pine.Core;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmAppCompilationInterfaceTests
{
    [Fact]
    public void Compilation_interface_with_file_paths_from_multiple_source_directories()
    {
        var defaultAppSourceFiles = TestSetup.GetElmAppFromSubdirectoryName("compilation-interface-multiple-source-dir");

        var demoFiles = new[]
        {
            new
            {
                path= ImmutableList.Create("demo-file.mp3"),
                content= (ReadOnlyMemory<byte>)Enumerable.Range(0, 10_000).SelectMany(elem => BitConverter.GetBytes((ushort)elem))
                    .Concat(System.Text.Encoding.UTF8.GetBytes("Default static file content from String\nAnother line"))
                    .Concat(Enumerable.Range(0, 100_000).SelectMany(elem => BitConverter.GetBytes((ushort)elem)))
                    .ToImmutableList()
                    .ToArray()
            },
            new
            {
                path= ImmutableList.Create("alpha", "beta","demo-file-gamma.text"),
                content= (ReadOnlyMemory<byte>)System.Text.Encoding.UTF8.GetBytes("Some file content")
            }
        };

        var webAppSourceFiles =
            demoFiles
            .Aggregate(
                seed: defaultAppSourceFiles,
                (prev, demoFile) =>
                {
                    var demoSourceFilePath = ImmutableList.Create("static-content").AddRange(demoFile.path);

                    return prev.SetItem(demoSourceFilePath, demoFile.content);
                });

        var webAppSource =
            PineValueComposition.FromTreeWithStringPath(PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(webAppSourceFiles));

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        foreach (var demoFile in demoFiles)
        {
            var httpResponse = publicAppClient.GetAsync(string.Join("/", demoFile.path)).Result;

            httpResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var responseContent =
                httpResponse.Content.ReadAsByteArrayAsync().Result;

            var inspectResponseContent = System.Text.Encoding.UTF8.GetString(responseContent);

            responseContent.Should().Equal(demoFile.content.ToArray());
        }

        {
            var httpResponse = publicAppClient.GetAsync("readme-md").Result;

            httpResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var responseContent = httpResponse.Content.ReadAsStringAsync().Result;

            responseContent.Should().Be("A text file we will integrate using UTF8 encoding ⚓\nNewline and special chars:\"'");
        }

        {
            var httpResponse = publicAppClient.GetAsync("alpha-file-via-other-interface-module").Result;

            httpResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var responseContent = httpResponse.Content.ReadAsStringAsync().Result;

            responseContent.Should().Be("Text file content");
        }

        {
            var httpResponse = publicAppClient.GetAsync("file-via-other-source-dir-beta").Result;

            httpResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var responseContent = httpResponse.Content.ReadAsStringAsync().Result;

            responseContent.Should().Be("Another text file content");
        }
    }
}
