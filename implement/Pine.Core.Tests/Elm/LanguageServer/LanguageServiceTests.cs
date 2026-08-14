using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.IO;
using Xunit;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class LanguageServiceTests
{
    [Fact]
    public void Request_and_response_ABI_encode_and_decode()
    {
        var encodedRequest =
            RequestEncoding.Encode(
                new Request.DeleteWorkspaceFileRequest("file:///workspace/Main.elm"));

        var requestElmValue =
            ElmValueEncoding.PineValueAsElmValue(encodedRequest, null, null)
            .Should().BeOfType<Result<string, ElmValue>.Ok>()
            .Which.Value;

        requestElmValue.Should().Be(
            ElmValue.TagInstance(
                "DeleteWorkspaceFileRequest",
                [ElmValue.StringInstance("file:///workspace/Main.elm")]));

        var encodedResponse =
            ElmValueEncoding.TagAsPineValue("WorkspaceSummaryResponse", []);

        ResponseEncoding.Decode(encodedResponse)
            .Should().BeOfType<Result<string, Response>.Ok>()
            .Which.Value.Should().BeOfType<Response.WorkspaceSummaryResponse>();
    }

    [Fact]
    public void Compilation_uses_caller_supplied_cache_without_compiling()
    {
        var sourceTree = FileTree.EmptyTree;
        var cache = new FileStoreFromConcurrentDictionary();
        var cachedEnvironment = PineValue.List([PineValue.Blob([1, 2, 3])]);
        var cacheKey = LanguageServiceCompilation.CacheKeyFromSourceTree(sourceTree);

        using (var stream = new MemoryStream())
        {
            ValueBinaryEncodingClassic.Encode(stream, cachedEnvironment);
            cache.SetFileContent([cacheKey + ".bin"], stream.ToArray());
        }

        var logs = new List<string>();

        var result =
            LanguageServiceCompilation.CompileLanguageServiceEnv(
                sourceTree,
                cache,
                logs.Add);

        result.Should().BeOfType<Result<string, PineValue>.Ok>()
            .Which.Value.Should().Be(cachedEnvironment);

        logs.Should().ContainSingle(message => message.Contains("Loaded compiled", StringComparison.Ordinal));
    }

    [Fact]
    public void Compilation_without_cache_reports_missing_language_service_root()
    {
        LanguageServiceCompilation.CompileLanguageServiceEnv(
            FileTree.EmptyTree,
            cache: null)
            .Should().BeOfType<Result<string, PineValue>.Err>()
            .Which.Value.Should().Contain("LanguageService.elm");
    }
}
