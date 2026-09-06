using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Core.PineVM;
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

    [Fact]
    public void Language_server_options_default_to_four_workers_and_require_positive_concurrency()
    {
        new LanguageServerOptions(serverVersion: "test")
            .MaxConcurrencyCount.Should().Be(4);

        var constructWithInvalidConcurrency =
            () =>
            new LanguageServerOptions(
                serverVersion: "test",
                maxConcurrencyCount: 0);

        constructWithInvalidConcurrency.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void Scheduled_session_creates_a_new_VM_for_each_request_attempt()
    {
        var vmCreationCount = 0;
        var sharedInvocationCache = new ConcurrentInvocationCache();

        ScheduledLanguageServiceSession.Worker CreateWorker() =>
            new(
                CreatePineVM:
                () =>
                {
                    vmCreationCount++;
                    return new WorkspaceSummaryResponsePineVM();
                },
                InvocationCache: new BufferedInvocationCacheAccess(sharedInvocationCache));

        var function =
            new FunctionRecord(
                InnerFunction: Expression.EnvironmentInstance,
                ParameterCount: 2,
                EnvFunctions: ReadOnlyMemory<PineValue>.Empty,
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty);

        var session =
            new ScheduledLanguageServiceSession(
                new LanguageServiceState.LanguageServiceProgram(
                    new LanguageServiceInterfaceStruct(function, function),
                    PineValue.EmptyList),
                maxConcurrencyCount: 1,
                firstWorker: CreateWorker(),
                createWorker: CreateWorker,
                logDelegate: null);

        session.DeleteFile("file:///workspace/First.elm")
            .Should().BeOfType<Result<string, Response.WorkspaceSummaryResponse>.Ok>();

        vmCreationCount.Should().Be(2);
    }

    private sealed class WorkspaceSummaryResponsePineVM : IPineVM
    {
        public Result<string, PineValue> EvaluateExpression(
            Expression expression,
            PineValue environment)
        {
            var response =
                ElmValueEncoding.TagAsPineValue(
                    "WorkspaceSummaryResponse",
                    []);

            var responseOk =
                ElmValueEncoding.TagAsPineValue(
                    "Ok",
                    [response]);

            return PineValue.List([responseOk, PineValue.EmptyBlob]);
        }
    }
}
