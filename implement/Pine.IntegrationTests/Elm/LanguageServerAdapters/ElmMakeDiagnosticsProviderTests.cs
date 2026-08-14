using AwesomeAssertions;
using Pine.Core;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm.LanguageServerAdapters;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.Elm.LanguageServerAdapters;

public class ElmMakeDiagnosticsProviderTests
{
    [Fact]
    public async Task Successful_invocation_returns_successful_empty_diagnostics()
    {
        var projectDirectory = Path.GetFullPath(Path.Combine(Path.GetTempPath(), "elm-project"));
        var entryPointPath = Path.Combine(projectDirectory, "src", "Main.elm");
        var entryPointUri = new Uri(entryPointPath).AbsoluteUri;
        var invocationCount = 0;

        var provider =
            new ElmMakeDiagnosticsProvider(
                _ => Path.Combine(projectDirectory, "elm.json"),
                (workingDirectory, invokedEntryPoint, _) =>
                {
                    invocationCount++;
                    workingDirectory.Should().Be(projectDirectory);
                    invokedEntryPoint.Should().Be(entryPointPath);

                    return ValueTask.FromResult(new ElmMakeInvocationResult(0, "", ""));
                });

        var result = await provider.GetDiagnosticsAsync(entryPointUri, CancellationToken.None);

        result.Should().BeOfType<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.Ok>()
            .Which.Value.Should().BeEmpty();

        invocationCount.Should().Be(1);
    }

    [Fact]
    public async Task Compile_errors_are_grouped_and_mapped_to_their_reported_document_uris()
    {
        var projectDirectory = Path.GetFullPath(Path.Combine(Path.GetTempPath(), "elm-project"));
        var entryPointPath = Path.Combine(projectDirectory, "src", "Main.elm");
        var dependencyPath = Path.GetFullPath(Path.Combine(projectDirectory, "..", "shared", "Shared.elm"));

        var report =
            JsonSerializer.Serialize(
                new
                {
                    type = "compile-errors",
                    errors =
                    new object[]
                    {
                        CompileError(Path.Combine("src", "Main.elm"), "FIRST"),
                        CompileError(entryPointPath, "SECOND"),
                        CompileError(dependencyPath, "DEPENDENCY"),
                    },
                });

        var provider =
            new ElmMakeDiagnosticsProvider(
                _ => Path.Combine(projectDirectory, "elm.json"),
                (_, _, _) =>
                ValueTask.FromResult(
                    new ElmMakeInvocationResult(
                        ExitCode: 1,
                        StandardOutput: "",
                        StandardError: report)));

        var result =
            await provider.GetDiagnosticsAsync(
                new Uri(entryPointPath).AbsoluteUri,
                CancellationToken.None);

        var diagnostics = result
            .Should().BeOfType<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.Ok>()
            .Which.Value;

        diagnostics.Should().HaveCount(2);

        var entryPointDiagnostics =
            diagnostics.Single(item => item.DocumentUri == new Uri(entryPointPath).AbsoluteUri);

        entryPointDiagnostics.Diagnostics.Select(diagnostic => diagnostic.Message)
            .Should().Equal("FIRST message", "SECOND message");

        diagnostics.Single(item => item.DocumentUri == new Uri(dependencyPath).AbsoluteUri)
            .Diagnostics.Should().ContainSingle()
            .Which.Message.Should().Be("DEPENDENCY message");

        entryPointDiagnostics.Diagnostics[0].Range.Start.Should().Be(new Position(1, 2));
        entryPointDiagnostics.Diagnostics[0].Source.Should().Be("elm make");
    }

    [Fact]
    public async Task Malformed_and_general_reports_are_provider_errors()
    {
        var projectDirectory = Path.GetFullPath(Path.Combine(Path.GetTempPath(), "elm-project"));
        var entryPointUri = new Uri(Path.Combine(projectDirectory, "Main.elm")).AbsoluteUri;

        async Task<DiagnosticsProviderError> Invoke(string standardError)
        {
            var provider =
                new ElmMakeDiagnosticsProvider(
                    _ => Path.Combine(projectDirectory, "elm.json"),
                    (_, _, _) =>
                    ValueTask.FromResult(
                        new ElmMakeInvocationResult(1, "", standardError)));

            var result = await provider.GetDiagnosticsAsync(entryPointUri, CancellationToken.None);

            return result
                .Should().BeOfType<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.Err>()
                .Which.Value;
        }

        (await Invoke("not json")).Kind.Should().Be(DiagnosticsProviderErrorKind.InvalidResponse);

        var generalReport =
            """
            {"type":"error","path":null,"title":"MISSING DEPENDENCY","message":["Install ",{"bold":true,"underline":false,"color":null,"string":"elm/json"}]}
            """;

        var generalError = await Invoke(generalReport);

        generalError.Kind.Should().Be(DiagnosticsProviderErrorKind.ProviderFailure);
        generalError.Message.Should().Contain("Install elm/json");
    }

    [Fact]
    public async Task Cancellation_does_not_invoke_elm_make()
    {
        var invoked = false;
        var provider =
            new ElmMakeDiagnosticsProvider(
                _ => "/workspace/elm.json",
                (_, _, _) =>
                {
                    invoked = true;
                    return ValueTask.FromResult(new ElmMakeInvocationResult(0, "", ""));
                });

        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        Func<Task> act =
            async () =>
            await provider.GetDiagnosticsAsync(
                new Uri(Path.GetFullPath("/workspace/Main.elm")).AbsoluteUri,
                cancellation.Token);

        await act.Should().ThrowAsync<OperationCanceledException>();
        invoked.Should().BeFalse();
    }

    private static object CompileError(string path, string title) =>
        new
        {
            path,
            name = "Main",
            problems =
            new[]
            {
                new
                {
                    title,
                    region =
                    new
                    {
                        start = new { line = 2, column = 3 },
                        end = new { line = 2, column = 5 },
                    },
                    message = new object[] { title, " message" },
                },
            },
        };
}
