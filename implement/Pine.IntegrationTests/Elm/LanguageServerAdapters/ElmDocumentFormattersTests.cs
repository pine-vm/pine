using AwesomeAssertions;
using Pine.Core;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm.LanguageServerAdapters;
using System;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.Elm.LanguageServerAdapters;

public class ElmDocumentFormattersTests
{
    [Fact]
    public async Task In_process_formatter_formats_Elm_and_reports_syntax_errors()
    {
        var formatter = new InProcessElmDocumentFormatter();

        var validResult =
            await formatter.FormatAsync(
                "file:///workspace/Main.elm",
                "module Main exposing (..)\n\n\nvalue=1\n",
                new FormattingOptions(),
                CancellationToken.None);

        validResult.Should().BeOfType<Result<DocumentFormattingError, string>.Ok>()
            .Which.Value.Should().Contain("value =\n    1");

        var invalidResult =
            await formatter.FormatAsync(
                "file:///workspace/Main.elm",
                "not an elm module",
                new FormattingOptions(),
                CancellationToken.None);

        invalidResult.Should().BeOfType<Result<DocumentFormattingError, string>.Err>()
            .Which.Value.Kind.Should().Be(DocumentFormattingErrorKind.SyntaxError);
    }

    [Fact]
    public async Task Formatters_reject_non_Elm_documents()
    {
        IDocumentFormatter[] formatters =
            [
                new InProcessElmDocumentFormatter(),
                new Avh4ElmDocumentFormatter((text, _) => ValueTask.FromResult(text)),
            ];

        foreach (var formatter in formatters)
        {
            var result =
                await formatter.FormatAsync(
                    "file:///workspace/notes.txt",
                    "notes",
                    new FormattingOptions(),
                    CancellationToken.None);

            result.Should().BeOfType<Result<DocumentFormattingError, string>.Err>()
                .Which.Value.Kind.Should().Be(DocumentFormattingErrorKind.UnsupportedDocument);
        }
    }

    [Fact]
    public async Task Avh4_formatter_delegates_and_converts_tool_failure()
    {
        var receivedText = "";

        var formatter =
            new Avh4ElmDocumentFormatter(
                (sourceText, cancellationToken) =>
                {
                    cancellationToken.IsCancellationRequested.Should().BeFalse();
                    receivedText = sourceText;
                    return ValueTask.FromResult("formatted");
                });

        var result =
            await formatter.FormatAsync(
                "file:///workspace/Main.elm",
                "before",
                new FormattingOptions(),
                CancellationToken.None);

        result.Should().BeOfType<Result<DocumentFormattingError, string>.Ok>()
            .Which.Value.Should().Be("formatted");

        receivedText.Should().Be("before");

        var failingFormatter =
            new Avh4ElmDocumentFormatter(
                (_, _) => throw new InvalidOperationException("process failed"));

        var failure =
            await failingFormatter.FormatAsync(
                "file:///workspace/Main.elm",
                "before",
                new FormattingOptions(),
                CancellationToken.None);

        failure.Should().BeOfType<Result<DocumentFormattingError, string>.Err>()
            .Which.Value.Kind.Should().Be(DocumentFormattingErrorKind.ProviderFailure);
    }

    [Fact]
    public async Task Avh4_formatter_propagates_cancellation()
    {
        var invoked = false;

        var formatter =
            new Avh4ElmDocumentFormatter(
                (sourceText, cancellationToken) =>
                {
                    invoked = true;
                    return ValueTask.FromResult(sourceText);
                });

        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        Func<Task> act =
            async () =>
            await formatter.FormatAsync(
                "file:///workspace/Main.elm",
                "before",
                new FormattingOptions(),
                cancellation.Token);

        await act.Should().ThrowAsync<OperationCanceledException>();
        invoked.Should().BeFalse();
    }
}
