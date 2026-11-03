using AwesomeAssertions;
using Pine.Core.Elm.LanguageServer;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class WorkspaceDocumentUriTests
{
    [Theory]
    [InlineData("file:///k%3A/Source/Repos/elm-in-elm/", "file:///K:/Source/Repos/elm-in-elm/")]
    [InlineData("file:///k%3a/Source/with%20space%23%3F%2520/%F0%9F%8C%B2.elm", "file:///K:/Source/with%20space%23%3F%2520/%F0%9F%8C%B2.elm")]
    [InlineData("file:///k:/src/Tests.elm", "file:///K:/src/Tests.elm")]
    [InlineData("file:///k%3A", "file:///K:/")]
    [InlineData("file:///src/a%3Ab/Tests.elm", "file:///src/a%3Ab/Tests.elm")]
    [InlineData("file:///k%3Aname/Tests.elm", "file:///k%3Aname/Tests.elm")]
    [InlineData("file:///src/with%20space%23%3F%2520.elm", "file:///src/with%20space%23%3F%2520.elm")]
    [InlineData("file://server/share/k%3A/Tests.elm", "file://server/share/k%3A/Tests.elm")]
    [InlineData("memory://workspace/k%3A/Tests.elm", "memory://workspace/k%3A/Tests.elm")]
    public void Normalizing_workspace_URIs_changes_only_Windows_drive_letters(string input, string expected)
    {
        var normalized = WorkspaceDocumentUri.Normalize(new Uri(input));
        normalized.AbsoluteUri.Should().Be(new Uri(expected).AbsoluteUri);
        normalized.LocalPath.Should().Be(new Uri(expected).LocalPath);
        WorkspaceDocumentUri.Normalize(normalized).AbsoluteUri.Should().Be(normalized.AbsoluteUri);
    }
}
