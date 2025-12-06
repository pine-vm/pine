using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7.Avh4Format;

using Rendering = Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering;
using Avh4Format = Core.Elm.ElmSyntax.Stil4mElmSyntax7.Avh4Format;

public class FormatCompleteTests
{
    private static readonly Rendering.Config s_renderingDefaultConfig =
        Rendering.ConfigPreserveLocations();

    private static string RenderDefault(File file) =>
        Rendering.ToString(
            file,
            s_renderingDefaultConfig);

    private static string FormatString(
        string input)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));

        var formatted =
            Avh4Format.Format(parsed);

        var rendered =
            RenderDefault(formatted);

        return rendered;
    }

    [Fact]
    public void Simple_module()
    {
        var input =
            """"
            module   Test       exposing (..)
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)
            """"
            .Trim());
    }
}
