using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System;
using System.Collections.Generic;
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

    [Fact]
    public void Simple_declaration()
    {
        var input =
            """"
            module Test exposing (..)


            decl = 42
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                42
            """"
            .Trim());
    }

    [Fact]
    public void Trim_whitespace_in_empty_list()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                [
                ]
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                []
            """"
            .Trim());
    }

    [Fact]
    public void Stable_configurations()
    {
        /*
         * Set of module texts that are already formatted according to Avh4 style.
         * Therefore, formatting them again should yield the same text.
         * */

        IReadOnlyList<string> testCases =
            [
            """"
            module Test exposing (..)


            decl a b c =
                a b c

            """",

            """"
            module Test exposing (..)


            decl a b c =
                a
                    b
                    c
            """",

            """"
            module Test exposing (..)


            decl a b c =
                [ a, b, c ]

            """",

            """"
            module Test exposing (..)


            decl a b c =
                [ a
                , b
                , c
                ]
            """",

            """"
            module Test exposing (..)


            decl : Int -> Int -> Int -> List Int
            decl a b c =
                []

            """",

            """"
            module Test exposing (..)


            decl :
                Int
                -> Int
                -> Int
                -> List Int
            decl a b c =
                []
            """",
            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var input = testCases[i];

            var formatted = FormatString(input);

            formatted.Trim().Should().Be(input.Trim(), $"Test case index: {i}");
        }
    }
}
