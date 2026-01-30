using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax;

public class RenderingConfigTests
{
    [Fact]
    public void Supports_mapping_qualified_names()
    {
        var inputModuleText =
            """"
            module Test exposing (..)

            a : Basics.Int
            a =
                aa


            b : String.String
            b =
                bb


            c : Char.Char
            c =
                cc

            """";

        var expectedModuleText =
            """"
            module Test exposing (..)
            
            
            a : Int
            a =
                aa


            b : String
            b =
                bb


            c : Char
            c =
                cc


            """";

        var parsed =
            ElmSyntaxParser.ParseModuleText(inputModuleText)
            .Extract(err => throw new System.Exception("Parsing failed: " + err.ToString()));

        var namesMap =
            new Dictionary<QualifiedNameRef, QualifiedNameRef>
            {
                [QualifiedNameRef.FromFullName("Basics.Int")] = QualifiedNameRef.FromFullName("Int"),
                [QualifiedNameRef.FromFullName("String.String")] = QualifiedNameRef.FromFullName("String"),
                [QualifiedNameRef.FromFullName("Char.Char")] = QualifiedNameRef.FromFullName("Char"),
            };

        var mapped =
            NameMapper.MapNames(parsed, namesMap);

        var formatted =
            Core.Elm.ElmSyntax.Avh4Format.Format(mapped);

        var rendered =
            Rendering.ToString(formatted);

        rendered.Trim().Should().Be(expectedModuleText.Trim());
    }
}
