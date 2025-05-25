using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Elm;

namespace TestElmTime;

[TestClass]
public class AVH4ElmFormatBinariesTests
{
    [TestMethod]
    public void Format_elm_module_text()
    {
        var elmModuleTextBeforeFormatting = @"
module Common exposing (..)

a =
    let
        b = 1
        c =
            2
    in
    b   +      c
";

        var expectedElmModuleTextAfterFormatting = @"
module Common exposing (..)


a =
    let
        b =
            1

        c =
            2
    in
    b + c
";


        var formatted =
            AVH4ElmFormatBinaries.RunElmFormat(elmModuleTextBeforeFormatting);

        formatted.Trim().Should().Be(expectedElmModuleTextAfterFormatting.Trim());
    }

    [TestMethod]
    public void Format_elm_module_text_containing_unicode()
    {
        var elmModuleTextBeforeFormatting =
            """
            module Common exposing (..)

            alfa : String
            alfa =
                "🌲"
            """;

        var expectedElmModuleTextAfterFormatting =
            """
            module Common exposing (..)


            alfa : String
            alfa =
                "🌲"
            """;

        var formatted =
            AVH4ElmFormatBinaries.RunElmFormat(elmModuleTextBeforeFormatting);

        var formattedTrimmed =
            formatted.Trim();

        formattedTrimmed.Should().Be(expectedElmModuleTextAfterFormatting.Trim());
    }
}
