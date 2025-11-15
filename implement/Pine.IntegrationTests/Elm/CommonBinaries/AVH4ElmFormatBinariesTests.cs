using AwesomeAssertions;
using Pine.Elm.CommonBinaries;
using Xunit;

namespace Pine.IntegrationTests.Elm.CommonBinaries;

public class AVH4ElmFormatBinariesTests
{
    [Fact]
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

    [Fact]
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
