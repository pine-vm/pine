using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax;

public class ElmFormatSyntaxErrorsTests
{
    [Fact]
    public void Reports_no_syntax_errors_for_valid_module()
    {
        var src =
            "module Test exposing (..)\n\n\nalfa =\n    \"a\"\n";

        var result = ElmFormat.FormatModuleTextReportingSyntaxErrors(src);

        result.IsErrOrNullable().Should().BeNull();

        var ok = result.IsOkOrNull();
        ok.Should().NotBeNull();
        ok!.SyntaxErrors.Should().BeEmpty();
    }

    [Fact]
    public void Reports_syntax_error_with_location_for_incomplete_declaration()
    {
        // 'beta = ?' is an incomplete declaration; the module is otherwise formattable.
        var src =
            "module Test_A exposing (..)\n\n\nalfa =\n    \"a\"\n\nbeta = ?\n\n\ngamma =\n    [ 1, 2, 3 ]\n";

        var result = ElmFormat.FormatModuleTextReportingSyntaxErrors(src);

        result.IsErrOrNullable().Should().BeNull();

        var ok = result.IsOkOrNull();
        ok.Should().NotBeNull();

        // Formatting still produced a rendering despite the incomplete declaration.
        ok!.FormattedText.Should().Contain("beta = ?");

        ok.SyntaxErrors.Should().HaveCount(1);

        var syntaxError = ok.SyntaxErrors[0];

        // The error is located on the 'beta' declaration line at the '?' token.
        syntaxError.Location.Row.Should().Be(7);
        syntaxError.Location.Column.Should().Be(8);
        syntaxError.Message.Should().NotBeNullOrWhiteSpace();
    }
}
