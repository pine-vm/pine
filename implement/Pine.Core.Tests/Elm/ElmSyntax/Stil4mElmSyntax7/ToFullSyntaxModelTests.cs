using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Xunit;

using FullTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

public class ToFullSyntaxModelTests
{
    [Fact]
    public void Application_converts_with_function_separated_from_arguments()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var legacyApplication =
            new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Application(
                [
                new Node<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression>(
                    range,
                    new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.FunctionOrValue(["Basics"], "add")),
                new Node<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression>(
                    range,
                    new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Integer(1)),
                new Node<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression>(
                    range,
                    new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Integer(2))
                ]);

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel.Convert(legacyApplication);

        result.Should().BeOfType<FullTypes.Expression.Application>();

        var application = (FullTypes.Expression.Application)result;

        application.Function.Value.Should().Be(new FullTypes.Expression.FunctionOrValue(["Basics"], "add"));
        application.Arguments.Should().HaveCount(2);
        application.Arguments[0].Value.Should().Be(new FullTypes.Expression.Integer("1"));
        application.Arguments[1].Value.Should().Be(new FullTypes.Expression.Integer("2"));
    }

    [Fact]
    public void Empty_legacy_application_is_rejected()
    {
        var legacyApplication =
            new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Application([]);

        var act =
            () => Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel.Convert(legacyApplication);

        act.Should()
            .Throw<System.InvalidOperationException>()
            .WithMessage("*without a function expression*");
    }
}
