using System.Linq;
using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Xunit;

namespace Pine.Core.Tests.DotNet;

public class StaticExpressionDeclarationOrderTests
{
    [Fact]
    public void Orders_by_subexpression_count_first()
    {
        var literal = CreateLiteral(0x01);
        var kernel = StaticExpression<DeclQualifiedName>.KernelApplicationInstance("head", literal);

        var ordered =
            new[] { kernel, literal }
            .OrderBy(static expr => expr, StaticExpressionDeclarationOrder.Instance)
            .ToArray();

        ordered.Should().Equal(literal, kernel);
    }

    [Fact]
    public void Orders_by_expression_kind_when_counts_match()
    {
        var environment = StaticExpression<DeclQualifiedName>.EnvironmentInstance;
        var literal = CreateLiteral(0x02);

        var ordered =
            new[] { literal, environment }
            .OrderBy(static expr => expr, StaticExpressionDeclarationOrder.Instance)
            .ToArray();

        ordered[0].Should().Be(environment);
        ordered[1].Should().Be(literal);
    }

    [Fact]
    public void Orders_lists_by_items_when_structure_matches()
    {
        var listWithSmallValue = StaticExpression<DeclQualifiedName>.ListInstance([CreateLiteral(0x01)]);
        var listWithLargeValue = StaticExpression<DeclQualifiedName>.ListInstance([CreateLiteral(0x02)]);

        var ordered =
            new[] { listWithLargeValue, listWithSmallValue }
            .OrderBy(static expr => expr, StaticExpressionDeclarationOrder.Instance)
            .ToArray();

        ordered.Should().Equal(listWithSmallValue, listWithLargeValue);
    }

    [Fact]
    public void Orders_function_applications_by_qualified_name()
    {
        var arguments = StaticExpression<DeclQualifiedName>.ListInstance([StaticExpression<DeclQualifiedName>.EnvironmentInstance]);

        var bar = StaticExpression<DeclQualifiedName>.FunctionApplicationInstance(
            new DeclQualifiedName(["Acme"], "Bar"),
            arguments);

        var foo = StaticExpression<DeclQualifiedName>.FunctionApplicationInstance(
            new DeclQualifiedName(["Acme"], "Foo"),
            arguments);

        var ordered =
            new[] { foo, bar }
            .OrderBy(static expr => expr, StaticExpressionDeclarationOrder.Instance)
            .ToArray();

        ordered.Should().Equal(bar, foo);
    }

    [Fact]
    public void Equal_expressions_compare_as_equal()
    {
        var first = StaticExpression<DeclQualifiedName>.ListInstance([
            CreateLiteral(0x05),
            StaticExpression<DeclQualifiedName>.EnvironmentInstance]);

        var second = StaticExpression<DeclQualifiedName>.ListInstance([
            CreateLiteral(0x05),
            StaticExpression<DeclQualifiedName>.EnvironmentInstance]);

        var comparison = StaticExpressionDeclarationOrder.Instance.Compare(first, second);

        comparison.Should().Be(0);
    }

    private static StaticExpression<DeclQualifiedName> CreateLiteral(byte value) =>
        StaticExpression<DeclQualifiedName>.LiteralInstance(PineValue.Blob([value]));
}
