using AwesomeAssertions;
using Xunit;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized;

public class ExpressionVisitorTests
{
    /// <summary>
    /// A test visitor that returns the expression type name as a string.
    /// </summary>
    private class TypeNameVisitor : SyntaxTypes.ExpressionVisitorBase<object?, string>
    {
        public override string VisitInteger(SyntaxTypes.Expression.Integer expr, object? context) =>
            "Integer";

        public override string VisitLiteral(SyntaxTypes.Expression.Literal expr, object? context) =>
            "Literal";

        public override string VisitCharLiteral(SyntaxTypes.Expression.CharLiteral expr, object? context) =>
            "CharLiteral";

        public override string VisitFunctionOrValue(SyntaxTypes.Expression.FunctionOrValue expr, object? context) =>
            "FunctionOrValue";

        public override string VisitApplication(SyntaxTypes.Expression.Application expr, object? context) =>
            "Application";

        public override string VisitListExpr(SyntaxTypes.Expression.ListExpr expr, object? context) =>
            "ListExpr";

        public override string VisitOperatorApplication(SyntaxTypes.Expression.OperatorApplication expr, object? context) =>
            "OperatorApplication";

        public override string VisitParenthesizedExpression(SyntaxTypes.Expression.ParenthesizedExpression expr, object? context) =>
            "ParenthesizedExpression";

        public override string VisitNegation(SyntaxTypes.Expression.Negation expr, object? context) =>
            "Negation";

        public override string VisitIfBlock(SyntaxTypes.Expression.IfBlock expr, object? context) =>
            "IfBlock";

        public override string VisitCaseExpression(SyntaxTypes.Expression.CaseExpression expr, object? context) =>
            "CaseExpression";

        public override string VisitLetExpression(SyntaxTypes.Expression.LetExpression expr, object? context) =>
            "LetExpression";

        public override string VisitTupledExpression(SyntaxTypes.Expression.TupledExpression expr, object? context) =>
            "TupledExpression";

        public override string VisitRecordExpr(SyntaxTypes.Expression.RecordExpr expr, object? context) =>
            "RecordExpr";

        public override string VisitRecordAccess(SyntaxTypes.Expression.RecordAccess expr, object? context) =>
            "RecordAccess";

        public override string VisitRecordAccessFunction(SyntaxTypes.Expression.RecordAccessFunction expr, object? context) =>
            "RecordAccessFunction";

        public override string VisitRecordUpdateExpression(SyntaxTypes.Expression.RecordUpdateExpression expr, object? context) =>
            "RecordUpdateExpression";

        public override string VisitLambdaExpression(SyntaxTypes.Expression.LambdaExpression expr, object? context) =>
            "LambdaExpression";

        public override string VisitHex(SyntaxTypes.Expression.Hex expr, object? context) =>
            "Hex";

        public override string VisitPrefixOperator(SyntaxTypes.Expression.PrefixOperator expr, object? context) =>
            "PrefixOperator";

        public override string VisitFloatable(SyntaxTypes.Expression.Floatable expr, object? context) =>
            "Floatable";

        public override string VisitUnitExpr(SyntaxTypes.Expression.UnitExpr expr, object? context) =>
            "UnitExpr";
    }

    private readonly TypeNameVisitor _visitor = new();

    [Fact]
    public void Visit_Integer_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.Integer(42);

        var result = _visitor.Visit(expr, null);

        result.Should().Be("Integer");
    }

    [Fact]
    public void Visit_Literal_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.Literal("hello");

        var result = _visitor.Visit(expr, null);

        result.Should().Be("Literal");
    }

    [Fact]
    public void Visit_CharLiteral_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.CharLiteral('a');

        var result = _visitor.Visit(expr, null);

        result.Should().Be("CharLiteral");
    }

    [Fact]
    public void Visit_FunctionOrValue_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.FunctionOrValue(["List"], "map");

        var result = _visitor.Visit(expr, null);

        result.Should().Be("FunctionOrValue");
    }

    [Fact]
    public void Visit_Application_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var expr = new SyntaxTypes.Expression.Application([
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.FunctionOrValue([], "f")),
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1))
        ]);

        var result = _visitor.Visit(expr, null);

        result.Should().Be("Application");
    }

    [Fact]
    public void Visit_ListExpr_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.ListExpr(
            new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Expression>>.Empty());

        var result = _visitor.Visit(expr, null);

        result.Should().Be("ListExpr");
    }

    [Fact]
    public void Visit_OperatorApplication_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var opRange = new Range(new Location(1, 3), new Location(1, 4));
        var expr = new SyntaxTypes.Expression.OperatorApplication(
            new Node<string>(opRange, "+"),
            InfixDirection.Left,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2)));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("OperatorApplication");
    }

    [Fact]
    public void Visit_ParenthesizedExpression_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var expr = new SyntaxTypes.Expression.ParenthesizedExpression(
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42)));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("ParenthesizedExpression");
    }

    [Fact]
    public void Visit_Negation_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var expr = new SyntaxTypes.Expression.Negation(
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42)));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("Negation");
    }

    [Fact]
    public void Visit_IfBlock_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var ifLoc = new Location(1, 1);
        var thenLoc = new Location(1, 10);
        var elseLoc = new Location(1, 20);
        var expr = new SyntaxTypes.Expression.IfBlock(
            ifLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.FunctionOrValue([], "True")),
            thenLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            elseLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(0)));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("IfBlock");
    }

    [Fact]
    public void Visit_CaseExpression_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var caseLoc = new Location(1, 1);
        var ofLoc = new Location(1, 5);
        var arrowLoc = new Location(1, 10);
        var caseBlock = new SyntaxTypes.CaseBlock(
            caseLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            ofLoc,
            [new SyntaxTypes.Case(
                new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.AllPattern()),
                arrowLoc,
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(0)))]);
        var expr = new SyntaxTypes.Expression.CaseExpression(caseBlock);

        var result = _visitor.Visit(expr, null);

        result.Should().Be("CaseExpression");
    }

    [Fact]
    public void Visit_LetExpression_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var letLoc = new Location(1, 1);
        var inLoc = new Location(1, 10);
        var equalsLoc = new Location(1, 5);
        var letBlock = new SyntaxTypes.Expression.LetBlock(
            letLoc,
            [new Node<SyntaxTypes.Expression.LetDeclaration>(range,
                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x")),
                    equalsLoc,
                    new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1))))],
            inLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.FunctionOrValue([], "x")));
        var expr = new SyntaxTypes.Expression.LetExpression(letBlock);

        var result = _visitor.Visit(expr, null);

        result.Should().Be("LetExpression");
    }

    [Fact]
    public void Visit_TupledExpression_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var commaLoc = new Location(1, 5);
        var expr = new SyntaxTypes.Expression.TupledExpression(
            new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Expression>>.NonEmpty(
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
                [(commaLoc, new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2)))]));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("TupledExpression");
    }

    [Fact]
    public void Visit_RecordExpr_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var equalsLoc = new Location(1, 5);
        var fieldName = new Node<string>(range, "name");
        var valueExpr = new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Literal("test"));
        var field = new SyntaxTypes.RecordExprField(fieldName, equalsLoc, valueExpr);
        var expr = new SyntaxTypes.Expression.RecordExpr(
            new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.NonEmpty(
                field, []));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("RecordExpr");
    }

    [Fact]
    public void Visit_RecordAccess_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var expr = new SyntaxTypes.Expression.RecordAccess(
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.FunctionOrValue([], "record")),
            new Node<string>(range, "field"));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("RecordAccess");
    }

    [Fact]
    public void Visit_RecordAccessFunction_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.RecordAccessFunction(".field");

        var result = _visitor.Visit(expr, null);

        result.Should().Be("RecordAccessFunction");
    }

    [Fact]
    public void Visit_RecordUpdateExpression_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 20));
        var pipeLoc = new Location(1, 10);
        var equalsLoc = new Location(1, 12);
        var fieldName = new Node<string>(range, "name");
        var valueExpr = new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Literal("test"));
        var field = new SyntaxTypes.RecordExprField(fieldName, equalsLoc, valueExpr);
        var expr = new SyntaxTypes.Expression.RecordUpdateExpression(
            new Node<string>(range, "record"),
            pipeLoc,
            new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.NonEmpty(
                field, []));

        var result = _visitor.Visit(expr, null);

        result.Should().Be("RecordUpdateExpression");
    }

    [Fact]
    public void Visit_LambdaExpression_dispatches_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var backslashLoc = new Location(1, 1);
        var arrowLoc = new Location(1, 5);
        var lambda = new SyntaxTypes.LambdaStruct(
            backslashLoc,
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            arrowLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.FunctionOrValue([], "x")));
        var expr = new SyntaxTypes.Expression.LambdaExpression(lambda);

        var result = _visitor.Visit(expr, null);

        result.Should().Be("LambdaExpression");
    }

    [Fact]
    public void Visit_Hex_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.Hex(0xFF);

        var result = _visitor.Visit(expr, null);

        result.Should().Be("Hex");
    }

    [Fact]
    public void Visit_PrefixOperator_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.PrefixOperator("+");

        var result = _visitor.Visit(expr, null);

        result.Should().Be("PrefixOperator");
    }

    [Fact]
    public void Visit_Floatable_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.Floatable("3.14");

        var result = _visitor.Visit(expr, null);

        result.Should().Be("Floatable");
    }

    [Fact]
    public void Visit_UnitExpr_dispatches_correctly()
    {
        var expr = new SyntaxTypes.Expression.UnitExpr();

        var result = _visitor.Visit(expr, null);

        result.Should().Be("UnitExpr");
    }

    [Fact]
    public void Unhandled_expression_throws_NotImplementedException()
    {
        // Create a visitor that doesn't override any methods
        var minimalVisitor = new MinimalVisitor();
        var expr = new SyntaxTypes.Expression.Integer(42);

        var action = () => minimalVisitor.Visit(expr, null);

        action.Should().Throw<System.NotImplementedException>()
            .Which.Message.Should().Contain("Integer");
    }

    private class MinimalVisitor : SyntaxTypes.ExpressionVisitorBase<object?, string>
    {
        // Does not override any visit methods - all will throw
    }

    [Fact]
    public void Context_is_passed_to_visitor_methods()
    {
        var contextCapturingVisitor = new ContextCapturingVisitor();
        var expr = new SyntaxTypes.Expression.Integer(42);
        var context = "test-context";

        contextCapturingVisitor.Visit(expr, context);

        contextCapturingVisitor.CapturedContext.Should().Be("test-context");
    }

    private class ContextCapturingVisitor : SyntaxTypes.ExpressionVisitorBase<string?, string>
    {
        public string? CapturedContext { get; private set; }

        public override string VisitInteger(SyntaxTypes.Expression.Integer expr, string? context)
        {
            CapturedContext = context;
            return "Integer";
        }
    }
}
