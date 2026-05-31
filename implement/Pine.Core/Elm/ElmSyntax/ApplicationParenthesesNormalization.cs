using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Linq;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Inserts the parentheses that the Elm grammar requires but that synthetic
/// expression trees (produced by compiler passes rather than by parsing real
/// source) frequently omit, and removes the redundant parenthesisation that
/// elm-format would normalise away in function-application position.
///
/// <para>
/// Two normalisations are applied, matching the behaviour of the avh4
/// <c>elm-format</c> binary:
/// </para>
/// <list type="number">
/// <item>
/// <b>Argument parenthesisation.</b> A function-application argument that is
/// itself a compound expression — another application, an operator
/// application, a negation, a lambda, or an <c>if</c>/<c>let</c>/<c>case</c>
/// block — must be wrapped in parentheses, otherwise the rendered text
/// reparses to a different (flattened) syntax tree. For example the tree
/// <c>head (skip xs)</c> rendered without parentheses becomes
/// <c>head skip xs</c>, which reparses as <c>head</c> applied to the two
/// arguments <c>skip</c> and <c>xs</c>.
/// </item>
/// <item>
/// <b>Function-position flattening.</b> A bare nested application in function
/// position, <c>(f a) b</c>, is flattened to the curried form <c>f a b</c>,
/// which is exactly the normalisation elm-format performs. Only <em>bare</em>
/// nested applications are flattened — an application already wrapped in an
/// explicit <see cref="ExpressionSyntax.ParenthesizedExpression"/> is left
/// untouched, so inputs that were parsed from real source (which carry their
/// parentheses explicitly) are never altered.
/// </item>
/// </list>
///
/// <para>
/// The pass only ever adds parentheses around, or flattens, expressions in
/// function-application position. It never removes parentheses that the input
/// already carries (other than by the function-position flattening above), so
/// it is a no-op on already well-parenthesised, parser-produced trees.
/// </para>
/// </summary>
public static class ApplicationParenthesesNormalization
{
    /// <summary>
    /// Normalises the given expression, applying the normalisation rules to any
    /// nested expressions within it.
    /// </summary>
    public static Node<ExpressionSyntax> NormalizeExpression(Node<ExpressionSyntax> exprNode)
    {
        var normalized =
            exprNode.Value switch
            {
                ExpressionSyntax.Application app =>
                NormalizeApplication(app),

                ExpressionSyntax.OperatorApplication opApp =>
                new ExpressionSyntax.OperatorApplication(
                    Operator: opApp.Operator,
                    Direction: opApp.Direction,
                    Left: NormalizeExpression(opApp.Left),
                    Right: NormalizeExpression(opApp.Right)),

                ExpressionSyntax.ListExpr listExpr =>
                new ExpressionSyntax.ListExpr(
                    Elements: MapSeparatedList(listExpr.Elements, NormalizeExpression)),

                ExpressionSyntax.TupledExpression tupled =>
                new ExpressionSyntax.TupledExpression(
                    Elements: MapSeparatedList(tupled.Elements, NormalizeExpression)),

                ExpressionSyntax.RecordExpr recordExpr =>
                new ExpressionSyntax.RecordExpr(
                    Fields: MapSeparatedList(recordExpr.Fields, NormalizeRecordField)),

                ExpressionSyntax.RecordUpdateExpression recordUpdate =>
                new ExpressionSyntax.RecordUpdateExpression(
                    RecordName: recordUpdate.RecordName,
                    PipeLocation: recordUpdate.PipeLocation,
                    Fields: MapSeparatedList(recordUpdate.Fields, NormalizeRecordField)),

                ExpressionSyntax.ParenthesizedExpression parenExpr =>
                new ExpressionSyntax.ParenthesizedExpression(
                    Expression: NormalizeExpression(parenExpr.Expression)),

                ExpressionSyntax.Negation negation =>
                new ExpressionSyntax.Negation(NormalizeExpression(negation.Expression)),

                ExpressionSyntax.RecordAccess recordAccess =>
                new ExpressionSyntax.RecordAccess(
                    Record: NormalizeExpression(recordAccess.Record),
                    FieldName: recordAccess.FieldName),

                ExpressionSyntax.IfBlock ifBlock =>
                new ExpressionSyntax.IfBlock(
                    IfTokenLocation: ifBlock.IfTokenLocation,
                    Condition: NormalizeExpression(ifBlock.Condition),
                    ThenTokenLocation: ifBlock.ThenTokenLocation,
                    ThenBlock: NormalizeExpression(ifBlock.ThenBlock),
                    ElseTokenLocation: ifBlock.ElseTokenLocation,
                    ElseBlock: NormalizeExpression(ifBlock.ElseBlock)),

                ExpressionSyntax.LambdaExpression lambdaExpr =>
                new ExpressionSyntax.LambdaExpression(
                    lambdaExpr.Lambda with
                    {
                        Expression = NormalizeExpression(lambdaExpr.Lambda.Expression),
                    }),

                ExpressionSyntax.CaseExpression caseExpr =>
                new ExpressionSyntax.CaseExpression(
                    new CaseBlock(
                        CaseTokenLocation: caseExpr.CaseBlock.CaseTokenLocation,
                        Expression: NormalizeExpression(caseExpr.CaseBlock.Expression),
                        OfTokenLocation: caseExpr.CaseBlock.OfTokenLocation,
                        Cases:
                        [
                        .. caseExpr.CaseBlock.Cases.Select(
                            c => c with { Expression = NormalizeExpression(c.Expression) })
                        ])),

                ExpressionSyntax.LetExpression letExpr =>
                new ExpressionSyntax.LetExpression(NormalizeLetBlock(letExpr.Value)),

                // Atoms and leaf expressions carry no sub-expressions to normalise.
                _ =>
                exprNode.Value,
            };

        return exprNode with { Value = normalized };
    }

    private static ExpressionSyntax NormalizeApplication(ExpressionSyntax.Application app)
    {
        // Normalise the function and the arguments first.
        var function = NormalizeExpression(app.Function);

        var arguments =
            app.Arguments.Select(NormalizeExpression).ToList();

        // Flatten a bare nested application in function position:
        // (f a) b  ->  f a b. This matches elm-format, which removes the
        // redundant parentheses around a curried application head. Only an
        // *unwrapped* application is flattened; one wrapped in an explicit
        // ParenthesizedExpression is preserved verbatim.
        while (function.Value is ExpressionSyntax.Application innerApp)
        {
            arguments =
                [.. innerApp.Arguments, .. arguments];

            function = innerApp.Function;
        }

        // The function position needs parentheses when, after flattening, it
        // is a compound expression that would otherwise capture the following
        // arguments (a lambda / if / let / case / operator application /
        // negation). A plain reference, record access, parenthesised
        // expression, etc. needs none.
        var functionWrapped = WrapInParensIfNeeded(function, isFunctionPosition: true);

        // Each argument that is itself a compound expression must be
        // parenthesised so the rendered text reparses to the same tree.
        var argumentsWrapped =
            arguments.Select(arg => WrapInParensIfNeeded(arg, isFunctionPosition: false)).ToList();

        return new ExpressionSyntax.Application(functionWrapped, argumentsWrapped);
    }

    private static Node<ExpressionSyntax> WrapInParensIfNeeded(
        Node<ExpressionSyntax> node,
        bool isFunctionPosition)
    {
        if (!NeedsParensInApplicationPosition(node.Value, isFunctionPosition))
            return node;

        return
            node with
            {
                Value = new ExpressionSyntax.ParenthesizedExpression(node),
            };
    }

    private static bool NeedsParensInApplicationPosition(
        ExpressionSyntax expression,
        bool isFunctionPosition)
    {
        return expression switch
        {
            // An application used as an argument must be parenthesised. In
            // function position the bare nested application has already been
            // flattened away, so any Application reaching here is an argument.
            ExpressionSyntax.Application =>
            !isFunctionPosition,

            ExpressionSyntax.OperatorApplication =>
            true,

            ExpressionSyntax.Negation =>
            true,

            ExpressionSyntax.LambdaExpression =>
            true,

            ExpressionSyntax.IfBlock =>
            true,

            ExpressionSyntax.LetExpression =>
            true,

            ExpressionSyntax.CaseExpression =>
            true,

            _ =>
            false,
        };
    }

    private static ExpressionSyntax.LetBlock NormalizeLetBlock(ExpressionSyntax.LetBlock letBlock)
    {
        return
            new ExpressionSyntax.LetBlock(
                LetTokenLocation: letBlock.LetTokenLocation,
                Declarations: [.. letBlock.Declarations.Select(NormalizeLetDeclaration)],
                InTokenLocation: letBlock.InTokenLocation,
                Expression: NormalizeExpression(letBlock.Expression));
    }

    private static Node<ExpressionSyntax.LetDeclaration> NormalizeLetDeclaration(
        Node<ExpressionSyntax.LetDeclaration> declNode)
    {
        var normalized =
            declNode.Value switch
            {
                ExpressionSyntax.LetDeclaration.LetFunction letFunc =>
                new ExpressionSyntax.LetDeclaration.LetFunction(
                    letFunc.Function with
                    {
                        Declaration =
                        letFunc.Function.Declaration with
                        {
                            Value =
                            letFunc.Function.Declaration.Value with
                            {
                                Expression =
                                NormalizeExpression(letFunc.Function.Declaration.Value.Expression),
                            },
                        },
                    }),

                ExpressionSyntax.LetDeclaration.LetDestructuring letDestr =>
                new ExpressionSyntax.LetDeclaration.LetDestructuring(
                    Pattern: letDestr.Pattern,
                    EqualsTokenLocation: letDestr.EqualsTokenLocation,
                    Expression: NormalizeExpression(letDestr.Expression)),

                _ =>
                declNode.Value,
            };

        return declNode with { Value = normalized };
    }

    private static RecordExprField NormalizeRecordField(RecordExprField field) =>
        new(field.FieldName, field.EqualsLocation, NormalizeExpression(field.ValueExpr));

    private static SeparatedSyntaxList<TNode> MapSeparatedList<TNode>(
        SeparatedSyntaxList<TNode> list,
        System.Func<TNode, TNode> mapper)
    {
        return list switch
        {
            SeparatedSyntaxList<TNode>.Empty =>
            list,

            SeparatedSyntaxList<TNode>.NonEmpty nonEmpty =>
            new SeparatedSyntaxList<TNode>.NonEmpty(
                First: mapper(nonEmpty.First),
                Rest: [.. nonEmpty.Rest.Select(r => (r.SeparatorLocation, mapper(r.Node)))]),

            _ =>
            throw new System.NotImplementedException(
                $"Unexpected SeparatedSyntaxList type: {list.GetType().Name}"),
        };
    }
}
