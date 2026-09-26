using Pine.Core.Internal;
using System;
using System.Linq;

using PreparedExpr = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter.PreparedExpression;
using LetDeclaration = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter.PreparedLetDeclaration;
using CaseDispatchSegment = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter.PreparedCaseDispatchSegment;
using Pattern = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Pattern;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Renders prepared Elm expressions in a readable, Elm-like form for diagnostics.
/// </summary>
public static class PreparedExpressionRendering
{
    /// <summary>
    /// Renders an expression, optionally overriding the rendering of each prepared value literal.
    /// Indistinguishable prepared values, such as Elm's unit and empty list, share a rendering.
    /// </summary>
    public static string Render(
        PreparedExpr expression,
        Func<PineValueInProcess, string>? renderLiteral = null) =>
        RenderExpression(
            expression,
            renderLiteral is null
            ?
            RenderLiteral
            :
            literal =>
            {
                var text = renderLiteral(literal.Value);
                return (text, text.StartsWith('-') || ElmSyntaxInterpreter.RenderAsElmExpression(literal.Value).needsParens);
            }).Text;

    private static (string Text, bool NeedsParens) RenderLiteral(PreparedExpr.ValueLiteral literal)
    {
        var elmValue = ElmSyntaxInterpreter.ToElmForErrorRendering(literal.Value);
        var (defaultText, needsParens) = ElmValue.RenderAsElmExpression(elmValue);

        if (elmValue is ElmValue.ElmString str)
            return (Rendering.RenderStringLiteral(str.Value), false);

        if (elmValue is ElmValue.ElmChar character)
            return (Rendering.RenderCharLiteral(character.Value), false);

        if (elmValue is ElmValue.ElmFloat number)
        {
            var text = ElmSyntaxAbstract.ConvertToConcrete.FloatLiteralText(number.Numerator, number.Denominator);
            return (text, text.StartsWith('-'));
        }

        return (defaultText, needsParens || defaultText.StartsWith('-'));
    }

    private static string RenderOperand(
        PreparedExpr expression,
        Func<PreparedExpr.ValueLiteral, (string Text, bool NeedsParens)> renderLiteral)
    {
        var (text, needsParens) = RenderExpression(expression, renderLiteral);

        return needsParens ? "(" + text + ")" : text;
    }

    private static (string Text, bool NeedsParens) RenderExpression(
        PreparedExpr expression,
        Func<PreparedExpr.ValueLiteral, (string Text, bool NeedsParens)> renderLiteral) =>
        expression switch
        {
            PreparedExpr.ValueLiteral literal =>
            renderLiteral(literal),

            PreparedExpr.Negation negation =>
            ("-" + RenderOperand(negation.Expression, renderLiteral), true),

            PreparedExpr.ListExpr list =>
            ("[" + string.Join(", ", list.Elements.Select(element => RenderExpression(element, renderLiteral).Text)) + "]", false),

            PreparedExpr.Identifier identifier =>
            (identifier.QualifiedName.FullName, false),

            PreparedExpr.IfBlock ifBlock =>
            RenderIfBlock(ifBlock, renderLiteral),

            PreparedExpr.PrefixOperator prefix =>
            ("(" + prefix.Operator + ")", false),

            PreparedExpr.Application application =>
            (RenderOperand(application.Function, renderLiteral) +
            (application.Arguments.Count is 0
            ?
            ""
            :
            " " +
            string.Join(
                " ",
                application.Arguments.Select(argument => RenderOperand(argument, renderLiteral)))),
            application.Arguments.Count is not 0),

            PreparedExpr.OperatorApplication op =>
            (RenderOperand(op.Left, renderLiteral) + " " + op.Operator + " " +
            RenderOperand(op.Right, renderLiteral),
            true),

            PreparedExpr.TupledExpression tuple =>
            ("(" + string.Join(", ", tuple.Elements.Select(element => RenderExpression(element, renderLiteral).Text)) + ")", false),

            PreparedExpr.LambdaExpression lambda =>
            ("\\" + string.Join(" ", lambda.Arguments.Select(argument => RenderPattern(argument, inArgument: true))) +
            " -> " + RenderExpression(lambda.Expression, renderLiteral).Text,
            true),

            PreparedExpr.CaseExpression caseExpression =>
            ("case " + RenderExpression(caseExpression.Expression, renderLiteral).Text + " of\n    " +
            string.Join(
                "\n    ",
                caseExpression.Dispatch.Steps.SelectMany(step => RenderCaseStep(step, renderLiteral))),
            true),

            PreparedExpr.LetExpression let =>
            ("let\n    " +
            string.Join(
                "\n    ",
                let.Declarations.Select(declaration => RenderLetDeclaration(declaration, renderLiteral))) +
            "\nin\n    " + RenderIndented(let.Expression, renderLiteral),
            true),

            PreparedExpr.RecordExpr record =>
            ("{" +
            string.Join(
                ", ",
                record.Fields.Select(
                    field =>
                    field.FieldName + " = " + RenderExpression(field.Value, renderLiteral).Text)) + "}",
            false),

            PreparedExpr.RecordAccess access =>
            (RenderOperand(access.Record, renderLiteral) + "." + access.FieldName, false),

            PreparedExpr.RecordAccessFunction accessFunction =>
            ("." + accessFunction.FieldName, false),

            PreparedExpr.RecordUpdateExpression update =>
            ("{" + update.RecordName + " | " +
            string.Join(
                ", ",
                update.Fields.Select(
                    field =>
                    field.FieldName + " = " + RenderExpression(field.Value, renderLiteral).Text)) + "}",
            false),

            PreparedExpr.GLSLExpression glsl =>
            ("[glsl|" + glsl.Code + "|]", false),

            _ =>
            throw new NotImplementedException(
                "RenderExpression does not handle expression variant: " + expression.GetType().Name),
        };

    private static string RenderIndented(
        PreparedExpr expression,
        Func<PreparedExpr.ValueLiteral, (string Text, bool NeedsParens)> renderLiteral) =>
        RenderExpression(expression, renderLiteral).Text.Replace("\n", "\n    ", StringComparison.Ordinal);

    private static (string Text, bool NeedsParens) RenderIfBlock(
        PreparedExpr.IfBlock ifBlock,
        Func<PreparedExpr.ValueLiteral, (string Text, bool NeedsParens)> renderLiteral)
    {
        var thenText = RenderIndented(ifBlock.ThenBlock, renderLiteral);

        return
            ("if " + RenderOperand(ifBlock.Condition, renderLiteral) +
            " then " + thenText +
            (thenText.Contains('\n') ? "\nelse " : " else ") +
            RenderIndented(ifBlock.ElseBlock, renderLiteral),
            true);
    }

    private static string RenderLetDeclaration(
        LetDeclaration declaration,
        Func<PreparedExpr.ValueLiteral, (string Text, bool NeedsParens)> renderLiteral) =>
        declaration switch
        {
            LetDeclaration.LetFunction function =>
            function.Function.Declaration.Name +
            (function.Function.Declaration.Arguments.Count is 0
            ?
            ""
            :
            " " +
            string.Join(
                " ",
                function.Function.Declaration.Arguments.Select(arg => RenderPattern(arg, inArgument: true)))) +
            " = " + RenderIndented(function.Function.Declaration.Expression, renderLiteral),

            LetDeclaration.LetDestructuring destructuring =>
            RenderPattern(destructuring.Pattern) + " = " + RenderIndented(destructuring.Expression, renderLiteral),

            _ =>
            throw new NotImplementedException(
                "RenderLetDeclaration does not handle let declaration variant: " + declaration.GetType().Name),
        };

    private static System.Collections.Generic.IEnumerable<string> RenderCaseStep(
        CaseDispatchSegment step,
        Func<PreparedExpr.ValueLiteral, (string Text, bool NeedsParens)> renderLiteral) =>
        step switch
        {
            CaseDispatchSegment.ConstantValuePatterns constants =>
            constants.Cases.Select(
                caseItem =>
                RenderPattern(caseItem.Pattern) + " -> " + RenderIndented(caseItem.Expression, renderLiteral)),

            CaseDispatchSegment.PatternCase patternCase =>
            [RenderPattern(patternCase.Pattern) + " -> " + RenderIndented(patternCase.Expression, renderLiteral)],

            CaseDispatchSegment.DiscardCase discard =>
            ["_ -> " + RenderIndented(discard.Expression, renderLiteral)],

            _ =>
            throw new NotImplementedException(
                "RenderCaseStep does not handle case dispatch variant: " + step.GetType().Name),
        };

    private static string RenderPattern(Pattern pattern, bool inArgument = false)
    {
        var (text, needsParens) =
            pattern switch
            {
                Pattern.AllPattern => ("_", false),
                Pattern.VarPattern variable => (variable.Name, false),
                Pattern.UnitPattern => ("()", false),
                Pattern.CharPattern character => (Rendering.RenderCharLiteral(character.Value), false),
                Pattern.StringPattern str => (Rendering.RenderStringLiteral(str.Value), false),
                Pattern.IntPattern integer => (integer.Value.ToString(), false),
                Pattern.FloatPattern number => (Rendering.FormatFloatForElm(number.Value), false),

                Pattern.TuplePattern tuple =>
                ("(" + string.Join(", ", tuple.Elements.Select(item => RenderPattern(item))) + ")", false),

                Pattern.RecordPattern record =>
                ("{" + string.Join(", ", record.Fields.Select(field => field.FieldName)) + "}", false),

                Pattern.UnConsPattern cons =>
                (RenderPattern(cons.Head, inArgument: true) + " :: " + RenderPattern(cons.Tail, inArgument: true), true),

                Pattern.ListPattern list =>
                ("[" + string.Join(", ", list.Elements.Select(item => RenderPattern(item))) + "]", false),

                Pattern.NamedPattern named =>
                (string.Join(".", named.Name.ModuleName.Append(named.Name.Name)) +
                (named.Arguments.Count is 0
                ?
                ""
                :
                " " + string.Join(" ", named.Arguments.Select(arg => RenderPattern(arg, inArgument: true)))),
                named.Arguments.Count is not 0),

                Pattern.AsPattern alias =>
                (RenderPattern(alias.Pattern, inArgument: true) + " as " + alias.Name, true),

                _ =>
                throw new NotImplementedException(
                    "RenderPattern does not handle pattern variant: " + pattern.GetType().Name),
            };

        return inArgument && needsParens ? "(" + text + ")" : text;
    }
}
