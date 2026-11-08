using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;
using System;
using System.Linq;

using AST = Pine.Core.Elm.Syntax.Abstract;
using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>Caller-side binding and observer identity projection, without literal or dispatch preparation.</summary>
internal static class PreparedSourceRebinding
{
    public static PreparedSourceDeclaration Apply(
        PreparedSourceDeclaration template, Func<DeclQualifiedName, DeclQualifiedName> bindings)
    {
        var syntax = Canonicalize.MapNames(template.Syntax, bindings);

        AST.Pattern Pattern(AST.Pattern pattern)
        {
            var container =
                new AST.Declaration.FunctionDeclaration(
                    new(
                        null,
                        new("$pattern", [pattern], new AST.Expression.UnitExpr())));

            return
                ((AST.Declaration.FunctionDeclaration)Canonicalize.MapNames(container, bindings))
                .Function.Declaration.Arguments[0];
        }

        AST.Signature? Signature(AST.Signature? signature) =>
            signature is null
            ?
            null
            :
            ((AST.Declaration.PortDeclaration)Canonicalize.MapNames(new AST.Declaration.PortDeclaration(signature), bindings)).Signature;

        Engine.PreparedFunctionStruct Function(Engine.PreparedFunctionStruct function) =>
            new(
                Signature(function.Signature),
                new(
                    function.Declaration.Name,
                    [.. function.Declaration.Arguments.Select(Pattern)],
                    Expression(function.Declaration.Expression)));

        Engine.PreparedLetDeclaration Let(Engine.PreparedLetDeclaration declaration) =>
            declaration switch
            {
                Engine.PreparedLetDeclaration.LetFunction function =>
                new Engine.PreparedLetDeclaration.LetFunction(Function(function.Function)),

                Engine.PreparedLetDeclaration.LetDestructuring destructuring =>
                new Engine.PreparedLetDeclaration.LetDestructuring(
                    Pattern(destructuring.Pattern),
                    Expression(destructuring.Expression)),

                _ =>
                throw new NotImplementedException(nameof(Let) + " does not handle declaration: " + declaration.GetType().Name)
            };

        Engine.PreparedCaseDispatchSegment Case(Engine.PreparedCaseDispatchSegment segment) =>
            segment switch
            {
                Engine.PreparedCaseDispatchSegment.ConstantValuePatterns constants =>
                new Engine.PreparedCaseDispatchSegment.ConstantValuePatterns(
                    [
                        .. constants.Cases.Select(
                            item =>
                            new Engine.PreparedConstantCase(
                                Pattern(item.Pattern),
                                item.Value,
                                Expression(item.Expression)))
                    ]),

                Engine.PreparedCaseDispatchSegment.PatternCase pattern =>
                new Engine.PreparedCaseDispatchSegment.PatternCase(
                    Pattern(pattern.Pattern),
                    Expression(pattern.Expression)),

                Engine.PreparedCaseDispatchSegment.DiscardCase discard =>
                new Engine.PreparedCaseDispatchSegment.DiscardCase(Expression(discard.Expression)),

                _ =>
                throw new NotImplementedException(nameof(Case) + " does not handle dispatch: " + segment.GetType().Name)
            };

        Engine.PreparedRecordSetter Field(Engine.PreparedRecordSetter field) =>
            new(field.FieldName, field.FieldNameValue, Expression(field.Value));

        Engine.PreparedExpression Expression(Engine.PreparedExpression expression) =>
            expression switch
            {
                Engine.PreparedExpression.ValueLiteral literal =>
                new Engine.PreparedExpression.ValueLiteral(literal.Value),

                Engine.PreparedExpression.Negation negation =>
                new Engine.PreparedExpression.Negation(Expression(negation.Expression)),

                Engine.PreparedExpression.ListExpr list =>
                new Engine.PreparedExpression.ListExpr([.. list.Elements.Select(Expression)]),

                Engine.PreparedExpression.Identifier identifier =>
                new Engine.PreparedExpression.Identifier(bindings(identifier.QualifiedName)),

                Engine.PreparedExpression.IfBlock conditional =>
                new Engine.PreparedExpression.IfBlock(
                    Expression(conditional.Condition),
                    Expression(conditional.ThenBlock),
                    Expression(conditional.ElseBlock)),

                Engine.PreparedExpression.PrefixOperator prefix =>
                new Engine.PreparedExpression.PrefixOperator(prefix.Operator),

                Engine.PreparedExpression.Application application =>
                new Engine.PreparedExpression.Application(
                    Expression(application.Function),
                    [.. application.Arguments.Select(Expression)]),

                Engine.PreparedExpression.OperatorApplication operation =>
                new Engine.PreparedExpression.OperatorApplication(
                    operation.Operator.Contains('.')
                    ?
                    bindings(DeclQualifiedName.FromString(operation.Operator)).FullName
                    :
                    operation.Operator,
                    operation.Direction,
                    Expression(operation.Left),
                    Expression(operation.Right)),

                Engine.PreparedExpression.TupledExpression tuple =>
                new Engine.PreparedExpression.TupledExpression([.. tuple.Elements.Select(Expression)]),

                Engine.PreparedExpression.LambdaExpression lambda =>
                new Engine.PreparedExpression.LambdaExpression(
                    [.. lambda.Arguments.Select(Pattern)],
                    Expression(lambda.Expression)),

                Engine.PreparedExpression.CaseExpression cases =>
                new Engine.PreparedExpression.CaseExpression(
                    Expression(cases.Expression),
                    new([.. cases.Dispatch.Steps.Select(Case)])),

                Engine.PreparedExpression.LetExpression let =>
                new Engine.PreparedExpression.LetExpression(
                    [.. let.Declarations.Select(Let)],
                    Expression(let.Expression)),

                Engine.PreparedExpression.RecordExpr record =>
                new Engine.PreparedExpression.RecordExpr([.. record.Fields.Select(Field)]),

                Engine.PreparedExpression.RecordAccess access =>
                new Engine.PreparedExpression.RecordAccess(
                    Expression(access.Record),
                    access.FieldName,
                    access.FieldNameValue),

                Engine.PreparedExpression.RecordAccessFunction access =>
                new Engine.PreparedExpression.RecordAccessFunction(access.FieldName, access.FieldNameValue),

                Engine.PreparedExpression.RecordUpdateExpression update =>
                new Engine.PreparedExpression.RecordUpdateExpression(
                    update.RecordName,
                    [.. update.Fields.Select(Field)]),

                Engine.PreparedExpression.GLSLExpression glsl =>
                new Engine.PreparedExpression.GLSLExpression(glsl.Code),

                _ =>
                throw new NotImplementedException(
                    nameof(Expression) + " does not handle prepared expression: " + expression.GetType().Name)
            };

        Engine.PreparedDeclaration prepared =
            template.Prepared switch
            {
                Engine.PreparedDeclaration.FunctionDeclaration function =>
                new Engine.PreparedDeclaration.FunctionDeclaration(Function(function.Function)),

                Engine.PreparedDeclaration.ChoiceTypeDeclaration =>
                new Engine.PreparedDeclaration.ChoiceTypeDeclaration(
                    ((AST.Declaration.ChoiceTypeDeclaration)syntax).TypeDeclaration),

                Engine.PreparedDeclaration.AliasDeclaration =>
                new Engine.PreparedDeclaration.AliasDeclaration(((AST.Declaration.AliasDeclaration)syntax).TypeAlias),

                Engine.PreparedDeclaration.PortDeclaration =>
                new Engine.PreparedDeclaration.PortDeclaration(((AST.Declaration.PortDeclaration)syntax).Signature),

                Engine.PreparedDeclaration.InfixDeclaration =>
                new Engine.PreparedDeclaration.InfixDeclaration(((AST.Declaration.InfixDeclaration)syntax).Infix),

                _ =>
                throw new NotImplementedException(
                    nameof(Apply) + " does not handle prepared declaration: " + template.Prepared.GetType().Name)
            };

        return new(syntax, prepared);
    }
}
