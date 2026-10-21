using System;
using System.Collections.Immutable;
using System.Linq;

using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Copies optimized expressions to emission IR without inventing names or re-resolving references.</summary>
public static class ToIntermediateRepresentation
{
    /// <summary>Projects exactly the function implementations used by the SCC worker.</summary>
    public static ImmutableDictionary<FunctionId, IR.FunctionStruct> Convert(Program program) =>
        program.Functions.ToImmutableDictionary(entry => entry.Key, entry => Convert(entry.Value));

    /// <summary>Projects a function without manufacturing an implementation name.</summary>
    public static IR.FunctionStruct Convert(Function function) =>
        new(function.Parameters, Convert(function.Body));

    /// <summary>Emits an expression. Lambdas and local functions must have been lifted.</summary>
    public static IR.Expression Convert(Expression expression) =>
        expression switch
        {
            Expression.Literal literal => new IR.Expression.LiteralValue(literal.Value),
            Expression.ReferenceExpression reference => new IR.Expression.Identifier(reference.Reference),

            Expression.List list =>
            new IR.Expression.ListExpr(new ValueList<IR.Expression>(list.Items.Select(Convert))),

            Expression.Conditional conditional =>
            new IR.Expression.IfBlock(
                Convert(conditional.Condition),
                Convert(conditional.Then),
                Convert(conditional.Else)),

            Expression.Application application =>
            new IR.Expression.Application(
                Convert(application.Function),
                new ValueList<IR.Expression>(application.Arguments.Select(Convert))),

            Expression.Let let =>
            new IR.Expression.LetExpression(
                new ValueList<IR.LetDeclaration>(let.Bindings.Select(Binding)),
                Convert(let.Body)),

            Expression.Match match =>
            new IR.Expression.CaseExpression(
                Convert(match.Value),
                new ValueList<IR.Case>(
                    match.Branches.Select(branch => new IR.Case(branch.Pattern, Convert(branch.Body))))),

            Expression.Record record =>
            new IR.Expression.RecordExpr(new ValueList<IR.RecordSetter>(record.Fields.Select(Field))),

            Expression.RecordAccess access => new IR.Expression.RecordAccess(Convert(access.Value), access.FieldName),
            Expression.RecordAccessor accessor => new IR.Expression.RecordAccessFunction(accessor.FieldName),

            Expression.RecordUpdate update =>
            new IR.Expression.RecordUpdateExpression(Convert(update.Receiver), new(update.Fields.Select(Field))),

            Expression.Project project => new IR.Expression.ProjectItem(Convert(project.Value), project.Index),

            Expression.Operator operation =>
            new IR.Expression.OperatorApplication(
                operation.Name,
                operation.Direction,
                Convert(operation.Left),
                Convert(operation.Right)),

            Expression.PrefixOperator prefix => new IR.Expression.PrefixOperator(prefix.Name),
            Expression.Negate negate => new IR.Expression.Negation(Convert(negate.Value)),
            Expression.Shader shader => new IR.Expression.GLSLExpression(shader.Code),

            Expression.Lambda =>
            throw new InvalidOperationException("Lambdas must be lifted before IR emission."),

            _ =>
            throw new NotImplementedException(
                $"{nameof(Convert)} does not handle expression variant: {expression.GetType().Name}")
        };

    private static IR.LetDeclaration Binding(Binding binding)
    {
        return new IR.LetDeclaration(binding.Pattern, Convert(binding.Value));
    }

    private static IR.RecordSetter Field(Field field) => new(field.Name, Convert(field.Value));
}
