using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public static partial class OptimizationOpportunityFinder
{
    /// <summary>
    /// Inventories generic reads in the lowered IR consumed by emission.
    /// ProjectItem is never reported, and every surviving accessor function is generic.
    /// Other opportunity categories continue to use the source-oriented entry point.
    /// </summary>
    public static ImmutableHashSet<Opportunity> FindGenericRecordAccessOpportunities(
        IReadOnlyDictionary<DeclQualifiedName, IR.Declaration> declarations,
        IReadOnlyCollection<DeclQualifiedName>? restrictToReachableFromEntryPoints = null,
        IReadOnlyDictionary<IR.Expression, GenericRecordAccessEvidence>? typeEvidence = null)
    {
        var reachable =
            restrictToReachableFromEntryPoints is null
            ?
            null
            :
            ReachableIRDeclarations(declarations, restrictToReachableFromEntryPoints);

        var result = ImmutableHashSet.CreateBuilder<Opportunity>();

        foreach (var (name, declaration) in declarations)
        {
            if (declaration is not IR.Declaration.FunctionDeclaration function ||
                reachable is not null && !reachable.Contains(name))
            {
                continue;
            }

            foreach (var expression in IR.ExpressionTraversal.Enumerate(function.Function.Declaration.Expression))
            {
                string? field = null;
                RecordOperationProvenance? provenance = null;

                switch (expression)
                {
                    case IR.Expression.RecordAccess access:
                        field = access.FieldName;
                        provenance = RecordOperationProvenance.UnknownRecordLayout;
                        break;

                    case IR.Expression.RecordAccessFunction accessor:
                        field = accessor.FieldName;
                        provenance = RecordOperationProvenance.RecordAccessFunctionEscaped;
                        break;

                    case IR.Expression.UnitExpr:
                    case IR.Expression.StringLiteral:
                    case IR.Expression.CharLiteral:
                    case IR.Expression.IntegerLiteral:
                    case IR.Expression.FloatLiteral:
                    case IR.Expression.Negation:
                    case IR.Expression.ListExpr:
                    case IR.Expression.Identifier:
                    case IR.Expression.IfBlock:
                    case IR.Expression.PrefixOperator:
                    case IR.Expression.Application:
                    case IR.Expression.OperatorApplication:
                    case IR.Expression.TupledExpression:
                    case IR.Expression.ProjectItem:
                    case IR.Expression.LambdaExpression:
                    case IR.Expression.CaseExpression:
                    case IR.Expression.LetExpression:
                    case IR.Expression.RecordExpr:
                    case IR.Expression.RecordUpdateExpression:
                    case IR.Expression.GLSLExpression:
                        break;

                    default:
                        throw new NotImplementedException(
                            $"{nameof(FindGenericRecordAccessOpportunities)} does not handle expression variant: {expression.GetType().Name}");
                }

                if (field is null)
                    continue;

                GenericRecordAccessEvidence? evidence = null;
                typeEvidence?.TryGetValue(expression, out evidence);

                result.Add(
                    new Opportunity(name, OpportunityCategory.RecordAccess, field)
                    {
                        TypeEvidence =
                        evidence is null ? null : new OpportunityTypeEvidence(SubjectType: evidence.SubjectType),
                        RecordOperationProvenance = evidence?.Provenance ?? provenance
                    });
            }
        }

        return result.ToImmutable();
    }

    private static IReadOnlySet<DeclQualifiedName> ReachableIRDeclarations(
        IReadOnlyDictionary<DeclQualifiedName, IR.Declaration> declarations,
        IReadOnlyCollection<DeclQualifiedName> roots)
    {
        var result = new HashSet<DeclQualifiedName>();
        var pending = new Stack<DeclQualifiedName>(roots);

        while (pending.TryPop(out var name))
        {
            if (!result.Add(name) ||
                !declarations.TryGetValue(name, out var declaration) ||
                declaration is not IR.Declaration.FunctionDeclaration function)
            {
                continue;
            }

            var unqualified = new HashSet<string>();
            ExpressionCompiler.CollectExpressionReferences(function.Function.Declaration.Expression, unqualified);

            foreach (var parameter in function.Function.Declaration.Arguments)
            {
                var names = new HashSet<string>();
                PatternCompiler.CollectPatternNames(parameter, names);
                unqualified.ExceptWith(names);
            }

            foreach (var identifier in IR.ExpressionTraversal.Enumerate(function.Function.Declaration.Expression).OfType<IR.Expression.Identifier>())
            {
                if (identifier.QualifiedName.Namespaces.Count is 0 &&
                    !unqualified.Contains(identifier.QualifiedName.DeclName))
                {
                    continue;
                }

                var referenced =
                    identifier.QualifiedName.Namespaces.Count is 0
                    ?
                    DeclQualifiedName.Create(name.Namespaces, identifier.QualifiedName.DeclName)
                    :
                    identifier.QualifiedName;

                if (declarations.ContainsKey(referenced))
                    pending.Push(referenced);
            }
        }

        return result;
    }
}
