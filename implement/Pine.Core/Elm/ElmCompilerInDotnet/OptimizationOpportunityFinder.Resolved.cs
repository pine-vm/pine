using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public static partial class OptimizationOpportunityFinder
{
    /// <summary>Inventories compiler-owned generic reads. Generated report names are assigned only at this output boundary.</summary>
    public static ImmutableHashSet<Opportunity> FindGenericRecordAccessOpportunities(
        Model.Program program,
        IReadOnlyCollection<DeclQualifiedName>? restrictToReachableFromEntryPoints = null,
        IReadOnlyDictionary<Model.Expression, GenericRecordAccessEvidence>? typeEvidence = null)
    {
        var names = Model.DisplayNames.Create(program);
        HashSet<Model.FunctionId>? reachable = null;

        if (restrictToReachableFromEntryPoints is not null)
        {
            reachable = [];

            var pending =
                new Stack<Model.FunctionId>(
                    restrictToReachableFromEntryPoints.Select(name => new Model.FunctionId.Source(name)));

            while (pending.TryPop(out var id))
            {
                if (!reachable.Add(id) || !program.Functions.TryGetValue(id, out var function))
                    continue;

                foreach (var reference in Model.Compilation.References(function.Body).OfType<Model.Reference.Function>())
                    pending.Push(reference.Id);
            }
        }

        var result = ImmutableHashSet.CreateBuilder<Opportunity>();

        foreach (var (id, function) in program.Functions)
        {
            if (reachable is not null && !reachable.Contains(id))
                continue;

            var name =
                id is Model.FunctionId.Source source
                ?
                source.Name
                :
                DeclQualifiedName.Create(id.SourceDeclaration.Namespaces, names.Functions[id]);

            void Visit(Model.Expression expression)
            {
                var field =
                    expression is Model.Expression.RecordAccess access
                    ?
                    access.FieldName
                    :
                    expression is Model.Expression.RecordAccessor accessor ? accessor.FieldName : null;

                if (field is not null)
                {
                    GenericRecordAccessEvidence? evidence = null;
                    typeEvidence?.TryGetValue(expression, out evidence);

                    result.Add(
                        new Opportunity(name, OpportunityCategory.RecordAccess, field)
                        {
                            TypeEvidence =
                            evidence is null ? null : new OpportunityTypeEvidence(SubjectType: evidence.SubjectType),
                            RecordOperationProvenance =
                            evidence?.Provenance ??
                            (expression is Model.Expression.RecordAccessor
                            ?
                            RecordOperationProvenance.RecordAccessFunctionEscaped
                            :
                            RecordOperationProvenance.UnknownRecordLayout)
                        });
                }

                foreach (var child in Model.ExpressionTransform.Children(expression))
                    Visit(child);
            }

            Visit(function.Body);
        }

        return result.ToImmutable();
    }
}
