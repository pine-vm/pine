using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Resolver indexes are derived once per prepared declaration dictionary. Callers must treat
    /// a dictionary as immutable after passing it to the interpreter; mutating the same instance
    /// later does not invalidate its cached index.
    /// </summary>
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<
        IReadOnlyDictionary<DeclQualifiedName, PreparedDeclaration>,
        PreparedDeclarationResolverIndex>
        s_declarationResolverIndexes = [];

    private static PreparedDeclarationResolverIndex GetDeclarationResolverIndex(
        IReadOnlyDictionary<DeclQualifiedName, PreparedDeclaration> declarations) =>
        s_declarationResolverIndexes.GetValue(
            declarations,
            static declarationsToIndex => new PreparedDeclarationResolverIndex(declarationsToIndex));

    private readonly record struct QualifiedCallableName(
        IReadOnlyList<string> Namespaces,
        string Name)
    {
        public bool Equals(QualifiedCallableName other) =>
            QualifiedCallableNameComparer.Instance.Equals(this, other);

        public override int GetHashCode() =>
            QualifiedCallableNameComparer.Instance.GetHashCode(this);
    }

    private sealed class QualifiedCallableNameComparer : IEqualityComparer<QualifiedCallableName>
    {
        public static QualifiedCallableNameComparer Instance { get; } = new();

        public bool Equals(QualifiedCallableName left, QualifiedCallableName right) =>
            left.Name == right.Name &&
            NamespacesEqual(left.Namespaces, right.Namespaces);

        public int GetHashCode(QualifiedCallableName value)
        {
            var hash = new System.HashCode();

            hash.Add(value.Name, System.StringComparer.Ordinal);

            foreach (var namespacePart in value.Namespaces)
                hash.Add(namespacePart, System.StringComparer.Ordinal);

            return hash.ToHashCode();
        }
    }

    private abstract record DeclarationResolverCandidate(
        DeclQualifiedName DeclarationName)
    {
        public sealed record Function(
            DeclQualifiedName DeclarationName,
            PreparedFunctionImplementation Implementation)
            : DeclarationResolverCandidate(DeclarationName);

        public sealed record RecordAlias(
            DeclQualifiedName DeclarationName,
            IReadOnlyList<(string FieldName, PineValue FieldNameValue)> Fields)
            : DeclarationResolverCandidate(DeclarationName);

        public sealed record ChoiceConstructor(
            DeclQualifiedName DeclarationName,
            ElmSyntaxAbstract.ValueConstructor Constructor)
            : DeclarationResolverCandidate(DeclarationName);
    }

    private sealed class PreparedDeclarationResolverIndex
    {
        private readonly IReadOnlyDictionary<
            QualifiedCallableName,
            IReadOnlyList<DeclarationResolverCandidate>>
            candidatesByQualifiedName;

        private readonly IReadOnlyDictionary<
            string,
            IReadOnlyList<DeclarationResolverCandidate>>
            candidatesBySimpleName;

        public PreparedDeclarationResolverIndex(
            IReadOnlyDictionary<DeclQualifiedName, PreparedDeclaration> declarations)
        {
            var qualified =
                new Dictionary<
                    QualifiedCallableName,
                    List<DeclarationResolverCandidate>>(
                    QualifiedCallableNameComparer.Instance);

            var simple =
                new Dictionary<string, List<DeclarationResolverCandidate>>(
                    System.StringComparer.Ordinal);

            foreach (var (declarationName, declaration) in declarations)
            {
                switch (declaration)
                {
                    case PreparedDeclaration.FunctionDeclaration functionDeclaration:
                        AddCandidate(
                            functionDeclaration.Function.Declaration.Name,
                            new DeclarationResolverCandidate.Function(
                                declarationName,
                                functionDeclaration.Function.Declaration));

                        break;

                    case PreparedDeclaration.AliasDeclaration aliasDeclaration:
                        if (aliasDeclaration.TypeAlias.TypeAnnotation
                            is ElmSyntaxAbstract.TypeAnnotation.Record recordAnnotation)
                        {
                            AddCandidate(
                                aliasDeclaration.TypeAlias.Name,
                                new DeclarationResolverCandidate.RecordAlias(
                                    declarationName,
                                    Fields:
                                    [
                                    .. recordAnnotation.RecordDefinition.Fields.Select(
                                        field => (field.FieldName, field.FieldNameValue))
                                    ]));
                        }

                        break;

                    case PreparedDeclaration.ChoiceTypeDeclaration choiceTypeDeclaration:
                        foreach (var constructor in choiceTypeDeclaration.TypeDeclaration.Constructors)
                        {
                            AddCandidate(
                                constructor.Name,
                                new DeclarationResolverCandidate.ChoiceConstructor(
                                    declarationName,
                                    constructor));
                        }

                        break;

                    case PreparedDeclaration.PortDeclaration:
                    case PreparedDeclaration.InfixDeclaration:
                        break;

                    default:
                        throw new System.NotImplementedException(
                            "PreparedDeclarationResolverIndex does not handle declaration variant: " +
                            declaration.GetType().Name);
                }
            }

            candidatesByQualifiedName =
                qualified.ToDictionary(
                    entry => entry.Key,
                    entry => (IReadOnlyList<DeclarationResolverCandidate>)entry.Value,
                    QualifiedCallableNameComparer.Instance);

            candidatesBySimpleName =
                simple.ToDictionary(
                    entry => entry.Key,
                    entry => (IReadOnlyList<DeclarationResolverCandidate>)entry.Value,
                    System.StringComparer.Ordinal);

            void AddCandidate(
                string callableName,
                DeclarationResolverCandidate candidate)
            {
                var qualifiedName =
                    new QualifiedCallableName(
                        candidate.DeclarationName.Namespaces,
                        callableName);

                if (!qualified.TryGetValue(qualifiedName, out var qualifiedCandidates))
                {
                    qualifiedCandidates = [];
                    qualified.Add(qualifiedName, qualifiedCandidates);
                }

                qualifiedCandidates.Add(candidate);

                if (!simple.TryGetValue(callableName, out var simpleCandidates))
                {
                    simpleCandidates = [];
                    simple.Add(callableName, simpleCandidates);
                }

                simpleCandidates.Add(candidate);
            }
        }

        public ApplicationResolution? Resolve(
            Application application,
            IReadOnlyList<string>? requiredNamespaces)
        {
            var requestedName = application.FunctionName.DeclName;
            var requestedNamespaces = application.FunctionName.Namespaces;

            var effectiveRequiredNamespaces =
                requestedNamespaces.Count is not 0
                ?
                requestedNamespaces
                :
                requiredNamespaces;

            if (effectiveRequiredNamespaces is not null)
            {
                if (!candidatesByQualifiedName.TryGetValue(
                    new QualifiedCallableName(effectiveRequiredNamespaces, requestedName),
                    out var qualifiedCandidates))
                {
                    return null;
                }

                return ResolveFirstCandidate(application, qualifiedCandidates);
            }

            if (!candidatesBySimpleName.TryGetValue(requestedName, out var candidates))
                return null;

            return ResolveFirstCandidate(application, candidates);
        }

        private static ApplicationResolution? ResolveFirstCandidate(
            Application application,
            IReadOnlyList<DeclarationResolverCandidate> candidates)
        {
            foreach (var candidate in candidates)
            {
                if (ResolveCandidate(application, candidate) is { } resolution)
                    return resolution;
            }

            return null;
        }

        private static ApplicationResolution? ResolveCandidate(
            Application application,
            DeclarationResolverCandidate candidate)
        {
            switch (candidate)
            {
                case DeclarationResolverCandidate.Function function:
                    return
                        new ApplicationResolution.ContinueWithFunction(
                            function.Implementation,
                            ResolvedName: function.DeclarationName);

                case DeclarationResolverCandidate.RecordAlias recordAlias:
                    {
                        if (recordAlias.Fields.Count == application.Arguments.Count)
                        {
                            var fields =
                                new List<(string FieldName, PineValue FieldNameValue, PineValueInProcess FieldValue)>(
                                    recordAlias.Fields.Count);

                            for (var i = 0; i < recordAlias.Fields.Count; i++)
                            {
                                fields.Add(
                                    (recordAlias.Fields[i].FieldName,
                                    recordAlias.Fields[i].FieldNameValue,
                                    application.Arguments[i]));
                            }

                            return
                                new ApplicationResolution.Resolved(
                                    BuildRecordValue([.. fields.OrderBy(f => f.FieldName)]));
                        }

                        if (application.Arguments.Count < recordAlias.Fields.Count)
                        {
                            return
                                new ApplicationResolution.Resolved(
                                    new ElmRecordTypeConstructorInProcess(
                                        typeName: recordAlias.DeclarationName,
                                        fieldNames: [.. recordAlias.Fields],
                                        arguments: [.. application.Arguments]));
                        }

                        return null;
                    }

                case DeclarationResolverCandidate.ChoiceConstructor choiceConstructor:
                    {
                        var constructor = choiceConstructor.Constructor;
                        var constructorArity = constructor.Arguments.Count;

                        if (constructorArity == application.Arguments.Count)
                        {
                            if (constructorArity is 0 && constructor.Name is "True")
                            {
                                return
                                    new ApplicationResolution.Resolved(
                                        PineValueInProcess.KernelTrueValue);
                            }

                            if (constructorArity is 0 && constructor.Name is "False")
                            {
                                return
                                    new ApplicationResolution.Resolved(
                                        PineValueInProcess.KernelFalseValue);
                            }

                            return
                                new ApplicationResolution.Resolved(
                                    BuildTaggedValue(
                                        PineValueInProcess.Create(constructor.NameValue),
                                        application.Arguments));
                        }

                        if (application.Arguments.Count < constructorArity)
                        {
                            return
                                new ApplicationResolution.Resolved(
                                    new ElmChoiceTagConstructorInProcess(
                                        typeName: choiceConstructor.DeclarationName,
                                        tagName: constructor.Name,
                                        totalArity: constructorArity,
                                        arguments: [.. application.Arguments]));
                        }

                        return null;
                    }

                default:
                    throw new System.NotImplementedException(
                        "ResolveCandidate does not handle candidate variant: " +
                        candidate.GetType().Name);
            }
        }
    }
}
