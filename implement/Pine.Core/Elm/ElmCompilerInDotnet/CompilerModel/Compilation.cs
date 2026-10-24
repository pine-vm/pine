using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;

using Pine.Core.Elm.Syntax.Concrete;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>A callable artifact, independent of source names and the compilation driver's accumulated state.</summary>
public sealed record CompiledCallable(
    PineValue CompiledValue, PineValue EncodedBody, int ParameterCount, ValueList<PineValue> EnvFunctions)
{
    private long _cachedHashCode;

    private CompiledCallable(CompiledCallable original)
    {
        CompiledValue = original.CompiledValue;
        EncodedBody = original.EncodedBody;
        ParameterCount = original.ParameterCount;
        EnvFunctions = original.EnvFunctions;
    }

    /// <inheritdoc/>
    public bool Equals(CompiledCallable? other) =>
        ReferenceEquals(this, other) ||
        other is not null &&
        ParameterCount == other.ParameterCount && Equals(CompiledValue, other.CompiledValue) &&
        Equals(EncodedBody, other.EncodedBody) && Equals(EnvFunctions, other.EnvFunctions);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        StructuralHash.GetOrCompute(
            ref _cachedHashCode,
            typeof(CompiledCallable),
            CompiledValue,
            EncodedBody,
            ParameterCount,
            EnvFunctions);
}

/// <summary>Only interfaces and direct compiled dependencies visible while emitting one SCC.</summary>
public sealed record SccCompilationContext(
    ImmutableDictionary<FunctionId, int> MemberArities,
    ImmutableList<FunctionId> Layout,
    ImmutableDictionary<FunctionId, CompiledCallable> ExternalFunctions);

/// <summary>A pure SCC compilation request. Source metadata is projected to referenced constructors.</summary>
public sealed record SccCompilationInput(
    ValueList<KeyValuePair<FunctionId, Function>> Members,
    ImmutableDictionary<FunctionId, CompiledCallable> ExternalFunctions,
    ImmutableDictionary<DeclQualifiedName, ValueList<string>> RecordConstructors,
    ImmutableDictionary<DeclQualifiedName, int> ChoiceConstructors,
    bool DisableGenericApplicationChainConsolidation)
{
    private long _cachedHashCode;

    private SccCompilationInput(SccCompilationInput original)
    {
        Members = original.Members;
        ExternalFunctions = original.ExternalFunctions;
        RecordConstructors = original.RecordConstructors;
        ChoiceConstructors = original.ChoiceConstructors;
        DisableGenericApplicationChainConsolidation = original.DisableGenericApplicationChainConsolidation;
    }

    /// <summary>Members in deterministic structural order, independent of scheduler traversal.</summary>
    public ValueList<KeyValuePair<FunctionId, Function>> Members { get; } =
        ValidateMembers(Members);

    /// <summary>Recursion is derived from the authoritative member bodies, not independently supplied.</summary>
    public bool IsRecursive =>
        Members.Count > 1 ||
        Compilation.References(Members[0].Value.Body).OfType<Reference.Function>().Any(
            reference => reference.Id == Members[0].Key);

    private static ValueList<KeyValuePair<FunctionId, Function>> ValidateMembers(
        ValueList<KeyValuePair<FunctionId, Function>> members)
    {
        ArgumentNullException.ThrowIfNull(members);

        if (members.Count is 0 || members.Any(entry => entry.Key is null || entry.Value is null) ||
            members.Select(entry => entry.Key).Distinct().Count() != members.Count)
        {
            throw new ArgumentException(
                "An SCC requires nonempty, distinct member identities and implementations.",
                nameof(members));
        }

        return new(members.OrderBy(entry => entry.Key, IdentityComparer.Instance));
    }

    /// <inheritdoc/>
    public bool Equals(SccCompilationInput? other) =>
        ReferenceEquals(this, other) ||
        other is not null &&
        DisableGenericApplicationChainConsolidation == other.DisableGenericApplicationChainConsolidation &&
        Members.Equals(other.Members) &&
        StructuralHash.SameEntries(ExternalFunctions, other.ExternalFunctions) &&
        StructuralHash.SameEntries(RecordConstructors, other.RecordConstructors) &&
        StructuralHash.SameEntries(ChoiceConstructors, other.ChoiceConstructors);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var hash))
            return hash;
        // KeyValuePair's default hash can omit its value; member implementations must contribute.
        var membersHash = new HashCode();

        foreach (var (id, function) in Members)
        {
            membersHash.Add(id);
            membersHash.Add(function);
        }

        return
            SyntaxHashCache.Store(
                ref _cachedHashCode,
                HashCode.Combine(
                    membersHash.ToHashCode(),
                    DisableGenericApplicationChainConsolidation,
                    StructuralHash.UnorderedMap(ExternalFunctions),
                    StructuralHash.UnorderedMap(RecordConstructors),
                    StructuralHash.UnorderedMap(ChoiceConstructors)));
    }
}

/// <summary>Schedules structural function identities and compiles SCCs without a global name supply.</summary>
public static class Compilation
{
    /// <summary>
    /// Builds all function artifacts. Roots and output packaging are deliberately separate.
    /// A caller may wrap the SCC worker with memoization without passing that cache into the worker.
    /// </summary>
    public static Result<CompilationError, ImmutableDictionary<FunctionId, CompiledCallable>> Compile(
        Program program,
        bool disableGenericApplicationChainConsolidation = false,
        Func<SccCompilationInput, Result<CompilationError, ImmutableDictionary<FunctionId, CompiledCallable>>>? compileScc = null)
    {
        compileScc ??= CompileScc;

        var dependencies =
            program.Functions.ToDictionary(
                entry => entry.Key,
                entry =>
                References(entry.Value.Body)
                .OfType<Reference.Function>().Select(reference => reference.Id).Distinct().ToArray());

        var compiled = ImmutableDictionary<FunctionId, CompiledCallable>.Empty;
        var allRecords = new Dictionary<DeclQualifiedName, ValueList<string>>();
        var allChoices = new Dictionary<DeclQualifiedName, int>();

        foreach (var (name, declaration) in program.SourceDeclarations)
        {
            switch (declaration)
            {
                case Syntax.Abstract.Declaration.AliasDeclaration alias:
                    if (alias.TypeAlias.TypeAnnotation is Syntax.Abstract.TypeAnnotation.Record record)
                        allRecords.Add(name, new(record.RecordDefinition.Fields.Select(field => field.FieldName)));

                    break;

                case Syntax.Abstract.Declaration.ChoiceTypeDeclaration choice:
                    foreach (var constructor in choice.TypeDeclaration.Constructors)
                    {
                        allChoices.Add(
                            DeclQualifiedName.Create(name.Namespaces, constructor.Name),
                            constructor.Arguments.Count);
                    }

                    break;

                case Syntax.Abstract.Declaration.FunctionDeclaration:
                    throw new InvalidOperationException("Compiler metadata must not contain source function bodies.");

                case Syntax.Abstract.Declaration.PortDeclaration:
                case Syntax.Abstract.Declaration.InfixDeclaration:
                    break;

                default:
                    throw new NotImplementedException(
                        $"{nameof(Compile)} does not handle declaration variant: {declaration.GetType().Name}");
            }
        }

        foreach (var members in Components(dependencies))
        {
            var memberSet = members.ToHashSet();

            var externalIds =
                members.SelectMany(id => dependencies[id]).Where(id => !memberSet.Contains(id)).Distinct();

            var external = ImmutableDictionary.CreateBuilder<FunctionId, CompiledCallable>();

            foreach (var id in externalIds)
            {
                if (!compiled.TryGetValue(id, out var artifact))
                    return new CompilationError.UnresolvedReference(id.SourceDeclaration.FullName, "");

                external.Add(id, artifact);
            }

            var records = ImmutableDictionary.CreateBuilder<DeclQualifiedName, ValueList<string>>();
            var choices = ImmutableDictionary.CreateBuilder<DeclQualifiedName, int>();

            foreach (var reference in members.SelectMany(id => References(program.Functions[id].Body)).OfType<Reference.Constructor>().Distinct())
            {
                if (allRecords.TryGetValue(reference.Name, out var fields))
                    records.Add(reference.Name, fields);

                if (allChoices.TryGetValue(reference.Name, out var arity))
                    choices.Add(reference.Name, arity);
            }

            var request =
                new SccCompilationInput(
                    new(members.Select(id => KeyValuePair.Create(id, program.Functions[id]))),
                    external.ToImmutable(),
                    records.ToImmutable(),
                    choices.ToImmutable(),
                    disableGenericApplicationChainConsolidation);

            var result = compileScc(request);

            if (result.IsErrOrNull() is { } error)
                return error;

            compiled =
                compiled.SetItems(result.Extract(error => throw new InvalidOperationException(error.ToString())));
        }

        return compiled;
    }

    /// <summary>Compiles one explicit dependency-projected SCC. Memoization can wrap this function directly.</summary>
    public static Result<CompilationError, ImmutableDictionary<FunctionId, CompiledCallable>> CompileScc(
        SccCompilationInput input)
    {
        if (input.Members.Count is 0)
            throw new ArgumentException("An SCC must contain at least one function.", nameof(input));

        var members = input.Members.OrderBy(entry => entry.Key, IdentityComparer.Instance).ToArray();
        var layout = input.IsRecursive ? members.Select(entry => entry.Key).ToImmutableList() : [];

        var functions =
            new SccCompilationContext(
                members.ToImmutableDictionary(entry => entry.Key, entry => entry.Value.Parameters.Count),
                layout,
                input.ExternalFunctions);

        var metadata =
            new ModuleCompilationContext(
                FrozenSet.Create(["Pine_builtin", "Pine_kernel"]),
                RecordTypeAliasConstructors: input.RecordConstructors,
                ChoiceConstructorArities: input.ChoiceConstructors);

        var parseCache = new PineVMParseCache();
        var reductions = new Dictionary<(PineLanguage.Expression, ReductionConfig), PineLanguage.Expression>();
        var encodings = new ExpressionEncodingCache();
        var bodies = new Dictionary<FunctionId, PineLanguage.Expression>();

        foreach (var (id, function) in members)
        {
            var parameters = new Dictionary<BindingId, int>();
            var bindings = new Dictionary<BindingId, PineLanguage.Expression>();

            for (var index = 0; index < function.Parameters.Count; index++)
            {
                var pattern = function.Parameters[index];

                if (pattern is Pattern.VarPattern variable)
                    parameters[variable.Binding] = index;

                else
                {
                    foreach (var (binding, value) in PatternCompiler.AnalyzePattern(pattern, BuiltinHelpers.BuildPathToParameter(index)).Bindings)
                        bindings.Add(binding, value);
                }
            }

            var context =
                new ExpressionCompilationContext(
                    parameters,
                    bindings,
                    functions,
                    metadata,
                    string.Join(".", id.SourceDeclaration.Namespaces),
                    parseCache,
                    reductions);

            var result = ExpressionCompiler.Compile(ToIntermediateRepresentation.Convert(function.Body), context);

            if (result.IsErrOrNull() is { } error)
                return new CompilationError.InDeclaration(id.SourceDeclaration.FullName, error);

            bodies.Add(
                id,
                ReducePineExpression.ReduceExpressionBottomUp(
                    result.Extract(error => throw new InvalidOperationException(error.ToString())),
                    ReductionConfig.Default with { DisableGenericApplicationChainConsolidation = input.DisableGenericApplicationChainConsolidation },
                    parseCache,
                    reducedExpressionCache: reductions));
        }

        var encoded =
            bodies.ToDictionary(
                entry => entry.Key,
                entry => ExpressionEncoding.EncodeExpressionAsValue(entry.Value, encodings));

        var environment = new ValueList<PineValue>(layout.Select(id => encoded[id]));
        var output = ImmutableDictionary.CreateBuilder<FunctionId, CompiledCallable>();

        foreach (var (id, function) in members)
        {
            var wrapper =
                function.Parameters.Count is 0
                ?
                FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                    bodies[id],
                    0,
                    environment,
                    encodeExprCache: encodings)
                :
                FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                    bodies[id],
                    function.Parameters.Count,
                    environment,
                    encodeExprCache: encodings)
                ?? throw new InvalidOperationException("Failed to build compiled function wrapper.");

            output.Add(id, new(wrapper, encoded[id], function.Parameters.Count, environment));
        }

        return output.ToImmutable();
    }

    /// <summary>Enumerates references in deterministic expression order.</summary>
    public static IEnumerable<Reference> References(Expression expression)
    {
        var result = new List<Reference>();

        void Visit(Expression current)
        {
            if (current is Expression.ReferenceExpression reference)
                result.Add(reference.Reference);

            foreach (var child in ExpressionTransform.Children(current))
                Visit(child);
        }

        Visit(expression);
        return result;
    }

    private static IEnumerable<ImmutableList<FunctionId>> Components(
        IReadOnlyDictionary<FunctionId, FunctionId[]> dependencies)
    {
        var index = 0;
        var indices = new Dictionary<FunctionId, int>();
        var low = new Dictionary<FunctionId, int>();
        var stack = new Stack<FunctionId>();
        var active = new HashSet<FunctionId>();
        var result = new List<ImmutableList<FunctionId>>();

        void Visit(FunctionId id)
        {
            indices.Add(id, index);
            low.Add(id, index++);
            stack.Push(id);
            active.Add(id);

            foreach (var dependency in dependencies[id].OrderBy(id => id, IdentityComparer.Instance))
            {
                if (!dependencies.ContainsKey(dependency))
                    throw new InvalidOperationException("Resolved function reference is absent from the program.");

                if (!indices.TryGetValue(dependency, out var value1))
                {
                    Visit(dependency);
                    low[id] = Math.Min(low[id], low[dependency]);
                }
                else if (active.Contains(dependency))
                    low[id] = Math.Min(low[id], value1);
            }

            if (low[id] != indices[id])
                return;

            var members = new List<FunctionId>();
            FunctionId member;

            do
            {
                member = stack.Pop();
                active.Remove(member);
                members.Add(member);
            }
            while (member != id);

            result.Add([.. members.OrderBy(id => id, IdentityComparer.Instance)]);
        }

        foreach (var id in dependencies.Keys.OrderBy(id => id, IdentityComparer.Instance))
        {
            if (!indices.ContainsKey(id))
                Visit(id);
        }

        return result;
    }
}
