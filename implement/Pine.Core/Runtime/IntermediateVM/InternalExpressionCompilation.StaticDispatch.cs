using Pine.Core.CodeAnalysis;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.InternalExpression;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

using Expression = Pine.Core.Runtime.InternalExpression.Expression;

namespace Pine.Core.Runtime.IntermediateVM;

internal static partial class InternalExpressionCompilation
{
    internal sealed record StaticFunction(
        Expression PreparedExpression,
        StaticFunctionInterface Parameters,
        ReturnValueLayout Returns,
        Func<PineControlFlowGraph> CreateGraph,
        Lazy<StackFrameInstructions> Instructions);

    internal sealed record StaticCodePaths(
        IReadOnlyList<IReadOnlyList<int>> Direct,
        IReadOnlyList<IReadOnlyList<int>> Construction);

    internal sealed class StaticDispatchCache
    {
        public readonly ConditionalWeakTable<ExpressionTemplatePlan, Lazy<StackFrameInstructions?>>
            Templates = [];

        public readonly ConcurrentDictionary<
            (Expression, PineValueClass?), Lazy<StaticFunction>>
            Functions = new();

        public readonly ConcurrentDictionary<PineValue, bool> ContainsCode = new();

        public readonly ConcurrentDictionary<Expression, StaticCodePaths> CodePaths = new();

        public readonly ConcurrentDictionary<(Expression, PineValueClass?), Expression> Prepared = new();

        public readonly ConcurrentDictionary<Expression, ReturnValueLayout> Returns = new();
    }

    internal sealed class StaticDispatchCompilation(
        Expression rootExpression,
        IReadOnlyList<PineValueClass> rootSpecializations,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, PineValueClass?, bool> skipInlining,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        int pathMaxLowExclusive,
        int pathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation,
        bool enableDirectInvocation,
        Func<PineValue, bool>? skipDirectInvocation,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses,
        StaticDispatchCache? sharedCache = null,
        bool requireStaticClosure = false)
    {
        private readonly ConcurrentDictionary<
            (Expression, PineValueClass?), Lazy<StaticFunction>>
            _functions = sharedCache?.Functions ?? new();

        private readonly ConcurrentDictionary<PineValue, bool> _containsCode = sharedCache?.ContainsCode ?? new();

        private readonly ConcurrentDictionary<Expression, StaticCodePaths> _codePaths =
            sharedCache?.CodePaths ?? new();

        private readonly ConcurrentDictionary<(Expression, PineValueClass), byte> _inferredFunctions = new();

        private readonly ConcurrentDictionary<(Expression, PineValueClass?), Expression> _prepared =
            sharedCache?.Prepared ?? new();

        private readonly ConcurrentDictionary<Expression, ReturnValueLayout> _returns = sharedCache?.Returns ?? new();

        public ExpressionCompilation CompileExpression()
        {
            var generic = GetFunction(rootExpression, null).Instructions.Value;

            var specialized =
                rootSpecializations
                .OrderDescending(PineValueClassSpecificityComparer.Instance)
                .Select(
                    constraint =>
                    ((IReadOnlyList<EnvConstraintItem>)
                    [.. constraint.ParsedItems.Select(item => new EnvConstraintItem(item.Key.ToArray(), item.Value))],
                    GetFunction(rootExpression, constraint).Instructions.Value))
                .ToArray();

            return new(generic, specialized);
        }

        public StackFrameInstructions? CompileTemplate(ExpressionTemplatePlan plan)
        {
            // Generated code values must not create a specialized template wrapper on every application.
            var constraint =
                InferConstraint(
                    plan.RetainedTerminalExpression,
                    path =>
                    plan.TerminalEnvironmentRecipe.Project(path).TryGetConstant() is { } value &&
                    parseCache.ParseInternalExpression(value).IsOkOrNull() is null
                    ?
                    value
                    :
                    null);

            return constraint is null ? null : GetFunction(plan.RetainedTerminalExpression, constraint).Instructions.Value;
        }

        public StaticFunction GetFunction(Expression expression, PineValueClass? constraint)
        {
            var key = (expression, constraint);

            var function =
                _functions.GetOrAdd(
                    key,
                    key => new Lazy<StaticFunction>(() => PrepareFunction(key.Item1, key.Item2)));

            try
            {
                return function.Value;
            }
            catch
            {
                ((ICollection<KeyValuePair<(Expression, PineValueClass?), Lazy<StaticFunction>>>)_functions)
                    .Remove(new(key, function));

                throw;
            }
        }

        private Expression PrepareBody(Expression expression, PineValueClass? constraint) =>
            _prepared.GetOrAdd(
                (expression, constraint),
                key =>
                PrepareExpression(
                    key.Item1,
                    key.Item2,
                    parseCache,
                    disableReduction,
                    skipInlining,
                    reducedExpressionCache,
                    pathMaxLowExclusive,
                    pathMaxHighInclusive,
                    disableGenericApplicationChainConsolidation));

        private StaticFunction PrepareFunction(Expression expression, PineValueClass? constraint)
        {
            var prepared = PrepareBody(expression, constraint);

            var parameters =
                StaticFunctionInterface.FromExpression(constraint is null ? expression : prepared);

            var returns =
                !enableDirectInvocation ||
                ExpressionTemplatePlan.TryParseTemplate(
                    ExpressionValueEncoding.EncodeExpressionAsValue(prepared), parseCache) is { EnvironmentCount: > 1 }
                ?
                ReturnValueLayout.Canonical
                :
                _returns.GetOrAdd(
                    prepared,
                    body => ReturnShapeInference.Infer(
                        body,
                        parseCache,
                        (callee, encoded, environment) =>
                        skipDirectInvocation?.Invoke(encoded) is true
                        ?
                        null
                        :
                        PrepareBody(
                            callee,
                            InferConstraint(callee, path => KnownValue(path, environment, constraint)))));

            PineControlFlowGraph CreateGraph() =>
                OptimizedControlFlowGraphFromExpression(
                    expression,
                    constraint,
                    parameters,
                    parseCache,
                    disableReduction,
                    enableTailRecursionOptimization,
                    skipInlining,
                    reducedExpressionCache,
                    pathMaxLowExclusive,
                    pathMaxHighInclusive,
                    disableGenericApplicationChainConsolidation,
                    enableDirectInvocation,
                    skipDirectInvocation,
                    (callee, encoded, environment) => ResolveInvocation(callee, encoded, environment, constraint),
                    prepared,
                    returns,
                    requireStaticClosure);

            return
                new(
                    prepared,
                    parameters,
                    returns,
                    CreateGraph,
                    new Lazy<StackFrameInstructions>(
                        () => new StackFrameInstructions(
                            parameters,
                            CreateGraph().LowerToStackInstructions(),
                            constraint,
                            returns)));
        }

        private DirectInvocation? ResolveInvocation(
            Expression expression,
            PineValue encoded,
            Expression environment,
            PineValueClass? callerConstraint)
        {
            var constraint = InferConstraint(expression, path => KnownValue(path, environment, callerConstraint));
            // Constants are retained only when needed for code identity or construction.
            // Bound unusual programs that construct an unending sequence of different code values.
            if (constraint is not null && !_functions.ContainsKey((expression, constraint)))
            {
                if (_inferredFunctions.Count >= 1_024)
                    return null;

                _inferredFunctions.TryAdd((expression, constraint), 0);
            }

            var target = GetFunction(expression, constraint);

            return
                new DirectInvocation(expression, encoded, target.Parameters)
                {
                    EnvironmentConstraint = constraint,
                    PreparedExpression = target.PreparedExpression,
                    ReturnLayout = target.Returns,
                    StaticInstructions = target.Instructions
                };
        }

        private PineValueClass? InferConstraint(Expression expression, Func<IReadOnlyList<int>, PineValue?> knownValue)
        {
            var knownItems = new SortedDictionary<IReadOnlyList<int>, PineValue>(IntPathComparer.Instance);

            var candidates =
                expression == rootExpression
                ?
                rootSpecializations
                :
                compilationEnvClasses?.GetValueOrDefault(expression) ?? [];

            foreach (var candidate in candidates.OrderDescending(PineValueClassSpecificityComparer.Instance))
            {
                if (candidate.ParsedItems.All(
                    item => knownValue(item.Key) is { } value && value.Equals(item.Value)))
                {
                    foreach (var item in candidate.ParsedItems)
                        knownItems[item.Key] = item.Value;

                    break;
                }
            }

            var codePaths =
                _codePaths.GetOrAdd(
                    expression,
                    AnalyzeStaticCodePaths);

            if (!requireStaticClosure)
            {
                foreach (var path in StaticFunctionInterface.FromExpression(expression).ParamsPaths)
                {
                    if (path.Count > 0 &&
                        codePaths.Direct.Any(
                            codePath => codePath.Count >= path.Count && codePath.Take(path.Count).SequenceEqual(path)) &&
                        knownValue(path) is { } value && ContainsCode(value))
                    {
                        knownItems[path] = value;
                    }
                }
            }
            else
            {
                foreach (var codePath in codePaths.Direct.Concat(codePaths.Construction))
                {
                    for (var length = 1; length <= codePath.Count; length++)
                    {
                        IReadOnlyList<int> path = [.. codePath.Take(length)];

                        if (knownItems.ContainsKey(path))
                            break;

                        if (knownValue(path) is not { } value ||
                            !ContainsCode(value) &&
                            !codePaths.Construction.Any(dependency => dependency.SequenceEqual(path)))
                        {
                            continue;
                        }

                        knownItems[path] = value;
                        break;
                    }
                }
            }

            if (knownItems.Count is 0)
                return null;

            return PineValueClass.Create([.. knownItems]);
        }

        private StaticCodePaths AnalyzeStaticCodePaths(Expression root)
        {
            var paths =
                new Dictionary<Expression, (HashSet<IReadOnlyList<int>> Direct, HashSet<IReadOnlyList<int>> Construction)>();

            var calls = new Dictionary<Expression, List<(Expression Target, Expression Environment)>>();
            var pending = new Queue<Expression>();
            pending.Enqueue(root);

            while (pending.TryDequeue(out var body))
            {
                if (paths.ContainsKey(body))
                    continue;

                var direct = new HashSet<IReadOnlyList<int>>(IntPathEqualityComparer.Instance);
                var construction = new HashSet<IReadOnlyList<int>>(IntPathEqualityComparer.Instance);
                paths.Add(body, (direct, construction));
                var staticCalls = new List<(Expression Target, Expression Environment)>();
                calls.Add(body, staticCalls);
                var seen = new HashSet<Expression>();

                foreach (var eval in Expression.EnumerateSelfAndDescendants(body, skipDescendants: item => !seen.Add(item))
                    .OfType<Expression.Eval>())
                {
                    if (ExpressionAnalysis.TryParseExprAsPathInEnv(eval.Encoded) is { } path)
                        direct.Add(path);

                    else if (eval.Encoded.EvalCount == 0)
                    {
                        foreach (var dependency in Expression.EnumerateSelfAndDescendants(eval.Encoded)
                            .Select(ExpressionAnalysis.TryParseExprAsPathInEnv).OfType<IReadOnlyList<int>>())
                        {
                            construction.Add(dependency);
                        }
                    }

                    if (requireStaticClosure && eval.Encoded.EvalCount == 0 && !eval.Encoded.ReferencesEnvironment &&
                        ExpressionReduction.TryEvalIndependent(eval.Encoded, parseCache).Value is { } encoded &&
                        parseCache.ParseInternalExpression(encoded).IsOkOrNull() is { } target)
                    {
                        staticCalls.Add((target, eval.Environment));

                        if (!paths.ContainsKey(target) && paths.Count + pending.Count < 256)
                            pending.Enqueue(target);
                    }
                }
            }

            bool changed;

            do
            {
                changed = false;

                foreach (var (body, staticCalls) in calls)
                {
                    foreach (var (target, environment) in staticCalls)
                    {
                        if (!paths.TryGetValue(target, out var required))
                            continue;

                        foreach (var (dependencies, destination) in new[]
                        {
                            (required.Direct, paths[body].Direct),
                            (required.Construction, paths[body].Construction)
                        })
                        {
                            foreach (var dependency in dependencies.ToArray())
                            {
                                if (destination.Count >= 1024 || dependency.Count >= 64)
                                    continue;

                                var argument = InternalPineIRCompiler.BuildArgument(dependency, environment);

                                if (ExpressionAnalysis.TryParseExprAsPathInEnv(argument) is { } mapped &&
                                    mapped.Count < 64)
                                {
                                    changed |= destination.Add(mapped);
                                }
                            }
                        }
                    }
                }
            }
            while (changed);

            return
                new(
                    [.. paths[root].Direct.Order(IntPathComparer.Instance)],
                    [.. paths[root].Construction.Order(IntPathComparer.Instance)]);
        }

        private PineValue? KnownValue(
            IReadOnlyList<int> path, Expression environment, PineValueClass? callerConstraint = null)
        {
            var argument = InternalPineIRCompiler.BuildArgument(path, environment);

            if (callerConstraint is not null &&
                ExpressionAnalysis.TryParseExprAsPathInEnv(argument) is { } mapped &&
                callerConstraint.TryGetValue(mapped) is { } constrained)
            {
                return constrained;
            }

            if (argument.ReferencesEnvironment || argument.EvalCount > 0)
                return null;

            return ExpressionReduction.TryEvalIndependent(argument, parseCache).Value;
        }

        private bool ContainsCode(PineValue value) =>
            _containsCode.GetOrAdd(
                value,
                candidate =>
                {
                    var pending = new Stack<PineValue>();
                    var seen = new HashSet<PineValue>();
                    pending.Push(candidate);

                    while (pending.TryPop(out var item))
                    {
                        if (item is not PineValue.ListValue list || !seen.Add(item))
                            continue;

                        if (parseCache.ParseInternalExpression(item).IsOkOrNull() is not null)
                            return true;

                        foreach (var child in list.Items.Span)
                            pending.Push(child);
                    }

                    return false;
                });
    }
}
