using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.InternalExpression;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

using Expression = Pine.Core.Runtime.InternalExpression.Expression;
using BuiltinId = Pine.Core.Runtime.InternalExpression.BuiltinFunction;
using Kernel = Pine.Core.PineLanguage.BuiltinFunction;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Compiles Pine expressions into stack-frame instructions for the intermediate VM.
/// </summary>
internal class InternalPineIRCompiler
{
    /// <summary>
    /// Accumulates emitted structured control flow together with the locals reserved for reused expressions.
    /// <para>
    /// The fragment contains no instruction offsets. Sequential stack instructions are only created
    /// after building a <see cref="PineControlFlowGraph"/> from it.
    /// </para>
    /// </summary>
    public record NodeCompilationResult(
        PineControlFlowFragment Fragment,
        ImmutableDictionary<Expression, int> LocalsSet)
    {
        internal ImmutableDictionary<Expression, (ReturnValueLayout Layout, int FirstLocal)> ReturnValues { get; init; } =
            ImmutableDictionary<Expression, (ReturnValueLayout, int)>.Empty;

        internal int ReservedLocals { get; init; }

        internal int NextLocal(int parameterCount) =>
            Math.Max(
                ReservedLocals,
                LocalsSet.IsEmpty ? parameterCount : Math.Max(parameterCount, LocalsSet.Values.Max() + 1));

        /// <summary>
        /// Continues compilation from the current state by compiling another expression.
        /// </summary>
        public NodeCompilationResult ContinueWithExpression(
            Expression expression,
            CompilationContext context,
            PineVMParseCache parseCache)
        {
            var exprResult =
                CompileExpressionTransitive(
                    expression,
                    context with { IsTailPosition = false },
                    this with { Fragment = PineControlFlowFragment.Empty },
                    parseCache);

            return
                exprResult with { Fragment = Fragment.Append(exprResult.Fragment) };
        }

        /// <summary>
        /// Appends a single instruction to the accumulated result.
        /// </summary>
        public NodeCompilationResult AppendInstruction(StackInstruction instruction) =>
            AppendInstructions([instruction]);

        /// <summary>
        /// Appends a sequence of instructions to the accumulated result.
        /// </summary>
        public NodeCompilationResult AppendInstructions(IEnumerable<StackInstruction> instructions) =>
            this with
            {
                Fragment = Fragment.AppendOperations(instructions)
            };

        /// <summary>
        /// Appends a structured control-flow node to the accumulated result.
        /// </summary>
        public NodeCompilationResult AppendNode(PineControlFlowNode node) =>
            this with
            {
                Fragment = Fragment.Append(node)
            };
    }

    /// <summary>
    /// Carries contextual information needed while compiling a single stack frame.
    /// </summary>
    public record CompilationContext(
        ImmutableHashSet<Expression> CopyToLocal,
        TailLoopTarget? TailLoop,
        StaticFunctionInterface StackFrameParameters,
        bool IsTailPosition,
        bool EnableDirectInvocation,
        Func<PineValue, bool>? SkipDirectInvocation,
        Func<Expression, PineValue, Expression, DirectInvocation?>? ResolveStaticInvocation = null,
        bool BindReturnOnly = false,
        bool RequireStaticClosure = false)
    {
        /// <summary>
        /// Creates the initial compilation context for a function with the given parameters.
        /// </summary>
        public static CompilationContext Init(
            StaticFunctionInterface stackFrameParameters,
            bool enableDirectInvocation,
            Func<PineValue, bool>? skipDirectInvocation) =>
            new(
                CopyToLocal: [],
                TailLoop: null,
                StackFrameParameters: stackFrameParameters,
                IsTailPosition: true,
                EnableDirectInvocation: enableDirectInvocation,
                SkipDirectInvocation: skipDirectInvocation);

        /// <summary>
        /// Resolves an expression to an already-available local index when possible.
        /// </summary>
        public int? IsAvailableAsLocalIndex(Expression expression)
        {
            if (ExpressionAnalysis.TryParseExprAsPathInEnv(expression) is { } path)
            {
                for (var i = 0; i < StackFrameParameters.ParamsPaths.Count; ++i)
                {
                    if (StackFrameParameters.ParamsPaths[i].SequenceEqual(path))
                    {
                        return i;
                    }
                }
            }

            if (!CopyToLocal.Contains(expression))
            {
                return null;
            }

            return null;
        }
    }

    /// <summary>
    /// Identifies the root function independently of any particular call occurrence.
    /// </summary>
    public record TailLoopTarget(
        IReadOnlySet<Expression> RootExpressionForms,
        PineValueClass? EnvironmentClass,
        IReadOnlyList<PineValue> GuardExpressionValues);

    private enum TailCallToRootResolution
    {
        Direct,
        KnownNormalInvocation,
        Unresolved,
    }

    /// <summary>
    /// Recursively compile an expression into structured control flow.
    /// </summary>
    public static NodeCompilationResult CompileExpression(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        PineValueClass? envClass,
        StaticFunctionInterface parametersAsLocals,
        PineVMParseCache parseCache,
        bool enableTailRecursionOptimization = false,
        bool enableDirectInvocation = true,
        Func<PineValue, bool>? skipDirectInvocation = null,
        Func<Expression, PineValue, Expression, DirectInvocation?>? resolveStaticInvocation = null,
        ReturnValueLayout? returnLayout = null,
        bool requireStaticClosure = false)
    {
        var prior =
            new NodeCompilationResult(
                Fragment: PineControlFlowFragment.Empty,
                []);

        var rootExpressionForms =
            rootExprAlternativeForms.Add(rootExpression);

        var rootExpressionValues =
            (rootExprAlternativeForms.IsEmpty ? rootExpressionForms : rootExprAlternativeForms)
            .Select(ExpressionValueEncoding.EncodeExpressionAsValue)
            .Distinct()
            .ToArray();

        return
            CompileReturning(
                rootExpression,
                context:
                CompilationContext.Init(
                    parametersAsLocals,
                    enableDirectInvocation,
                    skipDirectInvocation) with
                {
                    ResolveStaticInvocation = resolveStaticInvocation,
                    RequireStaticClosure = requireStaticClosure,
                    TailLoop =
                    enableTailRecursionOptimization
                    ?
                    new TailLoopTarget(
                        RootExpressionForms: rootExpressionForms,
                        EnvironmentClass: envClass,
                        GuardExpressionValues: rootExpressionValues)
                    :
                    null
                },
                prior: prior,
                parseCache,
                returnLayout ?? ReturnValueLayout.Canonical);
    }

    private static NodeCompilationResult CompileReturning(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        ReturnValueLayout layout)
    {
        if (ReferenceEquals(layout, ReturnValueLayout.Canonical))
            return CompileExpressionTransitive(expression, context, prior, parseCache);

        if (expression is Expression.Label label)
            return CompileReturning(label.Tagged, context, prior, parseCache, layout);

        if (expression is Expression.Conditional conditional)
            return CompileConditional(conditional, context, prior, parseCache, layout);

        if (expression is Expression.Switch dispatch)
            return CompileSwitch(dispatch, context, prior, parseCache, layout);

        if (expression is Expression.List list &&
            TryCompileReturningList(list, context, prior, parseCache, layout) is { } directProducer)
        {
            return directProducer;
        }

        var compiled =
            expression is Expression.Eval eval &&
            (prior.ReturnValues.ContainsKey(eval) || !prior.LocalsSet.ContainsKey(eval))
            ?
            CompileEval(eval, context with { BindReturnOnly = true }, prior, parseCache)
            :
            CompileExpressionTransitive(expression, context, prior, parseCache);

        if (compiled.Fragment.Nodes.LastOrDefault() is PineControlFlowNode.JumpToEntry)
            return compiled;

        if (expression is Expression.Eval returningCall &&
            compiled.ReturnValues.TryGetValue(returningCall, out var binding))
        {
            if (binding.Layout.Equals(layout))
            {
                return
                    compiled.AppendInstructions(
                        Enumerable.Range(0, layout.SlotCount).Select(
                            index => StackInstruction.Local_Get(binding.FirstLocal + index)));
            }

            compiled =
                compiled with { Fragment = compiled.Fragment.Append(binding.Layout.Reconstruct(binding.FirstLocal)) };
        }

        var local = compiled.NextLocal(context.StackFrameParameters.ParamsPaths.Count);

        return
            compiled with
            {
                Fragment =
                compiled.Fragment
                .AppendOperation(StackInstruction.Local_Set([local], popCount: 1))
                .Append(layout.Deconstruct(local))
            };
    }

    private static NodeCompilationResult? TryCompileReturningList(
        Expression.List expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        ReturnValueLayout layout)
    {
        if (prior.LocalsSet.ContainsKey(expression) ||
            context.IsAvailableAsLocalIndex(expression) is not null)
        {
            return null;
        }

        static PineValue? LiteralAtPath(Expression item, ImmutableArray<int> path)
        {
            for (var offset = 0; ; offset++)
            {
                while (item is Expression.Label label)
                    item = label.Tagged;

                if (item is Expression.Literal literal)
                    return PineValueExtension.ValueFromPathOrNull(literal.Value, path.AsSpan()[offset..]);

                if (offset == path.Length || item is not Expression.List list ||
                    path[offset] < 0 || path[offset] >= list.Items.Length)
                {
                    return null;
                }

                item = list.Items[path[offset]];
            }
        }

        var activeSlots = new List<int>();

        for (var index = 0; index < layout.Slots.Length; index++)
        {
            var active = true;

            foreach (var guard in layout.Slots[index].Guards)
            {
                if (LiteralAtPath(expression, guard.Path) is not { } value)
                    return null;

                if (!value.Equals(guard.Value))
                {
                    active = false;
                    break;
                }
            }

            if (active)
            {
                if (activeSlots.Count > 0 &&
                    layout.Slots[activeSlots[^1]].Path.AsSpan().SequenceCompareTo(layout.Slots[index].Path.AsSpan()) >
                    0)
                {
                    return null;
                }

                activeSlots.Add(index);
            }
        }

        context = PrepareCommonSubexpressions(expression, context with { IsTailPosition = false }, prior);
        var compiled = prior;
        var nextSlot = 0;

        void FillInactiveSlotsBefore(int index)
        {
            while (nextSlot < index)
            {
                compiled = compiled.AppendInstruction(StackInstruction.Push_Literal(PineValue.EmptyList));
                nextSlot++;
            }
        }

        void CompileProducer(Expression item, int depth, IReadOnlyList<int> slots)
        {
            if (item is Expression.Label label &&
                !context.CopyToLocal.Contains(item) &&
                !compiled.LocalsSet.ContainsKey(item))
            {
                CompileProducer(label.Tagged, depth, slots);
                return;
            }

            if (item is Expression.List list &&
                !context.CopyToLocal.Contains(item) &&
                !compiled.LocalsSet.ContainsKey(item) &&
                !slots.Any(index => layout.Slots[index].Path.Length == depth))
            {
                for (var child = 0; child < list.Items.Length; child++)
                {
                    CompileProducer(
                        list.Items[child],
                        depth + 1,
                        slots.Where(index => layout.Slots[index].Path[depth] == child).ToArray());
                }

                return;
            }

            if (slots.Count is 0 && item is Expression.Literal)
                return;

            if (slots.Count is 1 &&
                layout.Slots[slots[0]] is { } slot && slot.Path.Length == depth)
            {
                FillInactiveSlotsBefore(slots[0]);
                compiled = CompileExpressionTransitive(item, context, compiled, parseCache);

                compiled =
                    compiled with { Fragment = ReturnValueLayout.EncodeSlot(slot, compiled.Fragment) };

                nextSlot++;
                return;
            }

            compiled = CompileExpressionTransitive(item, context, compiled, parseCache);

            if (slots.Count is 0)
            {
                compiled = compiled.AppendInstruction(StackInstruction.Pop);
                return;
            }

            var sourceLocal = compiled.NextLocal(context.StackFrameParameters.ParamsPaths.Count);

            compiled =
                compiled.AppendInstruction(StackInstruction.Local_Set([sourceLocal], popCount: 1)) with
                {
                    ReservedLocals = sourceLocal + 1
                };

            foreach (var index in slots)
            {
                FillInactiveSlotsBefore(index);

                compiled =
                    compiled with
                    {
                        Fragment =
                        compiled.Fragment
                        .Append(ReturnValueLayout.ProjectSlot(layout.Slots[index], sourceLocal, depth))
                    };

                nextSlot++;
            }
        }

        CompileProducer(expression, depth: 0, activeSlots);
        FillInactiveSlotsBefore(layout.SlotCount);

        return compiled;
    }

    private static TailCallToRootResolution ResolveTailCallToRoot(
        Expression.Eval tailCall,
        IReadOnlySet<Expression> rootExpressionForms,
        PineValueClass? envClass,
        PineVMParseCache parseCache)
    {
        var encodedExpression =
            envClass is null
            ?
            tailCall.Encoded
            :
            InternalExpressionCompilation.SubstituteSubexpressionsForEnvironmentConstraint(
                tailCall.Encoded,
                envClass);

        if (TryEvalIndependent(encodedExpression, parseCache) is not { } encodedValue)
        {
            return TailCallToRootResolution.Unresolved;
        }

        if (parseCache.ParseInternalExpression(encodedValue).IsOkOrNull() is not { } parsedExpression)
        {
            return TailCallToRootResolution.KnownNormalInvocation;
        }

        if (!rootExpressionForms.Contains(parsedExpression))
        {
            return TailCallToRootResolution.KnownNormalInvocation;
        }

        if (envClass is null)
        {
            return TailCallToRootResolution.Direct;
        }

        foreach (var constraint in envClass.ParsedItems)
        {
            var nextValueExpression =
                BuildArgument(constraint.Key, tailCall.Environment);

            var nextValueExpressionWithConstraint =
                InternalExpressionCompilation.SubstituteSubexpressionsForEnvironmentConstraint(
                    nextValueExpression,
                    envClass);

            if (TryEvalIndependent(nextValueExpressionWithConstraint, parseCache) is not { } nextValue)
            {
                return TailCallToRootResolution.Unresolved;
            }

            if (!nextValue.Equals(constraint.Value))
            {
                return TailCallToRootResolution.KnownNormalInvocation;
            }
        }

        return TailCallToRootResolution.Direct;
    }

    /// <summary>
    /// Recursively compile an expression into structured control flow.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        CompilationContext context,
        ImmutableDictionary<Expression, int> localIndexFromExpr,
        PineVMParseCache parseCache)
    {
        return
            CompileExpressionTransitive(
                expression,
                context: context,
                new NodeCompilationResult(
                    Fragment: PineControlFlowFragment.Empty,
                    localIndexFromExpr),
                parseCache);
    }

    /// <summary>
    /// Compiles an expression and ensures the resulting value is addressable as a local.
    /// </summary>
    public static (NodeCompilationResult nodeResult, int localIndex)
        CompileExpressionTransitiveAsLocal(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (context.IsAvailableAsLocalIndex(expression) is { } paramLocalIndex)
        {
            var afterParamGet =
                prior
                .AppendInstruction(
                    StackInstruction.Local_Get(paramLocalIndex));

            return (afterParamGet, paramLocalIndex);
        }

        if (prior.LocalsSet.TryGetValue(expression, out var localIndex))
        {
            var afterLocalGet =
                prior
                .AppendInstruction(
                    StackInstruction.Local_Get(localIndex));

            return (afterLocalGet, localIndex);
        }

        var contextSettingLocal =
            context with
            {
                CopyToLocal =
                context.CopyToLocal.Add(expression)
            };

        var compiledExpr =
            CompileExpressionTransitive(
                expression,
                contextSettingLocal,
                prior,
                parseCache);

        var newLocalIndex =
            compiledExpr.LocalsSet[expression];

        return (compiledExpr, newLocalIndex);
    }

    /// <summary>
    /// Marks shared expressions before compiling their individual consumers.
    /// </summary>
    private static CompilationContext PrepareCommonSubexpressions(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        var subexprAppearingMultipleTimesIncludingConditional = new HashSet<Expression>();
        HashSet<Expression>? switchSelectors = null;

        {
            var allSubexpressions = new HashSet<Expression>();

            foreach (var subexpression in Expression.EnumerateSelfAndDescendants(
                expression,
                skipDescendants:
                subexpression =>
                {
                    if (!ExpressionLargeEnoughForCSE(subexpression))
                    {
                        return true;
                    }

                    if (context.IsAvailableAsLocalIndex(subexpression) is not null)
                    {
                        return true;
                    }

                    if (prior.LocalsSet.ContainsKey(subexpression))
                    {
                        return true;
                    }

                    if (subexprAppearingMultipleTimesIncludingConditional.Contains(subexpression))
                    {
                        return true;
                    }

                    if (allSubexpressions.Contains(subexpression))
                    {
                        subexprAppearingMultipleTimesIncludingConditional.Add(subexpression);

                        return true;
                    }

                    allSubexpressions.Add(subexpression);

                    if (subexpression is Expression.Switch dispatch)
                        (switchSelectors ??= []).Add(dispatch.Selector);

                    return false;
                },
                skipConditionalBranches: false))
            {
            }
        }

        var allSubexpressionsUnconditional = new HashSet<Expression>();

        var subexprAppearingMultipleTimesUnconditional = new HashSet<Expression>();

        {
            foreach (var subexpression in Expression.EnumerateSelfAndDescendants(
                expression,
                skipDescendants:
                subexpression =>
                {
                    if (!ExpressionLargeEnoughForCSE(subexpression))
                    {
                        return true;
                    }

                    if (context.IsAvailableAsLocalIndex(subexpression) is not null)
                    {
                        return true;
                    }

                    if (prior.LocalsSet.ContainsKey(subexpression))
                    {
                        return true;
                    }

                    if (subexprAppearingMultipleTimesUnconditional.Contains(subexpression))
                    {
                        return true;
                    }

                    if (allSubexpressionsUnconditional.Contains(subexpression))
                    {
                        subexprAppearingMultipleTimesUnconditional.Add(subexpression);

                        return true;
                    }

                    allSubexpressionsUnconditional.Add(subexpression);

                    return false;
                },
                skipConditionalBranches: true))
            {
            }
        }

        // The equality chain made its selector a reuse candidate even though only
        // the first comparison was unconditional. Keep that eligibility after fusion.
        if (switchSelectors is not null)
            subexprAppearingMultipleTimesIncludingConditional.UnionWith(switchSelectors);

        var copyToLocalNew =
            context.CopyToLocal
            .Union(subexprAppearingMultipleTimesUnconditional)
            .Union(subexprAppearingMultipleTimesIncludingConditional.Intersect(allSubexpressionsUnconditional));

        return context with { CopyToLocal = copyToLocalNew };
    }

    /// <summary>
    /// Recursively compile an expression into structured control flow.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        var lessCSE =
            CompileExpressionTransitiveLessCSE(
                expression,
                PrepareCommonSubexpressions(expression, context, prior),
                prior,
                parseCache);

        if (!prior.LocalsSet.ContainsKey(expression) &&
            context.CopyToLocal.Contains(expression) &&
            context.IsAvailableAsLocalIndex(expression) is null)
        {
            var newLocalIndex = lessCSE.NextLocal(context.StackFrameParameters.ParamsPaths.Count);

            return
                lessCSE
                .AppendInstruction(
                    StackInstruction.Local_Set([newLocalIndex])) with
                {
                    LocalsSet = lessCSE.LocalsSet.Add(expression, newLocalIndex)
                };
        }

        return lessCSE;
    }

    /// <summary>
    /// Compiles an expression without first augmenting the compilation context for extra CSE reuse.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitiveLessCSE(
        Expression expr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (context.IsAvailableAsLocalIndex(expr) is { } paramLocalIndex)
        {
            return
                prior
                .AppendInstruction(
                    StackInstruction.Local_Get(paramLocalIndex));
        }

        if (prior.LocalsSet.TryGetValue(expr, out var localIndex))
        {
            return
                prior
                .AppendInstruction(
                    StackInstruction.Local_Get(localIndex));
        }

        if (expr.EvalCount > 0 && expr is Expression.HeadSkip or Expression.Builtin)
        {
            foreach (var call in Expression.EnumerateSelfAndDescendants(expr).OfType<Expression.Eval>().Distinct())
            {
                if (ExpressionAnalysis.TryParseExprAsPathInExpr(expr, call) is not { Count: > 0 } path)
                    continue;

                var bound = prior;

                if (!bound.ReturnValues.ContainsKey(call))
                {
                    if (!context.EnableDirectInvocation || call.Encoded.EvalCount > 0 ||
                        TryEvalIndependent(call.Encoded, parseCache) is not { } encoded ||
                        context.SkipDirectInvocation?.Invoke(encoded) is true ||
                        ExpressionTemplatePlan.TryParseTemplate(encoded, parseCache) is not null ||
                        parseCache.ParseInternalExpression(encoded).IsOkOrNull() is not { } target ||
                        context.ResolveStaticInvocation?.Invoke(target, encoded, call.Environment) is not { } direct ||
                        ReferenceEquals(direct.ReturnLayout, ReturnValueLayout.Canonical))
                    {
                        continue;
                    }

                    bound =
                        CompileEval(
                            call,
                            context with { BindReturnOnly = true, IsTailPosition = false },
                            bound,
                            parseCache);
                }

                var binding = bound.ReturnValues[call];
                return bound with { Fragment = bound.Fragment.Append(binding.Layout.Reconstruct(binding.FirstLocal, path)) };
            }
        }

        switch (expr)
        {
            case Expression.Literal literalExpr:
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(literalExpr.Value));

            case Expression.Environment:
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Local_Get(0));

            case Expression.List listExpr:
                return
                    CompileList(
                        listExpr,
                        context,
                        prior,
                        parseCache);

            case Expression.Conditional conditional:
                return
                    CompileConditional(
                        conditional,
                        context,
                        prior,
                        parseCache);

            case Expression.Switch dispatch:
                return CompileSwitch(dispatch, context, prior, parseCache);

            case Expression.Eval evalExpr:
                return
                    CompileEval(
                        evalExpr,
                        context,
                        prior,
                        parseCache);

            case Expression.Builtin builtinExpr:
                return
                    CompileBuiltin(
                        builtinExpr,
                        context,
                        prior,
                        parseCache);

            case Expression.Label label:
                return CompileExpressionTransitive(label.Tagged, context, prior, parseCache);

            case Expression.BuiltinBinary binary:
                return CompileBuiltin(binary.Function, ExpressionInput.FromBinary(binary), context, prior, parseCache);

            case Expression.Length_Equal_Binary length:
                return CompileLengthEqual(length, context, prior, parseCache);

            case Expression.HeadSkip head:
                var projectionIndex = context.IsAvailableAsLocalIndex(head.Input);

                if (projectionIndex is null && prior.LocalsSet.TryGetValue(head.Input, out var storedIndex))
                    projectionIndex = storedIndex;

                if (projectionIndex is { } availableIndex)
                    return prior.AppendInstruction(StackInstruction.Local_Get_Skip_Head_Const(availableIndex, head.Count));

                return
                    CompileExpressionTransitive(head.Input, context, prior, parseCache)
                    .AppendInstruction(StackInstruction.Skip_Head_Const(head.Count));

            default:
                throw new NotImplementedException(
                    "Unexpected expression type: " + expr.GetType().Name);
        }
    }

    private static NodeCompilationResult CompileExpressionTransitive(
        ExpressionInput input, CompilationContext context, NodeCompilationResult prior, PineVMParseCache parseCache) =>
        input.GeneralExpression is { } expression
        ?
        CompileExpressionTransitive(expression, context, prior, parseCache)
        :
        CompileList(input, context, prior, parseCache);

    /// <summary>
    /// Compiles a list expression by compiling each item and then constructing the list on the stack.
    /// </summary>
    public static NodeCompilationResult CompileList(
        ExpressionInput listExpr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        context = context with { IsTailPosition = false };

        if (context.EnableDirectInvocation &&
            ExpressionTemplatePlan.TryParseTemplate(
                listExpr.EncodeList(),
                parseCache) is { EnvironmentCount: > 1 } templatePlan)
        {
            return
                prior
                .ContinueWithExpression(
                    context.StackFrameParameters.ParamsPaths.Any(path => path.Count is 0)
                    ?
                    Expression.EnvironmentInstance
                    :
                    Expression.LiteralInstance(PineValue.EmptyList),
                    context,
                    parseCache)
                .AppendInstruction(StackInstruction.CreateTemplate(templatePlan, producer: true));
        }

        if (listExpr.Length is 0)
        {
            return
                prior
                .AppendInstruction(
                    StackInstruction.Push_Literal(PineValue.EmptyList));
        }

        var literalPrefix = new List<PineValue>();

        while (literalPrefix.Count < listExpr.Length &&
            TryEvalIndependent(listExpr[literalPrefix.Count], parseCache) is { } literalValue)
        {
            literalPrefix.Add(literalValue);
        }

        var afterItems = prior;

        for (var i = literalPrefix.Count; i < listExpr.Length; ++i)
        {
            var itemExpr = listExpr[i];

            afterItems =
                CompileExpressionTransitive(
                    itemExpr,
                    context,
                    afterItems,
                    parseCache);
        }

        if (literalPrefix.Count is not 0)
        {
            return
                afterItems
                .AppendInstruction(
                    StackInstruction.Build_List_With_Prefix(
                        PineValue.List([.. literalPrefix]),
                        listExpr.Length - literalPrefix.Count));
        }

        /*
         * Assume that the subsequence for each item only leaves one value on the stack.
         * 
         * (When we need to reuse a value from a subexpression multiple times,
         * we don't use a non-consuming instruction but use local_get instead to copy it)
         * */

        return
            afterItems
            .AppendInstruction(
                StackInstruction.Build_List(listExpr.Length));
    }

    /// <summary>
    /// Compiles a conditional expression with explicit branch jumps,
    /// applying fusion to peel layers of equal/negate from the condition.
    /// </summary>
    public static NodeCompilationResult CompileConditional(
        Expression.Conditional conditional,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        ReturnValueLayout? returnLayout = null)
    {
        if (TryCompileSwitchEqual(conditional, context, prior, parseCache, returnLayout) is { } switchCompilation)
        {
            return switchCompilation;
        }

        // Fuse the condition into a Jump_If_Equal_Const instruction by analyzing the
        // condition expression and extracting a comparison value.
        // This loop peels nested layers of equal/negate to produce the most efficient jump.
        //
        // Pattern 1: condition is equal(var, const) or equal(const, var)
        // → emit var then Jump_If_Equal_Const(const, offset)
        // Pattern 2: condition is negate(expr)
        // → negate(x) is true when x == FalseValue, so emit expr then Jump_If_Equal_Const(FalseValue, offset)

        var conditionExpression = conditional.Condition;
        var jumpLiteralValue = PineKernelValues.TrueValue;

        if (conditionExpression is Expression.Length_Equal_Binary length &&
            TryEvalIndependent(length.Other, parseCache) is { } lengthConstant)
        {
            var afterLength =
                CompileExpressionTransitive(
                    length.Sequence,
                    context with { IsTailPosition = false },
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Length);

            return
                CompileConditionalBranches(
                    afterLength,
                    lengthConstant,
                    conditional.FalseBranch,
                    conditional.TrueBranch,
                    context,
                    parseCache,
                    returnLayout);
        }

        while (true)
        {
            if (TryBinaryEqual(conditionExpression) is { } equal)
            {
                if (jumpLiteralValue == PineKernelValues.TrueValue)
                {
                    if (TryEvalIndependent(equal.Left, parseCache) is { } leftConst)
                    {
                        conditionExpression = equal.Right;
                        jumpLiteralValue = leftConst;
                        continue;
                    }

                    if (TryEvalIndependent(equal.Right, parseCache) is { } rightConst)
                    {
                        conditionExpression = equal.Left;
                        jumpLiteralValue = rightConst;
                        continue;
                    }
                }

                break;
            }

            if (conditionExpression is Expression.Builtin { Function: BuiltinId.Negate } condBuiltin)
            {
                conditionExpression = condBuiltin.Input;

                jumpLiteralValue =
                    jumpLiteralValue == PineKernelValues.TrueValue
                    ?
                    PineKernelValues.FalseValue
                    :
                    PineKernelValues.TrueValue;

                continue;
            }

            break;
        }

        return
            CompileConditionalWithoutFusion(
                conditionExpression: conditionExpression,
                jumpLiteralValue: jumpLiteralValue,
                falseBranch: conditional.FalseBranch,
                trueBranch: conditional.TrueBranch,
                context: context,
                prior: prior,
                parseCache: parseCache,
                returnLayout: returnLayout);
    }

    private static NodeCompilationResult? TryCompileSwitchEqual(
        Expression.Conditional conditional,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        ReturnValueLayout? returnLayout)
    {
        if (TryParseEqualCondition(conditional.Condition, parseCache) is not { } firstCondition)
        {
            return null;
        }

        var cases =
            new List<(PineValue Literal, Expression Branch)>
            {
                (firstCondition.Literal, conditional.TrueBranch)
            };

        var comparedExpression = firstCondition.ComparedExpression;
        var defaultBranch = conditional.FalseBranch;

        while (defaultBranch is Expression.Conditional nestedConditional &&
            TryParseEqualCondition(nestedConditional.Condition, parseCache) is { } nestedCondition &&
            nestedCondition.ComparedExpression == comparedExpression)
        {
            if (cases.Any(item => item.Literal == nestedCondition.Literal))
            {
                return null;
            }

            cases.Add((nestedCondition.Literal, nestedConditional.TrueBranch));
            defaultBranch = nestedConditional.FalseBranch;
        }

        while (defaultBranch is Expression.Switch dispatch && dispatch.Selector == comparedExpression)
        {
            foreach (var arm in dispatch.Arms)
            {
                if (cases.Any(item => item.Literal == arm.Key))
                    return null;

                cases.Add((arm.Key, arm.Branch));
            }

            defaultBranch = dispatch.Default;
        }

        return
            TryCompileSwitchEqual(
                comparedExpression,
                cases,
                defaultBranch,
                context,
                prior,
                parseCache,
                returnLayout);
    }

    private static NodeCompilationResult CompileSwitch(
        Expression.Switch dispatch,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        ReturnValueLayout? returnLayout = null)
    {
        var cases = new List<(PineValue Literal, Expression Branch)>(dispatch.Arms.Count);
        var seen = new HashSet<PineValue>();

        foreach (var arm in dispatch.Arms)
        {
            if (seen.Add(arm.Key))
                cases.Add((arm.Key, arm.Branch));
        }

        return
            TryCompileSwitchEqual(
                dispatch.Selector,
                cases,
                dispatch.Default,
                context,
                prior,
                parseCache,
                returnLayout) ??
            CompileConditionalWithoutFusion(
                conditionExpression: dispatch.Selector,
                jumpLiteralValue: cases[0].Literal,
                falseBranch: dispatch.Default,
                trueBranch: cases[0].Branch,
                context: context,
                prior: prior,
                parseCache: parseCache,
                returnLayout: returnLayout);
    }

    private static NodeCompilationResult? TryCompileSwitchEqual(
        Expression comparedExpression,
        IReadOnlyList<(PineValue Literal, Expression Branch)> cases,
        Expression defaultBranch,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        ReturnValueLayout? returnLayout)
    {
        (Expression sourceExpr, Expression skipCountExpr)? sliceSkipVar = null;

        if (TryParse_Builtin_Take_Const(comparedExpression, parseCache) is { } parsedTake &&
            parsedTake.takeCount > 0 &&
            TryParse_Builtin_Skip(parsedTake.sourceExpr) is { } parsedSkip &&
            cases.All(
                switchCase =>
                switchCase.Literal switch
                {
                    PineValue.BlobValue blobValue =>
                    blobValue.Bytes.Length == parsedTake.takeCount,

                    PineValue.ListValue listValue =>
                    listValue.Items.Length == parsedTake.takeCount,

                    _ =>
                    false
                }))
        {
            sliceSkipVar = (parsedSkip.sourceExpr, parsedSkip.skipCountExpr);
        }

        if (cases.Count < 2 && sliceSkipVar is null)
        {
            return null;
        }

        NodeCompilationResult afterCondition;
        BigInteger skipCountMultiplier = 1;

        if (sliceSkipVar is { } parsedSliceSkipVar)
        {
            var afterSource =
                CompileExpressionTransitive(
                    parsedSliceSkipVar.sourceExpr,
                    context with { IsTailPosition = false },
                    prior,
                    parseCache);

            afterCondition =
                CompileExpressionTransitive(
                    parsedSliceSkipVar.skipCountExpr,
                    context with { IsTailPosition = false },
                    afterSource,
                    parseCache);

            var nodes = afterCondition.Fragment.Nodes;

            if (nodes.Count > afterSource.Fragment.Nodes.Count &&
                nodes[^1] is PineControlFlowNode.Operation
                {
                    Instruction:
                    {
                        Kind: StackInstructionKind.Int_Mul_Const,
                        IntegerLiteral: { } multiplier
                    }
                })
            {
                skipCountMultiplier = multiplier;

                afterCondition =
                    afterCondition with
                    {
                        Fragment = afterCondition.Fragment with { Nodes = nodes.RemoveAt(nodes.Count - 1) }
                    };
            }
        }
        else
        {
            afterCondition =
                CompileExpressionTransitive(
                    comparedExpression,
                    context with { IsTailPosition = false },
                    prior,
                    parseCache);
        }

        var defaultBranchFragment =
            CompileReturning(
                defaultBranch,
                context,
                afterCondition with { Fragment = PineControlFlowFragment.Empty },
                parseCache,
                returnLayout ?? ReturnValueLayout.Canonical)
            .Fragment;

        var distinctCaseBranches = new List<Expression>();
        var distinctCaseBranchIndexes = new Dictionary<Expression, int>();
        var switchCases = ImmutableArray.CreateBuilder<PineSwitchFragmentCase>(cases.Count);

        for (var caseIndex = 0; caseIndex < cases.Count; caseIndex++)
        {
            if (!distinctCaseBranchIndexes.TryGetValue(cases[caseIndex].Branch, out var branchIndex))
            {
                branchIndex = distinctCaseBranches.Count;
                distinctCaseBranches.Add(cases[caseIndex].Branch);
                distinctCaseBranchIndexes.Add(cases[caseIndex].Branch, branchIndex);
            }

            switchCases.Add(new PineSwitchFragmentCase(cases[caseIndex].Literal, branchIndex));
        }

        var caseBranchFragments =
            ImmutableArray.CreateBuilder<PineControlFlowFragment>(distinctCaseBranches.Count);

        for (var branchIndex = 0; branchIndex < distinctCaseBranches.Count; branchIndex++)
        {
            caseBranchFragments.Add(
                CompileReturning(
                    distinctCaseBranches[branchIndex],
                    context,
                    afterCondition with { Fragment = PineControlFlowFragment.Empty },
                    parseCache,
                    returnLayout ?? ReturnValueLayout.Canonical)
                .Fragment);
        }

        return
            afterCondition.AppendNode(
                new PineControlFlowNode.Switch(
                    Kind: sliceSkipVar is null ? PineSwitchKind.Equal : PineSwitchKind.SliceSkipVarEqual,
                    Cases: switchCases.MoveToImmutable(),
                    Default: defaultBranchFragment,
                    Branches: caseBranchFragments.MoveToImmutable(),
                    SkipCountMultiplier: skipCountMultiplier,
                    ResultCount: returnLayout?.SlotCount ?? 1));
    }

    private static (Expression ComparedExpression, PineValue Literal)? TryParseEqualCondition(
        Expression condition,
        PineVMParseCache parseCache)
    {
        if (TryBinaryEqual(condition) is not { } equalInput)
        {
            return null;
        }

        if (TryEvalIndependent(equalInput.Left, parseCache) is { } leftLiteral)
        {
            return (equalInput.Right, leftLiteral);
        }

        if (TryEvalIndependent(equalInput.Right, parseCache) is { } rightLiteral)
        {
            return (equalInput.Left, rightLiteral);
        }

        return null;
    }

    private static (Expression Left, Expression Right)? TryBinaryEqual(Expression? expression) =>
        TryBinaryBuiltin(expression, BuiltinId.Equal);

    private static (Expression Left, Expression Right)? TryBinaryBuiltin(Expression? expression, BuiltinId function) =>
        expression switch
        {
            Expression.BuiltinBinary binary => binary.Function == function ? (binary.Left, binary.Right) : null,

            Expression.Builtin builtin =>
            builtin.Function == function &&
            builtin.Input is Expression.List { Items: [var left, var right] }
            ?
            (left, right)
            :
            null,

            Expression.Literal or Expression.List or Expression.Eval or Expression.Conditional or Expression.Switch or
            Expression.Environment or Expression.Label or Expression.Length_Equal_Binary or Expression.HeadSkip =>
            null,

            null => null,

            _ =>
            throw new NotImplementedException(
                nameof(TryBinaryBuiltin) + " does not handle expression variant: " + expression.GetType().Name)
        };

    /// <summary>
    /// Compiles a conditional with an already-resolved condition expression and jump literal,
    /// emitting condition instructions followed by branch jumps.
    /// </summary>
    public static NodeCompilationResult CompileConditionalWithoutFusion(
        Expression conditionExpression,
        PineValue jumpLiteralValue,
        Expression falseBranch,
        Expression trueBranch,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        ReturnValueLayout? returnLayout = null)
    {
        var afterCondition =
            CompileExpressionTransitive(
                conditionExpression,
                context with { IsTailPosition = false },
                prior,
                parseCache);

        return
            CompileConditionalBranches(
                afterCondition,
                jumpLiteralValue,
                falseBranch,
                trueBranch,
                context,
                parseCache,
                returnLayout);
    }

    private static NodeCompilationResult CompileConditionalBranches(
        NodeCompilationResult afterCondition,
        PineValue jumpLiteralValue,
        Expression falseBranch,
        Expression trueBranch,
        CompilationContext context,
        PineVMParseCache parseCache,
        ReturnValueLayout? returnLayout)
    {
        var falseBranchFragment =
            CompileReturning(
                falseBranch,
                context,
                afterCondition with { Fragment = PineControlFlowFragment.Empty },
                parseCache,
                returnLayout ?? ReturnValueLayout.Canonical)
            .Fragment;

        var trueBranchFragment =
            CompileReturning(
                trueBranch,
                context,
                afterCondition with { Fragment = PineControlFlowFragment.Empty },
                parseCache,
                returnLayout ?? ReturnValueLayout.Canonical)
            .Fragment;

        return
            afterCondition.AppendNode(
                new PineControlFlowNode.Conditional(
                    Literal: jumpLiteralValue,
                    FallThrough: falseBranchFragment,
                    Branch: trueBranchFragment,
                    ResultCount: returnLayout?.SlotCount ?? 1));
    }

    /// <summary>
    /// Compiles an <c>Eval</c> expression, including direct parsing shortcuts and loop-based tail calls.
    /// </summary>
    public static NodeCompilationResult CompileEval(
        Expression.Eval evalExpr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        var bindReturnOnly = context.BindReturnOnly;
        context = context with { BindReturnOnly = false };

        if (prior.ReturnValues.TryGetValue(evalExpr, out var binding))
        {
            return
                bindReturnOnly
                ?
                prior
                :
                prior with { Fragment = prior.Fragment.Append(binding.Layout.Reconstruct(binding.FirstLocal)) };
        }

        if (context.RequireStaticClosure && evalExpr.Encoded is Expression.List && evalExpr.Encoded.EvalCount == 0 &&
            ExpressionReduction.TryDecodeApplicationOfConstructedEncoding(
                evalExpr.Encoded, evalExpr.Environment, parseCache) is
            Expression.Eval { Encoded: Expression.Literal } decodedCall)
        {
            var (bound, _) =
                CompileExpressionTransitiveAsLocal(
                    evalExpr.Environment,
                    context with { IsTailPosition = false },
                    prior,
                    parseCache);

            var compiled =
                CompileEval(
                    decodedCall,
                    context with { BindReturnOnly = bindReturnOnly },
                    bound.AppendInstruction(StackInstruction.Pop),
                    parseCache);

            return
                compiled.ReturnValues.TryGetValue(decodedCall, out var decodedBinding)
                ?
                compiled with { ReturnValues = compiled.ReturnValues.SetItem(evalExpr, decodedBinding) }
                :
                compiled;
        }

        if (context.IsTailPosition &&
            context.TailLoop is { } tailLoop)
        {
            var tailCallResolution =
                ResolveTailCallToRoot(
                    evalExpr,
                    tailLoop.RootExpressionForms,
                    tailLoop.EnvironmentClass,
                    parseCache);

            if (tailCallResolution is TailCallToRootResolution.KnownNormalInvocation ||
                tailCallResolution is TailCallToRootResolution.Unresolved &&
                tailLoop.EnvironmentClass is not null)
            {
                return CompileNormalEval(evalExpr, context, prior, parseCache, bindReturnOnly);
            }

            if (tailCallResolution is TailCallToRootResolution.Unresolved)
            {
                return
                    CompileGuardedJumpToLoop(
                        evalExpr,
                        tailLoop.GuardExpressionValues,
                        context,
                        prior,
                        parseCache);
            }

            var afterEnvironment =
                CompileLoopArguments(
                    evalExpr.Environment,
                    context,
                    prior,
                    parseCache);

            return
                afterEnvironment
                .AppendNode(PineControlFlowNode.JumpToEntry.Instance);
        }

        return CompileNormalEval(evalExpr, context, prior, parseCache, bindReturnOnly);
    }

    private static NodeCompilationResult CompileNormalEval(
        Expression.Eval evalExpr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache,
        bool bindReturnOnly = false)
    {
        if (ExpressionAnalysis.ParseGenericFunctionApplication(evalExpr)
            is { } genericApplication &&
            genericApplication.arguments.Count > 1 &&
            CanFuseGenericApplication(genericApplication.functionExpr, parseCache))
        {
            var childContext = context with { IsTailPosition = false };
            var afterArguments = prior;

            for (var i = genericApplication.arguments.Count - 1; i >= 0; --i)
            {
                afterArguments =
                    afterArguments.ContinueWithExpression(
                        genericApplication.arguments[i],
                        childContext,
                        parseCache);
            }

            var afterFunction =
                afterArguments.ContinueWithExpression(
                    genericApplication.functionExpr,
                    childContext,
                    parseCache);

            return
                afterFunction.AppendInstruction(
                    StackInstruction.Eval_Multi(genericApplication.arguments.Count));
        }

        static bool CanFuseGenericApplication(
            Expression functionExpression,
            PineVMParseCache parseCache)
        {
            if (functionExpression is not Expression.Literal functionLiteral)
                return true;

            return
                ExpressionTemplatePlan.TryParseTemplate(
                    functionLiteral.Value,
                    parseCache)
                is { EnvironmentCount: > 0 };
        }

        if (evalExpr.Encoded.EvalCount is 0 &&
            TryEvalIndependent(evalExpr.Encoded, parseCache) is { } encodedValue)
        {
            if (parseCache.ParseInternalExpression(encodedValue).IsErrOrNull() is { } parseError)
            {
                return
                    prior.ContinueWithExpression(evalExpr.Environment, context, parseCache)
                    .AppendInstruction(
                        new(
                            StackInstructionKind.Parse_Error_Const,
                            Literal: Internal.PineValueInProcess.Create(encodedValue))
                        {
                            ParseError = parseError
                        });
            }

            if (context.EnableDirectInvocation &&
                !(context.SkipDirectInvocation?.Invoke(encodedValue) ?? false) &&
                ExpressionTemplatePlan.TryParseTemplate(
                    encodedValue,
                    parseCache) is { } templatePlan &&
                (!context.RequireStaticClosure || templatePlan.EnvironmentCount > 1))
            {
                return
                    prior
                    .ContinueWithExpression(evalExpr.Environment, context, parseCache)
                    .AppendInstruction(StackInstruction.CreateTemplate(templatePlan, producer: false));
            }

            if (context.EnableDirectInvocation &&
                !(context.SkipDirectInvocation?.Invoke(encodedValue) ?? false) &&
                parseCache.ParseInternalExpression(encodedValue).IsOkOrNull() is { } targetExpression)
            {
                var resolvedInvocation =
                    context.ResolveStaticInvocation?.Invoke(targetExpression, encodedValue, evalExpr.Environment);

                var targetParameters =
                    resolvedInvocation?.InvocationInterface ??
                    StaticFunctionInterface.FromExpression(targetExpression);

                var call =
                    CompileInvocationArguments(
                        targetParameters,
                        evalExpr.Environment,
                        context,
                        prior,
                        parseCache)
                    .AppendInstruction(
                        resolvedInvocation is null
                        ?
                        StackInstruction.Invoke_StackFrame_Const(
                            targetExpression,
                            encodedValue,
                            targetParameters)
                        :
                        new StackInstruction(
                            StackInstructionKind.Invoke_StackFrame_Const,
                            TakeCount: targetParameters.ParamsPaths.Count,
                            OptimizedInvocation: resolvedInvocation));

                if (resolvedInvocation is null ||
                    ReferenceEquals(resolvedInvocation.ReturnLayout, ReturnValueLayout.Canonical))
                {
                    return call;
                }

                var firstLocal = call.NextLocal(context.StackFrameParameters.ParamsPaths.Count);

                for (var index = resolvedInvocation.ReturnLayout.SlotCount - 1; index >= 0; index--)
                    call = call.AppendInstruction(StackInstruction.Local_Set([firstLocal + index], popCount: 1));

                call =
                    call with
                    {
                        ReturnValues =
                        call.ReturnValues.SetItem(evalExpr, (resolvedInvocation.ReturnLayout, firstLocal)),
                        ReservedLocals = firstLocal + resolvedInvocation.ReturnLayout.SlotCount
                    };

                return
                    bindReturnOnly
                    ?
                    call
                    :
                    call with { Fragment = call.Fragment.Append(resolvedInvocation.ReturnLayout.Reconstruct(firstLocal)) };
            }

            return
                prior
                .ContinueWithExpression(
                    evalExpr.Environment,
                    context,
                    parseCache)
                .AppendInstruction(
                    StackInstruction.Eval_Const(encodedValue));
        }

        var afterEnvironment =
            prior.ContinueWithExpression(
                evalExpr.Environment,
                context,
                parseCache);

        var afterExpr =
            afterEnvironment.ContinueWithExpression(
                evalExpr.Encoded,
                context,
                parseCache);

        return
            afterExpr
            .AppendInstruction(
                StackInstruction.Eval_Binary);
    }

    private static NodeCompilationResult CompileGuardedJumpToLoop(
        Expression.Eval evalExpr,
        IReadOnlyList<PineValue> guardExpressionValues,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (guardExpressionValues.Count is not 1)
        {
            return CompileNormalEval(evalExpr, context, prior, parseCache);
        }

        var (afterEncoded, encodedLocalIndex) =
            CompileExpressionTransitiveAsLocal(
                evalExpr.Encoded,
                context with { IsTailPosition = false },
                prior,
                parseCache);

        var genericBranch =
            CompileExpressionTransitive(
                evalExpr.Environment,
                context,
                afterEncoded with { Fragment = PineControlFlowFragment.Empty },
                parseCache)
            .AppendInstruction(StackInstruction.Local_Get(encodedLocalIndex))
            .AppendInstruction(StackInstruction.Eval_Binary);

        var loopBranch =
            CompileLoopArguments(
                evalExpr.Environment,
                context,
                afterEncoded with { Fragment = PineControlFlowFragment.Empty },
                parseCache)
            .AppendNode(PineControlFlowNode.JumpToEntry.Instance);

        return
            afterEncoded
            .AppendNode(
                new PineControlFlowNode.Conditional(
                    Literal: guardExpressionValues[0],
                    FallThrough: genericBranch.Fragment,
                    Branch: loopBranch.Fragment));
    }

    private static NodeCompilationResult CompileLoopArguments(
        Expression nextEnvironment,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        var result =
            CompileInvocationArguments(
                context.StackFrameParameters,
                nextEnvironment,
                context,
                prior,
                parseCache);

        var parameterCount =
            context.StackFrameParameters.ParamsPaths.Count;

        if (parameterCount is not 0)
        {
            result =
                result
                .AppendInstruction(
                    StackInstruction.Local_Set(
                        [.. Enumerable.Range(0, parameterCount).Reverse()]))
                .AppendInstruction(StackInstruction.PopMultiple(parameterCount));
        }

        return result;
    }

    private static NodeCompilationResult CompileInvocationArguments(
        StaticFunctionInterface targetParameters,
        Expression environment,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        var result = PrepareArgumentEvaluations(environment, context, prior, parseCache);

        var arguments =
            targetParameters.ParamsPaths.Select(path => BuildArgument(path, environment)).ToArray();

        var argumentContext =
            PrepareCommonSubexpressions(
                Expression.ListInst(arguments),
                context with { IsTailPosition = false },
                result);

        for (var parameterIndex = 0;
            parameterIndex < arguments.Length;
            parameterIndex++)
        {
            result =
                CompileExpressionTransitive(
                    arguments[parameterIndex],
                    argumentContext,
                    result,
                    parseCache);
        }

        return result;
    }

    private static NodeCompilationResult PrepareArgumentEvaluations(
        Expression environment,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (environment.EvalCount is 0)
            return prior;

        if (environment is Expression.List list)
        {
            var result = prior;

            foreach (var item in list.Items)
                result = PrepareArgumentEvaluations(item, context, result, parseCache);

            return result;
        }

        var (compiled, _) =
            CompileExpressionTransitiveAsLocal(
                environment,
                context with { IsTailPosition = false },
                prior,
                parseCache);

        return compiled.AppendInstruction(StackInstruction.Pop);
    }

    internal static Expression BuildArgument(
        IReadOnlyList<int> path,
        Expression environment)
    {
        var current = environment;

        for (var pathIndex = 0; pathIndex < path.Count; pathIndex++)
        {
            if (current is Expression.List list &&
                path[pathIndex] >= 0 &&
                path[pathIndex] < list.Items.Length)
            {
                current = list.Items[path[pathIndex]];
                continue;
            }

            return
                ExpressionAnalysis.BuildExpressionForPathInExpression(
                    [.. path.Skip(pathIndex)],
                    current);
        }

        return current;
    }

    /// <summary>
    /// Compile a <see cref="Expression.Builtin"/> to instructions, including dispatching to specialized compilation methods for
    /// individual built-in functions and patterns of arguments.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin(
        Expression.Builtin builtinExpr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
        => CompileBuiltin(builtinExpr.Function, builtinExpr.Input, context, prior, parseCache);

    private static NodeCompilationResult CompileBuiltin(
        BuiltinId function,
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        context = context with { IsTailPosition = false };

        return
            function switch
            {
                BuiltinId.Length =>
                CompileExpressionTransitive(
                    input,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Length),

                BuiltinId.Negate =>
                CompileBuiltin_Negate(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.Equal =>
                CompileBuiltin_Equal(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.Head =>
                CompileBuiltin_Head(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.Skip =>
                CompileBuiltin_Skip(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.Take =>
                CompileBuiltin_Take(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.Concat =>
                CompileBuiltin_Concat(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.Reverse =>
                CompileBuiltin_Reverse(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.IntAdd =>
                CompileBuiltin_Int_Add(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.IntMul =>
                CompileBuiltin_Int_Mul(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.IntIsSortedAsc =>
                CompileBuiltin_Int_Is_Sorted_Asc(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.BitAnd =>
                CompileBuiltin_Bit_And(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.BitOr =>
                CompileBuiltin_Bit_Or(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.BitXor =>
                CompileBuiltin_Bit_Xor(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.BitNot =>
                CompileBuiltin_Bit_Not(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.BitShiftLeft =>
                CompileBuiltin_Bit_Shift_Left(
                    input,
                    context,
                    prior,
                    parseCache),

                BuiltinId.BitShiftRight =>
                CompileBuiltin_Bit_Shift_Right(
                    input,
                    context,
                    prior,
                    parseCache),

                _ =>
                throw new NotImplementedException(
                    "Unknown built-in function: " + function),
            };
    }

    /// <summary>
    /// Compiles the equality built-in function application and its specialized fast paths.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Equal(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input.IsList && input.Length is 2)
            return CompileBinaryEqual(input[0], input[1], context, prior, parseCache);

        return
            CompileExpressionTransitive(input, context, prior, parseCache)
            .AppendInstruction(StackInstruction.Equal_Generic);
    }

    private static NodeCompilationResult CompileBinaryEqual(
        Expression left,
        Expression right,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (Builtin_Equal_TryParse_Starting_With(left, right, parseCache) is { } startingWith)
        {
            if (TryParse_Builtin_Skip(startingWith.expr) is { } skip)
            {
                var afterSliced =
                    CompileExpressionTransitive(
                        skip.sourceExpr,
                        context,
                        prior,
                        parseCache);

                var afterSkipCount =
                    CompileExpressionTransitive(
                        skip.skipCountExpr,
                        context,
                        afterSliced,
                        parseCache);

                return
                    afterSkipCount
                    .AppendInstruction(
                        StackInstruction.Slice_Skip_Var_Equal_Const(startingWith.start));
            }

            {
                var afterSliced =
                    CompileExpressionTransitive(
                        startingWith.expr,
                        context,
                        prior,
                        parseCache);

                if (startingWith.start is PineValue.ListValue startList && startList.Items.Length is 0)
                {
                    return
                        afterSliced
                        .AppendInstruction(StackInstruction.Is_List_Value);
                }

                if (startingWith.start is PineValue.BlobValue startBlob && startBlob.Bytes.Length is 0)
                {
                    return
                        afterSliced
                        .AppendInstruction(StackInstruction.Is_Blob_Value);
                }

                return
                    afterSliced
                    .AppendInstruction(
                        StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(0)))
                    .AppendInstruction(
                        StackInstruction.Slice_Skip_Var_Equal_Const(startingWith.start));
            }
        }

        NodeCompilationResult ContinueForConst(
            PineValue partConst,
            Expression partVar)
        {
            if (IntegerEncoding.ParseSignedIntegerRelaxed(partConst).IsOkOrNullable() is { } constInteger &&
                partVar is Expression.Builtin varBuiltin &&
                varBuiltin.Function is BuiltinId.Length)
            {
                var afterVar =
                    CompileExpressionTransitive(
                        varBuiltin.Input,
                        context,
                        prior,
                        parseCache);

                return
                    afterVar
                    .AppendInstruction(
                        StackInstruction.Length_Equal_Const(constInteger));
            }

            {
                var afterVar =
                    CompileExpressionTransitive(
                        partVar,
                        context,
                        prior,
                        parseCache);

                return
                    afterVar
                    .AppendInstruction(
                        StackInstruction.Equal_Binary_Const(partConst));
            }
        }

        if (TryEvalIndependent(left, parseCache) is { } leftLiteralValue)
        {
            return ContinueForConst(leftLiteralValue, right);
        }

        if (TryEvalIndependent(right, parseCache) is { } rightLiteralValue)
        {
            return ContinueForConst(rightLiteralValue, left);
        }

        {
            var afterLeft =
                CompileExpressionTransitive(
                    left,
                    context,
                    prior,
                    parseCache);

            var afterRight =
                CompileExpressionTransitive(
                    right,
                    context,
                    afterLeft,
                    parseCache);

            return
                afterRight
                .AppendInstruction(StackInstruction.Equal_Binary);
        }
    }

    private static (Expression expr, PineValue start)? Builtin_Equal_TryParse_Starting_With(
        Expression left,
        Expression right,
        PineVMParseCache parseCache)
    {
        (Expression expr, PineValue start)? ContinueWithConst(
            PineValue partConst,
            Expression partVar)
        {
            if (TryParse_Builtin_Take_Const(partVar, parseCache) is { } varTake)
            {
                if (partConst is PineValue.BlobValue leftBlob &&
                    leftBlob.Bytes.Length == varTake.takeCount)
                {
                    return (varTake.sourceExpr, leftBlob);
                }

                if (partConst is PineValue.ListValue leftList &&
                    leftList.Items.Length == varTake.takeCount)
                {
                    return (varTake.sourceExpr, leftList);
                }
            }

            return null;
        }

        {
            if (TryEvalIndependent(left, parseCache) is { } leftValue)
            {
                return ContinueWithConst(leftValue, right);
            }
        }

        {
            if (TryEvalIndependent(right, parseCache) is { } rightValue)
            {
                return ContinueWithConst(rightValue, left);
            }
        }

        return null;
    }

    private static NodeCompilationResult CompileLengthEqual(
        Expression.Length_Equal_Binary expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (TryEvalIndependent(expression.Other, parseCache) is { } constant &&
            IntegerEncoding.ParseSignedIntegerRelaxed(constant).IsOkOrNullable() is { } integer &&
            IntegerEncoding.EncodeSignedInteger(integer).Equals(constant))
        {
            return
                CompileExpressionTransitive(expression.Sequence, context, prior, parseCache)
                .AppendInstruction(StackInstruction.Length_Equal_Const(integer));
        }

        if (expression.LengthOnLeft)
        {
            return
                CompileExpressionTransitive(
                    expression.Other,
                    context,
                    CompileExpressionTransitive(expression.Sequence, context, prior, parseCache)
                    .AppendInstruction(StackInstruction.Length),
                    parseCache)
                .AppendInstruction(StackInstruction.Equal_Binary);
        }

        return
            CompileExpressionTransitive(
                expression.Sequence,
                context,
                CompileExpressionTransitive(expression.Other, context, prior, parseCache),
                parseCache)
            .AppendInstruction(StackInstruction.Length)
            .AppendInstruction(StackInstruction.Equal_Binary);
    }

    /// <summary>
    /// Compiles the head built-in function application, including head-of-skip specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Head(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (TryParse_Builtin_Skip(input.GeneralExpression) is { } skip)
        {
            var afterSource =
                CompileExpressionTransitive(
                    skip.sourceExpr,
                    context,
                    prior,
                    parseCache);

            if (TryParse_IndependentSignedIntegerRelaxed(skip.skipCountExpr, parseCache) is { } skipCountConst)
            {
                if (afterSource.Fragment.LastOperationOrNull is
                    {
                        Kind: StackInstructionKind.Local_Get,
                        LocalIndices: { Length: 1 } localIndices
                    })
                {
                    return
                        afterSource with
                        {
                            Fragment =
                            afterSource.Fragment.ReplaceLastOperation(
                                StackInstruction.Local_Get_Skip_Head_Const(
                                    localIndices[0],
                                    (int)skipCountConst))
                        };
                }

                return
                    afterSource
                    .AppendInstruction(
                        StackInstruction.Skip_Head_Const((int)skipCountConst));
            }

            var afterSkipCount =
                CompileExpressionTransitive(
                    skip.skipCountExpr,
                    context,
                    afterSource,
                    parseCache);

            return
                afterSkipCount
                .AppendInstruction(StackInstruction.Skip_Head_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Head_Generic);
    }

    /// <summary>
    /// Compiles the skip built-in function application and its numeric specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Skip(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr && listExpr.Length is 2)
        {
            var skipCountExpr = listExpr[0];

            var skipSourceExpr = listExpr[1];

            var afterSource =
                CompileExpressionTransitive(
                    skipSourceExpr,
                    context,
                    prior,
                    parseCache);

            if (TryParse_IndependentSignedIntegerRelaxed(skipCountExpr, parseCache) is { } skipCount)
            {
                if (skipCount.IsOne &&
                    TryBinaryBuiltin(skipSourceExpr, BuiltinId.IntAdd) is { } addition)
                {
                    NodeCompilationResult? ContinueForAddZeroOperand(Expression addZeroOperand)
                    {
                        if (TryBinaryBuiltin(addZeroOperand, BuiltinId.Concat) is { } concat &&
                            TryEvalIndependent(concat.Left, parseCache) is { } prependValue &&
                            prependValue is PineValue.BlobValue prependBlob &&
                            prependBlob.Bytes.Length is 1 &&
                            prependBlob.Bytes.Span[0] is 2 or 4)
                        {
                            var afterSource =
                                CompileExpressionTransitive(
                                    concat.Right,
                                    context,
                                    prior,
                                    parseCache);

                            return
                                afterSource
                                .AppendInstruction(
                                    StackInstruction.Blob_Trim_Leading_Zeros(minRemainingCount: 1));
                        }

                        return null;
                    }

                    {
                        if (TryParse_IndependentSignedIntegerRelaxed(addition.Left, parseCache) is { } skipSourceAddValue &&
                            skipSourceAddValue.IsZero &&
                            ContinueForAddZeroOperand(addition.Right) is { } specialized)
                        {
                            return specialized;
                        }
                    }

                    {
                        if (TryParse_IndependentSignedIntegerRelaxed(addition.Right, parseCache) is { } skipSourceAddValue &&
                            skipSourceAddValue.IsZero &&
                            ContinueForAddZeroOperand(addition.Left) is { } specialized)
                        {
                            return specialized;
                        }
                    }
                }

                return
                    afterSource
                    .AppendInstruction(
                        StackInstruction.Skip_Const((int)skipCount));
            }

            {
                var afterSkipCount =
                    CompileExpressionTransitive(
                        skipCountExpr,
                        context,
                        afterSource,
                        parseCache);

                return
                    afterSkipCount
                    .AppendInstruction(StackInstruction.Skip_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Skip_Generic);
    }

    /// <summary>
    /// Compiles the take built-in function application and slice-style specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Take(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr && listExpr.Length is 2)
        {
            var takeCountValueExpr = listExpr[0];

            if (TryParse_Builtin_Skip(listExpr[1]) is { } sourceSkip)
            {
                var afterSource =
                    CompileExpressionTransitive(
                        sourceSkip.sourceExpr,
                        context,
                        prior,
                        parseCache);

                var afterSkipCount =
                    CompileExpressionTransitive(
                        sourceSkip.skipCountExpr,
                        context,
                        afterSource,
                        parseCache);

                if (TryParse_IndependentSignedIntegerRelaxed(takeCountValueExpr, parseCache) is { } takeCount)
                {
                    return
                        afterSkipCount
                        .AppendInstruction(
                            StackInstruction.Slice_Skip_Var_Take_Const((int)takeCount));
                }

                {
                    var afterTakeCount =
                        CompileExpressionTransitive(
                            takeCountValueExpr,
                            context,
                            afterSkipCount,
                            parseCache);

                    return
                        afterTakeCount
                        .AppendInstruction(
                            StackInstruction.Slice_Skip_Var_Take_Var);
                }
            }

            {
                var afterSource =
                    CompileExpressionTransitive(
                        listExpr[1],
                        context,
                        prior,
                        parseCache);

                if (TryParse_IndependentSignedIntegerRelaxed(takeCountValueExpr, parseCache) is { } takeCount)
                {
                    return
                        afterSource
                        .AppendInstruction(
                            StackInstruction.Take_Const((int)takeCount));
                }

                var afterTakeCount =
                    CompileExpressionTransitive(
                        takeCountValueExpr,
                        context,
                        afterSource,
                        parseCache);

                return
                    afterTakeCount
                    .AppendInstruction(StackInstruction.Take_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Take_Generic);
    }

    /// <summary>
    /// Compiles the concat built-in function application, including append/prepend list optimizations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Concat(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr)
        {
            if (listExpr.Length is 2)
            {
                // Check if left side is a list expression - emit Prepend_List_Items
                if (listExpr[0] is Expression.List leftListExpr && leftListExpr.Items.Length > 0)
                {
                    // Push items in order (first to last)
                    var afterLeftItems = prior;

                    for (var i = 0; i < leftListExpr.Items.Length; ++i)
                    {
                        afterLeftItems =
                            CompileExpressionTransitive(
                                leftListExpr.Items[i],
                                context,
                                afterLeftItems,
                                parseCache);
                    }

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr[1],
                            context,
                            afterLeftItems,
                            parseCache);

                    return
                        afterRight
                        .AppendInstruction(StackInstruction.Prepend_List_Items(leftListExpr.Items.Length));
                }

                // Check if right side is a list expression - emit Append_List_Items
                if (listExpr[1] is Expression.List rightListExpr && rightListExpr.Items.Length > 0)
                {
                    // Push the target list first
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr[0],
                            context,
                            prior,
                            parseCache);

                    // Then push items in order (first to last)
                    var afterRightItems = afterLeft;

                    for (var i = 0; i < rightListExpr.Items.Length; ++i)
                    {
                        afterRightItems =
                            CompileExpressionTransitive(
                                rightListExpr.Items[i],
                                context,
                                afterRightItems,
                                parseCache);
                    }

                    return
                        afterRightItems
                        .AppendInstruction(StackInstruction.Append_List_Items(rightListExpr.Items.Length));
                }
            }

            var concatOps = prior;

            for (var i = 0; i < listExpr.Length; ++i)
            {
                var item = listExpr[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        context,
                        concatOps,
                        parseCache);

                concatOps = itemOps;

                if (0 < i)
                {
                    concatOps =
                        concatOps.AppendInstruction(
                            StackInstruction.Concat_Binary);
                }
            }

            return concatOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Concat_Generic);
        }
    }

    /// <summary>
    /// Compiles the reverse built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Reverse(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (TryParse_Builtin_Take_Const(input.GeneralExpression, parseCache) is { } takeConst &&
            takeConst.sourceExpr is Expression.Builtin takeSourceBuiltin &&
            takeSourceBuiltin.Function is BuiltinId.Reverse)
        {
            var afterSource =
                CompileExpressionTransitive(
                    takeSourceBuiltin.Input,
                    context,
                    prior,
                    parseCache);

            return
                afterSource
                .AppendInstruction(StackInstruction.Take_Last_Const((int)takeConst.takeCount));
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Reverse);
    }

    /// <summary>
    /// Compiles the negate built-in application and related integer simplifications.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Negate(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (TryBinaryEqual(input.GeneralExpression) is { } equal)
        {
            if (equal.Left is Expression.Literal leftLiteralExpr)
            {
                var afterRight =
                    CompileExpressionTransitive(
                        equal.Right,
                        context,
                        prior,
                        parseCache);

                return
                    afterRight
                    .AppendInstruction(
                        StackInstruction.Not_Equal_Binary_Const(leftLiteralExpr.Value));
            }

            if (equal.Right is Expression.Literal rightLiteralExpr)
            {
                var afterLeft =
                    CompileExpressionTransitive(
                        equal.Left,
                        context,
                        prior,
                        parseCache);

                return
                    afterLeft
                    .AppendInstruction(
                        StackInstruction.Not_Equal_Binary_Const(rightLiteralExpr.Value));
            }

            {
                var afterLeft =
                    CompileExpressionTransitive(
                        equal.Left,
                        context,
                        prior,
                        parseCache);

                var afterRight =
                    CompileExpressionTransitive(
                        equal.Right,
                        context,
                        afterLeft,
                        parseCache);

                return
                    afterRight
                    .AppendInstruction(StackInstruction.Not_Equal_Binary);
            }
        }

        if (TryBinaryBuiltin(input.GeneralExpression, BuiltinId.IntIsSortedAsc) is { } sorted)
        {
            /*
             * not (int_is_sorted_asc [a, b]) = b <= a
             * */

            var afterLeft =
                CompileExpressionTransitive(
                    sorted.Right,
                    context,
                    prior,
                    parseCache);

            var afterRight =
                CompileExpressionTransitive(
                    sorted.Left,
                    context,
                    afterLeft,
                    parseCache);

            return
                afterRight
                .AppendInstruction(StackInstruction.Int_Less_Than_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Negate);
    }

    /// <summary>
    /// Compiles integer addition with constant folding and structural specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Int_Add(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr)
        {
            if (listExpr.Length is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(0)));
            }

            BigInteger constItemsSum = 0;
            var varItemsAdd = new List<Expression>();
            var varItemsSubtract = new List<Expression>();

            for (var i = 0; i < listExpr.Length; ++i)
            {
                var itemExpr = listExpr[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    if (Kernel.SignedIntegerFromValueRelaxed(literalExpr.Value) is not { } intValue)
                    {
                        return
                            prior
                            .AppendInstruction(
                                StackInstruction.Push_Literal(PineValue.EmptyList));
                    }

                    constItemsSum += intValue;
                }
                else
                {
                    if (IsIntNegated(itemExpr, parseCache) is { } subtractedExpr)
                    {
                        varItemsSubtract.Add(subtractedExpr);
                    }
                    else
                    {
                        varItemsAdd.Add(itemExpr);
                    }
                }
            }

            if (varItemsAdd.Count is 0)
            {
                var addOps =
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(constItemsSum)));

                for (var i = 0; i < varItemsSubtract.Count; ++i)
                {
                    var itemExpr = varItemsSubtract[i];

                    var itemOps =
                        CompileExpressionTransitive(
                            itemExpr,
                            context,
                            addOps,
                            parseCache);

                    addOps = itemOps;

                    addOps =
                        addOps.AppendInstruction(
                            StackInstruction.Int_Mul_Const_Add_Binary(-1));
                }
            }
            else
            {
                var firstVarItemAdd = varItemsAdd[0];

                if (varItemsAdd.Count is 1 && varItemsSubtract.Count is 0)
                {
                    if (IsIntUnsigned(firstVarItemAdd, parseCache) is { } unsignedExpr)
                    {
                        var addOps =
                            CompileExpressionTransitive(
                                unsignedExpr,
                                context,
                                prior,
                                parseCache);

                        return
                            addOps
                            .AppendInstruction(
                                StackInstruction.Int_Unsigned_Add_Const(constItemsSum));
                    }
                }

                {
                    var addOps =
                        CompileExpressionTransitive(
                            firstVarItemAdd,
                            context,
                            prior,
                            parseCache);

                    for (var i = 1; i < varItemsAdd.Count; ++i)
                    {
                        var itemExpr = varItemsAdd[i];

                        var itemOps =
                            CompileExpressionTransitive(
                                itemExpr,
                                context,
                                addOps,
                                parseCache);

                        addOps = itemOps;

                        addOps =
                            addOps.AppendInstruction(
                                StackInstruction.Int_Add_Binary);
                    }

                    for (var i = 0; i < varItemsSubtract.Count; ++i)
                    {
                        var itemExpr = varItemsSubtract[i];

                        var itemOps =
                            CompileExpressionTransitive(
                                itemExpr,
                                context,
                                addOps,
                                parseCache);

                        addOps = itemOps;

                        addOps =
                            addOps.AppendInstruction(
                                StackInstruction.Int_Mul_Const_Add_Binary(-1));
                    }

                    if (constItemsSum != 0 || varItemsAdd.Count + varItemsSubtract.Count < 2)
                    {
                        /*
                         * Using the 'add' function returns canonically encoded integer,
                         * so adding '0' does not always return the same value.
                         * */

                        addOps =
                            addOps
                            .AppendInstruction(
                                StackInstruction.Int_Add_Const(constItemsSum));
                    }

                    return addOps;
                }
            }
        }

        {
            var compiledInput =
                CompileExpressionTransitive(
                    input,
                    context,
                    prior,
                    parseCache);

            if (compiledInput.Fragment.LastOperationOrNull is
                {
                    Kind: StackInstructionKind.Build_List,
                    TakeCount: 2
                })
            {
                return
                    compiledInput with
                    {
                        Fragment =
                        compiledInput.Fragment.ReplaceLastOperation(StackInstruction.Int_Add_Binary)
                    };
            }

            return compiledInput.AppendInstruction(StackInstruction.Int_Add_Generic);
        }
    }

    private static Expression? IsIntUnsigned(
        Expression expr,
        PineVMParseCache parseCache)
    {
        if (TryBinaryBuiltin(expr, BuiltinId.Concat) is { } concat)
        {
            var prependedExpr = concat.Left;

            if (!prependedExpr.ReferencesEnvironment)
            {
                if (ExpressionReduction.TryEvalIndependent(
                    prependedExpr,
                    parseCache).Value is not { } prependedValue)
                {
                    return null;
                }

                if (prependedValue is PineValue.BlobValue prependedBlob &&
                    prependedBlob.Bytes.Length is 1 &&
                    prependedBlob.Bytes.Span[0] is 4)
                {
                    return concat.Right;
                }
            }
        }

        return null;
    }

    private static Expression? IsIntUnsignedNegated(
        Expression expr,
        PineVMParseCache parseCache)
    {
        if (TryBinaryBuiltin(expr, BuiltinId.Concat) is { } concat)
        {
            var prependedExpr = concat.Left;

            if (!prependedExpr.ReferencesEnvironment)
            {
                if (ExpressionReduction.TryEvalIndependent(
                    prependedExpr,
                    parseCache).Value is not { } prependedValue)
                {
                    return null;
                }

                if (prependedValue is PineValue.BlobValue prependedBlob &&
                    prependedBlob.Bytes.Length is 1 &&
                    prependedBlob.Bytes.Span[0] is 2)
                {
                    return concat.Right;
                }
            }
        }

        return null;
    }

    private static Expression? IsIntNegated(
        Expression expr,
        PineVMParseCache parseCache) => TryParse_IntNegation(expr, parseCache);

    /// <summary>
    /// Compiles integer multiplication with constant folding and algebraic simplifications.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Int_Mul(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr)
        {
            if (listExpr.Length is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(1)));
            }

            BigInteger constItemsProduct = 1;
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.Length; ++i)
            {
                var itemExpr = listExpr[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    if (Kernel.SignedIntegerFromValueRelaxed(literalExpr.Value) is not { } intValue)
                    {
                        return
                            prior
                            .AppendInstruction(
                                StackInstruction.Push_Literal(PineValue.EmptyList));
                    }

                    constItemsProduct *= intValue;
                }
                else
                {
                    varItems.Add(itemExpr);
                }
            }

            if (varItems.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(constItemsProduct)));
            }

            var firstVarItem = varItems[0];

            var mulOps =
                CompileExpressionTransitive(
                    firstVarItem,
                    context,
                    prior,
                    parseCache);

            for (var i = 1; i < varItems.Count; ++i)
            {
                var itemExpr = varItems[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        mulOps,
                        parseCache);

                mulOps = itemOps;

                mulOps =
                    mulOps.AppendInstruction(
                        StackInstruction.Int_Mul_Binary);
            }

            if (constItemsProduct != 1 || varItems.Count < 2)
            {
                mulOps =
                    mulOps
                    .AppendInstruction(
                        StackInstruction.Int_Mul_Const(constItemsProduct));
            }

            return mulOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Int_Mul_Generic);
        }
    }

    /// <summary>
    /// Compiles the integer sortedness built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Int_Is_Sorted_Asc(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (Int_Is_Sorted_Asc_TryParseIsIntegerSign(input, parseCache) is { } isIntegerWithSign)
        {
            return
                CompileExpressionTransitive(
                    isIntegerWithSign.expr,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(0)))
                .AppendInstruction(
                    StackInstruction.Slice_Skip_Var_Equal_Const(PineValue.BlobSingleByte(isIntegerWithSign.sign)));
        }

        if (input is { IsList: true } listExpr)
        {
            if (listExpr.Length is 0 or 1)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineKernelValues.TrueValue));
            }

            if (listExpr.Length is 2)
            {
                if (TryParse_IndependentSignedIntegerRelaxed(listExpr[0], parseCache) is { } leftInt)
                {
                    var rightExpr = listExpr[1];

                    if (IsIntUnsigned(rightExpr, parseCache) is { } rightUnsigned)
                    {
                        var afterRight =
                            CompileExpressionTransitive(
                                rightUnsigned,
                                context,
                                prior,
                                parseCache);

                        return
                            afterRight
                            .AppendInstruction(
                                StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(leftInt));
                    }

                    {
                        var afterRight =
                            CompileExpressionTransitive(
                                rightExpr,
                                context,
                                prior,
                                parseCache);

                        return
                            afterRight
                            .AppendInstruction(
                                StackInstruction.Int_Greater_Than_Or_Equal_Const(leftInt));
                    }
                }

                if (TryParse_IndependentSignedIntegerRelaxed(listExpr[1], parseCache) is { } rightInt)
                {
                    var leftExpr = listExpr[0];

                    if (IsIntUnsigned(leftExpr, parseCache) is { } leftUnsigned)
                    {
                        var afterLeft =
                            CompileExpressionTransitive(
                                leftUnsigned,
                                context,
                                prior,
                                parseCache);

                        return
                            afterLeft
                            .AppendInstruction(
                                StackInstruction.Int_Unsigned_Less_Than_Or_Equal_Const(rightInt));
                    }

                    {
                        var afterLeft =
                            CompileExpressionTransitive(
                                leftExpr,
                                context,
                                prior,
                                parseCache);

                        return
                            afterLeft
                            .AppendInstruction(
                                StackInstruction.Int_Less_Than_Or_Equal_Const(rightInt));
                    }
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr[0],
                            context,
                            prior,
                            parseCache);

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr[1],
                            context,
                            afterLeft,
                            parseCache);

                    return
                        afterRight
                        .AppendInstruction(StackInstruction.Int_Less_Than_Or_Equal_Binary);
                }
            }

            if (listExpr.Length is 3)
            {
                if (TryParse_IndependentSignedIntegerRelaxed(listExpr[0], parseCache) is { } leftInt)
                {
                    if (TryParse_IndependentSignedIntegerRelaxed(listExpr[2], parseCache) is { } rightInt)
                    {
                        var middleExpr = listExpr[1];

                        if (IsIntUnsigned(middleExpr, parseCache) is { } middleUnsigned)
                        {
                            var (afterMiddle, middleLocalIndex) =
                                CompileExpressionTransitiveAsLocal(
                                    middleUnsigned,
                                    context,
                                    prior,
                                    parseCache);

                            return
                                afterMiddle
                                .AppendInstruction(
                                    StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(leftInt))
                                .AppendInstruction(
                                    StackInstruction.Local_Get(middleLocalIndex))
                                .AppendInstruction(
                                    StackInstruction.Int_Unsigned_Less_Than_Or_Equal_Const(rightInt))
                                .AppendInstruction(
                                    StackInstruction.Logical_And_Binary);
                        }

                        {
                            var (afterMiddle, middleLocalIndex) =
                                CompileExpressionTransitiveAsLocal(
                                    middleExpr,
                                    context,
                                    prior,
                                    parseCache);

                            return
                                afterMiddle
                                .AppendInstruction(
                                    StackInstruction.Int_Greater_Than_Or_Equal_Const(leftInt))
                                .AppendInstruction(
                                    StackInstruction.Local_Get(middleLocalIndex))
                                .AppendInstruction(
                                    StackInstruction.Int_Less_Than_Or_Equal_Const(rightInt))
                                .AppendInstruction(
                                    StackInstruction.Logical_And_Binary);
                        }
                    }
                }
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Int_Is_Sorted_Asc_Generic);
    }

    private static (Expression expr, byte sign)? Int_Is_Sorted_Asc_TryParseIsIntegerSign(
        ExpressionInput input,
        PineVMParseCache parseCache)
    {
        if (input is not { IsList: true } inputList)
        {
            return null;
        }

        if (inputList.Length is not 2)
        {
            return null;
        }

        if (TryParse_IndependentSignedIntegerRelaxed(inputList[0], parseCache) == 0)
        {
            return (inputList[1], 4);
        }

        if (TryParse_IndependentSignedIntegerRelaxed(inputList[1], parseCache) == -1)
        {
            return (inputList[0], 2);
        }

        return null;
    }

    /// <summary>
    /// Compiles the bitwise-and built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_And(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr)
        {
            if (listExpr.Length is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var constItems = new List<PineValue>();
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.Length; ++i)
            {
                var itemExpr = listExpr[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    constItems.Add(literalExpr.Value);
                }
                else
                {
                    varItems.Add(itemExpr);
                }
            }

            var andOps = prior;

            for (var i = 0; i < varItems.Count; ++i)
            {
                var itemExpr = varItems[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        andOps,
                        parseCache);

                andOps = itemOps;

                if (0 < i)
                {
                    andOps =
                        andOps.AppendInstruction(
                            StackInstruction.Bit_And_Binary);
                }
            }

            for (var i = 0; i < constItems.Count; ++i)
            {
                var constItem = constItems[i];

                andOps =
                    andOps.AppendInstruction(
                        StackInstruction.Bit_And_Const(constItem));
            }

            return andOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Bit_And_Generic);
        }
    }

    /// <summary>
    /// Compiles the bitwise-or built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Or(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr)
        {
            if (listExpr.Length is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var constItems = new List<PineValue>();
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.Length; ++i)
            {
                var itemExpr = listExpr[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    constItems.Add(literalExpr.Value);
                }
                else
                {
                    varItems.Add(itemExpr);
                }
            }

            var orOps = prior;

            for (var i = 0; i < varItems.Count; ++i)
            {
                var itemExpr = varItems[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        orOps,
                        parseCache);

                orOps = itemOps;

                if (0 < i)
                {
                    orOps =
                        orOps.AppendInstruction(
                            StackInstruction.Bit_Or_Binary);
                }
            }

            for (var i = 0; i < constItems.Count; ++i)
            {
                var constItem = constItems[i];

                orOps =
                    orOps.AppendInstruction(
                        StackInstruction.Bit_Or_Const(constItem));
            }

            return orOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Bit_Or_Generic);
        }
    }

    /// <summary>
    /// Compiles the bitwise-xor built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Xor(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr)
        {
            if (listExpr.Length is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var xorOps = prior;

            for (var i = 0; i < listExpr.Length; ++i)
            {
                var itemExpr = listExpr[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        xorOps,
                        parseCache);

                xorOps = itemOps;

                if (0 < i)
                {
                    xorOps =
                        xorOps.AppendInstruction(
                            StackInstruction.Bit_Xor_Binary);
                }
            }

            return xorOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Bit_Xor_Generic);
        }
    }

    /// <summary>
    /// Compiles the bitwise-not built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Not(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Bit_Not);
    }

    /// <summary>
    /// Compiles the left-shift built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Shift_Left(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr && listExpr.Length is 2)
        {
            var shiftCountExpr =
                listExpr[0];

            var sourceExpr =
                listExpr[1];

            if (TryParse_IndependentSignedIntegerRelaxed(shiftCountExpr, parseCache) is { } shiftCount)
            {
                return
                    CompileExpressionTransitive(
                        sourceExpr,
                        context,
                        prior,
                        parseCache)
                    .AppendInstruction(
                        StackInstruction.Bit_Shift_Left_Const((int)shiftCount));
            }

            var sourceOps =
                CompileExpressionTransitive(
                    sourceExpr,
                    context,
                    prior,
                    parseCache);

            var shiftCountOps =
                CompileExpressionTransitive(
                    shiftCountExpr,
                    context,
                    sourceOps,
                    parseCache);

            return
                shiftCountOps
                .AppendInstruction(StackInstruction.Bit_Shift_Left_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Bit_Shift_Left_Generic);
    }

    /// <summary>
    /// Compiles the right-shift built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Shift_Right(
        ExpressionInput input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is { IsList: true } listExpr && listExpr.Length is 2)
        {
            var shiftCountExpr =
                listExpr[0];

            var sourceExpr =
                listExpr[1];

            if (TryParse_IndependentSignedIntegerRelaxed(shiftCountExpr, parseCache) is { } shiftCount)
            {
                return
                    CompileExpressionTransitive(
                        sourceExpr,
                        context,
                        prior,
                        parseCache)
                    .AppendInstruction(
                        StackInstruction.Bit_Shift_Right_Const((int)shiftCount));
            }

            var sourceOps =
                CompileExpressionTransitive(
                    sourceExpr,
                    context,
                    prior,
                    parseCache);

            var shiftCountOps =
                CompileExpressionTransitive(
                    shiftCountExpr,
                    context,
                    sourceOps,
                    parseCache);

            return
                shiftCountOps
                .AppendInstruction(StackInstruction.Bit_Shift_Right_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Bit_Shift_Right_Generic);
    }

    /// <summary>
    /// Tries to recognize an expression as integer negation and returns the negated operand when successful.
    /// </summary>
    public static Expression? TryParse_IntNegation(
        Expression expression,
        PineVMParseCache parseCache)
    {
        if (expression is Expression.Builtin { Function: BuiltinId.Negate } negate)
            return negate.Input;

        if (TryBinaryBuiltin(expression, BuiltinId.IntMul) is { } multiply)
        {
            if (TryParse_IndependentSignedIntegerRelaxed(multiply.Left, parseCache) is { } literalValue &&
                literalValue == -1)
            {
                return multiply.Right;
            }

            if (TryParse_IndependentSignedIntegerRelaxed(multiply.Right, parseCache) is { } literalValue2 &&
                literalValue2 == -1)
            {
                return multiply.Left;
            }
        }

        return null;
    }

    private static (BigInteger skipCount, Expression sourceExpr)? TryParse_Builtin_Skip_Const(
        Expression? expression,
        PineVMParseCache parseCache)
    {
        if (TryParse_Builtin_Skip(expression) is not { } parsed)
        {
            return null;
        }

        if (TryParse_IndependentSignedIntegerRelaxed(parsed.skipCountExpr, parseCache) is not { } skipCount)
        {
            return null;
        }

        return (skipCount, parsed.sourceExpr);
    }

    private static (Expression skipCountExpr, Expression sourceExpr)? TryParse_Builtin_Skip(Expression? expression) =>
        TryBinaryBuiltin(expression, BuiltinId.Skip);

    private static (BigInteger takeCount, Expression sourceExpr)? TryParse_Builtin_Take_Const(
        Expression? expression,
        PineVMParseCache parseCache)
    {
        if (TryParse_Builtin_Take(expression) is not { } parsed)
        {
            return null;
        }

        if (TryParse_IndependentSignedIntegerRelaxed(parsed.takeCountExpr, parseCache) is not { } takeCount)
        {
            return null;
        }

        return (takeCount, parsed.sourceExpr);
    }

    private static (Expression takeCountExpr, Expression sourceExpr)? TryParse_Builtin_Take(
        Expression? expression) => TryBinaryBuiltin(expression, BuiltinId.Take);

    private static BigInteger? TryParse_IndependentSignedIntegerRelaxed(
        Expression expression,
        PineVMParseCache parseCache)
    {
        if (TryEvalIndependent(expression, parseCache) is not { } value)
        {
            return null;
        }

        return Kernel.SignedIntegerFromValueRelaxed(value);
    }

    private static PineValue? TryEvalIndependent(
        Expression expression,
        PineVMParseCache parseCache)
    {
        if (expression.ReferencesEnvironment)
        {
            return null;
        }

        return
            ExpressionReduction.TryEvalIndependent(
                expression,
                parseCache).Value;
    }

    /// <summary>
    /// Determines whether an expression is substantial enough to consider for common subexpression elimination.
    /// </summary>
    public static bool ExpressionLargeEnoughForCSE(Expression expression)
    {
        if (expression is Expression.Literal or Expression.Environment)
            return false;

        if (expression is Expression.Builtin or Expression.BuiltinBinary or Expression.Length_Equal_Binary or Expression.HeadSkip)
            return true;

        if (expression is Expression.Eval)
            return true;

        if (expression is Expression.List list)
        {
            for (var i = 0; i < list.Items.Length; ++i)
            {
                if (ExpressionLargeEnoughForCSE(list.Items[i]))
                    return true;
            }
        }

        if (expression is Expression.Label stringTag)
        {
            return ExpressionLargeEnoughForCSE(stringTag.Tagged);
        }

        return 3 < expression.SubexpressionCount;
    }
}
