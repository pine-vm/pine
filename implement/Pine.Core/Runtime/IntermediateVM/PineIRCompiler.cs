using Pine.Core.CodeAnalysis;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using static Pine.Core.Runtime.InternalExpression.CanonicalExpressionAdapter;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Compiles Pine expressions into stack-frame instructions for the intermediate VM.
/// </summary>
public class PineIRCompiler
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
                    LocalsSet,
                    parseCache);

            return
                new NodeCompilationResult(
                    Fragment.Append(exprResult.Fragment),
                    LocalsSet.AddRange(exprResult.LocalsSet));
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
        Func<PineValue, bool>? SkipDirectInvocation)
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
            if (CodeAnalysis.CodeAnalysis.TryParseExprAsPathInEnv(expression) is { } path)
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
        Func<PineValue, bool>? skipDirectInvocation = null) =>
        ToCanonical(
            InternalPineIRCompiler.CompileExpression(
                ToInternal(rootExpression),
                ToInternal(rootExprAlternativeForms),
                envClass,
                parametersAsLocals,
                parseCache,
                enableTailRecursionOptimization,
                enableDirectInvocation,
                skipDirectInvocation));

    /// <summary>
    /// Recursively compile an expression into structured control flow.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        CompilationContext context,
        ImmutableDictionary<Expression, int> localIndexFromExpr,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileExpressionTransitive(
                ToInternal(expression),
                ToInternal(context),
                ToInternal(localIndexFromExpr),
                parseCache));

    /// <summary>
    /// Compiles an expression and ensures the resulting value is addressable as a local.
    /// </summary>
    public static (NodeCompilationResult nodeResult, int localIndex)
        CompileExpressionTransitiveAsLocal(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileExpressionTransitiveAsLocal(
                ToInternal(expression),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Recursively compile an expression into structured control flow.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileExpressionTransitive(
                ToInternal(expression),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles an expression without first augmenting the compilation context for extra CSE reuse.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitiveLessCSE(
        Expression expr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileExpressionTransitiveLessCSE(
                ToInternal(expr),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles a list expression by compiling each item and then constructing the list on the stack.
    /// </summary>
    public static NodeCompilationResult CompileList(
        Expression.List listExpr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileList(ToInternal(listExpr), ToInternal(context), ToInternal(prior), parseCache));

    /// <summary>
    /// Compiles a conditional expression with explicit branch jumps,
    /// applying fusion to peel layers of equal/negate from the condition.
    /// </summary>
    public static NodeCompilationResult CompileConditional(
        Expression.Conditional conditional,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileConditional(
                ToInternal(conditional),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

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
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileConditionalWithoutFusion(
                ToInternal(conditionExpression),
                jumpLiteralValue,
                ToInternal(falseBranch),
                ToInternal(trueBranch),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles an <c>Eval</c> expression, including direct parsing shortcuts and loop-based tail calls.
    /// </summary>
    public static NodeCompilationResult CompileEval(
        Expression.Eval evalExpr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileEval(ToInternal(evalExpr), ToInternal(context), ToInternal(prior), parseCache));

    /// <summary>
    /// Compile a <see cref="Expression.Builtin"/> to instructions, including dispatching to specialized compilation methods for
    /// individual built-in functions and patterns of arguments.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin(
        Expression.Builtin builtinExpr,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileExpressionTransitiveLessCSE(
                ToInternal(builtinExpr),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the equality built-in function application and its specialized fast paths.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Equal(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Equal(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the head built-in function application, including head-of-skip specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Head(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Head(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the skip built-in function application and its numeric specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Skip(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Skip(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the take built-in function application and slice-style specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Take(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Take(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the concat built-in function application, including append/prepend list optimizations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Concat(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Concat(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the reverse built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Reverse(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Reverse(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the negate built-in application and related integer simplifications.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Negate(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Negate(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles integer addition with constant folding and structural specializations.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Int_Add(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Int_Add(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles integer multiplication with constant folding and algebraic simplifications.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Int_Mul(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Int_Mul(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the integer sortedness built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Int_Is_Sorted_Asc(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Int_Is_Sorted_Asc(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the bitwise-and built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_And(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Bit_And(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the bitwise-or built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Or(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Bit_Or(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the bitwise-xor built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Xor(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Bit_Xor(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the bitwise-not built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Not(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Bit_Not(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the left-shift built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Shift_Left(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Bit_Shift_Left(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Compiles the right-shift built-in application.
    /// </summary>
    public static NodeCompilationResult CompileBuiltin_Bit_Shift_Right(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache) =>
        ToCanonical(
            InternalPineIRCompiler.CompileBuiltin_Bit_Shift_Right(
                ToInternal(input),
                ToInternal(context),
                ToInternal(prior),
                parseCache));

    /// <summary>
    /// Tries to recognize an expression as integer negation and returns the negated operand when successful.
    /// </summary>
    public static Expression? TryParse_IntNegation(
        Expression expression,
        PineVMParseCache parseCache) =>
        ToCanonicalOrNull(InternalPineIRCompiler.TryParse_IntNegation(ToInternal(expression), parseCache));

    /// <summary>
    /// Determines whether an expression is substantial enough to consider for common subexpression elimination.
    /// </summary>
    public static bool ExpressionLargeEnoughForCSE(Expression expression) =>
        InternalPineIRCompiler.ExpressionLargeEnoughForCSE(ToInternal(expression));

}
