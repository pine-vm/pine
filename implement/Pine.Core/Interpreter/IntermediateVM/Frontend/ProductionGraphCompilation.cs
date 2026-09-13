using Pine.Core.CodeAnalysis;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>
/// Bounded source-expression production activation for call-elimination candidates. Unsupported bodies,
/// opaque targets, residual calls, and allocations retain the established compiler.
/// </summary>
internal static class ProductionGraphCompilation
{
    internal static bool FitsBody(Expression expression)
    {
        if (expression.SubexpressionCount > 256)
            return false;
        return Check();

        bool Check()
        {
            long remaining = 65_536;
            foreach (var literal in Expression.EnumerateSelfAndDescendants(expression).OfType<Expression.Litral>())
            {
                var size = literal.Value switch
                {
                    PineValue.BlobValue blob => blob.Bytes.Length,
                    PineValue.ListValue list => list.MaxDepth > 64 || list.NodesCount > remaining || list.BlobsBytesCount > remaining
                        ? remaining + 1 : list.NodesCount + list.BlobsBytesCount,
                    _ => throw new NotImplementedException("FitsBody does not handle value variant: " + literal.Value.GetType().Name),
                };
                if (size > remaining)
                    return false;
                remaining -= size;
            }
            return true;
        }
    }

    internal static StackFrameInstructions? TryCompile(
        Expression specializedBody, StaticFunctionInterface parameters, ImmutableHashSet<OwnedExpression> authorizedBodies)
    {
        if (!FitsBody(specializedBody) || authorizedBodies.Count == 0 ||
            !Expression.EnumerateSelfAndDescendants(specializedBody).Any(expression => expression is Expression.Eval))
            return null;
        if (!ExpressionGraphOptimizer.MeasureBody(OwnedExpression.Capture(specializedBody), 512, 64).Fits)
            return null;
        var signature = new FunctionSignature(
            [.. parameters.ParamsPaths.Select(path => new FunctionParameter(new([.. path])))],
            [Semantic.ValueType.PineValue]);
        var compiled = ValidatedFunctionGraph.ValidateGraph(
            ExpressionGraphCompiler.Compile(specializedBody, signature),
            []);
        if (compiled.IsOkOrNull() is not { } graph)
            return null;
        var optimized = ExpressionGraphOptimizer.Optimize(graph,
            new(AllowedInlineBodies: authorizedBodies, MaxCandidates: 8), CompilerMemo.Empty);
        if (optimized.Stats.InlinedCalls == 0 ||
            optimized.Graph.Graph.Blocks.Values.Any(block => block.Terminator is Terminator.Invoke or Terminator.TailInvoke))
            return null;
        var selected = GraphCompiler.Compile(optimized.Graph, fuseScalarBuiltins: true, legacyParameterLocals: true, compact: true)
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        if (selected.Layout.SelectMany(fragment => fragment.Instructions).Any(instruction => instruction is SelectedInstruction.MakeList))
            return null;
        return GraphVMAdapter.ToStackFrame(selected);
    }
}
