using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>Recursion evidence is the exact encoding of an unspecialized owned body, not its signature.</summary>
public static class ExpressionSelfTailLoops
{
    /// <summary>
    /// Compiles the owned body here so an arbitrary graph/encoding pair cannot authorize a
    /// backedge. Only an unchanged, unspecialized body matching the parsed encoding is guarded.
    /// </summary>
    public static (ValidatedFunctionGraph Graph, int RewrittenCalls) Compile(
        PreparedFunction prepared, LiteralValue sourceEncoding,
        long maxWorkUnits = 65_536, long maxExpansionUnits = 20_000, int maxDepth = 64)
    {
        var graph = ValidatedFunctionGraph.ValidateGraph(
            ExpressionGraphCompiler.CompileExpressionToGraph(prepared),
            ImmutableDictionary<FunctionId, FunctionSignature>.Empty)
            .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        return Rewrite(graph, prepared, sourceEncoding, maxWorkUnits, maxExpansionUnits, maxDepth);
    }

    private static (ValidatedFunctionGraph Graph, int RewrittenCalls) Rewrite(
        ValidatedFunctionGraph input, PreparedFunction prepared, LiteralValue sourceEncoding,
        long maxWorkUnits, long maxExpansionUnits, int maxDepth)
    {
        return Run();

        (ValidatedFunctionGraph, int) Run()
        {
            if (input.Graph.Signature != FunctionSignature.Canonical ||
                prepared.Request.Id != input.Graph.Id || prepared.Request.Specialization is not null ||
                prepared.Body != prepared.Source || maxWorkUnits <= 0 || maxExpansionUnits <= 0)
                return (input, 0);
            var encodingSize = ExpressionGraphOptimizer.MeasureLiteral(sourceEncoding, maxWorkUnits, maxDepth);
            if (!encodingSize.Fits ||
                ExpressionGraphOptimizer.GraphUnits(input.Graph) > maxWorkUnits - encodingSize.Units ||
                input.Graph.Blocks.Values.LongCount(block => block.Terminator is Terminator.TailInvoke { Call.Target: CallTarget.Dynamic }) >
                    maxExpansionUnits / 16 ||
                FunctionPreparation.ParseExpression(sourceEncoding, CompilerMemo.Empty).Result.Expression != prepared.Source)
                return (input, 0);
            var maxBlock = input.Graph.Blocks.Keys.Max(id => id.Value);
            var maxValue = input.Graph.Blocks.Values.SelectMany(block =>
                block.Parameters.Select(parameter => parameter.Id).Concat(block.Operations.Select(Result)))
                .Max(id => id.Value);
            if (maxBlock > int.MaxValue - input.Graph.Blocks.Count ||
                maxValue > int.MaxValue - 2L * input.Graph.Blocks.Count)
                return (input, 0);
            long nextBlock = (long)maxBlock + 1;
            long nextValue = (long)maxValue + 1;
            var blocks = input.Graph.Blocks;
            var count = 0;
            foreach (var block in input.Graph.Blocks.Values.OrderBy(block => block.Id.Value))
            {
                switch (block.Terminator)
                {
                    case Terminator.TailInvoke { Call.Target: CallTarget.Dynamic target } tail:
                        var literal = block.Operations.OfType<Operation.Literal>()
                            .FirstOrDefault(operation => operation.Result.Id == target.EncodedExpression);
                        if (literal is not null)
                        {
                            if (literal.Value == sourceEncoding)
                            {
                                blocks = blocks.SetItem(block.Id, block with
                                {
                                    Terminator = new Terminator.Jump(new(input.Graph.Entry, tail.Call.Arguments)),
                                });
                                ++count;
                            }
                            break;
                        }
                        var fallbackId = new PineBlockId(checked((int)nextBlock++));
                        var environment = new ValueDefinition(new(checked((int)nextValue++)));
                        var encoded = new ValueDefinition(new(checked((int)nextValue++)));
                        blocks = blocks.Add(fallbackId, new(fallbackId, [environment, encoded], [],
                            new Terminator.TailInvoke(tail.Call with
                            {
                                Target = new CallTarget.Dynamic(encoded.Id),
                                Arguments = [environment.Id],
                            })));
                        blocks = blocks.SetItem(block.Id, block with
                        {
                            Terminator = new Terminator.Branch(target.EncodedExpression, sourceEncoding,
                                new(input.Graph.Entry, tail.Call.Arguments),
                                new(fallbackId, [tail.Call.Arguments[0], target.EncodedExpression])),
                        });
                        ++count;
                        break;
                    case Terminator.TailInvoke:
                    case Terminator.Return:
                    case Terminator.Jump:
                    case Terminator.Branch:
                    case Terminator.Switch:
                    case Terminator.Invoke:
                        break;
                    default:
                        throw new NotImplementedException("Rewrite does not handle terminator variant: " + block.Terminator.GetType().Name);
                }
            }
            if (count == 0)
                return (input, 0);
            return (ValidatedFunctionGraph.ValidateGraph(
                new(input.Graph.Id, input.Graph.Signature, input.Graph.Entry, blocks), input.KnownFunctionSignatures)
                .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors))), count);
        }
    }

    private static PineVirtualValueId Result(Operation operation) => operation switch
    {
        Operation.Literal literal => literal.Result.Id,
        Operation.MakeList list => list.Result.Id,
        Operation.Project project => project.Result.Id,
        Operation.Builtin builtin => builtin.Result.Id,
        _ => throw new NotImplementedException("Result does not handle operation variant: " + operation.GetType().Name),
    };
}
