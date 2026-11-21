using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Renders virtual values, block edges, natural-loop nesting and exits, and irreducible regions.
    /// Literal formatting can be supplied without deriving hashes or lowering instruction offsets.
    /// </summary>
    public string Render(Func<PineValue, string>? renderLiteral = null)
    {
        renderLiteral ??= StackInstruction.LiteralDisplayStringDefault;
        var analysis = AnalyzeLoops();
        var loops = analysis.Loops.ToDictionary(loop => loop.Header);

        int Depth(PineControlFlowLoop loop)
        {
            var depth = 0;

            while (loop.ParentHeader is { } parent)
            {
                depth++;
                loop = loops[parent];
            }

            return depth;
        }

        static string B(PineBlockId block) => "b" + block.Value;
        static string V(PineVirtualValueId value) => "v" + value.Value;
        static string Edge(PineControlFlowEdge edge) => B(edge.Source) + " -> " + B(edge.Target);
        static string Values(IEnumerable<PineVirtualValueId> values) => string.Join(", ", values.Select(V));

        static string Target(PineBlockId block, IEnumerable<PineVirtualValueId> arguments) =>
            B(block) + "(" + Values(arguments) + ")";

        var text = new StringBuilder();
        text.Append("entry ").Append(B(Entry)).Append('\n');

        foreach (var loop in analysis.Loops.OrderBy(Depth).ThenBy(loop => loop.Header.Value))
        {
            text.Append(' ', Depth(loop) * 2).Append("loop ").Append(B(loop.Header));

            if (loop.ParentHeader is { } parent)
                text.Append(" in ").Append(B(parent));

            if (loop.Preheader is { } preheader)
                text.Append(" preheader ").Append(B(preheader));

            text.Append('\n').Append(' ', Depth(loop) * 2 + 2)
                .Append("blocks: ").AppendJoin(", ", loop.Blocks.Select(B)).Append('\n')
                .Append(' ', Depth(loop) * 2 + 2)
                .Append("backedges: ").AppendJoin(", ", loop.Backedges.Select(Edge)).Append('\n')
                .Append(' ', Depth(loop) * 2 + 2)
                .Append("exits: ").AppendJoin(", ", loop.Exits.Select(Edge)).Append('\n');
        }

        foreach (var cycle in analysis.IrreducibleCycles)
        {
            text.Append("irreducible cycle: ").AppendJoin(", ", cycle.Blocks.Select(B))
                .Append(" entries: ").AppendJoin(", ", cycle.Entries.Select(B)).Append('\n');
        }

        foreach (var block in Blocks)
        {
            var loop =
                analysis.Loops.Where(loop => loop.Blocks.Contains(block.Id))
                .OrderBy(loop => loop.Blocks.Length).FirstOrDefault();

            text.Append('\n').Append("block ").Append(Target(block.Id, block.Parameters));

            if (loop is not null)
                text.Append(" [loop ").Append(B(loop.Header)).Append(']');

            text.Append(":\n");
            var stack = block.Parameters.ToList();

            foreach (var operation in block.Operations)
            {
                var display = StackInstruction.RenderInstructionDisplay(operation.Instruction, renderLiteral);
                text.Append("  ");

                if (!operation.Results.IsEmpty)
                    text.Append(Values(operation.Results)).Append(" = ");

                text.Append(display.Replace("\n", "\n  "));

                if (!operation.Inputs.IsEmpty)
                    text.Append(" <- ").Append(Values(operation.Inputs));

                text.Append('\n');
                stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
                stack.AddRange(operation.Results);
            }

            text.Append("  ");

            switch (block.Terminator)
            {
                case PineControlFlowTerminator.Return returned:
                    text.Append("return ").Append(Values(stack.TakeLast(returned.ValueCount)));
                    break;

                case PineControlFlowTerminator.Jump jump:
                    text.Append("jump ").Append(Target(jump.Target, jump.Arguments));
                    break;

                case PineControlFlowTerminator.ConditionalJump conditional:
                    text.Append("if ").Append(V(stack[^1])).Append(" == ").Append(renderLiteral(conditional.Literal))
                        .Append(" then ").Append(Target(conditional.Branch, conditional.BranchArguments))
                        .Append(" else ").Append(Target(conditional.FallThrough, conditional.FallThroughArguments));

                    break;

                case PineControlFlowTerminator.Switch choice:
                    text.Append("switch ").Append(Values(stack.TakeLast(choice.PopCount)))
                        .Append(" [").Append(choice.Kind).Append("]\n");

                    foreach (var item in choice.Cases)
                    {
                        text.Append("    ").Append(renderLiteral(item.Literal)).Append(": ")
                            .Append(Target(item.Target, choice.Arguments)).Append('\n');
                    }

                    text.Append("    default: ").Append(Target(choice.FallThrough, choice.Arguments));
                    break;

                case PineControlFlowTerminator.Invoke invoke:
                    text.Append(
                        StackInstruction.RenderInstructionDisplay(invoke.Instruction, renderLiteral).Replace(
                            "\n",
                            "\n  "))
                        .Append(" <- ").Append(Values(invoke.Inputs)).Append(" continue ")
                        .Append(Target(invoke.Continuation, invoke.Arguments));

                    break;

                case PineControlFlowTerminator.ValueMap map:
                    text.Append("map ").Append(V(map.Input)).Append(" -> ").Append(V(map.Result)).Append('\n');

                    foreach (var item in map.Map.OrderBy(item => renderLiteral(item.Key), StringComparer.Ordinal))
                    {
                        text.Append("    ")
                            .Append(renderLiteral(item.Key)).Append(": ").Append(renderLiteral(item.Value)).Append('\n');
                    }

                    text.Append("    success ").Append(Target(map.FallThrough, map.Arguments.Append(map.Result)))
                        .Append(" fallback ").Append(Target(map.Fallback, map.Arguments));

                    break;

                case PineControlFlowTerminator.TailInvoke tail:
                    text.Append("tail ").Append(
                        StackInstruction.RenderInstructionDisplay(
                            tail.InvokeInstruction,
                            renderLiteral).Replace("\n", "\n  "));

                    break;

                default:
                    throw new NotImplementedException("Render does not handle terminator: " + block.Terminator.GetType().Name);
            }

            text.Append('\n');
        }

        return text.ToString();
    }
}
