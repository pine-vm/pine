using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Shared operation selection and stack effects for already-known straight-line fragments.</summary>
internal static class InstructionSelection
{
    internal static ValueDefinition Result(Operation operation) =>
        operation switch
        {
            Operation.Literal literal => literal.Result,
            Operation.MakeList list => list.Result,
            Operation.Project project => project.Result,
            Operation.Builtin builtin => builtin.Result,
            _ => throw new NotImplementedException(
                "Result does not handle operation variant: " + operation.GetType().Name),
        };

    internal static ImmutableList<SelectedInstruction> Project(EnvironmentPath path) =>
        path.Indices.Select(index => (SelectedInstruction)new SelectedInstruction.Project(index)).ToImmutableList();

    internal static ImmutableList<SelectedInstruction> Store(int local) =>
        [new SelectedInstruction.Store(local), new SelectedInstruction.Pop()];

    internal static ImmutableList<SelectedInstruction> Select(
        Operation operation, ImmutableDictionary<PineVirtualValueId, int> locals) =>
        Compute(operation, locals).AddRange(Store(locals[Result(operation).Id]));

    private static ImmutableList<SelectedInstruction> Compute(
        Operation operation, ImmutableDictionary<PineVirtualValueId, int> locals) =>
        operation switch
        {
            Operation.Literal literal => [new SelectedInstruction.Literal(literal.Value)],
            Operation.MakeList list =>
                list.Items.Select(value => (SelectedInstruction)new SelectedInstruction.Load(locals[value]))
                .ToImmutableList().Add(new SelectedInstruction.MakeList(list.Items.Count)),
            Operation.Project project =>
                ImmutableList.Create<SelectedInstruction>(new SelectedInstruction.Load(locals[project.Source]))
                .AddRange(Project(project.Path)),
            Operation.Builtin builtin =>
                [new SelectedInstruction.Load(locals[builtin.Argument]),
                new SelectedInstruction.Builtin(StraightLineCompiler.SelectBuiltin(builtin.Name))],
            _ => throw new NotImplementedException(
                "Compute does not handle operation variant: " + operation.GetType().Name),
        };

    internal static int MaximumStack(ImmutableList<SelectedInstruction> instructions)
    {
        return Fold();

        int Fold()
        {
            var depth = 0;
            var maximum = 0;
            foreach (var instruction in instructions)
            {
                var (read, produced) = instruction switch
                {
                    SelectedInstruction.Literal => (0, 1),
                    SelectedInstruction.Load => (0, 1),
                    SelectedInstruction.Store => (1, 1),
                    SelectedInstruction.Pop => (1, 0),
                    SelectedInstruction.MakeList list => (list.Count, 1),
                    SelectedInstruction.Project => (1, 1),
                    SelectedInstruction.Builtin => (1, 1),
                    SelectedInstruction.Return => (1, 0),
                    _ => throw new NotImplementedException(
                        "MaximumStack does not handle selected instruction variant: " + instruction.GetType().Name),
                };
                if (depth < read)
                    throw new InvalidOperationException("Selection produced stack underflow.");
                depth = checked(depth - read + produced);
                maximum = Math.Max(maximum, depth);
            }
            if (depth != 0)
                throw new InvalidOperationException("A selected fragment left values on the stack.");
            return maximum;
        }
    }
}
