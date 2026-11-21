using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private int MaximumVirtualValueId()
    {
        var maximum = -1;

        void Include(ImmutableArray<PineVirtualValueId> values)
        {
            foreach (var value in values)
                maximum = Math.Max(maximum, value.Value);
        }

        foreach (var block in Blocks)
        {
            Include(block.Parameters);

            foreach (var operation in block.Operations)
            {
                Include(operation.Inputs);
                Include(operation.Results);
            }

            foreach (var (_, arguments) in Successors(block.Terminator))
                Include(arguments);

            switch (block.Terminator)
            {
                case PineControlFlowTerminator.ValueMap map:
                    Include([map.Input, map.Result]);
                    break;

                case PineControlFlowTerminator.Invoke invoke:
                    Include(invoke.Inputs);
                    break;

                case PineControlFlowTerminator.Return:
                case PineControlFlowTerminator.Jump:
                case PineControlFlowTerminator.ConditionalJump:
                case PineControlFlowTerminator.Switch:
                case PineControlFlowTerminator.TailInvoke:
                    break;

                default:
                    throw new NotImplementedException(
                        "MaximumVirtualValueId does not handle terminator variant: " + block.Terminator.GetType().Name);
            }
        }

        return maximum;
    }

    private static PineControlFlowTerminator RewriteTerminatorInputs(
        PineControlFlowTerminator terminator, Func<PineVirtualValueId, PineVirtualValueId> rewrite)
    {
        ImmutableArray<PineVirtualValueId> Map(ImmutableArray<PineVirtualValueId> values) =>
            [
            .. values.Select(rewrite)
            ];

        return terminator switch
        {
            PineControlFlowTerminator.Return returned => returned,
            PineControlFlowTerminator.Jump jump => jump with { Arguments = Map(jump.Arguments) },

            PineControlFlowTerminator.ConditionalJump branch =>
            branch with
            {
                BranchArguments = Map(branch.BranchArguments),
                FallThroughArguments = Map(branch.FallThroughArguments)
            },

            PineControlFlowTerminator.Switch choice => choice with { Arguments = Map(choice.Arguments) },

            PineControlFlowTerminator.Invoke invoke =>
            invoke with
            {
                Inputs = Map(invoke.Inputs),
                Arguments = Map(invoke.Arguments)
            },

            PineControlFlowTerminator.ValueMap map =>
            map with
            {
                Input = rewrite(map.Input),
                Arguments = Map(map.Arguments)
            },

            PineControlFlowTerminator.TailInvoke tail => tail,

            _ =>
            throw new NotImplementedException(
                "RewriteTerminatorInputs does not handle terminator variant: " + terminator.GetType().Name)
        };
    }

    private ImmutableArray<PineBasicBlock> RewriteBlocks(Func<PineBasicBlock, PineBasicBlock> rewrite)
    {
        ImmutableArray<PineBasicBlock>.Builder? changed = null;

        foreach (var block in Blocks)
        {
            var rewritten = rewrite(block);

            if (!ReferenceEquals(block, rewritten))
            {
                changed ??= Blocks.ToBuilder();
                changed[block.Id.Value] = rewritten;
            }
        }

        return changed?.ToImmutable() ?? Blocks;
    }

    private struct OperationBuilder(ImmutableArray<PineControlFlowOperation> original, bool includeAll = false)
    {
        private readonly ImmutableArray<PineControlFlowOperation> _original = original;

        private ImmutableArray<PineControlFlowOperation>.Builder? _builder;

        // Until the first edit, the output is only a prefix of the original array.
        private int _count = includeAll ? original.Length : 0;

        public readonly int Count => _builder?.Count ?? _count;

        public PineControlFlowOperation this[int index]
        {
            readonly get => _builder is { } builder ? builder[index] : _original.AsSpan(.._count)[index];
            set => Edit()[index] = value;
        }

        public void Add(PineControlFlowOperation operation)
        {
            if (_builder is null &&
                _count < _original.Length &&
                ReferenceEquals(operation, _original[_count]))
            {
                _count++;
                return;
            }

            Edit().Add(operation);
        }

        public void RemoveAt(int index) => Edit().RemoveAt(index);

        public void RemoveRange(int index, int count) => Edit().RemoveRange(index, count);

        private ImmutableArray<PineControlFlowOperation>.Builder Edit()
        {
            if (_builder is null)
            {
                _builder = ImmutableArray.CreateBuilder<PineControlFlowOperation>(_original.Length);

                for (var index = 0; index < _count; index++)
                    _builder.Add(_original[index]);
            }

            return _builder;
        }

        public readonly PineBasicBlock Build(PineBasicBlock block) =>
            _builder is { } builder
            ?
            block with { Operations = builder.ToImmutable() }
            :
            _count == _original.Length
            ?
            block
            :
            block with { Operations = _original[.._count] };
    }
}
