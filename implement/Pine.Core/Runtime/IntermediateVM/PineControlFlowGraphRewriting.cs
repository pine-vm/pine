using System;
using System.Collections.Immutable;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
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
