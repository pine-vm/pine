using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private struct LocalSet
    {
        private const int DenseLimit = 4096;

        private ulong _first;

        private ulong[]? _rest;

        private HashSet<int>? _sparse;

        public readonly bool Contains(int local) =>
            (uint)local >= DenseLimit
            ?
            _sparse?.Contains(local) is true
            :
            local < 64
            ?
            (_first & (1UL << local)) != 0
            :
            _rest is { } rest && (local / 64 - 1) < rest.Length &&
            (rest[local / 64 - 1] & (1UL << (local & 63))) != 0;

        public readonly bool ContainsAll(ImmutableArray<int> locals)
        {
            foreach (var local in locals)
            {
                if (!Contains(local))
                    return false;
            }

            return true;
        }

        public readonly bool ContainsAny(ImmutableArray<int> locals)
        {
            foreach (var local in locals)
            {
                if (Contains(local))
                    return true;
            }

            return false;
        }

        public void Add(int local)
        {
            if ((uint)local >= DenseLimit)
            {
                (_sparse ??= []).Add(local);
            }
            else if (local < 64)
            {
                _first |= 1UL << local;
            }
            else
            {
                var index = local / 64 - 1;
                EnsureWords(index + 1);
                _rest![index] |= 1UL << (local & 63);
            }
        }

        public void Remove(int local)
        {
            if ((uint)local >= DenseLimit)
                _sparse?.Remove(local);

            else if (local < 64)
                _first &= ~(1UL << local);

            else if (_rest is { } rest && local / 64 - 1 < rest.Length)
                rest[local / 64 - 1] &= ~(1UL << (local & 63));
        }

        private void EnsureWords(int count)
        {
            if ((_rest?.Length ?? 0) < count)
                Array.Resize(ref _rest, count);
        }

        public void Clear()
        {
            _first = 0;

            if (_rest is { } rest)
                Array.Clear(rest);

            _sparse?.Clear();
        }

        // Struct assignment alone would alias the mutable backing storage.
        public readonly LocalSet Clone()
        {
            var clone = new LocalSet();
            clone.CopyFrom(this);
            return clone;
        }

        public void CopyFrom(in LocalSet other)
        {
            _first = other._first;
            var count = other._rest?.Length ?? 0;
            EnsureWords(count);

            if (_rest is { } rest)
            {
                if (other._rest is { } source)
                    source.CopyTo(rest, 0);

                Array.Clear(rest, count, rest.Length - count);
            }

            if (!ReferenceEquals(_sparse, other._sparse))
            {
                _sparse?.Clear();

                if (other._sparse is { Count: > 0 } sparse)
                    (_sparse ??= []).UnionWith(sparse);
            }
        }

        public void UnionWith(in LocalSet other)
        {
            _first |= other._first;

            if (other._rest is { } source)
            {
                EnsureWords(source.Length);

                for (var index = 0; index < source.Length; index++)
                    _rest![index] |= source[index];
            }

            if (other._sparse is { Count: > 0 } sparse)
                (_sparse ??= []).UnionWith(sparse);
        }

        public void ExceptWith(in LocalSet other)
        {
            _first &= ~other._first;

            if (_rest is { } rest && other._rest is { } source)
            {
                for (var index = 0; index < Math.Min(rest.Length, source.Length); index++)
                    rest[index] &= ~source[index];
            }

            if (other._sparse is { } sparse)
                _sparse?.ExceptWith(sparse);
        }

        public bool IntersectWith(in LocalSet other)
        {
            var changed = (_first & other._first) != _first;
            _first &= other._first;

            if (_rest is { } rest)
            {
                for (var index = 0; index < rest.Length; index++)
                {
                    var word = index < (other._rest?.Length ?? 0) ? other._rest![index] : 0;
                    changed |= (rest[index] & word) != rest[index];
                    rest[index] &= word;
                }
            }

            if (_sparse is { Count: > 0 } sparse)
            {
                var before = sparse.Count;

                if (other._sparse is { } otherSparse)
                    sparse.IntersectWith(otherSparse);

                else
                    sparse.Clear();

                changed |= before != sparse.Count;
            }

            return changed;
        }

        public readonly bool SetEquals(in LocalSet other)
        {
            if (_first != other._first)
                return false;

            var count = Math.Max(_rest?.Length ?? 0, other._rest?.Length ?? 0);

            for (var index = 0; index < count; index++)
            {
                if ((index < (_rest?.Length ?? 0) ? _rest![index] : 0) !=
                    (index < (other._rest?.Length ?? 0) ? other._rest![index] : 0))
                {
                    return false;
                }
            }

            return
                _sparse is { Count: > 0 } sparse
                ?
                other._sparse is { } otherSparse && sparse.SetEquals(otherSparse)
                :
                (other._sparse?.Count ?? 0) == 0;
        }
    }
}
