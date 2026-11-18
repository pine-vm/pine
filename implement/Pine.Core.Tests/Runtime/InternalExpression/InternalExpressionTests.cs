using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Runtime.InternalExpression;
using System;
using System.Buffers.Binary;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Security.Cryptography;
using Xunit;

using Canonical = Pine.Core.PineLanguage.Expression;
using Expression = Pine.Core.Runtime.InternalExpression.Expression;
using BuiltinFunction = Pine.Core.Runtime.InternalExpression.BuiltinFunction;

namespace Pine.Core.Tests.Runtime.InternalExpression;

public class InternalExpressionTests
{
    [Theory]
    [InlineData("arithmetic", "BuiltinBinary(IntAdd, BuiltinBinary(IntMul, Literal(0x0405), Environment), Literal(0x0404))")]
    [InlineData("selection", "Literal(0x0408)")]
    [InlineData("length", "Literal(0x04)")]
    [InlineData("concat", "List(Environment)")]
    [InlineData("take", "BuiltinBinary(Take, Literal(0x0402), Environment)")]
    [InlineData("skip", "BuiltinBinary(Skip, Literal(0x0405), Environment)")]
    public void Early_fused_reductions_have_rendered_snapshots_and_preserve_evaluation(
        string scenario,
        string snapshot)
    {
        var environment = Canonical.EnvironmentInstance;

        var source =
            scenario switch
            {
                "arithmetic" =>
                Call(
                    "int_add",
                    Call("int_mul", Integer(2), environment),
                    Call("int_add", Call("int_mul", Integer(3), environment), Integer(4))),

                "selection" =>
                Canonical.BuiltinInst(
                    "head",
                    Call(
                        "skip",
                        Integer(2),
                        Canonical.ListInst([environment, Integer(7), Integer(8)]))),

                "length" =>
                Call(
                    "equal",
                    Canonical.BuiltinInst(
                        "length",
                        Call(
                            "concat",
                            Canonical.ListInst([environment]),
                            Canonical.ListInst([environment, environment]))),
                    Integer(3)),

                "concat" =>
                Call("concat", Canonical.LitralInst(PineValue.EmptyList), Canonical.ListInst([environment])),

                "take" => Call("take", Integer(2), Call("take", Integer(5), environment)),
                "skip" => Call("skip", Integer(2), Call("skip", Integer(3), environment)),

                _ =>
                throw new ArgumentOutOfRangeException(nameof(scenario))
            };

        var cache = new PineVMParseCache();

        var retained =
            cache.ParseInternalExpression(ExpressionEncoding2026.EncodeExpressionAsValue(source))
            .Extract(error => throw new InvalidOperationException(error));

        var reduced =
            ExpressionReduction.ReduceExpressionBottomUp(
                retained,
                ReductionConfig.Default,
                cache,
                new Dictionary<(Expression, ReductionConfig), Expression>());

        Render(reduced).Should().Be(snapshot);
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        foreach (var value in new[]
        {
            PineValue.EmptyList, PineValue.Blob([4, 1]), PineValue.Blob([1, 2, 3, 4, 5, 6]),
            PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList, IntegerEncoding.EncodeSignedInteger(7)])
        })
        {
            interpreter.EvaluateExpressionDefault(ExpressionConversion.ToCanonical(reduced), value)
                .Should().Be(interpreter.EvaluateExpressionDefault(source, value));
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Immutable_policy_lookups_use_native_keys_without_allocating_canonical_views(bool frozen)
    {
        var source = Call("equal", Canonical.EnvironmentInstance, Integer(7));

        IReadOnlyDictionary<Canonical, int> table =
            frozen
            ?
            new Dictionary<Canonical, int> { [source] = 42 }.ToFrozenDictionary()
            :
            ImmutableDictionary<Canonical, int>.Empty.Add(source, 42);

        var lookup = ExpressionLookup<int>.Create(table) ?? throw new InvalidOperationException();

        var query =
            new Expression.BuiltinBinary(
                BuiltinFunction.Equal,
                Expression.Environment.Instance,
                new Expression.Literal(IntegerEncoding.EncodeSignedInteger(7)));

        var before = GC.GetAllocatedBytesForCurrentThread();
        var found = lookup.TryGetValue(query, out var value);
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        found.Should().BeTrue();
        value.Should().Be(42);
        allocated.Should().Be(0);
    }

    [Fact]
    public void Mutable_policy_lookups_preserve_replacement_removal_and_custom_key_comparers()
    {
        var source = Call("equal", Canonical.EnvironmentInstance, Integer(7));
        var retained = ExpressionConversion.FromCanonicalCached(source);
        var table = new Dictionary<Canonical, int> { [source] = 1 };
        var lookup = ExpressionLookup<int>.Create(table) ?? throw new InvalidOperationException();
        lookup.TryGetValue(retained, out var first).Should().BeTrue();
        first.Should().Be(1);
        table[source] = 2;
        lookup.TryGetValue(retained, out var replacement).Should().BeTrue();
        replacement.Should().Be(2);
        table.Remove(source);
        lookup.ContainsKey(retained).Should().BeFalse();

        var referenceTable =
            new Dictionary<Canonical, int>(ReferenceEqualityComparer.Instance) { [source] = 1 }
            .ToFrozenDictionary(ReferenceEqualityComparer.Instance);

        var referenceLookup = ExpressionLookup<int>.Create(referenceTable) ?? throw new InvalidOperationException();
        referenceLookup.ContainsKey(retained).Should().BeTrue();

        referenceLookup.ContainsKey(
            ExpressionConversion.FromCanonicalCached(
                Call("equal", Canonical.EnvironmentInstance, Integer(7)))).Should().BeFalse();
    }

    [Fact]
    public void Fused_direct_evaluation_matches_the_canonical_oracle_for_every_builtin()
    {
        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        foreach (var function in Enum.GetValues<BuiltinFunction>())
        {
            var canonical = Call(BuiltinFunctions.ToName(function), Integer(1), Canonical.EnvironmentInstance);
            var retained = ExpressionConversion.FromCanonicalCached(canonical);

            foreach (var environment in new[]
            {
                PineValue.EmptyList, PineValue.Blob([4, 2]), PineValue.Blob([1, 2, 3]),
                PineValue.List([PineValue.EmptyBlob, IntegerEncoding.EncodeSignedInteger(9)])
            })
            {
                var direct =
                    Core.Runtime.IntermediateVM.PineVM.EvalDirect(retained, PineValueInProcess.Create(environment))
                    ?? throw new InvalidOperationException("A static builtin pair should support direct evaluation.");

                direct.value.Evaluate().Should().Be(interpreter.EvaluateExpressionDefault(canonical, environment));
            }
        }
    }

    [Fact]
    public void Fused_recognition_does_not_bypass_deprecated_wrapper_validation()
    {
        var environment = ExpressionEncoding2026.EncodeExpressionAsValue(Canonical.EnvironmentInstance);

        var legacyLength =
            ExpressionEncoding2024.EncodeExpressionAsValue(
                Canonical.BuiltinInst("length", Canonical.EnvironmentInstance));

        var legacySkip =
            ExpressionEncoding2024.EncodeExpressionAsValue(Call("skip", Integer(2), Canonical.EnvironmentInstance));

        var malformed =
            new[]
            {
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("Builtin"), StringEncoding.ValueFromString("equal"),
                        PineValue.List([StringEncoding.ValueFromString("List"), legacyLength, environment])
                    ]),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("Builtin"),
                    StringEncoding.ValueFromString("head"),
                    legacySkip
                    ]),
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("Builtin"), StringEncoding.ValueFromString("equal"),
                        ExpressionEncoding2024.EncodeExpressionAsValue(Canonical.ListInst([Integer(1), Integer(2)]))
                    ])
            };

        var cache = new PineVMParseCache();

        foreach (var value in malformed)
        {
            ExpressionValueEncoding.ParseExpressionFromValue(value).IsErrOrNull().Should().NotBeNull();
            cache.ParseInternalExpression(value).IsErrOrNull().Should().NotBeNull();
            cache.ParseExpression(value).IsErrOrNull().Should().NotBeNull();
        }

        var legacyConstruction =
            new Expression.List(
                [
                new Expression.Literal(StringEncoding.ValueFromString("Literal")),
                new Expression.List([Expression.Environment.Instance])
                ]);

        ExpressionReduction.TryDecodeApplicationOfConstructedEncoding(
            legacyConstruction,
            Expression.Environment.Instance,
            cache)
            .Should().BeNull();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Reduction_cache_adapters_preserve_caller_storage_and_collection_contracts(bool shared)
    {
        IDictionary<(Canonical, ReductionConfig), Canonical> canonicalCache =
            shared
            ?
            new RetainedReductionCache()
            :
            new Dictionary<(Canonical, ReductionConfig), Canonical>();

        var source = Call("equal", Canonical.EnvironmentInstance, Integer(1));
        var retained = ExpressionConversion.FromCanonicalCached(source);
        var initial = Integer(2);
        canonicalCache[(source, ReductionConfig.Default)] = initial;

        var nativeCache =
            CanonicalExpressionAdapter.ToInternal(canonicalCache)
            ?? throw new InvalidOperationException("A supplied cache must have a native adapter.");

        nativeCache[(retained, ReductionConfig.Default)].Should()
            .BeSameAs(ExpressionConversion.FromCanonicalCached(initial));

        var replacement = Integer(3);
        nativeCache[(retained, ReductionConfig.Default)] = ExpressionConversion.FromCanonicalCached(replacement);
        canonicalCache[(source, ReductionConfig.Default)].Should().BeSameAs(replacement);

        KeyValuePair<(Expression, ReductionConfig), Expression>[] copied =
            new KeyValuePair<(Expression, ReductionConfig), Expression>[2];

        nativeCache.CopyTo(copied, 1);
        copied[1].Key.Should().Be((retained, ReductionConfig.Default));
        copied[1].Value.Should().BeSameAs(ExpressionConversion.FromCanonicalCached(replacement));
        Action insufficientDestination = () => nativeCache.CopyTo([], 0);
        insufficientDestination.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Frame_equality_and_hashing_ignore_lazy_representation_caches()
    {
        var source = Call("equal", Canonical.EnvironmentInstance, Integer(1));
        var retained = ExpressionConversion.FromCanonical(source);

        var instructions =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Return]);

        var input = StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyList);

        var frame =
            new StackFrame(
                null,
                new ExpressionIdentity(retained),
                instructions,
                input,
                Memory<PineValueInProcess?>.Empty,
                Memory<PineValueInProcess>.Empty,
                default,
                null);

        var hash = frame.GetHashCode();
        var copy = frame with { };
        _ = frame.Expression;
        frame.Should().Be(copy);
        frame.GetHashCode().Should().Be(hash);

        frame.Should().Be(
            new StackFrame(
                null,
                source,
                instructions,
                input,
                Memory<PineValueInProcess?>.Empty,
                Memory<PineValueInProcess>.Empty,
                default,
                null));
    }

    [Fact]
    public void Production_parsing_and_cached_metrics_commute_with_canonical_parsing()
    {
        var cache = new PineVMParseCache();

        foreach (var canonical in CanonicalCorpus())
        {
            foreach (var encoded in new[] { ExpressionEncoding2026.EncodeExpressionAsValue(canonical) })
            {
                var expected = cache.ParseExpression(encoded).Extract(error => throw new Exception(error));
                var retained = cache.ParseInternalExpression(encoded).Extract(error => throw new Exception(error));
                retained.Should().Be(ExpressionConversion.FromCanonical(expected));
                ExpressionConversion.FromCanonicalCached(canonical).Should().Be(retained);
                ExpressionConversion.ToCanonical(retained).Should().Be(expected);
                var fused = ExpressionFusion.Fuse(retained);
                ExpressionConversion.ToCanonical(fused).Should().Be(expected);
                fused.SubexpressionCount.Should().Be(expected.SubexpressionCount);
                fused.EvalCount.Should().Be(expected.EvalCount);
                fused.ConditionCount.Should().Be(expected.ConditionCount);
                fused.BuiltinCount.Should().Be(expected.BuiltinCount);
                fused.MaxDepth.Should().Be(expected.MaxDepth);
                fused.ReferencesEnvironment.Should().Be(expected.ReferencesEnvironment);

                cache.ParseInternalExpression(encoded).Extract(error => throw new Exception(error)).Should().BeSameAs(
                    retained);
            }
        }
    }

    [Fact]
    public void Final_fusion_skips_wrappers_and_preserves_unchanged_nodes()
    {
        var canonical =
            Canonical.ListInst(
                [
                    Call("equal", Canonical.EnvironmentInstance, Integer(7)),
                    Call(
                        "equal",
                        Canonical.EnvironmentInstance,
                        Canonical.BuiltinInst("length", Canonical.BuiltinInst("reverse", Canonical.EnvironmentInstance))),
                    Path(IntegerEncoding.EncodeSignedInteger(2))
                ]);

        var general = ExpressionConversion.FromCanonical(canonical, fusePopularShapes: false);
        var fused = ExpressionFusion.Fuse(general);

        Render(fused).Should().Be(
            "List(BuiltinBinary(Equal, Environment, Literal(0x0407)), Length_Equal_Binary(Builtin(Reverse, Environment), Environment, False), HeadSkip(2, Environment))");

        ExpressionConversion.ToCanonical(fused).Should().Be(canonical);
        ExpressionFusion.Fuse(fused).Should().BeSameAs(fused);
        var noOp = new Expression.List([Expression.Environment.Instance, new Expression.Literal(PineValue.EmptyBlob)]);
        ExpressionFusion.Fuse(noOp).Should().BeSameAs(noOp);

        ExpressionReduction.TransformPineExpressionWithOptionalReplacement(_ => null, fused).expr.Should().BeSameAs(
            fused);
    }

    [Fact]
    public void Retained_traversal_avoids_per_leaf_iterator_allocations()
    {
        var tree = new Expression.List([.. Enumerable.Repeat(Expression.Environment.Instance, 1024)]);
        _ = Count(tree);
        var before = GC.GetAllocatedBytesForCurrentThread();
        var count = Count(tree);
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        count.Should().Be(1025);
        allocated.Should().BeLessThan(24_000);

        static int Count(Expression root)
        {
            var count = 0;

            foreach (var node in Expression.EnumerateSelfAndDescendants(root))
                count++;

            return count;
        }
    }

    [Fact]
    public void Binary_builtin_fusion_covers_every_identifier_without_retaining_the_input_list()
    {
        var names = Enum.GetValues<BuiltinFunction>();

        var source =
            names.Select(
                function => Call(BuiltinFunctions.ToName(function), Canonical.EnvironmentInstance, Integer(7))).ToArray();

        var fused = source.Select(expression => ExpressionConversion.FromCanonical(expression)).ToArray();

        string.Join("\n", fused.Select(Render)).Should().Be(
            """
            BuiltinBinary(Equal, Environment, Literal(0x0407))
            BuiltinBinary(Length, Environment, Literal(0x0407))
            BuiltinBinary(Head, Environment, Literal(0x0407))
            BuiltinBinary(Skip, Environment, Literal(0x0407))
            BuiltinBinary(Take, Environment, Literal(0x0407))
            BuiltinBinary(Concat, Environment, Literal(0x0407))
            BuiltinBinary(Reverse, Environment, Literal(0x0407))
            BuiltinBinary(Negate, Environment, Literal(0x0407))
            BuiltinBinary(IntAdd, Environment, Literal(0x0407))
            BuiltinBinary(IntMul, Environment, Literal(0x0407))
            BuiltinBinary(IntIsSortedAsc, Environment, Literal(0x0407))
            BuiltinBinary(BitAnd, Environment, Literal(0x0407))
            BuiltinBinary(BitOr, Environment, Literal(0x0407))
            BuiltinBinary(BitXor, Environment, Literal(0x0407))
            BuiltinBinary(BitNot, Environment, Literal(0x0407))
            BuiltinBinary(BitShiftLeft, Environment, Literal(0x0407))
            BuiltinBinary(BitShiftRight, Environment, Literal(0x0407))
            """);

        for (var index = 0; index < source.Length; index++)
        {
            var general = ExpressionConversion.FromCanonical(source[index], fusePopularShapes: false);
            ExpressionFusion.Fuse(general).Should().Be(fused[index]);
            ExpressionFusion.Fuse(fused[index]).Should().BeSameAs(fused[index]);
            Expression.EnumerateSelfAndDescendants(general).Count().Should().Be(4);
            Expression.EnumerateSelfAndDescendants(fused[index]).Count().Should().Be(3);

            foreach (var encoded in new[] { ExpressionEncoding2026.EncodeExpressionAsValue(source[index]) })
            {
                ExpressionValueEncoding.ParseExpressionFromValue(encoded)
                    .Extract(error => throw new Exception(error)).Should().Be(fused[index]);
            }

            ExpressionReduction.IsKnownBooleanExpression(fused[index]).Should()
                .Be(names[index] is BuiltinFunction.Equal or BuiltinFunction.IntIsSortedAsc);

            var changed =
                ExpressionReduction.TransformPineExpressionWithOptionalReplacement(
                    expression =>
                    expression is Expression.Environment ? new Expression.Literal(PineValue.EmptyList) : null,
                    fused[index]).expr;

            ExpressionConversion.ToCanonical(changed).Should().Be(
                Call(BuiltinFunctions.ToName(names[index]), Canonical.LitralInst(PineValue.EmptyList), Integer(7)));
        }

        ExpressionAnalysis.TryParseExprAsPathInEnv(
            ExpressionConversion.FromCanonical(Path(PineValue.Blob([4, 0, 2])))).Should().Equal(2);
    }

    [Fact]
    public void Binary_builtin_independent_evaluation_preserves_function_and_first_failure()
    {
        var invalidLeft =
            new Expression.Eval(
                new Expression.Literal(PineValue.EmptyBlob),
                new Expression.Literal(PineValue.EmptyList));

        var environmentRight = Expression.Environment.Instance;
        var cache = new PineVMParseCache();

        foreach (var function in Enum.GetValues<BuiltinFunction>())
        {
            var binary =
                new Expression.BuiltinBinary(
                    function,
                    new Expression.Literal(IntegerEncoding.EncodeSignedInteger(1)),
                    new Expression.Literal(IntegerEncoding.EncodeSignedInteger(2)));

            ExpressionReduction.TryEvalIndependent(binary, cache).Value.Should().Be(
                Core.PineLanguage.BuiltinFunction.ApplyFunctionGeneric(
                    BuiltinFunctions.ToName(function),
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(1), IntegerEncoding.EncodeSignedInteger(2)])));

            ExpressionReduction.TryEvalIndependent(
                new Expression.BuiltinBinary(function, invalidLeft, environmentRight),
                cache)
                .Error.Should().Be(ExpressionReduction.TryEvalIndependent(invalidLeft, cache).Error);
        }

        Action invalid = () => new Expression.BuiltinBinary((BuiltinFunction)1234, environmentRight, environmentRight);
        invalid.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Native_fused_IR_matches_canonical_interpreter(bool blob)
    {
        var environment =
            blob
            ?
            PineValue.Blob([1, 2, 3])
            :
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(1),
                IntegerEncoding.EncodeSignedInteger(2),
                PineValue.EmptyList
                ]);

        var source = Canonical.BuiltinInst("reverse", Canonical.EnvironmentInstance);

        Canonical[] cases =
            [
                Call("equal", source, Canonical.EnvironmentInstance),
                Call("equal", Canonical.BuiltinInst("length", source), Integer(3)),
                Call("equal", Integer(3), Canonical.BuiltinInst("length", source)),
                Call(
                    "equal",
                    Canonical.BuiltinInst("length", source),
                    Canonical.BuiltinInst("length", Canonical.EnvironmentInstance)),
                Call("equal", Canonical.LitralInst(PineValue.Blob([4, 0, 3])), Canonical.BuiltinInst("length", source)),
                Path(IntegerEncoding.EncodeSignedInteger(-2)),
                Path(IntegerEncoding.EncodeSignedInteger(2)),
                Path(IntegerEncoding.EncodeSignedInteger(int.MaxValue))
            ];

        var interpreter = DirectInterpreter.WithoutEvalCaching(new PineVMParseCache());

        foreach (var canonical in cases.Concat(
            Enum.GetValues<BuiltinFunction>().Select(
                function => Call(BuiltinFunctions.ToName(function), Integer(1), source))))
        {
            var retained = ExpressionConversion.FromCanonical(canonical);

            var graph =
                InternalExpressionCompilation.ControlFlowGraphFromExpression(
                    retained,
                    [],
                    null,
                    StaticFunctionInterface.Generic,
                    new PineVMParseCache());

            var compilation =
                new ExpressionCompilation(
                    new StackFrameInstructions(StaticFunctionInterface.Generic, graph.LowerToStackInstructions(), null),
                    []);

            var vm =
                Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                    evalCache: null,
                    evaluationConfigDefault: null,
                    reportFunctionApplication: null,
                    compilationEnvClasses: null,
                    disableReductionInCompilation: false,
                    selectPrecompiled: null,
                    skipInlineForExpression: null,
                    enableTailRecursionOptimization: true,
                    parseCache: null,
                    precompiledLeaves: null,
                    reportEnterPrecompiledLeaf: null,
                    reportExitPrecompiledLeaf: null,
                    optimizationParametersSerial: null,
                    cacheFileStore: null,
                    expressionCompilationOverrides:
                    new Dictionary<Expression, ExpressionCompilation> { [retained] = compilation });

            var expected =
                canonical is Canonical.Builtin builtin
                ?
                interpreter.EvaluateBuiltinExpressionGeneric(environment, builtin)
                :
                interpreter.EvaluateExpressionDefault(canonical, environment);

            vm.EvaluateExpression(canonical, environment).Extract(error => throw new Exception(error))
                .Should().Be(expected);
        }
    }

    private static Canonical Integer(BigInteger value) =>
        Canonical.LitralInst(IntegerEncoding.EncodeSignedInteger(value));

    private static Canonical Call(string name, params Canonical[] items) =>
        Canonical.BuiltinInst(name, Canonical.ListInst(items));

    private static Canonical Path(PineValue count) =>
        Canonical.BuiltinInst("head", Call("skip", Canonical.LitralInst(count), Canonical.EnvironmentInstance));

    private static byte[] Encode(Expression expression)
    {
        using var stream = new MemoryStream();
        ExpressionBinary.Encode(stream, expression);
        return stream.ToArray();
    }

    private static string Render(Expression expression) =>
        expression switch
        {
            Expression.Literal literal => "Literal(" + Value(literal.Value) + ")",
            Expression.List list => "List(" + string.Join(", ", list.Items.Select(Render)) + ")",
            Expression.Eval eval => "Eval(" + Render(eval.Encoded) + ", " + Render(eval.Environment) + ")",
            Expression.Builtin builtin => "Builtin(" + builtin.Function + ", " + Render(builtin.Input) + ")",

            Expression.Conditional conditional =>
            "Conditional(" + Render(conditional.Condition) + ", " + Render(conditional.FalseBranch) + ", " +
            Render(conditional.TrueBranch) +
            ")",

            Expression.Environment => "Environment",
            Expression.Label label => "Label(" + Value(label.Value) + ", " + Render(label.Tagged) + ")",

            Expression.BuiltinBinary binary =>
            "BuiltinBinary(" + binary.Function + ", " + Render(binary.Left) + ", " + Render(binary.Right) + ")",

            Expression.Length_Equal_Binary length =>
            "Length_Equal_Binary(" + Render(length.Sequence) + ", " + Render(length.Other) + ", " + length.LengthOnLeft +
            ")",

            Expression.HeadSkip head => "HeadSkip(" + head.Count + ", " + Render(head.Input) + ")",

            _ =>
            throw new NotImplementedException(
                nameof(Render) + " does not handle expression variant: " + expression.GetType().Name)
        };

    private static string Value(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob => "0x" + Convert.ToHexStringLower(blob.Bytes.Span),
            PineValue.ListValue list => "[" + string.Join(",", list.Items.ToArray().Select(Value)) + "]",

            _ =>
            throw new NotImplementedException(nameof(Value) + " does not handle value variant: " + value.GetType().Name)
        };

    private static Canonical[] CanonicalCorpus() =>
        [
            Integer(7),
            Canonical.ListInst([]),
            Canonical.ListInst([Integer(1), Integer(2)]),
            new Canonical.Eval(Integer(0), Canonical.EnvironmentInstance),
            Canonical.BuiltinInst("length", Canonical.EnvironmentInstance),
            Canonical.ConditionalInst(Canonical.EnvironmentInstance, Integer(0), Integer(1)),
            Canonical.EnvironmentInstance,
            new Canonical.Label(PineValue.List([PineValue.Blob([0xff])]), Integer(7)),
            Canonical.BuiltinInst("equal", Canonical.EnvironmentInstance),
            Call("equal", Canonical.EnvironmentInstance, Integer(7)),
            Canonical.BuiltinInst("head", Canonical.EnvironmentInstance),
            Path(IntegerEncoding.EncodeSignedInteger(2)),
            Path(IntegerEncoding.EncodeSignedInteger(-3)),
            Path(IntegerEncoding.EncodeSignedInteger(int.MinValue)),
            Path(IntegerEncoding.EncodeSignedInteger(int.MaxValue)),
            Path(IntegerEncoding.EncodeSignedInteger(BigInteger.One << 100)),
            Path(PineValue.Blob([4, 0, 2])),
            Path(PineValue.Blob([2, 0])),
            Path(PineValue.EmptyList),
            Call("equal"),
            Call("equal", Integer(1), Integer(2), Integer(3)),
            Canonical.BuiltinInst("head", Call("skip", Integer(1))),
            Canonical.BuiltinInst("head", Call("skip", Canonical.EnvironmentInstance, Integer(1))),
            Call("equal", Canonical.BuiltinInst("length", Canonical.EnvironmentInstance), Integer(3)),
            Call("equal", Integer(3), Canonical.BuiltinInst("length", Canonical.EnvironmentInstance)),
            Call(
                "equal",
                Canonical.BuiltinInst("length", Path(IntegerEncoding.EncodeSignedInteger(2))),
                Canonical.EnvironmentInstance),
            Call(
                "equal",
                Canonical.EnvironmentInstance,
                Canonical.BuiltinInst("length", Canonical.EnvironmentInstance)),
            Call(
                "equal",
                Canonical.BuiltinInst("length", Canonical.EnvironmentInstance),
                Canonical.BuiltinInst("length", Integer(7))),
            Call("equal", Canonical.BuiltinInst("length", Canonical.EnvironmentInstance)),
            Call("equal", Canonical.BuiltinInst("length", Canonical.EnvironmentInstance), Integer(1), Integer(2))
        ];

    private static Expression[] RetainedCorpus() =>
        [
            .. CanonicalCorpus().Select(expression => ExpressionConversion.FromCanonical(expression)),
            .. Enum.GetValues<BuiltinFunction>().Select(
                function => new Expression.Builtin(function, Expression.Environment.Instance)),
            .. Enum.GetValues<BuiltinFunction>().Select(
                function =>
                new Expression.BuiltinBinary(function, Expression.Environment.Instance, Expression.Environment.Instance)),
            new Expression.Literal(PineValue.Blob([0, 1, 2, 255])),
            new Expression.Literal(PineValue.List([PineValue.Blob([7]), PineValue.EmptyList])),
            new Expression.Label(PineValue.Blob(new byte[4097]), Expression.Environment.Instance),
            new Expression.Label(
                StringEncoding.ValueFromString_2024("length"),
                new Expression.Literal(
                    PineValue.List([StringEncoding.ValueFromString("Condition"), PineValue.Blob([2, 0])])))
        ];

    [Fact]
    public void Conversion_snapshot_exposes_fused_shapes_and_preserves_exact_canonical_syntax()
    {
        var corpus = CanonicalCorpus();

        string.Join(
            "\n",
            corpus.Select(expression => Render(ExpressionConversion.FromCanonical(expression)))).Should().Be(
            """
            Literal(0x0407)
            List()
            List(Literal(0x0401), Literal(0x0402))
            Eval(Literal(0x0400), Environment)
            Builtin(Length, Environment)
            Conditional(Environment, Literal(0x0400), Literal(0x0401))
            Environment
            Label([0xff], Literal(0x0407))
            Builtin(Equal, Environment)
            BuiltinBinary(Equal, Environment, Literal(0x0407))
            Builtin(Head, Environment)
            HeadSkip(2, Environment)
            HeadSkip(-3, Environment)
            HeadSkip(-2147483648, Environment)
            HeadSkip(2147483647, Environment)
            Builtin(Head, BuiltinBinary(Skip, Literal(0x0410000000000000000000000000), Environment))
            Builtin(Head, BuiltinBinary(Skip, Literal(0x040002), Environment))
            Builtin(Head, BuiltinBinary(Skip, Literal(0x0200), Environment))
            Builtin(Head, BuiltinBinary(Skip, Literal([]), Environment))
            Builtin(Equal, List())
            Builtin(Equal, List(Literal(0x0401), Literal(0x0402), Literal(0x0403)))
            Builtin(Head, Builtin(Skip, List(Literal(0x0401))))
            Builtin(Head, BuiltinBinary(Skip, Environment, Literal(0x0401)))
            Length_Equal_Binary(Environment, Literal(0x0403), True)
            Length_Equal_Binary(Environment, Literal(0x0403), False)
            Length_Equal_Binary(HeadSkip(2, Environment), Environment, True)
            Length_Equal_Binary(Environment, Environment, False)
            Length_Equal_Binary(Environment, Builtin(Length, Literal(0x0407)), True)
            Builtin(Equal, List(Builtin(Length, Environment)))
            Builtin(Equal, List(Builtin(Length, Environment), Literal(0x0401), Literal(0x0402)))
            """);

        foreach (var canonical in corpus)
        {
            foreach (var fuse in new[] { false, true })
            {
                var retained = ExpressionConversion.FromCanonical(canonical, fuse);
                ExpressionConversion.ToCanonical(retained).Should().Be(canonical);

                ExpressionConversion.FromCanonical(ExpressionConversion.ToCanonical(retained), fuse).Should().Be(
                    retained);
            }
        }
    }

    [Fact]
    public void Builtin_enum_covers_all_language_names_and_rejects_unknown_keys_even_below_fused_nodes()
    {
        var names = Enum.GetValues<BuiltinFunction>().Select(BuiltinFunctions.ToName).ToArray();

        string.Join(", ", names).Should().Be(
            "equal, length, head, skip, take, concat, reverse, negate, int_add, int_mul, int_is_sorted_asc, bit_and, bit_or, bit_xor, bit_not, bit_shift_left, bit_shift_right");

        foreach (var name in names)
        {
            BuiltinFunctions.ToName(BuiltinFunctions.FromName(name)).Should().Be(name);
            var canonical = Canonical.BuiltinInst(name, Canonical.EnvironmentInstance);
            ExpressionConversion.ToCanonical(ExpressionConversion.FromCanonical(canonical)).Should().Be(canonical);
            Pine.Core.PineLanguage.BuiltinFunction.ApplyFunctionGeneric(name, PineValue.EmptyList).Should().NotBeNull();
        }

        foreach (var name in new[] { "", "Equal", "equal_binary", "unknown" })
        {
            Action convert =
                () => ExpressionConversion.FromCanonical(Call("equal", Canonical.BuiltinInst(name, Integer(0)), Integer(1)));

            convert.Should().Throw<ArgumentException>().WithMessage("*Unknown Pine builtin*");
        }

        Action invalidEnum = () => new Expression.Builtin((BuiltinFunction)1234, Expression.Environment.Instance);
        invalidEnum.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void Fused_binary_snapshot_has_only_constructor_hashes_and_retained_fields()
    {
        var expression = new Expression.HeadSkip(2, Expression.Environment.Instance);
        var bytes = Encode(expression);

        Convert.ToHexStringLower(bytes).Should().Be(
            "0000000600000005b9937055783274d3a36bf45b4cf2a56c3be282325678742baf38769ef1d22f09" +
            "0000000300000002000000010000000204020000" +
            "00000006000000058de96843187da81f001af05d563609093b806eb0fdc59d633312d046b61cb2960000000300000000");

        Convert.ToHexStringLower(ExpressionBinary.ComputeHash(expression).Span).Should().Be(
            "08a242681a7d498dfd2e325f69021a4c0ecb0cdb8ffbac00a10782dab412e621");

        ExpressionBinary.Decode(bytes, out var consumed).Should().Be(expression);
        consumed.Should().Be(108);
    }

    [Fact]
    public void Shared_constructor_hash_snapshot_and_unshared_legacy_wire_decode_to_the_same_retained_model()
    {
        var first = new Expression.HeadSkip(1, Expression.Environment.Instance);
        var second = new Expression.HeadSkip(2, Expression.Environment.Instance);
        var original = new Expression.List([first, second]);
        var bytes = Encode(original);
        bytes.Length.Should().Be(196);

        Convert.ToHexStringLower(bytes.AsSpan(156)).Should().Be(
            "0000000600000004ffffff90000000030000000200000001000000020402000000000004ffffffac");

        ExpressionBinary.Decode(bytes, out _).Should().Be(original);

        using var stream = new MemoryStream();
        stream.Write(bytes.AsSpan(0, 48));
        stream.Write(Encode(first));
        stream.Write(Encode(second));
        var legacy = stream.ToArray();
        ExpressionBinary.Decode(legacy, out _).Should().Be(original);
        Encode(ExpressionBinary.Decode(legacy, out _)).Should().Equal(bytes);
        ExpressionBinary.ComputeHashUncached(original).ToArray().Should().Equal(SHA256.HashData(bytes));
    }

    [Fact]
    public void Binary_and_canonical_symmetries_hold_for_every_retained_variant_and_builtin()
    {
        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        ValueResult<string, PineValue> Evaluate(PineValue expression, PineValue environment) =>
            interpreter.EvaluateExpressionDefault(
                cache.ParseExpression(expression).Extract(error => throw new InvalidOperationException(error)),
                environment);

        foreach (var original in RetainedCorpus())
        {
            var bytes = Encode(original);
            var decoded = ExpressionBinary.Decode(bytes, out var consumed);
            consumed.Should().Be(bytes.Length);
            decoded.Should().Be(original);
            Encode(decoded).Should().Equal(bytes);
            ExpressionConversion.ToCanonical(decoded).Should().Be(ExpressionConversion.ToCanonical(original));

            var interpreted =
                PineTreeBinaryEncoding.DecodeRoot(
                    bytes,
                    resolveExternalReference: ExpressionBinary.ResolveConstructorExpression,
                    evaluate: Evaluate)
                .Extract(error => throw new InvalidOperationException(error.ToString()));

            interpreted.Should().Be(
                ExpressionEncoding.EncodeExpressionAsValue(ExpressionConversion.ToCanonical(original)));

            ExpressionBinary.ComputeHash(original).ToArray().Should().Equal(SHA256.HashData(bytes));
            ExpressionBinary.ComputeHashUncached(original).ToArray().Should().Equal(SHA256.HashData(bytes));
        }
    }

    [Fact]
    public void Direct_canonical_value_encoding_and_decoding_commute_with_canonical_conversion()
    {
        var cache = new PineVMParseCache();

        foreach (var original in RetainedCorpus())
        {
            var canonical = ExpressionConversion.ToCanonical(original);
            var expectedValue = ExpressionEncoding.EncodeExpressionAsValue(canonical);
            var directValue = ExpressionValueEncoding.EncodeExpressionAsValue(original);
            directValue.Should().Be(expectedValue);

            ExpressionEncoding.ParseExpressionFromValue(directValue)
                .Extract(error => throw new InvalidOperationException(error)).Should().Be(canonical);

            foreach (var fuse in new[] { false, true })
            {
                var viaCanonical =
                    ExpressionConversion.FromCanonical(
                        ExpressionEncoding.ParseExpressionFromValue(expectedValue)
                        .Extract(error => throw new InvalidOperationException(error)),
                        fuse);

                var direct =
                    ExpressionValueEncoding.ParseExpressionFromValue(expectedValue, fuse)
                    .Extract(error => throw new InvalidOperationException(error));

                direct.Should().Be(viaCanonical);
                ExpressionConversion.ToCanonical(direct).Should().Be(canonical);
                ExpressionValueEncoding.EncodeExpressionAsValue(direct).Should().Be(expectedValue);
            }

            ExpressionValueEncoding.EncodeExpressionAsValue(
                cache.ParseInternalExpression(directValue).Extract(error => throw new InvalidOperationException(error)))
                .Should().Be(directValue);

            ExpressionEncoding2026.EncodeExpressionAsValue(
                cache.ParseExpression(directValue).Extract(error => throw new InvalidOperationException(error)))
                .Should().Be(directValue);
        }
    }

    [Fact]
    public void Production_parsers_reject_2024_encodings_and_noncanonical_compact_variants()
    {
        var corpus =
            CanonicalCorpus().Concat(
                [new Canonical.Label(StringEncoding.ValueFromString("legacy"), Canonical.EnvironmentInstance)]);

        foreach (var original in corpus)
        {
            var value = ExpressionEncoding2024.EncodeExpressionAsValue(original);

            foreach (var fuse in new[] { false, true })
                ExpressionValueEncoding.ParseExpressionFromValue(value, fuse).IsErr().Should().BeTrue();

            new PineVMParseCache().ParseInternalExpression(value).IsErr().Should().BeTrue();
            new PineVMParseCache().ParseExpression(value).IsErr().Should().BeTrue();
        }

        var cache = new PineVMParseCache();

        foreach (var value in NoncanonicalExpressionValues())
        {
            foreach (var fuse in new[] { false, true })
                ExpressionValueEncoding.ParseExpressionFromValue(value, fuse).IsErr().Should().BeTrue();

            cache.ParseInternalExpression(value).IsErr().Should().BeTrue();
            cache.ParseExpression(value).IsErr().Should().BeTrue();

            var construction =
                ExpressionConversion.FromCanonical(
                    Canonical.ListInst([.. value.Items.ToArray().Select(Canonical.LitralInst)]));

            ExpressionReduction.TryDecodeApplicationOfConstructedEncoding(
                construction,
                Expression.Environment.Instance,
                cache).Should().BeNull();
        }
    }

    private static PineValue.ListValue[] NoncanonicalExpressionValues()
    {
        var environment = ExpressionEncoding2026.EncodeExpressionAsValue(Canonical.EnvironmentInstance);
        var builtin = StringEncoding.ValueFromString("Builtin");

        var legacyLength =
            PineValue.List([builtin, StringEncoding.ValueFromString_2024("length"), environment]);

        var alias =
            PineValue.List([StringEncoding.ValueFromString("Condition"), environment, environment, environment]);

        return
            [
            alias,
            legacyLength,
            PineValue.List([StringEncoding.ValueFromString_2024("Environment")]),
            PineValue.List([StringEncoding.ValueFromString_2024("Litral"), PineValue.EmptyList]),
            PineValue.List(
                [
                builtin, StringEncoding.ValueFromString("equal"),
                PineValue.List([StringEncoding.ValueFromString("List"), legacyLength, environment])
                ]),
            PineValue.List(
                [
                builtin, StringEncoding.ValueFromString("head"),
                PineValue.List(
                    [
                    builtin, StringEncoding.ValueFromString_2024("skip"),
                    ExpressionEncoding2026.EncodeExpressionAsValue(
                        Canonical.ListInst([Integer(0), Canonical.EnvironmentInstance]))
                    ])
                ]),
            PineValue.List(
                [
                StringEncoding.ValueFromString("Conditional"),
                ExpressionEncoding2026.EncodeExpressionAsValue(Canonical.LitralInst(PineKernelValues.TrueValue)),
                alias, environment
                ])
            ];
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Eval_rejects_noncanonical_values_including_constructed_code_with_and_without_optimizations(
        bool optimize)
    {
        var vm =
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: !optimize,
                selectPrecompiled: null,
                skipInlineForExpression: null,
                enableTailRecursionOptimization: optimize,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                disableGenericApplicationChainConsolidation: !optimize,
                disableDirectContinueForSimpleEval: !optimize,
                disableDirectEvalForSimpleTemplate: !optimize,
                disableDirectInvocation: !optimize);

        foreach (var value in NoncanonicalExpressionValues())
        {
            foreach (var code in new Canonical[]
            {
                Canonical.EnvironmentInstance,
                Canonical.LitralInst(value),
                Canonical.ListInst([.. value.Items.ToArray().Select(Canonical.LitralInst)])
            })
            {
                var result =
                    vm.EvaluateExpressionOnCustomStack(
                        new Canonical.Eval(code, Canonical.LitralInst(PineValue.EmptyList)),
                        value,
                        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Unbounded);

                var error = result.IsErrOrNull();
                error.Should().NotBeNull();
                error!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>();
            }
        }
    }

    [Fact]
    public void Direct_value_parser_rejects_malformed_shapes_and_unknown_builtins_inside_fusions()
    {
        var environment = ExpressionEncoding.EncodeExpressionAsValue(Canonical.EnvironmentInstance);

        foreach (var invalid in new PineValue[]
        {
            PineValue.EmptyList, PineValue.Blob([0]),
            PineValue.List([StringEncoding.ValueFromString("Missing")]),
            PineValue.List([PineValue.Blob([0])]),
            PineValue.List([StringEncoding.ValueFromString("Litral")]),
            PineValue.List([StringEncoding.ValueFromString("Environment"), PineValue.Blob([0])]),
            PineValue.List([StringEncoding.ValueFromString("Eval"), environment]),
            PineValue.List([StringEncoding.ValueFromString("Conditional"), environment, environment]),
            PineValue.List([StringEncoding.ValueFromString("Label"), environment]),
            PineValue.List([StringEncoding.ValueFromString("Builtin"), PineValue.EmptyList, environment]),
            PineValue.List([StringEncoding.ValueFromString("List"), PineValue.Blob([0])])
        })
        {
            ExpressionValueEncoding.ParseExpressionFromValue(invalid).IsErrOrNull().Should().NotBeNull();
        }

        foreach (var name in new[] { "unknown", "Length", "equal_binary", "Length_Equal_Binary" })
        {
            var invalid =
                Call("equal", Canonical.BuiltinInst("length", Canonical.BuiltinInst(name, Integer(0))), Integer(1));

            ExpressionValueEncoding.ParseExpressionFromValue(ExpressionEncoding.EncodeExpressionAsValue(invalid))
                .IsErrOrNull().Should().Contain("Unknown Pine builtin");
        }
    }

    [Fact]
    public void Direct_canonical_codec_preserves_payload_and_child_identity_and_handles_deep_trees()
    {
        var payload = PineValue.List([PineValue.Blob([255]), PineValue.EmptyList]);
        var child = new Expression.Label(payload, new Expression.Literal(payload));
        var root = new Expression.List([child, child]);
        var value = ExpressionValueEncoding.EncodeExpressionAsValue(root);
        value.Items.Span[1].Should().BeSameAs(value.Items.Span[2]);

        var restored =
            (Expression.List)ExpressionValueEncoding.ParseExpressionFromValue(value)
            .Extract(error => throw new InvalidOperationException(error));

        restored.Items[0].Should().BeSameAs(restored.Items[1]);
        var label = (Expression.Label)restored.Items[0];
        label.Value.Should().BeSameAs(payload);
        ((Expression.Literal)label.Tagged).Value.Should().BeSameAs(payload);

        Expression deep = Expression.Environment.Instance;

        for (var depth = 0; depth < 600; depth++)
            deep = new Expression.Builtin(BuiltinFunction.Head, deep);

        var encoded = ExpressionValueEncoding.EncodeExpressionAsValue(deep);

        ExpressionValueEncoding.ParseExpressionFromValue(encoded)
            .Extract(error => throw new InvalidOperationException(error)).Should().Be(deep);
    }

    [Fact]
    public void Length_equal_wire_preserves_operand_order_and_rejects_invalid_ordering_flag()
    {
        var sequence = new Expression.HeadSkip(2, Expression.Environment.Instance);
        var other = new Expression.Eval(Expression.Environment.Instance, Expression.Environment.Instance);
        var left = new Expression.Length_Equal_Binary(sequence, other);
        var right = new Expression.Length_Equal_Binary(sequence, other, lengthOnLeft: false);
        Encode(left).Should().NotEqual(Encode(right));
        ExpressionBinary.ComputeHash(left).ToArray().Should().NotEqual(ExpressionBinary.ComputeHash(right).ToArray());

        var bytes = Encode(left);
        bytes[^3] = 2;
        Action decode = () => ExpressionBinary.Decode(bytes, out _);
        decode.Should().Throw<InvalidDataException>().WithMessage("*ordering flag*");
    }

    [Fact]
    public void Immutable_children_payload_sharing_and_nonfused_normalization_are_explicit()
    {
        var payload = PineValue.List([PineValue.Blob([7, 8, 9])]);

        Expression[] mutable =
            [
                new Expression.Literal(payload),
                new Expression.Label(payload, Expression.Environment.Instance)
            ];

        var retained = new Expression.List(mutable);
        mutable[0] = Expression.Environment.Instance;
        var decoded = (Expression.List)ExpressionBinary.Decode(Encode(retained), out _);
        decoded.Should().Be(retained);
        ((Expression.Literal)decoded.Items[0]).Value.Should().BeSameAs(((Expression.Label)decoded.Items[1]).Value);

        ExpressionConversion.ToCanonical(retained).Should().Be(
            Canonical.ListInst(
                [
                Canonical.LitralInst(payload),
                new Canonical.Label(payload, Canonical.EnvironmentInstance)
                ]));

        var generic =
            new Expression.Builtin(
                BuiltinFunction.Equal,
                new Expression.List(
                    ImmutableArray.Create<Expression>(Expression.Environment.Instance, Expression.Environment.Instance)));

        Render(ExpressionBinary.Decode(Encode(generic), out _)).Should().Be(
            "Builtin(Equal, List(Environment, Environment))");

        Render(ExpressionConversion.FromCanonical(ExpressionConversion.ToCanonical(generic))).Should().Be(
            "BuiltinBinary(Equal, Environment, Environment)");

        ExpressionConversion.FromCanonical(ExpressionConversion.ToCanonical(generic), fusePopularShapes: false)
            .Should().Be(generic);
    }

    [Fact]
    public void Canonical_expansion_keeps_evaluation_order_and_failure_semantics()
    {
        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);
        var encodedIdentity = ExpressionEncoding.EncodeExpressionAsValue(Canonical.EnvironmentInstance);

        Canonical[] expressions =
            [
                .. CanonicalCorpus(),
                new Canonical.Eval(Canonical.LitralInst(encodedIdentity), Integer(7)),
                Call("equal", new Canonical.Eval(Canonical.LitralInst(encodedIdentity), Integer(7)), Integer(7))
            ];

        foreach (var original in expressions)
        {
            foreach (var environment in new[]
            {
                PineValue.EmptyList, PineValue.Blob([4, 1]),
                PineValue.List([PineValue.Blob([4, 1]), PineValue.Blob([4, 2]), PineValue.Blob([4, 3])])
            })
            {
                var restored =
                    ExpressionConversion.ToCanonical(
                        ExpressionBinary.Decode(
                            Encode(ExpressionConversion.FromCanonical(original)),
                            out _));

                PineValue expected;

                try
                {
                    expected = interpreter.EvaluateExpressionDefault(original, environment);
                }
                catch (ParseExpressionException error)
                {
                    Action evaluate = () => interpreter.EvaluateExpressionDefault(restored, environment);
                    evaluate.Should().Throw<ParseExpressionException>().WithMessage(error.Message);
                    continue;
                }

                interpreter.EvaluateExpressionDefault(restored, environment).Should().Be(expected);
            }
        }
    }

    [Fact]
    public void Structural_sharing_and_embedding_survive_without_referenceable_scalar_wrappers()
    {
        var child = new Expression.HeadSkip(2, Expression.Environment.Instance);

        var original =
            new Expression.List(
                ImmutableArray.Create<Expression>(
                    child,
                    new Expression.HeadSkip(2, Expression.Environment.Instance)));

        var bytes = Encode(original);
        var restored = (Expression.List)ExpressionBinary.Decode(bytes, out _);
        restored.Items[0].Should().BeSameAs(restored.Items[1]);
        Encode(new Expression.List(ImmutableArray.Create<Expression>(child, child))).Should().Equal(bytes);
        new Dictionary<Expression, int> { [original] = 1 }.ContainsKey(restored).Should().BeTrue();

        using var stream = new MemoryStream();
        stream.WriteByte(0xaa);

        using (var encoder = new ExpressionBinary.Encoder(stream.Write))
        {
            encoder.WriteExpression(child);
            encoder.WriteExpression(original);
            encoder.WriteExpression(child);
        }

        var extent = (int)stream.Length - 1;
        ExpressionBinary.Encode(stream, Expression.Environment.Instance);
        stream.WriteByte(0xbb);
        using var decoder = new ExpressionBinary.Decoder(stream.ToArray().AsMemory(1));
        var first = decoder.ReadExpression();
        ((Expression.List)decoder.ReadExpression()).Items[0].Should().BeSameAs(first);
        decoder.ReadExpression().Should().BeSameAs(first);
        decoder.BytesRead.Should().Be(extent);

        ExpressionBinary.Decode(stream.ToArray().AsMemory(extent + 1), out var secondExtent)
            .Should().Be(Expression.Environment.Instance);

        stream.ToArray()[extent + 1 + secondExtent].Should().Be(0xbb);

        var canonical = ExpressionConversion.ToCanonical(original);
        var converted = (Expression.List)ExpressionConversion.FromCanonical(canonical);
        converted.Items[0].Should().BeSameAs(converted.Items[1]);
    }

    [Fact]
    public void Rejects_unknown_hashes_wrong_arities_types_truncations_and_skipped_entry_references()
    {
        var environment = Encode(Expression.Environment.Instance);
        var unknown = (byte[])environment.Clone();
        unknown.AsSpan(8, 32).Clear();
        var wrongArity = (byte[])environment.Clone();
        BinaryPrimitives.WriteInt32BigEndian(wrongArity.AsSpan(44), 1);

        foreach (var bytes in new[] { unknown, wrongArity })
        {
            Action decode = () => ExpressionBinary.Decode(bytes, out _);
            decode.Should().Throw<InvalidDataException>();
        }

        var head = Encode(new Expression.HeadSkip(0, Expression.Environment.Instance));

        for (var length = 0; length < head.Length; length++)
        {
            Action decode = () => ExpressionBinary.Decode(head.AsMemory(0, length), out _);
            decode.Should().Throw<InvalidDataException>();
        }

        using (var stream = new MemoryStream())
        {
            PineTreeBinaryEncoding.Encode(stream, PineValue.EmptyList);
            Action decode = () => ExpressionBinary.Decode(stream.ToArray(), out _);
            decode.Should().Throw<InvalidDataException>();
        }

        foreach (var target in new[] { 4, 40 })
        {
            var bytes = new byte[environment.Length + 8];
            environment.CopyTo(bytes, 0);

            BinaryPrimitives.WriteInt32BigEndian(
                bytes.AsSpan(environment.Length),
                PineTreeBinaryEncoding.TagReferenceInteral);

            BinaryPrimitives.WriteInt32BigEndian(
                bytes.AsSpan(environment.Length + 4),
                target - (environment.Length + 4));

            Action decode =
                () =>
                {
                    using var reader = new ExpressionBinary.Decoder(bytes);
                    reader.ReadExpression();
                    reader.ReadExpression();
                };

            decode.Should().Throw<InvalidDataException>();
        }
    }

    [Fact]
    public void Rejects_invalid_inline_counts_and_builtin_ids_without_canonical_allocations()
    {
        foreach (var constructor in new[] { "HeadSkip", "Builtin", "BuiltinBinary" })
        {
            foreach (var value in new[]
            {
                PineValue.Blob([4, 0, 1]), PineValue.Blob([2, 0]),
                IntegerEncoding.EncodeSignedInteger(BigInteger.One << 40)
            })
            {
                using var stream = new MemoryStream();

                using (var writer = new PineTreeBinaryEncoding.TreeEncoder<object>(stream.Write))
                {
                    writer.StartExternalEval(
                        Pine.Core.Addressing.PineValueHashTree.ComputeHash(
                            ExpressionBinary.GetConstructorExpressions()[constructor]).Span,
                        constructor is "BuiltinBinary" ? 3 : 2);

                    writer.Blob(((PineValue.BlobValue)value).Bytes.Span);
                    writer.StartExternalEval(Encode(Expression.Environment.Instance).AsSpan(8, 32), 0);

                    if (constructor is "BuiltinBinary")
                        writer.StartExternalEval(Encode(Expression.Environment.Instance).AsSpan(8, 32), 0);
                }

                Action decode = () => ExpressionBinary.Decode(stream.ToArray(), out _);
                decode.Should().Throw<InvalidDataException>();
            }
        }

        foreach (var original in new Expression[]
        {
            new Expression.Builtin(BuiltinFunction.Length, Expression.Environment.Instance),
            new Expression.BuiltinBinary(
                BuiltinFunction.Length,
                Expression.Environment.Instance,
                Expression.Environment.Instance)
        })
        {
            var invalidFunction = Encode(original);
            invalidFunction[57] = 100;
            Action invalid = () => ExpressionBinary.Decode(invalidFunction, out _);
            invalid.Should().Throw<InvalidDataException>().WithMessage("*builtin identifier*");
        }
    }

    [Fact]
    public void Bounded_nesting_rejects_overflow_and_supports_deep_valid_expressions()
    {
        Expression deep = Expression.Environment.Instance;

        for (var depth = 0; depth < 100; depth++)
            deep = new Expression.Builtin(BuiltinFunction.Head, deep);

        ExpressionBinary.Decode(Encode(deep), out _).Should().Be(deep);

        for (var depth = 0; depth < 500; depth++)
            deep = new Expression.Builtin(BuiltinFunction.Head, deep);

        Action encode = () => Encode(deep);
        encode.Should().Throw<ArgumentException>();
        ExpressionConversion.FromCanonical(ExpressionConversion.ToCanonical(deep)).Should().Be(deep);

        using var stream = new MemoryStream();

        using (var writer = new PineTreeBinaryEncoding.TreeEncoder<object>(stream.Write))
        {
            var headHash = Encode(new Expression.HeadSkip(0, Expression.Environment.Instance)).AsSpan(8, 32).ToArray();

            for (var depth = 0; depth < 600; depth++)
            {
                writer.StartExternalEval(headHash, 2);
                writer.Integer(0);
            }

            writer.StartExternalEval(Encode(Expression.Environment.Instance).AsSpan(8, 32), 0);
        }

        Action decode = () => ExpressionBinary.Decode(stream.ToArray(), out _);
        decode.Should().Throw<InvalidDataException>();
    }
}
