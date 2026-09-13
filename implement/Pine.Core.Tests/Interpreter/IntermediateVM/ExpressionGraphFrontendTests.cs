using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;
using Xunit;
using Analysis = Pine.Core.CodeAnalysis.CodeAnalysis;
using VM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class ExpressionGraphFrontendTests
{
    private static Expression Lit(int value) => Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(value));
    private static Expression Value(PineValue value) => Expression.LitralInst(value);
    private static Expression Env => Expression.EnvironmentInstance;
    private static Expression List(params Expression[] items) => Expression.ListInst(items);
    private static Expression Builtin(string name, Expression input) => Expression.BuiltinInst(name, input);
    private static Expression Call(Expression body, Expression environment) =>
        new Expression.Eval(Value(ExpressionEncoding.EncodeExpressionAsValue(body)), environment);
    private static Expression Conditional(Expression condition, Expression whenFalse, Expression whenTrue) =>
        Expression.ConditionalInst(condition, whenFalse, whenTrue);
    private static Expression Path(int index) => Builtin("head", Builtin("skip", List(Lit(index), Env)));

    [Fact]
    public void Owned_request_copies_nested_expression_value_label_and_specialization_arrays()
    {
        Check();
        void Check()
        {
            byte[] bytes = [81, 82, 83, 84];
            byte[] labelBytes = [91, 92, 93, 94];
            PineValue[] values = [PineValue.Blob(bytes)];
            Expression[] children = [new Expression.Litral(PineValue.List(values)), new Expression.Label(PineValue.Blob(labelBytes), Env)];
            int[] path = [1, 2];
            var constraint = PineValueClass.Create(
                [new KeyValuePair<IReadOnlyList<int>, PineValue>(path, PineValue.Blob(bytes))]);
            var request = CompilationRequest.Capture(new Expression.List(children), new(DisableReduction: true)) with
            {
                Specialization = SpecializationFacts.Capture(constraint),
            };
            var snapshot = request.Root;
            var hash = request.GetHashCode();
            var before = FunctionPreparation.PrepareFunction(request, CompilerMemo.Empty);
            bytes[0] = 0;
            labelBytes[0] = 0;
            values[0] = PineValue.EmptyList;
            children[0] = Env;
            path[0] = 99;

            request.Root.Should().Be(snapshot);
            request.GetHashCode().Should().Be(hash);
            request.Specialization!.Items[0].Path.Indices.Should().Equal(1, 2);
            FunctionPreparation.PrepareFunction(request, CompilerMemo.Empty).Should().Be(before);
            var roundtrip = request.Root.ToExpression();
            OwnedExpression.Capture(roundtrip).Should().Be(snapshot);
            var detached = (Expression.List)roundtrip;
            ((Expression[])detached.Items)[0] = Lit(999);
            request.Root.Should().Be(snapshot);
            OwnedExpression.Capture(request.Root.ToExpression()).Should().Be(snapshot);
        }
    }

    [Fact]
    public void Repeated_legacy_exports_do_not_share_arrays_or_interned_builtin_expression_instances()
    {
        Check();
        void Check()
        {
            var literal = new LiteralValue.List(
                [new LiteralValue.Blob([4]), new LiteralValue.List([new LiteralValue.Blob([4, 1])])]);
            var firstValue = (PineValue.ListValue)OwnedExpression.ToValue(literal);
            var secondValue = (PineValue.ListValue)OwnedExpression.ToValue(literal);
            ReferenceEquals(firstValue, secondValue).Should().BeFalse();
            var firstBlob = (PineValue.BlobValue)firstValue.Items.Span[0];
            var secondBlob = (PineValue.BlobValue)secondValue.Items.Span[0];
            ReferenceEquals(firstBlob, secondBlob).Should().BeFalse();
            MemoryMarshal.TryGetArray(firstBlob.Bytes, out var firstBytes).Should().BeTrue();
            firstBytes.Array![firstBytes.Offset] = 255;
            MemoryMarshal.TryGetArray(firstValue.Items, out var firstItems).Should().BeTrue();
            firstItems.Array![firstItems.Offset + 1] = PineValue.EmptyList;
            OwnedExpression.CaptureValue(secondValue).Should().Be(literal);
            OwnedExpression.CaptureValue(OwnedExpression.ToValue(literal)).Should().Be(literal);

            var expression = new OwnedExpression.Builtin("head", new OwnedExpression.List(
                [new OwnedExpression.Literal(literal),
                 OwnedExpression.Capture(new Expression.Label(PineValue.Blob([91]), Env))]));
            var firstExpression = (Expression.Builtin)expression.ToExpression();
            var secondExpression = (Expression.Builtin)expression.ToExpression();
            ReferenceEquals(firstExpression, secondExpression).Should().BeFalse();
            var firstExpressionItems = (Expression[])((Expression.List)firstExpression.Input).Items;
            var secondExpressionItems = (Expression[])((Expression.List)secondExpression.Input).Items;
            ReferenceEquals(firstExpressionItems, secondExpressionItems).Should().BeFalse();
            var firstLiteral = (PineValue.ListValue)((Expression.Litral)firstExpressionItems[0]).Value;
            var firstLiteralBlob = (PineValue.BlobValue)firstLiteral.Items.Span[0];
            MemoryMarshal.TryGetArray(firstLiteralBlob.Bytes, out var literalBytes).Should().BeTrue();
            literalBytes.Array![literalBytes.Offset] = 254;
            firstExpressionItems[0] = Lit(999);
            OwnedExpression.Capture(secondExpression).Should().Be(expression);
            OwnedExpression.Capture(expression.ToExpression()).Should().Be(expression);
            var secondLiteral = (PineValue.ListValue)((Expression.Litral)secondExpressionItems[0]).Value;
            OwnedExpression.CaptureValue(secondLiteral).Should().Be(literal);
        }
    }

    [Fact]
    public void Mutating_exported_prepared_literals_does_not_change_cold_or_warm_preparation()
    {
        Check();
        void Check()
        {
            var request = CompilationRequest.Capture(
                Builtin("head", List(Value(new PineValue.BlobValue(new byte[] { 4 })), Env)));
            var cold = FunctionPreparation.PrepareFunction(request, CompilerMemo.Empty);
            var exported = (Expression.Litral)cold.Function.Body.ToExpression();
            var independentlyExported = (Expression.Litral)cold.Function.Body.ToExpression();
            var bytes = ((PineValue.BlobValue)exported.Value).Bytes;
            MemoryMarshal.TryGetArray(bytes, out var array).Should().BeTrue();
            array.Array![array.Offset] = 255;
            OwnedExpression.Capture(independentlyExported).Should().Be(cold.Function.Body);
            FunctionPreparation.PrepareFunction(request, CompilerMemo.Empty).Should().Be(cold);
            FunctionPreparation.PrepareFunction(request, cold.Memo).Should().Be(cold);
            FunctionPreparation.PrepareFunction(request, cold.Memo with
            {
                Preparations = ImmutableDictionary<CompilationRequest, PreparedFunction>.Empty,
            }).Should().Be(cold);
        }
    }

    [Fact]
    public void Parse_and_preparation_memos_are_persistent_structural_and_configuration_specific()
    {
        Check();
        void Check()
        {
            var expression = Call(List(Env, Lit(1)), Builtin("reverse", Env));
            var request = CompilationRequest.Capture(expression);
            var empty = CompilerMemo.Empty;
            var cold = FunctionPreparation.PrepareFunction(request, empty);
            var warm = FunctionPreparation.PrepareFunction(request, cold.Memo);
            warm.Should().Be(cold);
            ReferenceEquals(warm.Function, cold.Function).Should().BeTrue();
            ReferenceEquals(warm.Memo, cold.Memo).Should().BeTrue();
            empty.Parses.Should().BeEmpty();
            empty.Reductions.Should().BeEmpty();
            empty.Preparations.Should().BeEmpty();
            cold.Memo.Parses.Should().NotBeEmpty();
            cold.Memo.Reductions.Should().NotBeEmpty();
            cold.Function.Body.Should().NotBe(request.Root);
            var replay = FunctionPreparation.PrepareFunction(request,
                cold.Memo with { Preparations = ImmutableDictionary<CompilationRequest, PreparedFunction>.Empty });
            replay.Should().Be(cold);
            replay.Memo.GetHashCode().Should().Be(cold.Memo.GetHashCode());

            var changed = request with { Options = new(DisableReduction: true) };
            var unreduced = FunctionPreparation.PrepareFunction(changed, cold.Memo);
            unreduced.Function.Body.Should().Be(request.Root);
            unreduced.Memo.Preparations.Count.Should().Be(2);
            var exclusion = request with { InlineExclusions = [new(OwnedExpression.Capture(List(Env, Lit(1))), null)] };
            FunctionPreparation.PrepareFunction(exclusion, unreduced.Memo).Memo.Preparations.Count.Should().Be(3);
            var specialization = request with { Specialization = SpecializationFacts.Capture(PineValueClass.CreateEquals(PineValue.EmptyList)) };
            var specialized = FunctionPreparation.PrepareFunction(specialization, cold.Memo);
            specialized.Function.Request.Should().NotBe(cold.Function.Request);
            specialized.Memo.Preparations.Count.Should().Be(2);
            FunctionPreparation.PrepareFunction(specialization, CompilerMemo.Empty).Function.Should().Be(specialized.Function);

            var validEncoding = OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(expression));
            var parsed = FunctionPreparation.ParseExpression(validEncoding, empty);
            parsed.Result.Expression.Should().Be(request.Root);
            FunctionPreparation.ParseExpression(validEncoding, parsed.Memo).Should().Be(parsed);
            var invalidEncoding = new LiteralValue.Blob([255, 17]);
            var invalid = FunctionPreparation.ParseExpression(invalidEncoding, parsed.Memo);
            invalid.Result.Expression.Should().BeNull();
            invalid.Result.Error.Should().NotBeNullOrEmpty();
            FunctionPreparation.ParseExpression(invalidEncoding, invalid.Memo).Should().Be(invalid);
            parsed.Memo.Parses.Should().NotContainKey(invalidEncoding);
        }
    }

    [Fact]
    public void Pure_parser_matches_runtime_acceptance_for_both_formats_and_mixed_malformed_encodings()
    {
        Check();
        void Check()
        {
            var expression = List(new Expression.Label("parse-provenance", Call(Env, Lit(17))), Builtin("reverse", Env));
            var modern = ExpressionEncoding2026.EncodeExpressionAsValue(expression);
            var legacy = ExpressionEncoding2024.EncodeExpressionAsValue(expression);
            var modernMixed = modern.Items.ToArray();
            modernMixed[^1] = legacy;
            var legacyMixed = legacy.Items.ToArray();
            legacyMixed[^1] = PineValue.List([modern]);
            var builtinMixed = ExpressionEncoding2026.EncodeExpressionAsValue(
                Builtin("head", Value(PineValue.List([IntegerEncoding.EncodeSignedInteger(42)])))).Items.ToArray();
            builtinMixed[^1] = ExpressionEncoding2024.EncodeExpressionAsValue(
                Value(PineValue.List([IntegerEncoding.EncodeSignedInteger(42)])));
            var mixedBuiltinEncoding = PineValue.List(builtinMixed);
            new PineVMParseCache().ParseExpression(mixedBuiltinEncoding).IsErrOrNull().Should().NotBeNull();
            foreach (var encoding in new PineValue[]
            {
                modern, legacy, PineValue.List(modernMixed), PineValue.List(legacyMixed), mixedBuiltinEncoding,
                PineValue.EmptyList, PineValue.Blob([255, 1]), PineValue.List([PineValue.EmptyBlob]),
            })
            {
                var expected = new PineVMParseCache().ParseExpression(encoding) switch
                {
                    Result<string, Expression>.Ok ok => new ParseMemoEntry(OwnedExpression.Capture(ok.Value), null),
                    Result<string, Expression>.Err error => new ParseMemoEntry(null, error.Value),
                    _ => throw new NotImplementedException(),
                };
                var ownedEncoding = OwnedExpression.CaptureValue(encoding);
                var publiclyParsed = FunctionPreparation.ParseExpression(ownedEncoding, CompilerMemo.Empty);
                publiclyParsed.Result.Should().Be(expected);
                var request = CompilationRequest.Capture(new Expression.Eval(Value(encoding), Env));
                var cold = FunctionPreparation.PrepareFunction(request, CompilerMemo.Empty);
                var publicSeeded = FunctionPreparation.PrepareFunction(request, publiclyParsed.Memo);
                publicSeeded.Function.Should().Be(cold.Function);
                FunctionPreparation.ParseExpression(ownedEncoding, cold.Memo).Result.Should().Be(expected);
                FunctionPreparation.PrepareFunction(request, cold.Memo with
                {
                    Preparations = ImmutableDictionary<CompilationRequest, PreparedFunction>.Empty,
                }).Function.Should().Be(cold.Function);
            }
        }
    }

    [Fact]
    public void Preparation_matches_legacy_reduction_substitution_inlining_and_application_chain_policy()
    {
        Check();
        void Check()
        {
            var functionValue = FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(Env, 3, []);
            var genericChain = Analysis.BuildGenericFunctionApplication(Value(functionValue), [Env, Lit(13)]);
            Expression[] fixtures =
            [
                Builtin("int_add", List(Lit(3), Lit(7))),
                Builtin("int_mul", List(Lit(3), Env, Lit(7))),
                Call(List(Env, Lit(1)), Builtin("reverse", Env)),
                Conditional(Builtin("equal", List(Path(0), Lit(7))), Call(Env, Env), List(Env, Lit(8))),
                new Expression.Label("prepared-label", Call(Env, Builtin("skip", List(Lit(1), Env)))),
                genericChain,
                new Expression.Eval(Value(PineValue.Blob([255, 17])), Env),
                Conditional(Value(PineValue.EmptyList), Lit(2), Lit(3)),
            ];
            PreparationOptions[] options =
            [
                new(),
                new(DisableReduction: true),
                new(DisableGenericApplicationChainConsolidation: true),
                new(PathMaxLowExclusive: 0, PathMaxHighInclusive: 30),
                new(PathMaxLowExclusive: 30, PathMaxHighInclusive: 0),
            ];
            var specialization = SpecializationFacts.Capture(PineValueClass.Create(
                [new KeyValuePair<IReadOnlyList<int>, PineValue>(new[] { 0 }, IntegerEncoding.EncodeSignedInteger(7))]));
            foreach (var expression in fixtures)
                foreach (var option in options)
                    foreach (var facts in new SpecializationFacts?[] { null, specialization })
                    {
                        var request = CompilationRequest.Capture(expression, option) with { Specialization = facts };
                        var prepared = FunctionPreparation.PrepareFunction(request, CompilerMemo.Empty);
                        prepared.Function.Body.Should().Be(OwnedExpression.Capture(LegacyPreparation(request)));
                        prepared.Function.InferredSignature.Parameters.Select(parameter => parameter.Path.Indices)
                            .Should().BeEquivalentTo(StaticFunctionInterface.FromExpression(expression).ParamsPaths);
                        FunctionPreparation.PrepareFunction(request, prepared.Memo).Should().Be(prepared);
                    }
        }
    }

    private static Expression LegacyPreparation(CompilationRequest request)
    {
        var expression = request.Root.ToExpression();
        var options = request.Options;
        var cache = new PineVMParseCache();
        var constraint = request.Specialization?.ToValueClass();
        var inlined = options.DisableReduction ? expression :
            ExpressionCompilation.InlineStaticInvocationsAndReduceRecursive(
                expression, [], 6, 4_000, cache, false, _ => false,
                pathMaxLowExclusive: options.PathMaxLowExclusive,
                pathMaxHighInclusive: options.PathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: options.DisableGenericApplicationChainConsolidation);
        var substituted = constraint is null ? inlined :
            ExpressionCompilation.SubstituteSubexpressionsForEnvironmentConstraint(inlined, constraint);
        return options.DisableReduction ? substituted :
            ExpressionCompilation.ReduceExpressionAndInlineRecursive(
                substituted, [], constraint, [expression], 7, 4_000, cache, false, (_, _) => false,
                pathMaxLowExclusive: options.PathMaxLowExclusive,
                pathMaxHighInclusive: options.PathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: options.DisableGenericApplicationChainConsolidation);
    }

    [Theory]
    [InlineData("bit_shift_left", "-8")]
    [InlineData("bit_shift_right", "-8")]
    [InlineData("bit_shift_left", "17179869184")]
    [InlineData("bit_shift_right", "17179869184")]
    public void Preparation_defers_known_primitive_failures_until_the_branch_is_evaluated(string builtin, string count)
    {
        var invalidShift = Builtin(builtin, List(
            Value(IntegerEncoding.EncodeSignedInteger(System.Numerics.BigInteger.Parse(count))),
            Value(PineValue.Blob([17]))));
        var expression = Conditional(Env, Lit(7), invalidShift);
        var prepared = FunctionPreparation.PrepareFunction(CompilationRequest.Capture(expression), CompilerMemo.Empty);
        FunctionPreparation.PrepareFunction(prepared.Function.Request, prepared.Memo).Should().Be(prepared);
        var preparedExpression = prepared.Function.Body.ToExpression();
        foreach (var condition in new[] { PineKernelValues.FalseValue, PineValue.EmptyList })
            Execute(preparedExpression, condition).Result.Extract(error => throw new Exception(error.ToString()))
                .ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(7));

        var staticallyUnselected = Conditional(Value(PineKernelValues.FalseValue), Lit(7), invalidShift);
        FunctionPreparation.PrepareFunction(CompilationRequest.Capture(staticallyUnselected), CompilerMemo.Empty)
            .Function.Body.Should().Be(OwnedExpression.Capture(Lit(7)));
        var directFailure = Record.Exception(() => new DirectInterpreter(new(), null)
            .EvaluateExpressionDefault(expression, PineKernelValues.TrueValue));
        directFailure.Should().NotBeNull();
        var graphFailure = Record.Exception(() => Execute(preparedExpression, PineKernelValues.TrueValue));
        graphFailure.Should().BeOfType<InvalidIntermediateCodeException>()
            .Which.InnerException!.GetType().Should().Be(directFailure!.GetType());
    }

    [Fact]
    public void Frontend_constructs_stable_validated_graphs_and_retains_label_provenance()
    {
        var expression = new Expression.Label(PineValue.Blob([77, 78]), List(
            Env, Conditional(Env, Call(Env, Lit(1)), Call(Env, Lit(2))), Env));
        var prepared = FunctionPreparation.PrepareFunction(
            CompilationRequest.Capture(expression, new(DisableReduction: true), new(71)), CompilerMemo.Empty);
        var compiled = ExpressionGraphCompiler.CompileExpressionToGraph(prepared.Function, prepared.Memo);
        var graph = compiled.Graph.Extract(errors => throw new Exception(string.Join(", ", errors))).Graph;
        prepared.Function.Source.Should().BeOfType<OwnedExpression.Label>()
            .Which.Value.Should().Be(new LiteralValue.Blob([77, 78]));
        graph.Id.Should().Be(new FunctionId(71));
        graph.Blocks.Values.Select(block => block.Terminator).OfType<Terminator.Invoke>().Count().Should().Be(2);
        graph.Blocks.Values.SelectMany(block => block.Operations).OfType<Operation.Project>().Should().BeEmpty();
        ExpressionGraphCompiler.CompileExpressionToGraph(prepared.Function, compiled.Memo).Memo.Should().Be(compiled.Memo);
        var again = ExpressionGraphCompiler.CompileExpressionToGraph(prepared.Function);
        again.Should().Be(graph);
        again.GetHashCode().Should().Be(graph.GetHashCode());
        GraphRendering.Render(again).Should().Be(GraphRendering.Render(graph));
        GraphRendering.Render(ExpressionGraphCompiler.Compile(Env)).Should().Be(
            "function f0 ([]:pine) -> (pine) entry b0\nblock b0(v0:pine):\n  return (v0)\n");
    }

    [Fact]
    public void All_expression_variants_match_direct_interpretation_over_bounded_generated_corpus()
    {
        Check();
        void Check()
        {
            PineValue[] environments =
            [
                PineValue.EmptyList, PineValue.EmptyBlob,
                PineKernelValues.TrueValue, PineKernelValues.FalseValue,
                PineValue.Blob([4, 0, 0]), PineValue.List([IntegerEncoding.EncodeSignedInteger(7), PineValue.EmptyBlob]),
            ];
            var atoms = ImmutableList.Create(Env, Lit(-1), Lit(0), Lit(19), Value(PineValue.EmptyList),
                Value(PineValue.EmptyBlob), Value(PineKernelValues.TrueValue), Value(PineKernelValues.FalseValue));
            var corpus = Enumerable.Range(0, 160).Select(index => Generate(index, 3)).Concat(atoms).ToImmutableList();
            foreach (var expression in corpus)
                foreach (var environment in environments)
                {
                    var expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, environment);
                    var result = Execute(expression, environment);
                    result.Result.IsErrOrNull().Should().BeNull();
                    result.Result.Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate().Should().Be(expected);
                }

            Expression Generate(int seed, int depth)
            {
                if (depth == 0)
                    return atoms[seed % atoms.Count];
                var a = Generate((seed * 7 + 3) % 997, depth - 1);
                var b = Generate((seed * 11 + 9) % 997, depth - 1);
                return (seed % 10) switch
                {
                    0 => List(a, b, Env),
                    1 => Conditional(a, b, List(Env, a)),
                    2 => Builtin("reverse", a),
                    3 => Builtin("equal", List(a, b)),
                    4 => Builtin("int_add", List(a, b)),
                    5 => Call(List(Env, Lit(seed)), a),
                    6 => new Expression.Label("generated-" + seed, a),
                    7 => Builtin("concat", List(a, b)),
                    8 => List(a, Call(Builtin("length", Env), b), a),
                    9 => Builtin("head", a),
                    _ => throw new InvalidOperationException(),
                };
            }
        }
    }

    [Theory]
    [InlineData("equal")]
    [InlineData("length")]
    [InlineData("head")]
    [InlineData("skip")]
    [InlineData("take")]
    [InlineData("concat")]
    [InlineData("reverse")]
    [InlineData("negate")]
    [InlineData("int_add")]
    [InlineData("int_mul")]
    [InlineData("int_is_sorted_asc")]
    [InlineData("bit_and")]
    [InlineData("bit_or")]
    [InlineData("bit_xor")]
    [InlineData("bit_not")]
    [InlineData("bit_shift_left")]
    [InlineData("bit_shift_right")]
    public void Builtins_use_generic_semantics_for_valid_and_malformed_inputs(string name)
    {
        foreach (var input in new PineValue[]
        {
            PineValue.EmptyList, PineValue.EmptyBlob, PineValue.Blob([4, 99, 7]),
            PineValue.List([IntegerEncoding.EncodeSignedInteger(2), IntegerEncoding.EncodeSignedInteger(3)]),
            PineValue.List([IntegerEncoding.EncodeSignedInteger(1), PineValue.Blob([17, 29, 31])]),
            PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList]),
        })
        {
            var expression = Builtin(name, Env);
            var expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input);
            Execute(expression, input).Result.Extract(error => throw new Exception(error.ToString()))
                .ReturnValue.Evaluate().Should().Be(expected);
        }
    }

    [Fact]
    public void Syntactic_head_skip_preserves_list_only_semantics_while_data_list_input_remains_generic()
    {
        var blob = PineValue.Blob([17, 29, 31]);
        var syntactic = Builtin("head", Builtin("skip", List(Lit(1), Env)));
        var generic = Builtin("head", Builtin("skip", Env));
        var genericInput = PineValue.List([IntegerEncoding.EncodeSignedInteger(1), blob]);
        new DirectInterpreter(new(), null).EvaluateExpressionDefault(syntactic, blob).Should().Be(PineValue.EmptyList);
        Execute(syntactic, blob).Result.Extract(error => throw new Exception(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(PineValue.EmptyList);
        ExpressionGraphCompiler.Compile(generic).Blocks.Values.SelectMany(block => block.Operations)
            .OfType<Operation.Project>().Should().BeEmpty();
        var expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(generic, genericInput);
        expected.Should().Be(PineValue.Blob([29]));
        Execute(generic, genericInput).Result.Extract(error => throw new Exception(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Dynamic_syntactic_head_skip_preserves_count_probe_fallback_and_operand_order()
    {
        foreach (var count in new[]
        {
            IntegerEncoding.EncodeSignedInteger(-3), IntegerEncoding.EncodeSignedInteger(0),
            IntegerEncoding.EncodeSignedInteger(1), IntegerEncoding.EncodeSignedInteger(int.MaxValue),
            PineValue.Blob([4, 0, 1]), PineValue.EmptyList, PineValue.Blob([255, 1]),
        })
            foreach (var source in new[]
            {
            PineValue.Blob([17, 29, 31]), PineValue.EmptyBlob,
            PineValue.List([IntegerEncoding.EncodeSignedInteger(7), IntegerEncoding.EncodeSignedInteger(11)]),
        })
            {
                var expression = Builtin("head", Builtin("skip", List(Call(Env, Value(count)), Call(Env, Value(source)))));
                var expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, PineValue.EmptyList);
                var execution = Execute(expression, PineValue.EmptyList);
                execution.Result.Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate().Should().Be(expected);
                execution.Depths.Count.Should().Be(BuiltinFunction.SignedIntegerFromValueRelaxed(count) is null ? 4 : 3);
            }
        var badCountValue = PineValue.Blob([255, 71]);
        var badSourceValue = PineValue.Blob([255, 91]);
        var countFailure = new Expression.Eval(Value(badCountValue), Env);
        var sourceFailure = new Expression.Eval(Value(badSourceValue), Env);
        var bothFail = Builtin("head", Builtin("skip", List(countFailure, sourceFailure)));
        Execute(bothFail, PineValue.EmptyList).Result.IsErrOrNull()!.Reason
            .Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>().Which.ExpressionValue.Should().Be(badCountValue);
        var malformedCount = Builtin("head", Builtin("skip", List(Value(PineValue.EmptyList), sourceFailure)));
        Execute(malformedCount, PineValue.EmptyList).Result.IsErrOrNull()!.Reason
            .Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>().Which.ExpressionValue.Should().Be(badSourceValue);
    }

    [Fact]
    public void Eval_operands_are_nontail_environment_first_and_nested_calls_keep_caller_values()
    {
        var identityEncoding = ExpressionEncoding.EncodeExpressionAsValue(Env);
        var expression = List(Env,
            new Expression.Eval(Call(Env, Value(identityEncoding)), Call(Env, Lit(17))),
            Conditional(Call(Env, Value(PineKernelValues.FalseValue)), Call(Env, Lit(29)), Lit(-1)), Env);
        var graph = ExpressionGraphCompiler.Compile(expression);
        graph.Blocks.Values.Select(block => block.Terminator).OfType<Terminator.TailInvoke>().Should().BeEmpty();
        var input = IntegerEncoding.EncodeSignedInteger(71);
        var execution = Execute(expression, input);
        execution.Result.Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate()
            .Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
        execution.Compiled.Should().Contain(OwnedExpression.Capture(Env));
        execution.Depths.Max().Should().Be(2);
        var operandCalls = graph.Blocks.Values.OrderBy(block => block.Id.Value)
            .Select(block => block.Terminator).OfType<Terminator.Invoke>().ToArray();
        operandCalls.Length.Should().Be(5);
        operandCalls.Should().OnlyContain(call => call.Continuation.Bindings
            .OfType<ContinuationBinding.ReturnedResult>().Count() == 1);

        var tail = new Expression.Eval(Call(Env, Value(identityEncoding)), Call(Env, Lit(19)));
        var tailGraph = ExpressionGraphCompiler.Compile(tail);
        tailGraph.Blocks.Values.Select(block => block.Terminator).OfType<Terminator.Invoke>().Count().Should().Be(2);
        tailGraph.Blocks.Values.Select(block => block.Terminator).OfType<Terminator.TailInvoke>().Count().Should().Be(1);
        Execute(tail, input).Result.Extract(error => throw new Exception(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(19));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Invalid_targets_fail_at_runtime_after_environment_and_before_continuation(bool tail)
    {
        var environmentFailureValue = PineValue.Blob([81, 82, 83]);
        var encodedFailureValue = PineValue.Blob([91, 92, 93]);
        Expression environmentFailure = new Expression.Eval(Value(environmentFailureValue), Lit(1));
        Expression encodedFailure = new Expression.Eval(Value(encodedFailureValue), Lit(2));
        var inner = new Expression.Eval(encodedFailure, environmentFailure);
        var expression = tail ? (Expression)inner : List(Env, inner, Lit(99));
        var result = Execute(expression, PineValue.EmptyList);
        result.Result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Which.ExpressionValue.Should().Be(environmentFailureValue);
        Action direct = () => new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, PineValue.EmptyList);
        direct.Should().Throw<ParseExpressionException>();
        result.Compiled.Count.Should().Be(1, "neither invalid target should have reached callee compilation");
        var validEnvironment = new Expression.Eval(encodedFailure, Call(Env, Lit(19)));
        Execute(validEnvironment, PineValue.EmptyList).Result.IsErrOrNull()!.Reason
            .Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Which.ExpressionValue.Should().Be(encodedFailureValue);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Ignored_call_arguments_preserve_failure_and_divergence_under_quotas(bool diverge)
    {
        Expression failing = new Expression.Eval(Value(PineValue.Blob([255, 19])), Env);
        Expression loop = new Expression.Eval(Path(0), Env);
        var encodedLoop = ExpressionEncoding.EncodeExpressionAsValue(loop);
        var effect = diverge ? loop : failing;
        var environment = PineValue.List([encodedLoop]);
        var expression = Call(Lit(7), List(Lit(1), effect));
        var result = Execute(expression, environment, new(20, 100, 30));
        if (diverge)
            result.Result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>()
                .Which.QuotaKind.Should().Be(EvaluationQuotaKind.InvocationCount);
        else
            result.Result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>();
    }

    [Fact]
    public void Diverging_environment_precedes_invalid_encoded_operand_and_no_arm_is_speculated()
    {
        Expression loop = new Expression.Eval(Path(0), Env);
        var input = PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(loop)]);
        var invalid = new Expression.Eval(Value(PineValue.Blob([255, 87])), Lit(3));
        var evaluation = new Expression.Eval(invalid, loop);
        Execute(evaluation, input, new(17, 100, 30)).Result.IsErrOrNull()!.Reason
            .Should().BeOfType<EvaluationErrorReason.QuotaExhausted>();
        foreach (var condition in new[] { PineKernelValues.FalseValue, PineValue.EmptyList, PineValue.Blob([0, 1, 0]) })
        {
            var safe = Conditional(Value(condition), Call(Env, Lit(23)), evaluation);
            Execute(safe, input).Result.Extract(error => throw new Exception(error.ToString()))
                .ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(23));
            new DirectInterpreter(new(), null).EvaluateExpressionDefault(safe, input)
                .Should().Be(IntegerEncoding.EncodeSignedInteger(23));
        }
        var trueSafe = Conditional(Value(PineKernelValues.TrueValue), evaluation, Call(Env, Lit(31)));
        Execute(trueSafe, input).Result.Extract(error => throw new Exception(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(31));
    }

    [Fact]
    public void Prepared_expression_executes_end_to_end_without_instruction_first_callee_compilation()
    {
        var expression = List(
            Call(Builtin("reverse", Env), Env),
            Call(List(Env, Lit(3)), Builtin("skip", List(Lit(1), Env))));
        var prepared = FunctionPreparation.PrepareFunction(CompilationRequest.Capture(expression), CompilerMemo.Empty);
        prepared.Function.Body.Should().NotBe(prepared.Function.Source);
        var graph = ExpressionGraphCompiler.CompileExpressionToGraph(prepared.Function);
        ValidatedFunctionGraph.ValidateGraph(graph, ImmutableDictionary<FunctionId, FunctionSignature>.Empty)
            .IsErrOrNull().Should().BeNull();
        var input = PineValue.List([IntegerEncoding.EncodeSignedInteger(7), IntegerEncoding.EncodeSignedInteger(11)]);
        Execute(prepared.Function.Body.ToExpression(), input).Result.Extract(error => throw new Exception(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
    }

    [Fact]
    public void Opt_in_factory_compiles_dynamic_callees_recursively_through_the_frontend()
    {
        var expression = Call(Call(Builtin("head", Builtin("skip", Env)), Env), Env);
        var blob = PineValue.Blob([17, 29, 31]);
        var input = PineValue.List([IntegerEncoding.EncodeSignedInteger(1), blob]);
        ExpressionGraphVM.Create().EvaluateExpressionOnCustomStack(expression, input, new(20, 100, 10))
            .Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate()
            .Should().Be(PineValue.Blob([29]));
        var preparedExpression = Call(List(Env, Lit(2)), Env);
        ExpressionGraphVM.Create(new PreparationOptions()).EvaluateExpressionOnCustomStack(preparedExpression, blob, new(20, 100, 10))
            .Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate()
            .Should().Be(PineValue.List([blob, IntegerEncoding.EncodeSignedInteger(2)]));
    }

    private sealed record Execution(
        Result<EvaluationError, EvaluationReport> Result,
        ImmutableList<OwnedExpression> Compiled,
        ImmutableList<int> Depths);

    private static Execution Execute(Expression expression, PineValue environment, VM.EvaluationConfig? config = null)
    {
        return Run();
        Execution Run()
        {
            var compiled = ImmutableList.CreateBuilder<OwnedExpression>();
            var depths = ImmutableList.CreateBuilder<int>();
            ExpressionCompilation Compile(Expression requested)
            {
                compiled.Add(OwnedExpression.Capture(requested));
                var graph = ExpressionGraphCompiler.Compile(requested);
                var validated = ValidatedFunctionGraph.ValidateGraph(graph,
                    ImmutableDictionary<FunctionId, FunctionSignature>.Empty)
                    .Extract(errors => throw new Exception(string.Join(", ", errors)));
                var function = GraphCompiler.Compile(validated).Extract(error => throw new Exception(error.ToString()));
                return new(GraphVMAdapter.ToStackFrame(function), []);
            }
            var vm = VM.CreateCustom(
                evalCache: null, evaluationConfigDefault: null, reportFunctionApplication: null,
                compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
                skipInlineForExpression: _ => true, enableTailRecursionOptimization: false, parseCache: null,
                precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null, cacheFileStore: null,
                disableDirectContinueForSimpleEval: true, disableDirectEvalForSimpleTemplate: true,
                compileExpression: Compile);
            var result = vm.EvaluateExpressionOnCustomStack(expression, environment,
                config ?? new(1_000, 10_000, 100),
                reportEnteredStackFrame: (in EnteredStackFrame frame) => depths.Add(frame.StackFrameDepth));
            return new(result, compiled.ToImmutable(), depths.ToImmutable());
        }
    }
}
