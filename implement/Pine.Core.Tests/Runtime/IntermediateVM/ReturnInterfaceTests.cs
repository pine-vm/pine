using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using Pine.Core.PineVM;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

using VM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class ReturnInterfaceTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_parser =
        new(
            () => ElmCompilerTestHelper.CompileElmModules(
                [
                Encoding.UTF8.GetString(
                    ((FileTree.FileNode)BundledFiles.CompilerSourceContainerFilesDefault.Value.GetNodeAtPath(
                    ["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Parser", "StringParsing.elm"])!).Bytes.Span)
                ],
                disableInlining: true).parsedEnv);

    [Theory]
    [InlineData("findDoubleQuotedLiteralRunEnd", "abc\"", 0, 3, "LiteralRunTermination")]
    [InlineData("findDoubleQuotedLiteralRunEnd", "abc", 0, 3, "LiteralRunUnterminated")]
    [InlineData("findDoubleQuotedLiteralRunEnd", "a\\", 0, 1, "LiteralRunBackslash")]
    [InlineData("findDoubleQuotedLiteralRunEnd", "a\n", 0, 1, "LiteralRunNewlineLF")]
    [InlineData("findDoubleQuotedLiteralRunEnd", "a\r\n", 0, 1, "LiteralRunNewlineCRLF")]
    [InlineData("findDoubleQuotedLiteralRunEnd", "a\rx", 0, 1, "LiteralRunNewlineCR")]
    [InlineData("scanUnicodeEscapeDigits", "41}", 0, 2, null)]
    [InlineData("scanUnicodeEscapeDigits", "x41}", 1, 3, null)]
    [InlineData("scanUnicodeEscapeDigits", "x", 0, -1, null)]
    [InlineData("findLiteralRunEnd", "abc'", 0, 3, "LiteralRunTermination", "SingleQuoteTermination")]
    [InlineData("findLiteralRunEnd", "abc\"", 0, 3, "LiteralRunTermination", "DoubleQuoteTermination")]
    [InlineData("findLiteralRunEnd", "abc\"\"\"", 0, 3, "LiteralRunTermination", "TripleQuoteTermination")]
    public void Actual_parser_functions_expose_flat_returns(
        string name,
        string source,
        int offset,
        int end,
        string? boundary,
        string? termination = null)
    {
        var function =
            s_parser.Value.Modules.Single(module => module.moduleName is "ElmSyntax.Concrete.Parser.StringParsing")
            .moduleContent.FunctionDeclarations[name];

        var frames = new List<EnteredStackFrame>();

        var (value, _) =
            CoreLibraryTestHelper.ApplyGenericWithProfiling(
                function,
                termination is null
                ?
                [ElmValue.StringInstance(source), ElmValue.Integer(offset)]
                :
                [ElmValue.TagInstance(termination, []), ElmValue.StringInstance(source), ElmValue.Integer(offset)],
                CreateVM(),
                (in frame) => frames.Add(frame));

        var tuple =
            ElmValue.ListInstance(
                [
                ElmValue.Integer(end),
                boundary is null ? ElmValue.Integer(65) : ElmValue.TagInstance(boundary, [])
                ]);

        value.Should().Be(
            boundary is not null
            ?
            tuple
            :
            end < 0 ? ElmValue.TagInstance("Nothing", []) : ElmValue.TagInstance("Just", [tuple]));

        var count = name == "scanUnicodeEscapeDigits" ? 3 : 2;
        frames.Should().Contain(frame => frame.Instructions.ReturnLayout.SlotCount == count);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Separate_frames_forward_record_or_tuple_components_without_lists(bool record)
    {
        var producer =
            Expression.ListInst(
                record
                ?
                [Text("<Record>"), Text("alfa"), Path(0), Text("beta"), Path(1)]
                :
                [Path(0), Path(1)]);

        var call = Call(producer);

        var consumer =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst([Project(call, record ? 2 : 0), Project(call, record ? 4 : 1)]));

        var entered = new List<EnteredStackFrame>();
        var vm = CreateVM();
        var input = PineValue.List([Number(17), Number(42)]);

        EvaluationReport Run() =>
            vm.EvaluateExpressionOnCustomStack(
                consumer,
                input,
                VM.EvaluationConfig.Unbounded,
                reportEnteredStackFrame: (in frame) => entered.Add(frame))
            .Extract(error => throw new Exception(error.ToString()));

        var result = Run();
        result.ReturnValue.Evaluate().Should().Be(Number(59));
        entered.Should().Contain(frame => frame.Instructions.ReturnLayout.SlotCount == 2);
        result.Counters.BuildListCount.Should().Be(0);
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
            Run().ReturnValue.Evaluate().Should().Be(Number(59));

        profile[PineVMWorkProfile.Metric.InProcessChildArrays].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(0);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void Nested_choice_flattens_payload_and_persistence_ignores_inactive_slots(bool record, bool disableTagging)
    {
        var payload =
            Expression.ListInst(
                record
                ?
                [Text("<Record>"), Text("alfa"), Path(0), Text("beta"), Path(1)]
                :
                [Path(0), Path(1)]);

        var producer =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_is_sorted_asc),
                    Expression.ListInst([Path(0), Literal(0)])),
                falseBranch: Expression.ListInst([Text("<Choice>"), Text("Just"), payload]),
                trueBranch: Expression.ListInst([Text("<Choice>"), Text("Nothing")]));

        var entered = new List<EnteredStackFrame>();
        var vm = CreateVM();

        foreach (var first in new[] { -1, 17, 99 })
        {
            var input = PineValue.List([Number(first), Number(42)]);

            var result =
                vm.EvaluateExpressionOnCustomStack(
                    Call(producer),
                    input,
                    VM.EvaluationConfig.Unbounded,
                    reportEnteredStackFrame: (in frame) => entered.Add(frame))
                .Extract(error => throw new Exception(error.ToString()));

            var expected =
                first <= 0
                ?
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("<Choice>"),
                    StringEncoding.ValueFromString("Nothing")
                    ])
                :
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("<Choice>"), StringEncoding.ValueFromString("Just"),
                        record
                        ?
                        PineValue.List(
                            [
                            StringEncoding.ValueFromString("<Record>"),
                            StringEncoding.ValueFromString("alfa"),
                            Number(first),
                            StringEncoding.ValueFromString("beta"),
                            Number(42)
                            ])
                        :
                        PineValue.List([Number(first), Number(42)])
                    ]);

            result.ReturnValue.Evaluate().Should().Be(expected);
        }

        var layout = entered.First(frame => frame.FrameExpression == producer).Instructions.ReturnLayout;

        if (disableTagging)
        {
            layout =
                new(
                    layout.Recipe,
                    [.. layout.Slots.Select(slot => new ReturnValueLayout.Slot(slot.Path, slot.Values, slot.Guards))],
                    new(MinimumBlobSize: null));
        }

        layout.SlotCount.Should().Be(3);
        var store = new FileStoreFromConcurrentDictionary();
        var expression = ExpressionEncoding.EncodeExpressionAsValue(producer);
        var (hash, _) = PineValueHashFlat.ComputeHashForValue(expression);

        var configuration =
            new OptimizationParametersSerial(
                [
                new OptimizationParametersSerial.ExpressionEntry(
                    Convert.ToHexStringLower(hash.Span),
                    new OptimizationParametersSerial.ExpressionConfig(
                        PersistentCachePredicate: OptimizationParametersSerial.InputPredicate.Unconditional,
                        ParallelThreadPredicate: null))
                ]);

        var cache = new PersistentInvocationCacheAccess(null, store, configuration);

        var key =
            new EvalCacheEntryKey(
                expression,
                StackFrameInput.FromArguments(StaticFunctionInterface.ZeroParameters, []));

        var canonicalNothing =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("<Choice>"),
                    StringEncoding.ValueFromString("Nothing")
                    ]));

        var slots = new PineValueInProcess?[layout.SlotCount];
        layout.Deconstruct(canonicalNothing, slots);
        slots[1] = null;
        slots[2] = null;
        var borrowed = new InvocationResult(layout, slots);
        cache.TryAddResult(key, borrowed).Should().BeTrue();
        cache.TryGetResult(key, layout, out var loaded).Should().BeTrue();
        loaded.Materialize().Evaluate().Should().Be(canonicalNothing.Evaluate());
        var file = store.ListFilesInDirectory([]).Single();
        var firstBytes = store.GetFileContent(file)!.Value.ToArray();
        slots[1] = PineValueInProcess.Create(Number(12345));
        slots[2] = PineValueInProcess.Create(Number(67890));
        cache.TryAddResult(key, borrowed).Should().BeTrue();
        store.GetFileContent(file)!.Value.ToArray().Should().Equal(firstBytes);

        var just =
            PineValue.List(
                [
                    StringEncoding.ValueFromString("<Choice>"), StringEncoding.ValueFromString("Just"),
                    record
                    ?
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("<Record>"),
                        StringEncoding.ValueFromString("alfa"),
                        Number(17),
                        StringEncoding.ValueFromString("beta"),
                        Number(42)
                        ])
                    :
                    PineValue.List([Number(17), Number(42)])
                ]);

        layout.Deconstruct(PineValueInProcess.Create(just), slots);
        cache.TryAddResult(key, borrowed).Should().BeTrue();
        cache.TryGetResult(key, layout, out var loadedJust).Should().BeTrue();
        loadedJust.Materialize().Evaluate().Should().Be(just);
        store.SetFileContent(file, store.GetFileContent(file)!.Value.ToArray().Concat(new byte[] { 0 }).ToArray());
        var readCorrupt = () => cache.TryGetResult(key, layout, out _);
        readCorrupt.Should().Throw<FormatException>();
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(3)]
    [InlineData(4)]
    [InlineData(5)]
    public void Finite_blob_returns_only_tag_values_at_or_above_five_bytes(int size)
    {
        var first = PineValue.Blob(new byte[size]);
        var second = PineValue.Blob([.. Enumerable.Repeat((byte)42, Math.Max(1, size))]);

        var producer =
            Expression.ConditionalInst(
                Expression.BuiltinInst(nameof(BuiltinFunction.equal), Expression.ListInst([Path(0), Literal(0)])),
                falseBranch: Expression.LitralInst(second),
                trueBranch: Expression.LitralInst(first));

        var entered = new List<EnteredStackFrame>();
        var vm = CreateVM();

        foreach (var input in new[] { 0, 1 })
        {
            var report =
                vm.EvaluateExpressionOnCustomStack(
                    Call(producer),
                    PineValue.List([Number(input)]),
                    VM.EvaluationConfig.Unbounded,
                    reportEnteredStackFrame: (in frame) => entered.Add(frame))
                .Extract(error => throw new Exception(error.ToString()));

            report.ReturnValue.Evaluate().Should().Be(input == 0 ? first : second);
        }

        var frame = entered.First(item => item.FrameExpression == producer).Instructions;
        var layout = frame.ReturnLayout;
        var restored = ReturnValueLayout.Decode(layout.Encoding);
        restored.Should().Be(layout);
        var slot = layout.Slots.Single();
        slot.IsMapped.Should().Be(size >= 5);

        if (size < 5)
        {
            slot.Keys.Should().Equal(slot.Values);

            frame.Instructions.Should().NotContain(
                instruction => instruction.Kind == StackInstructionKind.Select_Value_Const);

            frame.Instructions.Where(instruction => instruction.Kind == StackInstructionKind.Push_Literal)
                .Select(instruction => instruction.Literal!.Evaluate()).Should().Contain(first).And.Contain(second);
        }

        foreach (var value in new[] { first, second })
        {
            var slots = new PineValueInProcess?[layout.SlotCount];
            layout.Deconstruct(PineValueInProcess.Create(value), slots);

            if (size < 5)
                slots[0]!.Evaluate().Should().Be(value);

            else
                slots[0]!.Evaluate().Should().NotBe(value);

            new InvocationResult(restored, slots).Materialize().Evaluate().Should().Be(value);
        }
    }

    [Theory]
    [InlineData(5)]
    [InlineData(4)]
    [InlineData(0)]
    [InlineData(null)]
    public void Mixed_return_keys_are_collision_free_and_tagging_can_be_tuned_or_disabled(int? minimumBlobSize)
    {
        PineValue[] tags =
            [
            Number(0),
            Number(1),
            PineValue.Blob([0, 1, 2, 3]),
            PineValue.Blob([0, 1, 2, 3, 4]),
            PineValue.List([Number(123)])
            ];

        Expression producer = Expression.ListInst([Expression.LitralInst(tags[^1]), Path(0)]);

        for (var index = tags.Length - 2; index >= 0; index--)
        {
            producer =
                Expression.ConditionalInst(
                    Expression.BuiltinInst(
                        nameof(BuiltinFunction.equal),
                        Expression.ListInst([Path(0), Literal(index)])),
                    falseBranch: producer,
                    trueBranch: Expression.ListInst([Expression.LitralInst(tags[index]), Path(0), Path(1)]));
        }

        var layout =
            ReturnShapeInference.Infer(
                Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(producer),
                new PineVMParseCache(),
                (_, _, _) => null,
                new(minimumBlobSize));

        layout.Recipe.Should().BeOfType<ReturnValueLayout.Node.Variant>();
        var discriminator = layout.Slots[0];
        discriminator.Keys.Distinct().Count().Should().Be(tags.Length);

        foreach (var tag in tags)
        {
            var index = discriminator.Values.IndexOf(tag);

            var preserve =
                minimumBlobSize is null ||
                tag is PineValue.BlobValue blob && blob.Bytes.Length < minimumBlobSize.Value;

            if (preserve)
                discriminator.Keys[index].Should().Be(tag);

            else if (!tag.Equals(Number(0)) && !tag.Equals(Number(1)))
                discriminator.Keys[index].Should().NotBe(tag);

            var canonical =
                PineValue.List(tag.Equals(tags[^1]) ? [tag, Number(17)] : [tag, Number(17), Number(42)]);

            var slots = new PineValueInProcess?[layout.SlotCount];
            layout.Deconstruct(PineValueInProcess.Create(canonical), slots);
            var restored = ReturnValueLayout.Decode(layout.Encoding);
            restored.Materialize(slots).Evaluate().Should().Be(canonical);
        }

        if (minimumBlobSize is null)
        {
            layout.Slots.Should().OnlyContain(slot => !slot.IsMapped);

            PineControlFlowGraph.FromFragment(layout.Deconstruct(0), layout.SlotCount).LowerToStackInstructions()
                .Should().NotContain(instruction => instruction.Kind == StackInstructionKind.Select_Value_Const);
        }

        if (minimumBlobSize == 5)
        {
            var vm = CreateVM();

            for (var index = 0; index < tags.Length; index++)
            {
                var canonical =
                    PineValue.List(
                        index == tags.Length - 1
                        ?
                        [tags[index], Number(index)]
                        :
                        [tags[index], Number(index), Number(42)]);

                vm.EvaluateExpression(Call(producer), PineValue.List([Number(index), Number(42)]))
                    .IsOkOrNull().Should().Be(canonical);
            }
        }
    }

    [Fact]
    public void Legacy_return_layouts_retain_their_original_ordinal_keys()
    {
        var values = new[] { Number(17), Number(42) };

        var current =
            new ReturnValueLayout(
                new ReturnValueLayout.Node.Component(0, [.. values]),
                [new([], [.. values], [])]);

        var encoded = (PineValue.ListValue)current.Encoding;

        var legacy =
            PineValue.List(
                [
                    Number(1), encoded.Items.Span[1],
                    PineValue.List([PineValue.List([PineValue.EmptyList, PineValue.List(values), PineValue.EmptyList])])
                ]);

        var restored = ReturnValueLayout.Decode(legacy);
        restored.Slots[0].Keys.Should().Equal(Number(0), Number(1));

        foreach (var value in values)
        {
            var slots = new PineValueInProcess?[1];
            restored.Deconstruct(PineValueInProcess.Create(value), slots);
            restored.Materialize(slots).Evaluate().Should().Be(value);
        }

        var duplicateKeys =
            new ReturnValueLayout(current.Recipe, [new([], [.. values], [], [Number(0), Number(0)])]);

        var decodeDuplicate = () => ReturnValueLayout.Decode(duplicateKeys.Encoding);
        decodeDuplicate.Should().Throw<FormatException>();
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(17)]
    public void Direct_choice_consumers_do_not_allocate_nested_return_wrappers(int first)
    {
        var producer =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_is_sorted_asc),
                    Expression.ListInst([Path(0), Literal(0)])),
                falseBranch:
                Expression.ListInst(
                    [
                        Text("<Choice>"), Text("Just"),
                        Expression.ListInst([Text("<Record>"), Text("alfa"), Path(0), Text("beta"), Path(1)])
                    ]),
                trueBranch: Expression.ListInst([Text("<Choice>"), Text("Nothing")]));

        var call = Call(producer);

        var consumer =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst([Project(call, 1), Text("Nothing")])),
                falseBranch:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.ListInst([Project(call, 2, 2), Project(call, 2, 4)])),
                trueBranch: Literal(-1));

        var input = PineValue.List([Number(first), Number(42)]);
        var trace = new List<ExecutedStackInstruction>();
        var vm = CreateVM(trace);

        EvaluationReport Run() =>
            vm.EvaluateExpressionOnCustomStack(consumer, input, VM.EvaluationConfig.Unbounded)
            .Extract(error => throw new Exception(error.ToString()));

        var expected = Number(first < 0 ? -1 : first + 42);
        Run().ReturnValue.Evaluate().Should().Be(expected);
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
        {
            var report = Run();
            report.ReturnValue.Evaluate().Should().Be(expected);
            report.Counters.BuildListCount.Should().Be(0);
        }

        profile[PineVMWorkProfile.Metric.InProcessChildArrays].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(0);

        foreach (var (projection, expectedValue, builders) in new[]
        {
            (Project(call, 1), StringEncoding.ValueFromString(first < 0 ? "Nothing" : "Just"), 0),
            (Project(call, 2, 2), first < 0 ? PineValue.EmptyList : Number(first), 0),
            (Project(call, 2),
            first < 0
            ?
            PineValue.EmptyList
            :
            PineValue.List(
                [
                    StringEncoding.ValueFromString("<Record>"), StringEncoding.ValueFromString("alfa"), Number(first),
                    StringEncoding.ValueFromString("beta"), Number(42)
                ]),
            first < 0 ? 0 : 1)
        })
        {
            var report =
                vm.EvaluateExpressionOnCustomStack(projection, input, VM.EvaluationConfig.Unbounded)
                .Extract(error => throw new Exception(error.ToString()));

            report.ReturnValue.Evaluate().Should().Be(expectedValue);
            report.Counters.BuildListCount.Should().Be(builders);
        }
    }

    [Fact]
    public void Persisted_return_components_feed_a_compiled_consumer_without_materialization()
    {
        var producer = Expression.ListInst([Text("<Record>"), Text("alfa"), Path(0), Text("beta"), Path(1)]);

        var consumer =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst([Project(Call(producer), 2), Project(Call(producer), 4)]));

        var (hash, _) = PineValueHashFlat.ComputeHashForValue(ExpressionEncoding.EncodeExpressionAsValue(producer));

        var config =
            new OptimizationParametersSerial(
                [
                    new OptimizationParametersSerial.ExpressionEntry(
                        Convert.ToHexStringLower(hash.Span),
                        new(OptimizationParametersSerial.InputPredicate.Unconditional, ParallelThreadPredicate: null))
                ]);

        var store = new FileStoreFromConcurrentDictionary();
        var input = PineValue.List([Number(123), Number(456)]);

        var writer =
            CreateVM(cache: new PersistentInvocationCacheAccess(new ConcurrentInvocationCache(), store, config));

        writer.EvaluateExpression(consumer, input).IsOkOrNull().Should().Be(Number(579));
        store.ListFilesInDirectory([]).Should().ContainSingle();

        var reader =
            CreateVM(cache: new PersistentInvocationCacheAccess(new ConcurrentInvocationCache(), store, config));

        reader.EvaluateExpression(consumer, input).IsOkOrNull().Should().Be(Number(579));
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
            reader.EvaluateExpression(consumer, input).IsOkOrNull().Should().Be(Number(579));

        profile[PineVMWorkProfile.Metric.InProcessChildArrays].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(0);
    }

    [Fact]
    public void Persisted_return_layouts_reject_invalid_slot_references_and_alternatives()
    {
        var invalidIndex = new ReturnValueLayout(new ReturnValueLayout.Node.Component(1, []), [new([], [], [])]);
        var decodeIndex = () => ReturnValueLayout.Decode(invalidIndex.Encoding);
        decodeIndex.Should().Throw<FormatException>();

        var invalidVariant =
            new ReturnValueLayout(new ReturnValueLayout.Node.Variant(0, [Number(7)], []), [new([], [Number(7)], [])]);

        var decodeVariant = () => ReturnValueLayout.Decode(invalidVariant.Encoding);
        decodeVariant.Should().Throw<FormatException>();
        ReturnValueLayout.Decode(ReturnValueLayout.Canonical.Encoding).Should().BeSameAs(ReturnValueLayout.Canonical);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Recursive_returns_keep_two_slots_across_frames(bool mutual, bool nonTail)
    {
        Expression Body(int next, int increment)
        {
            var call =
                new Expression.Eval(
                    Path(0, next),
                    Expression.ListInst(
                        [Path(0), Add(Path(1), Literal(-1)), Add(Path(2), Literal(increment)), Path(3)]));

            return
                Expression.ConditionalInst(
                    Expression.BuiltinInst(
                        nameof(BuiltinFunction.int_is_sorted_asc),
                        Expression.ListInst([Path(1), Literal(0)])),
                    falseBranch:
                    nonTail ? Expression.ListInst([Add(Project(call, 0), Literal(1)), Project(call, 1)]) : call,
                    trueBranch: Expression.ListInst([Path(2), Path(3)]));
        }

        var first = Body(mutual ? 1 : 0, 2);
        var second = Body(0, 5);

        var group =
            PineValue.List(
                mutual
                ?
                [ExpressionEncoding.EncodeExpressionAsValue(first), ExpressionEncoding.EncodeExpressionAsValue(second)]
                :
                [ExpressionEncoding.EncodeExpressionAsValue(first)]);

        var entry =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(first)),
                Expression.ListInst([Expression.LitralInst(group), Literal(4), Path(0), Path(1)]));

        var consumer = Add(Project(entry, 0), Project(entry, 1));
        var input = PineValue.List([Number(10), Number(100)]);
        var entered = new List<EnteredStackFrame>();
        var trace = new List<ExecutedStackInstruction>();
        var vm = CreateVM(trace);

        var report =
            vm.EvaluateExpressionOnCustomStack(
                consumer,
                input,
                VM.EvaluationConfig.Unbounded,
                reportEnteredStackFrame: (in frame) => entered.Add(frame))
            .Extract(error => throw new Exception(error.ToString()));

        report.ReturnValue.Evaluate().Should().Be(Number(110 + (mutual ? 14 : 8) + (nonTail ? 4 : 0)));

        var recursiveFrames =
            entered.Where(frame => frame.FrameExpression == first || frame.FrameExpression == second).ToArray();

        recursiveFrames.Length.Should().Be(5);
        recursiveFrames.Should().OnlyContain(frame => frame.Instructions.ReturnLayout.SlotCount == 2);
        report.Counters.BuildListCount.Should().Be(0);
        trace.Should().NotContain(item => item.Instruction.Kind == StackInstructionKind.Eval_Binary);
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
            vm.EvaluateExpression(consumer, input).IsOkOrNull().Should().Be(report.ReturnValue.Evaluate());

        profile[PineVMWorkProfile.Metric.InProcessChildArrays].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(0);
    }

    [Fact]
    public void Return_layouts_use_the_prepared_constraint_not_only_the_original_expression()
    {
        var producer = Expression.ListInst([Path(0), Path(1)]);
        var constraint = PineValueClass.Create([new([0], Number(7))]);
        var entered = new List<EnteredStackFrame>();

        var vm =
            CreateVM(
                constraints: new Dictionary<Expression, IReadOnlyList<PineValueClass>> { [producer] = [constraint] });

        foreach (var first in new[] { 7, 8, 7 })
        {
            var call =
                new Expression.Eval(
                    Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(producer)),
                    Expression.ListInst([Literal(first), Path(0)]));

            var report =
                vm.EvaluateExpressionOnCustomStack(
                    Add(Project(call, 0), Project(call, 1)),
                    PineValue.List([Number(42)]),
                    VM.EvaluationConfig.Unbounded,
                    reportEnteredStackFrame: (in frame) => entered.Add(frame))
                .Extract(error => throw new Exception(error.ToString()));

            report.ReturnValue.Evaluate().Should().Be(Number(first + 42));

            entered.Last(frame => frame.FrameExpression == producer).Instructions.ReturnLayout.SlotCount.Should().Be(
                first == 7 ? 1 : 2);
        }
    }

    [Fact]
    public void Fixed_return_shapes_do_not_discard_failing_computations()
    {
        var producer =
            Expression.ConditionalInst(
                new Expression.Eval(Path(0), Expression.LitralInst(PineValue.EmptyList)),
                falseBranch: Literal(7),
                trueBranch: Literal(7));

        var entered = new List<EnteredStackFrame>();

        var result =
            CreateVM().EvaluateExpressionOnCustomStack(
                Call(producer),
                PineValue.List([Number(123)]),
                VM.EvaluationConfig.Unbounded,
                reportEnteredStackFrame: (in frame) => entered.Add(frame));

        result.IsOkOrNull().Should().BeNull();
        entered.Single(frame => frame.FrameExpression == producer).Instructions.ReturnLayout.SlotCount.Should().Be(0);
    }

    [Fact]
    public void Return_contract_identity_does_not_rehash_large_fixed_program_values()
    {
        var programValue = PineValue.Blob(new byte[1024 * 1024]);

        ReturnValueLayout CreateLayout() =>
            new(
                new ReturnValueLayout.Node.List(
                    [
                    new ReturnValueLayout.Node.Fixed(programValue),
                    new ReturnValueLayout.Node.Component(0, [])
                    ]),
                [new([1], [], [])]);

        _ = CreateLayout().GetHashCode();
        var before = GC.GetAllocatedBytesForCurrentThread();
        var first = CreateLayout();
        var second = CreateLayout();
        var equal = first.Equals(second);
        var sameHash = first.GetHashCode() == second.GetHashCode();
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        equal.Should().BeTrue();
        sameHash.Should().BeTrue();
        allocated.Should().BeLessThan(64 * 1024);
    }

    private static Expression Add(Expression left, Expression right) =>
        Expression.BuiltinInst(nameof(BuiltinFunction.int_add), Expression.ListInst([left, right]));

    private static Expression Call(Expression producer) =>
        new Expression.Eval(
            Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(producer)),
            Expression.EnvironmentInstance);

    private static Expression Project(Expression value, params int[] path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(path, value);

    private static Expression Path(params int[] path) => Project(Expression.EnvironmentInstance, path);

    private static PineValue Number(int number) => IntegerEncoding.EncodeSignedInteger(number);

    private static Expression Literal(int number) => Expression.LitralInst(Number(number));

    private static Expression Text(string text) => Expression.LitralInst(StringEncoding.ValueFromString(text));

    private static VM CreateVM(
        List<ExecutedStackInstruction>? trace = null,
        IInvocationCacheAccess? cache = null,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? constraints = null) =>
        VM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses:
            constraints?.ToDictionary(
                item => Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(item.Key), item => item.Value),
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => true,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            reportExecutedStackInstruction: trace is null ? null : (in item) => trace.Add(item),
            invocationCache: cache,
            invocationCacheConfiguration: new(0, 0, 0, 0));
}
