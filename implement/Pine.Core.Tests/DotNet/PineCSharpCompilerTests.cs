using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading;
using Xunit;

using VM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.DotNet;

public class PineCSharpCompilerTests
{
    public static IEnumerable<object[]> Scenarios() => PineCSharpScenarios.Discover();

    [Theory]
    [MemberData(nameof(Scenarios))]
    public void Pine_CSharp_snapshot_and_execution(string name, string variantName)
    {
        var scenario = PineCSharpScenarios.Load(name);
        var variant = scenario.Variant(variantName);
        var result = PineCSharpCompiler.Compile(scenario.Entry, variant.Specializations, variant.Options);
        var outputs = new Dictionary<string, string>(StringComparer.Ordinal);

        if (result.IsErrOrNull() is { } rejection)
        {
            var path = new Stack<PineCSharpCallPath>();

            for (var step = rejection.Path; step is not null; step = step.Parent)
                path.Push(step);

            outputs.Add(
                "rejection.json",
                PineCSharpScenarios.Serialize(
                    new
                    {
                        kind = rejection.Kind.ToString(),
                        instruction = rejection.Instruction?.Kind.ToString(),
                        block = rejection.Block.Value,
                        operationIndex = rejection.OperationIndex,
                        path =
                        path.Select(
                            step => new
                            {
                                function = step.Function.Index,
                                callerBlock = step.CallerBlock?.Value,
                                rootIndex = step.RootIndex
                            }).ToArray()
                    }));
        }
        else
        {
            var compilation = result.Extract(error => throw new Exception(error.Describe()));
            outputs.Add("methods.csharp-snapshot", compilation.MethodsSource);
            outputs.Add("literals.csharp-snapshot", compilation.LiteralsSource);

            outputs.Add(
                "metadata.json",
                PineCSharpScenarios.Serialize(
                    new
                    {
                        compilation.Complexity,
                        compilation.DispatchMethodName,
                        methods =
                        compilation.Methods.Select(
                            method => new
                            {
                                method.Name,
                                parameterPaths = method.Parameters.ParamsPaths,
                                returnSlotCount = method.Returns.SlotCount
                            }).ToArray()
                    }));

            using var loaded = Load(compilation);
            var vm = CreateVM();

            var evaluations =
                scenario.Evaluations.Select(
                    evaluation =>
                    {
                        var value = loaded.Evaluate(PineValueInProcess.Create(evaluation.Environment))?.Evaluate();

                        if (value is not null)
                        {
                            value.Should().Be(
                                vm.EvaluateExpression(scenario.Entry, evaluation.Environment)
                                .Extract(error => throw new Exception(error)),
                                "evaluation {0} must agree with PineVM",
                                evaluation.Name);
                        }

                        return new { evaluation.Name, value };
                    }).ToArray();

            outputs.Add("evaluations.json", PineCSharpScenarios.Serialize(evaluations));
        }

        var mismatches = PineCSharpScenarios.CompareSnapshots(scenario.ExpectationDirectory(variantName), outputs);
        mismatches.Should().BeEmpty(string.Join("\n", mismatches));
    }

    [Fact]
    public void Pine_CSharp_spread_workers_avoid_transport_allocations()
    {
        var scenario = PineCSharpScenarios.Load("spread-call");
        var compilation = Compile(scenario, "without-cancellation");
        using var loaded = Load(compilation);
        var environment = PineValueInProcess.Create(scenario.Evaluation("small").Environment);
        var expected = PineCSharpScenarios.ExpectedValue(scenario, "without-cancellation", "small");
        loaded.Evaluate(environment)!.Evaluate().Should().Be(expected);
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
            loaded.Evaluate(environment)!.Evaluate().Should().Be(expected);

        profile[PineVMWorkProfile.Metric.InProcessChildArrays].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(0);
        compilation.Methods.Should().Contain(method => method.Returns.SlotCount == 2);
    }

    [Theory]
    [InlineData("self-tail")]
    [InlineData("mutual-tail")]
    public void Pine_CSharp_tail_loops_run_deeply_and_cancellation_is_optional(string name)
    {
        var scenario = PineCSharpScenarios.Load(name);
        var compilation = Compile(scenario, "with-cancellation");
        using var loaded = Load(compilation);
        var environment = PineValueInProcess.Create(scenario.Evaluation("deep").Environment);
        var expected = PineCSharpScenarios.ExpectedValue(scenario, "with-cancellation", "deep");
        loaded.Evaluate(environment)!.Evaluate().Should().Be(expected);
        using var cancelled = new CancellationTokenSource();
        cancelled.Cancel();
        Action run = () => loaded.Evaluate(environment, cancelled.Token);
        run.Should().Throw<OperationCanceledException>();

        using var withoutCancellation = Load(Compile(scenario, "without-cancellation"));
        withoutCancellation.Evaluate(environment, cancelled.Token)!.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Pine_CSharp_cancellation_interrupts_a_running_tail_loop()
    {
        var scenario = PineCSharpScenarios.Load("mutual-tail");
        using var loaded = Load(Compile(scenario, "with-cancellation"));
        var environment = PineValueInProcess.Create(scenario.Values["cancellation-environment"]);
        using var cancellation = new CancellationTokenSource(TimeSpan.FromMilliseconds(20));
        Action run = () => loaded.Evaluate(environment, cancellation.Token);
        run.Should().Throw<OperationCanceledException>();
    }

    [Fact]
    public void Pine_CSharp_constraint_mismatch_falls_back_only_outside_generated_methods()
    {
        var scenario = PineCSharpScenarios.Load("guards");
        var expression = scenario.Entry;
        var compilation = Compile(scenario, "specialized-only");
        using var loaded = Load(compilation);
        var matching = scenario.Evaluation("matching").Environment;
        var mismatching = scenario.Evaluation("mismatching").Environment;
        loaded.Evaluate(PineValueInProcess.Create(mismatching)).Should().BeNull();
        loaded.Evaluate(PineValueInProcess.Create(scenario.Evaluation("missing-path").Environment)).Should().BeNull();
        var leaf = loaded.CreateLeaf();
        var encoded = ExpressionEncoding.EncodeExpressionAsValue(expression);
        var vm = CreateVM(new Dictionary<PineValue, Core.Runtime.PrecompiledLeaf> { [encoded] = leaf });
        var caller = new Expression.Eval(Expression.LitralInst(encoded), Expression.EnvironmentInstance);

        vm.EvaluateExpression(caller, matching)
            .Extract(error => throw new Exception(error)).Should().Be(
            PineCSharpScenarios.ExpectedValue(scenario, "generic-and-specialized", "matching"));

        vm.EvaluateExpression(caller, mismatching)
            .Extract(error => throw new Exception(error)).Should().Be(
            PineCSharpScenarios.ExpectedValue(scenario, "generic-and-specialized", "mismatching"));
    }

    [Fact]
    public void Pine_CSharp_rejection_retains_a_transitive_call_path_without_hashing()
    {
        var scenario = PineCSharpScenarios.Load("transitive-dynamic");
        var variant = scenario.Variant("generic");
        var error = PineCSharpCompiler.Compile(scenario.Entry, variant.Specializations, variant.Options).IsErrOrNull()!;
        error.Should().NotBeNull();
        error.Kind.Should().Be(PineCSharpRejectionKind.DynamicEvaluation);
        error.Path.Parent.Should().NotBeNull();
        error.Instruction!.Kind.Should().BeOneOf(StackInstructionKind.Eval_Binary, StackInstructionKind.Eval_Multi);
        error.Describe().Should().Contain("root 0: function 0 -> block").And.Contain(" / terminator");
        error.Describe(includeExpressionHashes: true).Should().Contain(error.Path.Function.ExpressionHash);
    }

    [Fact]
    public void Pine_CSharp_literals_retain_precomputed_materialized_values()
    {
        var compilation = Compile(PineCSharpScenarios.Load("literal-reuse"), "without-cancellation");
        using var loaded = Load(compilation);
        var type = loaded.Assembly.GetType(compilation.Options.LiteralsTypeName)!;
        var properties = type.GetProperties(BindingFlags.Public | BindingFlags.Static);
        properties.Should().NotBeEmpty();

        foreach (var property in properties.Where(property => property.PropertyType == typeof(PineValueInProcess)))
        {
            var materialized = type.GetField("Materialized_" + property.Name)!;
            var value = (PineValueInProcess)property.GetValue(null)!;
            value.Evaluate().Should().BeSameAs(materialized.GetValue(null));
        }
    }

    [Fact]
    public void Pine_CSharp_module_names_cannot_shadow_generated_members_or_aliases()
    {
        foreach (var name in new[] { "PineValueInProcess", "Evaluate", "Function_0", "Map_0", "Switch_0", "@Escaped" })
        {
            Action compile = () => PineCSharpCompiler.Compile(Number(7), options: new(ClassName: name));
            compile.Should().Throw<ArgumentException>();
            Action compileLiterals = () => PineCSharpCompiler.Compile(Number(7), options: new(LiteralsClassName: name));
            compileLiterals.Should().Throw<ArgumentException>();
        }

        using var loaded =
            Load(Compile(Number(7), options: new(ClassName: "WorkerModule", LiteralsClassName: "environment")));

        loaded.Evaluate(PineValueInProcess.EmptyList)!.Evaluate().Should().Be(NumberValue(7));
    }

    [Fact]
    public void Pine_CSharp_primitive_lowering_matches_stack_VM_edge_cases()
    {
        var blob = PineValue.Blob([1, 2, 3, 4]);
        var list = PineValue.List([NumberValue(10), NumberValue(20), NumberValue(30)]);

        (StackInstruction Instruction, PineValue[] Arguments)[] cases =
            [
                (StackInstruction.Slice_Skip_Var_Take_Var, [blob, NumberValue(-1), NumberValue(2)]),
                (StackInstruction.Slice_Skip_Var_Take_Const(2), [list, PineValue.EmptyBlob]),
                (StackInstruction.Int_Mul_Const_Add_Binary(3), [NumberValue(4), NumberValue(5)]),
                (StackInstruction.Int_Add_Const(BigInteger.One << 80), [NumberValue(4)]),
                (StackInstruction.Int_Unsigned_Add_Const(2), [blob]),
                (StackInstruction.Int_Mul_Binary, [PineValue.EmptyList, NumberValue(2)]),
                (StackInstruction.Prepend_List_Items(2), [NumberValue(5), NumberValue(6), list]),
                (StackInstruction.Append_List_Items(2), [list, NumberValue(5), NumberValue(6)]),
                (StackInstruction.Build_List_With_Prefix(list, 2), [NumberValue(5), NumberValue(6)]),
                (StackInstruction.Bit_And_Const(PineValue.Blob([7, 0])), [blob]),
                (StackInstruction.Bit_Shift_Left_Binary, [blob, NumberValue(3)]),
                (StackInstruction.Blob_Trim_Leading_Zeros(1), [PineValue.Blob([0, 0, 3])]),
                (StackInstruction.Slice_Skip_Var_Equal_Const(PineValue.Blob([2, 3])), [blob, NumberValue(1)]),
                (StackInstruction.Slice_Skip_Var_Equal_Const(PineValue.EmptyList), [blob, PineValue.EmptyBlob]),
                (StackInstruction.Select_Value_Const(
                    ImmutableDictionary<PineValue, PineValue>.Empty.Add(NumberValue(1), NumberValue(5)),
                    fallbackValue: NumberValue(8)),
                [NumberValue(1)]),
                (StackInstruction.Select_Value_Const(
                    ImmutableDictionary<PineValue, PineValue>.Empty.Add(NumberValue(1), NumberValue(5)),
                    fallbackValue: NumberValue(8)),
                [NumberValue(2)]),
            ];

        foreach (var (instruction, arguments) in cases)
        {
            var fragment =
                PineControlFlowFragment.Empty.AppendOperations(
                    Enumerable.Range(0, arguments.Length).Select(
                        index =>
                        StackInstruction.Local_Get_Skip_Head_Const(0, index)).Append(instruction));

            var graph = PineControlFlowGraph.FromFragment(fragment);
            var environment = PineValue.List(arguments);
            var compilation = EmitGraph(graph);
            using var loaded = Load(compilation);

            if (instruction.Kind == StackInstructionKind.Blob_Trim_Leading_Zeros)
                compilation.Complexity.HasLoops.Should().BeTrue();

            var expected =
                CreateVM(frame: new(StaticFunctionInterface.Generic, graph.LowerToStackInstructions()))
                .EvaluateExpressionOnCustomStack(Expression.EnvironmentInstance, environment, VM.EvaluationConfig.Unbounded)
                .Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate();

            loaded.Evaluate(PineValueInProcess.Create(environment))!.Evaluate().Should().Be(
                expected,
                "lowering {0} must preserve VM semantics",
                instruction.Kind);
        }
    }

    [Fact]
    public void Pine_CSharp_slice_switch_supports_multiple_literals_with_the_same_target()
    {
        var fragment =
            PineControlFlowFragment.Empty
            .AppendOperations(
                [
                StackInstruction.Local_Get_Skip_Head_Const(0, 0),
                StackInstruction.Local_Get_Skip_Head_Const(0, 1)
                ])
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.SliceSkipVarEqual,
                    [
                        new(PineValue.Blob([1, 2]), 0),
                        new(PineValue.Blob([3, 4]), 0)
                    ],
                    PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(0))),
                    [PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(1)))],
                    SkipCountMultiplier: BigInteger.One,
                    ResultCount: 1));

        using var loaded = Load(EmitGraph(PineControlFlowGraph.FromFragment(fragment)));

        foreach (var bytes in new byte[][] { [1, 2, 9], [3, 4, 9], [5, 6, 9] })
        {
            loaded.Evaluate(PineValueInProcess.Create(PineValue.List([PineValue.Blob(bytes), NumberValue(0)])))!
                .Evaluate().Should().Be(NumberValue(bytes[0] == 5 ? 0 : 1));
        }
    }

    [Fact]
    public void Pine_CSharp_slice_switch_hoists_offset_and_preserves_invalid_and_overflowing_counts()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperations(
                    [
                    StackInstruction.Local_Get_Skip_Head_Const(0, 0),
                    StackInstruction.Local_Get_Skip_Head_Const(0, 1)
                    ])
                .Append(
                    new PineControlFlowNode.Switch(
                        PineSwitchKind.SliceSkipVarEqual,
                        [new(PineValue.Blob([1, 2]), 0), new(PineValue.Blob([3, 4]), 1), new(PineValue.EmptyList, 2)],
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(0))),
                        Enumerable.Range(1, 3).Select(
                            value => PineControlFlowFragment.Empty
                            .AppendOperation(StackInstruction.Push_Literal(NumberValue(value)))).ToImmutableArray(),
                        SkipCountMultiplier: 4,
                        ResultCount: 1)));

        var compilation = EmitGraph(graph);
        var syntax = SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource);

        syntax.DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Count(call => call.Expression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "Get" })
            .Should().Be(1);

        syntax.DescendantNodes().OfType<BinaryExpressionSyntax>()
            .Count(binary => binary.Kind() == SyntaxKind.MultiplyExpression).Should().Be(0);

        using var loaded = Load(compilation);
        var bytes = PineValue.Blob([1, 2, 9, 9, 3, 4]);

        (PineValue Count, int Expected)[] cases =
            [
                (NumberValue(0), 1), (NumberValue(1), 2), (NumberValue(-1), 1),
                (NumberValue(2), 0), (PineValue.EmptyBlob, 3),
                (IntegerEncoding.EncodeSignedInteger(-(BigInteger.One << 80)), 1)
            ];

        foreach (var (count, expected) in cases)
        {
            var environment = PineValue.List([bytes, count]);
            var value = loaded.Evaluate(PineValueInProcess.Create(environment))!.Evaluate();
            value.Should().Be(NumberValue(expected));

            value.Should().Be(
                CreateVM(frame: new(StaticFunctionInterface.Generic, graph.LowerToStackInstructions()))
                .EvaluateExpressionOnCustomStack(Expression.EnvironmentInstance, environment, VM.EvaluationConfig.Unbounded)
                .Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate());
        }

        Action overflowing =
            () => loaded.Evaluate(
                PineValueInProcess.Create(
                    PineValue.List(
                        [bytes, IntegerEncoding.EncodeSignedInteger(BigInteger.One << 80)])));

        overflowing.Should().Throw<OverflowException>();
    }

    [Fact]
    public void Pine_CSharp_equality_switch_uses_canonical_lookup_and_shared_targets()
    {
        var key = PineValue.List([NumberValue(2)]);

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperation(StackInstruction.Local_Get(0))
                .Append(
                    new PineControlFlowNode.Switch(
                        PineSwitchKind.Equal,
                        [new(NumberValue(1), 0), new(key, 1), new(NumberValue(3), 0)],
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(0))),
                        [
                            PineControlFlowFragment.Empty.AppendOperation(
                                StackInstruction.Push_Literal(NumberValue(10))),
                            PineControlFlowFragment.Empty.AppendOperation(
                                StackInstruction.Push_Literal(NumberValue(20)))
                        ],
                        SkipCountMultiplier: 1)));

        var compilation = EmitGraph(graph);
        compilation.MethodsSource.Should().Contain("BlobMemory").And.Contain("ListMemory");
        using var loaded = Load(compilation);

        foreach (var (input, expected) in new[] { (NumberValue(1), 10), (key, 20), (NumberValue(3), 10), (NumberValue(4), 0) })
            loaded.Evaluate(PineValueInProcess.Create(input))!.Evaluate().Should().Be(NumberValue(expected));

        loaded.Evaluate(PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(2)]))!
            .Evaluate().Should().Be(NumberValue(20));
    }

    [Fact]
    public void Pine_CSharp_value_maps_reuse_static_tables_and_precomputed_results()
    {
        var key = PineValue.List([NumberValue(2)]);
        var mapped = PineValue.List([NumberValue(20)]);

        var map =
            ImmutableDictionary<PineValue, PineValue>.Empty.Add(NumberValue(1), NumberValue(10))
            .Add(key, mapped).Add(NumberValue(4), NumberValue(40));

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperation(StackInstruction.Local_Get_Skip_Head_Const(0, 0))
                .Append(
                    new PineControlFlowNode.ValueMap(
                        map,
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(30)))))
                .AppendOperations(
                    [
                        StackInstruction.Local_Get_Skip_Head_Const(0, 0),
                        StackInstruction.Select_Value_Const(map, fallbackValue: NumberValue(30)),
                        StackInstruction.Build_List(2)
                    ]));

        var compilation = EmitGraph(graph);
        compilation.MethodsSource.Should().Contain(".TryGetValue(");
        using var loaded = Load(compilation);

        loaded.Assembly.GetType(compilation.Options.LiteralsTypeName)!.GetFields()
            .Count(field => field.Name.StartsWith("Map_", StringComparison.Ordinal)).Should().Be(1);

        foreach (var (input, expected) in new[] { (NumberValue(1), NumberValue(10)), (key, mapped), (NumberValue(3), NumberValue(30)) })
        {
            loaded.Evaluate(PineValueInProcess.Create(PineValue.List([input])))!.Evaluate()
                .Should().Be(PineValue.List([expected, expected]));
        }

        var deferred = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(2)]);
        var result = loaded.Evaluate(PineValueInProcess.CreateList([deferred]))!;
        result.GetElementAt(0).Should().BeSameAs(result.GetElementAt(1));
        result.Evaluate().Should().Be(PineValue.List([mapped, mapped]));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Pine_CSharp_cancellation_checks_entries_and_natural_or_irreducible_backedges(bool irreducible)
    {
        var entry =
            irreducible
            ?
            new PineBasicBlock(
                new(0),
                [],
                [new(StackInstruction.Local_Get(0), [], [new(0)])],
                new PineControlFlowTerminator.ConditionalJump(new(1), new(2), [], [], PineKernelValues.TrueValue))
            :
            new PineBasicBlock(new(0), [], [], new PineControlFlowTerminator.Jump(new(1), [], true));

        var graph =
            new PineControlFlowGraph(
                new(0),
                [
                    entry,
                    new(new(1), [], [], new PineControlFlowTerminator.Jump(new(2), [], true)),
                    new(new(2), [], [], new PineControlFlowTerminator.Jump(new(1), [], false))
                ]);

        var compilation = EmitGraph(graph, enableCancellation: true);
        compilation.Complexity.HasLoops.Should().BeTrue();

        SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource).DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Count(
            call => call.Expression is MemberAccessExpressionSyntax
            { Name.Identifier.ValueText: "ThrowIfCancellationRequested" }).Should().Be(3);

        EmitGraph(graph).MethodsSource.Should().NotContain("ThrowIfCancellationRequested");
        using var loaded = Load(compilation);

        foreach (var method in loaded.Assembly.GetType(compilation.Options.MethodsTypeName)!.GetMethods(
            BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.DeclaredOnly))
        {
            RuntimeHelpers.PrepareMethod(method.MethodHandle);
        }

        using var cancellation = new CancellationTokenSource(TimeSpan.FromMilliseconds(20));
        Action run = () => loaded.Evaluate(PineValueInProcess.EmptyList, cancellation.Token);
        run.Should().Throw<OperationCanceledException>();
    }

    [Fact]
    public void Pine_CSharp_narrowed_declarations_preserve_state_across_reentry_to_one_block()
    {
        var map =
            ImmutableDictionary<PineValue, PineValue>.Empty
            .Add(PineValue.EmptyList, NumberValue(0)).Add(NumberValue(0), NumberValue(1)).Add(NumberValue(1), NumberValue(2));

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperations(
                    [
                        StackInstruction.Local_Get(1),
                        StackInstruction.Select_Value_Const(map, fallbackValue: NumberValue(3)),
                        StackInstruction.Local_Set([1], popCount: 1),
                        StackInstruction.Local_Get(1),
                        StackInstruction.Int_Less_Than_Const(3)
                    ])
                .Append(
                    new PineControlFlowNode.Conditional(
                        PineKernelValues.TrueValue,
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(99))),
                        PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry()))));

        var compilation = EmitGraph(graph, enableCancellation: true);

        var worker =
            SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource).DescendantNodes()
            .OfType<MethodDeclarationSyntax>().Single(method => method.Identifier.ValueText == compilation.Methods[0].Name);

        worker.Body!.Statements.OfType<LocalDeclarationStatementSyntax>()
            .SelectMany(statement => statement.Declaration.Variables)
            .Select(variable => variable.Identifier.ValueText)
            .Where(name => name.StartsWith("local_", StringComparison.Ordinal)).Should().Equal("local_0_1");

        worker.Body.DescendantNodes().OfType<LocalDeclarationStatementSyntax>()
            .Should().Contain(
            declaration => declaration.Parent != worker.Body &&
                declaration.Declaration.Variables.Any(
                    variable => variable.Identifier.ValueText.StartsWith("value_", StringComparison.Ordinal)));

        using var loaded = Load(compilation);
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(1));
        loaded.Evaluate(PineValueInProcess.EmptyList, cancellation.Token)!.Evaluate().Should().Be(NumberValue(99));
    }

    [Fact]
    public void Pine_CSharp_scopes_and_fallthrough_simplification_preserve_nested_branching()
    {
        var source =
            """
            using PineValueInProcess = Pine.Core.Internal.PineValueInProcess;
            class Generated
            {
                static PineValueInProcess Run(bool condition)
                {
                    PineValueInProcess value_0_0 = PineValueInProcess.EmptyList;
                    goto first;
                first:
                    {
                        if (condition)
                        {
                            value_0_0 = PineValueInProcess.EmptyList;
                            return value_0_0;
                        }
                        goto last;
                    }
                last:
                    { return PineValueInProcess.EmptyList; }
                }
            }
            """;

        var rendered = PineCSharpSourceSimplification.Format(source, new HashSet<string>());
        rendered.Should().NotContain("goto");

        var method =
            SyntaxFactory.ParseCompilationUnit(rendered).DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        method.Body!.Statements.Should().NotContain(statement => statement is LocalDeclarationStatementSyntax);
        var declaration = method.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().Single();
        declaration.Parent!.Parent.Should().BeOfType<IfStatementSyntax>();
    }

    [Fact]
    public void Pine_CSharp_nonpopping_local_stores_preserve_stack_peek_order()
    {
        var fragment =
            PineControlFlowFragment.Empty.AppendOperations(
                [
                    StackInstruction.Local_Get_Skip_Head_Const(0, 0),
                    StackInstruction.Local_Get_Skip_Head_Const(0, 1),
                    StackInstruction.Local_Set([1, 2], popCount: 0),
                    StackInstruction.Pop with { PopCount = 2 },
                    new(StackInstructionKind.Local_Int_Add_Const, IntegerLiteral: BigInteger.One, LocalIndices: [1, 1]),
                    StackInstruction.Local_Get([1, 2]),
                    StackInstruction.Build_List(2)
                ]);

        using var loaded = Load(EmitGraph(PineControlFlowGraph.FromFragment(fragment)));

        loaded.Evaluate(PineValueInProcess.Create(PineValue.List([NumberValue(10), NumberValue(20)])))!
            .Evaluate().Should().Be(PineValue.List([NumberValue(22), NumberValue(10)]));
    }

    [Fact]
    public void Pine_CSharp_parallel_local_swaps_use_separate_assignments()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty.AppendOperations(
                    [
                        StackInstruction.Local_Get_Skip_Head_Const(0, 1),
                        StackInstruction.Local_Get_Skip_Head_Const(0, 0),
                        StackInstruction.Local_Set([1, 2], popCount: 2),
                        StackInstruction.Local_Get([1, 2]),
                        StackInstruction.Local_Set([1, 2], popCount: 2),
                        StackInstruction.Local_Get([1, 2]),
                        StackInstruction.Build_List(2)
                    ]));

        var compilation = EmitGraph(graph);
        AssertNoConstructedTupleAssignments(compilation);
        using var assembly = Load(compilation);
        var input = PineValue.List([NumberValue(10), NumberValue(20)]);

        assembly.Evaluate(PineValueInProcess.Create(input))!.Evaluate()
            .Should().Be(PineValue.List([NumberValue(20), NumberValue(10)]));
    }

    [Fact]
    public void Pine_CSharp_cursor_assignments_capture_values_before_dependent_writes()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperations([StackInstruction.Local_Get_Skip_Head_Const(0, 2), StackInstruction.Local_Get(1)])
                .Append(
                    new PineControlFlowNode.Switch(
                        PineSwitchKind.SliceSkipVarEqual,
                        [new PineSwitchFragmentCase(StringEncoding.ValueFromString("a"), 0)],
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Local_Get(2)),
                        [
                            PineControlFlowFragment.Empty.AppendOperations(
                                [
                                    StackInstruction.Local_Get(1),
                                    StackInstruction.Local_Set([2], popCount: 1),
                                    new(StackInstructionKind.Local_Int_Add_Const, IntegerLiteral: 1, LocalIndices: [1])
                                ]).Append(new PineControlFlowNode.JumpToEntry())
                        ],
                        SkipCountMultiplier: 4)));

        var parameters = StaticFunctionInterface.FromPathsSorted([[0], [1], [2]]);
        var compilation = EmitGraph(graph, parameters: parameters);
        compilation.MethodsSource.Should().Contain("BigInteger assignment_").And.Contain("BlobMemory sourceBytes");
        AssertNoConstructedTupleAssignments(compilation);
        using var assembly = Load(compilation);

        foreach (var text in new[] { "", "a", "aaaa!" })
        {
            var input =
                PineValue.List(
                    [
                    PineValue.List(
                        [
                        PineValue.EmptyList,
                        PineValue.EmptyList,
                        StringEncoding.ValueFromString(text)
                        ]),
                    NumberValue(0),
                    NumberValue(99)
                    ]);

            assembly.Evaluate(PineValueInProcess.Create(input))!.Evaluate().Should().Be(
                EvaluateGraph(graph, input, parameters));
        }
    }

    private static void AssertNoConstructedTupleAssignments(PineCSharpCompilation compilation) =>
        SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource).DescendantNodes()
        .OfType<AssignmentExpressionSyntax>().Any(
            assignment =>
            assignment.Left is TupleExpressionSyntax && assignment.Right is TupleExpressionSyntax).Should().BeFalse();

    private static PineCSharpCompilation EmitGraph(
        PineControlFlowGraph graph,
        bool enableCancellation = false,
        int matchLimit = 4,
        StaticFunctionInterface? parameters = null)
    {
        parameters ??= StaticFunctionInterface.Generic;

        var retained =
            Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(Expression.EnvironmentInstance);

        var identity = new PineCSharpFunction(0, retained, null);

        var prepared =
            new InternalExpressionCompilation.StaticFunction(
                retained,
                parameters,
                ReturnValueLayout.Canonical,
                () => graph,
                new(() => new(parameters, graph.LowerToStackInstructions())));

        var function = new PineCSharpCompiler.Function(identity, prepared, new(identity, null, null, 0));

        return
            new PineCSharpEmitter(
                [function],
                [function],
                new(EnableCancellation: enableCancellation, MaximumInlineMatchDifferingPositions: matchLimit)).Emit();
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Pine_CSharp_typed_length_equality_and_negation_use_length_predicates(
        bool negate,
        bool inequalityInstruction)
    {
        var fragment =
            PineControlFlowFragment.Empty.AppendOperations(
                [
                    StackInstruction.Local_Get(0), StackInstruction.Length,
                    inequalityInstruction
                    ?
                    StackInstruction.Not_Equal_Binary_Const(NumberValue(0))
                    :
                    StackInstruction.Equal_Binary_Const(NumberValue(0))
                ]);

        if (negate && !inequalityInstruction)
            fragment = fragment.AppendOperation(StackInstruction.Negate);

        var graph = PineControlFlowGraph.FromFragment(fragment);
        var compilation = EmitGraph(graph);

        compilation.MethodsSource.Should().Contain(".LengthEqual(0)")
            .And.NotContain(".GetLength()").And.NotContain("Kernel.negate").And.NotContain("CreateInteger");

        using var assembly = Load(compilation);

        foreach (var value in new[] { PineValue.EmptyList, PineValue.EmptyBlob, PineValue.Blob([1]), PineValue.List([NumberValue(1)]) })
        {
            assembly.Evaluate(PineValueInProcess.Create(value))!.Evaluate()
                .Should().Be(EvaluateGraph(graph, value));
        }
    }

    [Fact]
    public void Pine_CSharp_length_switches_use_predicates_and_preserve_unbounded_integer_constants()
    {
        foreach (var length in new BigInteger[] { -1, 0, 2, (BigInteger)int.MaxValue + 1, BigInteger.One << 80 })
        {
            var graph =
                PineControlFlowGraph.FromFragment(
                    PineControlFlowFragment.Empty
                    .AppendOperations([StackInstruction.Local_Get(0), StackInstruction.Length])
                    .Append(
                        new PineControlFlowNode.Switch(
                            PineSwitchKind.Equal,
                            [new PineSwitchFragmentCase(IntegerEncoding.EncodeSignedInteger(length), 0)],
                            PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(0))),
                            [
                            PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(1)))
                            ],
                            SkipCountMultiplier: 1)));

            var compilation = EmitGraph(graph);
            compilation.MethodsSource.Should().NotContain(".GetLength()");

            if (length >= 0 && length <= int.MaxValue)
            {
                compilation.MethodsSource.Should().Contain(".LengthEqual(");

                SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource).DescendantNodes()
                    .OfType<IfStatementSyntax>().Any(
                    branch =>
                    branch.Condition is InvocationExpressionSyntax
                    { Expression: MemberAccessExpressionSyntax { Name.Identifier.ValueText: "LengthEqual" } }).Should().BeTrue();
            }

            using var assembly = Load(compilation);

            foreach (var value in new[] { PineValue.EmptyList, PineValue.EmptyBlob, PineValue.Blob([1, 2]), PineValue.List([NumberValue(1), NumberValue(2)]) })
                assembly.Evaluate(PineValueInProcess.Create(value))!.Evaluate().Should().Be(EvaluateGraph(graph, value));
        }
    }

    [Fact]
    public void Pine_CSharp_lengths_shared_with_other_consumers_keep_the_numeric_result()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty.AppendOperations(
                    [
                        StackInstruction.Local_Get(0), StackInstruction.Length,
                        StackInstruction.Local_Set([1], popCount: 0), StackInstruction.Equal_Binary_Const(NumberValue(2)),
                        StackInstruction.Local_Get(1), StackInstruction.Build_List(2)
                    ]));

        var compilation = EmitGraph(graph);
        compilation.MethodsSource.Should().Contain(".GetLength()");
        using var assembly = Load(compilation);

        foreach (var value in new[] { PineValue.EmptyList, PineValue.Blob([1, 2]) })
            assembly.Evaluate(PineValueInProcess.Create(value))!.Evaluate().Should().Be(EvaluateGraph(graph, value));
    }

    [Fact]
    public void Pine_CSharp_common_projections_are_shared_without_parameter_cache_assumptions()
    {
        var fragment =
            PineControlFlowFragment.Empty.AppendOperations(
                [StackInstruction.Local_Get(0), StackInstruction.Local_Set([1], popCount: 1)]);

        foreach (var index in new[] { 2, 4, 6, 8, 10 })
        {
            fragment =
                fragment.AppendOperations(
                    [StackInstruction.Local_Get_Skip_Head_Const(1, 1), StackInstruction.Skip_Head_Const(index)]);
        }

        var graph = PineControlFlowGraph.FromFragment(fragment.AppendOperation(StackInstruction.Build_List(5)));
        var compilation = EmitGraph(graph);

        var calls =
            SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource).DescendantNodes()
            .OfType<InvocationExpressionSyntax>().Where(
                call =>
                call.Expression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "GetElementAt" } &&
                call.ArgumentList.Arguments.Single().Expression.ToString() == "1");

        calls.Should().ContainSingle();
        using var assembly = Load(compilation);

        var input =
            PineValue.List(
                [
                PineValue.EmptyList,
                PineValue.List(Enumerable.Range(0, 11).Select(index => NumberValue(index)).ToArray())
                ]);

        assembly.Evaluate(PineValueInProcess.Create(input))!.Evaluate().Should().Be(EvaluateGraph(graph, input));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Pine_CSharp_shared_blob_and_list_matches_preserve_exact_values_and_source_kinds(bool lists)
    {
        PineValue[] literals =
            Enumerable.Range(1, 3).Select(
                index => lists
                ?
                (PineValue)PineValue.List([NumberValue(99), NumberValue(index), NumberValue(100)])
                :
                PineValue.Blob([0, (byte)index, 0])).ToArray();

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperation(StackInstruction.Local_Get(0))
                .Append(
                    new PineControlFlowNode.Switch(
                        PineSwitchKind.Equal,
                        [.. literals.Select((literal, index) => new PineSwitchFragmentCase(literal, index))],
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(0))),
                        [
                            .. Enumerable.Range(1, 3).Select(
                                index => PineControlFlowFragment.Empty
                                .AppendOperation(StackInstruction.Push_Literal(NumberValue(index))))
                        ],
                        SkipCountMultiplier: 1)));

        var compilation = EmitGraph(graph);

        compilation.MethodsSource.Should().Contain(lists ? "ListMemory" : "BlobMemory")
            .And.Contain(lists ? ".EvaluateIfList() is { }" : ".EvaluateIfBlob() is { }")
            .And.NotContain(".GetLength()").And.NotContain("TryGetValue");

        var syntax = SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource);

        syntax.DescendantNodes().OfType<ElementAccessExpressionSyntax>().Count(
            access =>
            access.ArgumentList.Arguments[0].Expression.ToString() == "0").Should().Be(1);

        using var assembly = Load(compilation);
        var malformed = PineValue.List([NumberValue(99), PineValue.Blob([4, 0, 1]), NumberValue(100)]);

        foreach (var input in literals.Concat(
            new[]
            {
                PineValue.EmptyList, PineValue.EmptyBlob, PineValue.Blob([0, 1]), PineValue.Blob([0, 1, 0, 0]),
                PineValue.List([NumberValue(99), NumberValue(1)]), malformed
            }))
        {
            assembly.Evaluate(PineValueInProcess.Create(input))!.Evaluate().Should().Be(EvaluateGraph(graph, input));
        }

        using var disabled = Load(EmitGraph(graph, matchLimit: 0));
        disabled.Evaluate(PineValueInProcess.Create(literals[1]))!.Evaluate().Should().Be(NumberValue(2));
    }

    [Fact]
    public void Pine_CSharp_shared_slice_matches_preserve_prefix_order_and_list_slots()
    {
        PineValue[] literals =
            [
                PineValue.Blob([0]), PineValue.Blob([0, 1]), PineValue.EmptyBlob,
                PineValue.List([NumberValue(9), NumberValue(1)]),
                PineValue.List([NumberValue(9), NumberValue(2)]),
                PineValue.EmptyList
            ];

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperations(
                    [
                    StackInstruction.Local_Get_Skip_Head_Const(0, 0),
                    StackInstruction.Local_Get_Skip_Head_Const(0, 1)
                    ])
                .Append(
                    new PineControlFlowNode.Switch(
                        PineSwitchKind.SliceSkipVarEqual,
                        [.. literals.Select((literal, index) => new PineSwitchFragmentCase(literal, index))],
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Push_Literal(NumberValue(0))),
                        [
                            .. Enumerable.Range(1, literals.Length).Select(
                                index => PineControlFlowFragment.Empty
                                .AppendOperation(StackInstruction.Push_Literal(NumberValue(index))))
                        ],
                        SkipCountMultiplier: 1)));

        using var assembly = Load(EmitGraph(graph));

        PineValue[] sources =
            [
                PineValue.Blob([0, 1]), PineValue.Blob([5, 0, 1]), PineValue.EmptyBlob,
                PineValue.List([NumberValue(5), NumberValue(9), NumberValue(1), NumberValue(10)]),
                PineValue.List([NumberValue(9), NumberValue(2)]),
                PineValue.List([NumberValue(9), PineValue.Blob([4, 0, 1])]),
                PineValue.EmptyList
            ];

        foreach (var source in sources)
        {
            foreach (var count in new[] { NumberValue(-1), NumberValue(0), NumberValue(1), NumberValue(2), NumberValue(8), PineValue.EmptyList })
            {
                var environment = PineValue.List([source, count]);

                assembly.Evaluate(PineValueInProcess.Create(environment))!.Evaluate()
                    .Should().Be(EvaluateGraph(graph, environment));
            }
        }
    }

    [Fact]
    public void Pine_CSharp_integer_facts_do_not_equate_noncanonical_blobs_or_escape_cfg_branches()
    {
        var noncanonical = PineValue.Blob([4, 0, 1]);

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty
                .AppendOperation(StackInstruction.Local_Get_Skip_Head_Const(0, 0))
                .Append(
                    new PineControlFlowNode.Conditional(
                        PineKernelValues.TrueValue,
                        PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Local_Get_Skip_Head_Const(0, 1)),
                        PineControlFlowFragment.Empty.AppendOperations(
                            [StackInstruction.Local_Get_Skip_Head_Const(0, 2), StackInstruction.Length])))
                .AppendOperation(StackInstruction.Equal_Binary_Const(NumberValue(1))));

        using var assembly = Load(EmitGraph(graph));

        foreach (var condition in new[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            var environment = PineValue.List([condition, noncanonical, PineValue.Blob([9])]);

            assembly.Evaluate(PineValueInProcess.Create(environment))!.Evaluate()
                .Should().Be(EvaluateGraph(graph, environment));
        }

        var literalComparison =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty.AppendOperations(
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Length,
                    StackInstruction.Equal_Binary_Const(noncanonical)
                    ]));

        using var comparison = Load(EmitGraph(literalComparison));

        comparison.Evaluate(PineValueInProcess.Create(PineValue.Blob([9])))!.Evaluate().Should().Be(
            PineKernelValues.FalseValue);
    }

    [Theory]
    [InlineData(0, false)]
    [InlineData(3, false)]
    [InlineData(4, true)]
    [InlineData(5, true)]
    public void Pine_CSharp_cursor_specialization_counts_varying_positions_not_blob_length(int limit, bool optimized)
    {
        var literals = new[] { StringEncoding.ValueFromString("abcd"), StringEncoding.ValueFromString("efgh") };
        var graph = CursorGraph(literals, 4);
        var parameters = StaticFunctionInterface.FromPathsSorted([[0], [1], [2]]);
        var compilation = EmitGraph(graph, matchLimit: limit, parameters: parameters);
        AssertNoConstructedTupleAssignments(compilation);

        if (optimized)
        {
            compilation.MethodsSource
                .Should().Contain("BlobMemory sourceBytes").And.Contain("BigInteger? initialOffset")
                .And.Contain(".EvaluateIfBlob() is not { }").And.NotContain("using BigInt")
                .And.NotContain("SliceSkipVarEqualConst").And.NotContain("assignment_");
        }
        else
        {
            compilation.MethodsSource.Should().Contain("SliceSkipVarEqualConst").And.NotContain(
                "BlobMemory sourceBytes");
        }

        using var assembly = Load(compilation);

        foreach (var source in new[]
        {
            StringEncoding.ValueFromString("abcdefgh!"), StringEncoding.ValueFromString("abc"),
            PineValue.Blob([0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0, 100, 1]),
            PineValue.List([NumberValue(1)]), PineValue.EmptyBlob
        })
        {
            var environment =
                PineValue.List(
                    [
                    PineValue.List([PineValue.EmptyList, PineValue.EmptyList, source]),
                    NumberValue(0),
                    NumberValue(10)
                    ]);

            assembly.Evaluate(PineValueInProcess.Create(environment))!.Evaluate()
                .Should().Be(EvaluateGraph(graph, environment, parameters));
        }
    }

    [Fact]
    public void Pine_CSharp_cursor_typed_paths_preserve_invalid_counts_identity_and_cancellation()
    {
        var graph = CursorGraph([StringEncoding.ValueFromString("a"), StringEncoding.ValueFromString("b")], 1);
        var parameters = StaticFunctionInterface.FromPathsSorted([[0], [1], [2]]);
        var compilation = EmitGraph(graph, enableCancellation: true, parameters: parameters);
        using var assembly = Load(compilation);

        var worker =
            assembly.CreateWorkerDelegate<Func<PineValueInProcess, PineValueInProcess,
            PineValueInProcess, CancellationToken, PineValueInProcess>>(compilation.Methods.Single());

        var original = PineValueInProcess.Create(PineValue.Blob([4, 0, 10]));
        var blob = StringEncoding.ValueFromString("aabb!");
        var source = PineValue.List([PineValue.EmptyList, PineValue.EmptyList, blob]);

        foreach (var count in new[] { NumberValue(0), NumberValue(2), NumberValue(-1), PineValue.EmptyList, NumberValue(99) })
        {
            var environment = PineValue.List([source, count, original.Evaluate()]);

            assembly.Evaluate(PineValueInProcess.Create(environment))!.Evaluate()
                .Should().Be(EvaluateGraph(graph, environment, parameters));
        }

        var notBlob =
            PineValueInProcess.Create(PineValue.List([PineValue.EmptyList, PineValue.EmptyList, PineValue.EmptyList]));

        worker(notBlob, PineValueInProcess.CreateInteger(0), original, default).Should().BeSameAs(original);

        worker(
            PineValueInProcess.Create(source),
            PineValueInProcess.EmptyList,
            original,
            default).Should().BeSameAs(original);

        Action overflow =
            () => worker(notBlob, PineValueInProcess.CreateInteger(BigInteger.One << 80), original, default);

        overflow.Should().Throw<OverflowException>();

        var many =
            PineValue.List(
                [
                PineValue.EmptyList,
                PineValue.EmptyList,
                StringEncoding.ValueFromString(new string('a', 100000))
                ]);

        using var cancellation = new CancellationTokenSource(TimeSpan.FromMilliseconds(20));

        Action run =
            () => worker(
                PineValueInProcess.Create(many),
                PineValueInProcess.CreateInteger(-(BigInteger.One << 80)),
                original,
                cancellation.Token);

        run.Should().Throw<OperationCanceledException>();
    }

    private static PineControlFlowGraph CursorGraph(PineValue[] literals, int step) =>
        PineControlFlowGraph.FromFragment(
            PineControlFlowFragment.Empty
            .AppendOperations([StackInstruction.Local_Get_Skip_Head_Const(0, 2), StackInstruction.Local_Get(1)])
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.SliceSkipVarEqual,
                    [.. literals.Select(literal => new PineSwitchFragmentCase(literal, 0))],
                    PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Local_Get(2)),
                    [
                        PineControlFlowFragment.Empty.AppendOperations(
                            [
                                new(StackInstructionKind.Local_Int_Add_Const, IntegerLiteral: step, LocalIndices: [1]),
                                new(StackInstructionKind.Local_Int_Add_Const, IntegerLiteral: 1, LocalIndices: [2])
                            ])
                        .Append(new PineControlFlowNode.JumpToEntry())
                    ],
                    SkipCountMultiplier: 4)));

    private static PineValue EvaluateGraph(
        PineControlFlowGraph graph, PineValue environment, StaticFunctionInterface? parameters = null) =>
        CreateVM(frame: new(parameters ?? StaticFunctionInterface.Generic, graph.LowerToStackInstructions()))
        .EvaluateExpressionOnCustomStack(Expression.EnvironmentInstance, environment, VM.EvaluationConfig.Unbounded)
        .Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate();

    [Fact]
    public void Pine_CSharp_escaping_numeric_aliases_share_one_materialization()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.Empty.AppendOperations(
                    [
                        StackInstruction.Local_Get(0),
                        StackInstruction.Int_Add_Const(100000),
                        StackInstruction.Local_Set([1], popCount: 0),
                        StackInstruction.Local_Get(1),
                        StackInstruction.Build_List(2)
                    ]));

        using var assembly = Load(EmitGraph(graph));
        var result = assembly.Evaluate(PineValueInProcess.CreateInteger(19999))!;
        result.GetElementAt(0).Should().BeSameAs(result.GetElementAt(1));
        result.GetElementAt(0).AsInteger().Should().Be(new BigInteger(119999));
    }

    [Fact]
    public void Pine_CSharp_pine_state_is_not_replaced_by_its_initial_argument_across_backedges()
    {
        var graph =
            new PineControlFlowGraph(
                new(0),
                [
                    new(
                        new(0),
                        [],
                        [
                            new(StackInstruction.Local_Get(0), [], [new(0)]),
                            new(StackInstruction.Local_Set([1], popCount: 1), [new(0)], [])
                        ],
                        new PineControlFlowTerminator.Jump(new(1), [], true)),
                    new(
                        new(1),
                        [],
                        [
                            new(StackInstruction.Local_Get(1), [], [new(1)]),
                            new(StackInstruction.Int_Less_Than_Or_Equal_Const(0), [new(1)], [new(2)])
                        ],
                        new PineControlFlowTerminator.ConditionalJump(
                            new(2),
                            new(3),
                            [],
                            [],
                            PineKernelValues.TrueValue)),
                    new(
                        new(2),
                        [],
                        [
                            new(StackInstruction.Local_Get(1), [], [new(3)]),
                            new(StackInstruction.Int_Add_Const(-1), [new(3)], [new(4)]),
                            new(StackInstruction.Local_Set([1], popCount: 0), [], []),
                            new(StackInstruction.Build_List(1), [new(4)], [new(5)]),
                            new(StackInstruction.Pop, [new(5)], [])
                        ],
                        new PineControlFlowTerminator.Jump(new(1), [], false)),
                    new(
                        new(3),
                        [],
                        [
                        new(StackInstruction.Local_Get(1), [], [new(6)])
                        ],
                        new PineControlFlowTerminator.Return(1))
                ]);

        using var assembly = Load(EmitGraph(graph, enableCancellation: true));
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(2));

        assembly.Evaluate(PineValueInProcess.CreateInteger(3), cancellation.Token)!.Evaluate().Should().Be(
            NumberValue(0));
    }

    private static PineCSharpCompilation Compile(PineCSharpScenario scenario, string variantName)
    {
        var variant = scenario.Variant(variantName);
        return Compile(scenario.Entry, variant.Specializations, variant.Options);
    }

    private static PineCSharpCompilation Compile(
        Expression expression,
        IReadOnlyList<PineValueClass>? specializations = null,
        PineCSharpCompilationOptions? options = null) =>
        PineCSharpCompiler.Compile(expression, specializations, options ?? new(EnableCancellation: false))
        .Extract(error => throw new Exception(error.Describe()));

    private static PineCSharpAssembly Load(PineCSharpCompilation compilation) =>
        PineCSharpAssembly.Load(compilation).Extract(
            diagnostics =>
            throw new Exception(string.Join("\n", diagnostics) + "\n" + compilation.MethodsSource));

    private static PineValue NumberValue(int value) => IntegerEncoding.EncodeSignedInteger(value);

    private static Expression Number(int value) => Expression.LitralInst(NumberValue(value));

    private static VM CreateVM(
        IReadOnlyDictionary<PineValue, Core.Runtime.PrecompiledLeaf>? leaves = null,
        StackFrameInstructions? frame = null) =>
        VM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: VM.EvaluationConfig.Unbounded,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: false,
            selectPrecompiled: null,
            skipInlineForExpression: _ => true,
            enableTailRecursionOptimization: true,
            parseCache: null,
            precompiledLeaves: leaves,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            expressionCompilationOverrides:
            frame is null
            ?
            null
            :
            new Dictionary<Core.Runtime.InternalExpression.Expression, ExpressionCompilation>
            {
                [Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(Expression.EnvironmentInstance)] =
                new(frame, [])
            });
}
