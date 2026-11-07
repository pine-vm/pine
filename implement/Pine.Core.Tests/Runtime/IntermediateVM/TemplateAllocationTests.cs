using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Xunit;

using VM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class TemplateAllocationTests
{
    [Fact]
    public void Prepared_projections_are_scoped_to_compilation_and_recipe()
    {
        var recipe = new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(0), new TemplateValueRecipe.Slot(1)]);
        var firstCompilation = Compilation(StaticFunctionInterface.FromPathsSorted([[0]]));

        var secondCompilation =
            firstCompilation with
            {
                Generic =
                new StackFrameInstructions(
                    StaticFunctionInterface.FromPathsSorted([[1]]),
                    [StackInstruction.Local_Get(0), StackInstruction.Return])
            };

        var first = PreparedTemplateInput.Get(firstCompilation, recipe);
        var second = PreparedTemplateInput.Get(secondCompilation, recipe);
        first.Should().BeSameAs(PreparedTemplateInput.Get(firstCompilation, recipe));
        second.Should().NotBeSameAs(first);
        var environments = new[] { PineValueInProcess.EmptyBlob, PineValueInProcess.EmptyList };
        first.Instantiate(environments).Arguments[0].Should().BeSameAs(environments[0]);
        second.Instantiate(environments).Arguments[0].Should().BeSameAs(environments[1]);

        first.Should().NotBeSameAs(
            PreparedTemplateInput.Get(
                firstCompilation,
                new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(1)])));
    }

    [Fact]
    public void Cached_preparation_allocates_nothing_on_repeated_lookup()
    {
        var compilation = Compilation(StaticFunctionInterface.FromPathsSorted([[0]]));
        var recipe = new TemplateValueRecipe.Slot(0);
        var prepared = PreparedTemplateInput.Get(compilation, recipe);

        for (var index = 0; index < 1000; ++index)
            _ = PreparedTemplateInput.Get(compilation, recipe);

        var before = GC.GetAllocatedBytesForCurrentThread();
        var same = true;

        for (var index = 0; index < 1000; ++index)
            same &= ReferenceEquals(prepared, PreparedTemplateInput.Get(compilation, recipe));

        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        same.Should().BeTrue();
        allocated.Should().Be(0);
    }

    [Fact]
    public void Empty_parameter_layouts_reuse_empty_arrays_without_reading_environments()
    {
        var parameters = StaticFunctionInterface.FromPathsSorted([]);

        var compilation =
            new ExpressionCompilation(
                new StackFrameInstructions(
                    parameters,
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Return
                    ]),
                []);

        var prepared = PreparedTemplateInput.Get(compilation, new TemplateValueRecipe.Slot(100));
        var first = prepared.Instantiate([]);
        var second = prepared.Instantiate([]);
        first.Arguments.Should().BeEmpty();
        first.Arguments.Should().BeSameAs(second.Arguments);
        first.EvaluatedArguments.Should().BeEmpty();
        first.EvaluatedArguments.Should().BeSameAs(second.EvaluatedArguments);
    }

    [Fact]
    public void Tree_recipes_avoid_the_per_invocation_dictionary()
    {
        var recipe = new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(0), new TemplateValueRecipe.Slot(1)]);
        var parameters = StaticFunctionInterface.Generic;
        var prepared = PreparedTemplateInput.Get(Compilation(parameters), recipe);
        var environments = new[] { PineValueInProcess.EmptyBlob, PineValueInProcess.EmptyList };

        Func<StackFrameInput> original =
            () => StackFrameInput.FromArguments(parameters, new[] { recipe.Instantiate(environments) });

        Func<StackFrameInput> optimized = () => prepared.Instantiate(environments);

        for (var index = 0; index < 1000; ++index)
        {
            _ = original();
            _ = optimized();
        }

        var originalBytes = Allocated(original);
        var optimizedBytes = Allocated(optimized);
        (originalBytes - optimizedBytes).Should().BeGreaterThanOrEqualTo(160_000);
        original().EvaluatedArguments.Should().Equal(optimized().EvaluatedArguments);

        static long Allocated(Func<StackFrameInput> create)
        {
            var before = GC.GetAllocatedBytesForCurrentThread();

            for (var index = 0; index < 1000; ++index)
                _ = create();

            return GC.GetAllocatedBytesForCurrentThread() - before;
        }
    }

    [Fact]
    public void Shared_constructor_nodes_reuse_one_memo_across_argument_projections()
    {
        var shared = new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(0)]);
        var recipe = new TemplateValueRecipe.List([shared, shared]);
        var parameters = StaticFunctionInterface.FromPathsSorted([[0], [1]]);
        var prepared = PreparedTemplateInput.Get(Compilation(parameters), recipe);
        var environment = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(37)]);
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);
        StackFrameInput input;

        using (profile.Enter())
            input = prepared.Instantiate([environment]);

        input.Arguments[0].Should().BeSameAs(input.Arguments[1]);
        environment.EvaluatedOrNull.Should().BeNull();
        profile[PineVMWorkProfile.Metric.RecipeInstantiateSharedMemoHits].Should().Be(1);
        profile[PineVMWorkProfile.Metric.RecipeInstantiateSharedChildArrays].Should().Be(1);
    }

    [Fact]
    public void Argument_arrays_and_memos_remain_fresh_between_invocations()
    {
        var shared = new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(0)]);
        var recipe = new TemplateValueRecipe.List([shared, shared]);

        var prepared =
            PreparedTemplateInput.Get(
                Compilation(StaticFunctionInterface.FromPathsSorted([[0], [1]])),
                recipe);

        var first = prepared.Instantiate([PineValueInProcess.EmptyBlob]).Materialize();
        var second = prepared.Instantiate([PineValueInProcess.EmptyList]).Materialize();
        first.Arguments.Should().NotBeSameAs(second.Arguments);
        first.Arguments[0].Should().NotBeSameAs(second.Arguments[0]);
        first.EvaluatedArguments.Should().NotEqual(second.EvaluatedArguments);
        first.EvaluatedArguments[0].Should().Be(PineValue.List([PineValue.EmptyBlob]));
    }

    [Fact]
    public void Concurrent_preparation_and_instantiation_keep_execution_values_isolated()
    {
        var compilation = Compilation(StaticFunctionInterface.Generic);
        var recipe = new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(0)]);
        var prepared = PreparedTemplateInput.Get(compilation, recipe);

        Parallel.For(
            0,
            100,
            index =>
            {
                PreparedTemplateInput.Get(compilation, recipe).Should().BeSameAs(prepared);
                var environment = PineValueInProcess.CreateInteger(index);
                var input = prepared.Instantiate([environment]);
                input.EvaluatedArguments[0].Should().Be(PineValue.List([environment.Evaluate()]));
            });
    }

    [Fact]
    public void Preparation_does_not_keep_unreferenced_recipes_alive()
    {
        var compilation = Compilation(StaticFunctionInterface.Generic);
        var references = PrepareUnreferencedRecipe(compilation);
        Collect();
        references.recipe.IsAlive.Should().BeFalse();
        references.prepared.IsAlive.Should().BeFalse();
        GC.KeepAlive(compilation);
    }

    [Fact]
    public void Preparation_does_not_keep_unreferenced_compilations_alive()
    {
        var recipe = new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(0)]);
        var reference = PrepareUnreferencedCompilation(recipe);
        Collect();
        reference.IsAlive.Should().BeFalse();
        GC.KeepAlive(recipe);
    }

    [Fact]
    public void Frame_storage_regions_share_only_their_backing_array_not_argument_slots()
    {
        var parameters = StaticFunctionInterface.Generic;
        var original = PineValueInProcess.CreateInteger(37);
        var input = StackFrameInput.FromArguments(parameters, new[] { original });

        var instructions =
            new StackFrameInstructions(parameters, [StackInstruction.Local_Get(0), StackInstruction.Return]);

        var method = typeof(VM).GetMethod("BuildStackFrame", BindingFlags.Static | BindingFlags.NonPublic)!;

        var frame =
            (StackFrame)method.Invoke(null, [
            null, Expression.EnvironmentInstance, instructions, input, new StackFrameProfilingBaseline()
        ])!;

        MemoryMarshal.TryGetArray((ReadOnlyMemory<PineValueInProcess>)frame.LocalsValues, out var locals)
            .Should().BeTrue();

        MemoryMarshal.TryGetArray((ReadOnlyMemory<PineValueInProcess?>)frame.StackValues, out var stack)
            .Should().BeTrue();

        ((object?)locals.Array).Should().BeSameAs(stack.Array);
        (locals.Offset + locals.Count).Should().BeLessThanOrEqualTo(stack.Offset);
        frame.LocalSet(0, PineValueInProcess.EmptyBlob);
        frame.PushInstructionResult(PineValueInProcess.EmptyList);
        input.Arguments[0].Should().BeSameAs(original);
        frame.LocalGet(0).Should().BeSameAs(PineValueInProcess.EmptyBlob);
        frame.PopTopmostFromStack().Should().BeSameAs(PineValueInProcess.EmptyList);
        frame.LocalGet(0).Should().BeSameAs(PineValueInProcess.EmptyBlob);
        input.EvaluatedArguments[0].Should().Be(original.Evaluate());
    }

    private static ExpressionCompilation Compilation(StaticFunctionInterface parameters) =>
        new(new StackFrameInstructions(parameters, [StackInstruction.Local_Get(0), StackInstruction.Return]), []);

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static (WeakReference recipe, WeakReference prepared) PrepareUnreferencedRecipe(
        ExpressionCompilation compilation)
    {
        var recipe = new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(0)]);
        return (new(recipe), new(PreparedTemplateInput.Get(compilation, recipe)));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static WeakReference PrepareUnreferencedCompilation(TemplateValueRecipe recipe)
    {
        var compilation = Compilation(StaticFunctionInterface.Generic);
        _ = PreparedTemplateInput.Get(compilation, recipe);
        return new(compilation);
    }

    private static void Collect()
    {
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();
    }
}
