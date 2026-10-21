using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class ExpressionTemplatePlanTests
{
    [Theory]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(4)]
    [InlineData(12)]
    public void Recognized_chain_recipes_match_each_ordinary_application(int count)
    {
        var terminal = Expression.EnvironmentInstance;
        var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(terminal, count)!;
        var cache = new PineVMParseCache();
        var plan = ExpressionTemplatePlan.TryParseTemplate(entry, cache)!;
        plan.Should().NotBeNull();
        plan.EnvironmentCount.Should().Be(count);
        plan.TerminalExpression.Should().Be(terminal);
        plan.IntermediateRecipes.Count.Should().Be(count - 1);
        plan.Materialize([], new DirectInterpreterCounters()).Should().BeSameAs(entry);

        var environments = new List<PineValueInProcess>();
        var current = entry;
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        for (var stage = 0; stage < count; ++stage)
        {
            var argument = PineValue.List([IntegerEncoding.EncodeSignedInteger(stage), PineValue.EmptyBlob]);
            environments.Add(PineValueInProcess.Create(argument));
            current = Apply(interpreter, current, argument);

            if (stage + 1 < count)
            {
                plan.Materialize(environments, new DirectInterpreterCounters()).Should().Be(current);
            }
            else
            {
                interpreter.EvaluateExpressionDefault(
                    plan.TerminalExpression,
                    plan.BuildTerminalEnvironment(environments).Evaluate())
                    .Should().Be(current);
            }
        }
    }

    [Fact]
    public void Repeated_environment_references_are_recipes_not_probe_values()
    {
        var terminal = Expression.EnvironmentInstance;
        var terminalEncoded = ExpressionEncoding.EncodeExpressionAsValue(terminal);

        var next =
            new Expression.Eval(
                Expression.LitralInst(terminalEncoded),
                Expression.ListInst(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(19)),
                    Expression.EnvironmentInstance
                    ]));

        // The entry captures its environment twice as literal payloads of the next stage.
        var entryExpression =
            Expression.ListInst(
                [
                Tag("Eval"),
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(next.Encoded)),
                BuildEncodedListWithCapturedEnvironment()
                ]);

        var entry = ExpressionEncoding.EncodeExpressionAsValue(entryExpression);
        var cache = new PineVMParseCache();
        var plan = ExpressionTemplatePlan.TryParseTemplate(entry, cache)!;
        plan.Should().NotBeNull();
        plan.EnvironmentCount.Should().Be(2);
        plan.TerminalExpression.Should().Be(terminal);

        var first = PineValue.List([IntegerEncoding.EncodeSignedInteger(37)]);
        var second = PineValue.EmptyBlob;

        PineValueInProcess[] environments =
            [PineValueInProcess.Create(first), PineValueInProcess.Create(second)];

        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);
        var intermediate = Apply(interpreter, entry, first);

        plan.Materialize([environments[0]], new DirectInterpreterCounters()).Should().Be(intermediate);

        plan.BuildTerminalEnvironment(environments).Evaluate()
            .Should().Be(Apply(interpreter, intermediate, second));

        plan.TerminalEnvironmentRecipe.Project([0]).Instantiate(environments)
            .Should().BeSameAs(environments[0]);

        plan.TerminalEnvironmentRecipe.Project([2]).Instantiate(environments)
            .Should().BeSameAs(environments[0]);
    }

    private static Expression BuildEncodedListWithCapturedEnvironment()
    {
        var literal = Expression.ListInst([Tag("Litral"), Expression.EnvironmentInstance]);

        Expression[] children =
            [
            literal,
            Expression.LitralInst(
                ExpressionEncoding.EncodeExpressionAsValue(
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(19)))),
            literal
            ];

        return Expression.ListInst([Tag("List"), .. children]);
    }

    [Fact]
    public void Legacy_constructors_are_not_recognized()
    {
        var entry =
            ExpressionEncoding2024.EncodeExpressionAsValue(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)));

        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache()).Should().BeNull();
    }

    [Fact]
    public void Partially_materialized_entry_constants_are_not_new_environment_slots()
    {
        var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(Expression.EnvironmentInstance, 3)!;
        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);
        var captured = PineValue.List([PineValue.EmptyBlob, IntegerEncoding.EncodeSignedInteger(41)]);
        var partialEntry = Apply(interpreter, entry, captured);
        var plan = ExpressionTemplatePlan.TryParseTemplate(partialEntry, cache)!;
        plan.EnvironmentCount.Should().Be(2);
        PineValueInProcess[] environments = [PineValueInProcess.EmptyList, PineValueInProcess.EmptyBlob];

        plan.BuildTerminalEnvironment(environments).Evaluate().Should().Be(
            Apply(interpreter, Apply(interpreter, partialEntry, PineValue.EmptyList), PineValue.EmptyBlob));
    }

    [Fact]
    public void Dynamic_literal_terminal_is_not_frozen()
    {
        var entry =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst([Tag("Litral"), Expression.EnvironmentInstance]));

        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache()).Should().BeNull();
    }

    [Fact]
    public void Dynamic_code_tag_is_not_recognized()
    {
        var entry =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst([Expression.EnvironmentInstance, Expression.LitralInst(PineValue.EmptyList)]));

        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache()).Should().BeNull();
    }

    [Fact]
    public void Future_malformed_code_is_a_recognition_miss_not_a_parse_exception()
    {
        var entry =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst([Tag("Litral")]));

        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache()).Should().BeNull();

        Apply(DirectInterpreter.WithoutEvalCaching(new PineVMParseCache()), entry, PineValue.EmptyList)
            .Should().Be(PineValue.List([StringEncoding.ValueFromString("Litral")]));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Mixed_encoding_children_do_not_prove_a_constructor_or_terminal_eval(bool eval)
    {
        var legacyLiteral =
            ExpressionEncoding2024.EncodeExpressionAsValue(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)));

        var malformed =
            PineValue.List(
                eval
                ?
                [
                StringEncoding.ValueFromString("Eval"), legacyLiteral,
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)
                ]
                :
                [StringEncoding.ValueFromString("List"), legacyLiteral]);

        var entry = ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(malformed));

        ExpressionEncoding.ParseExpressionFromValue(malformed).IsErrOrNull().Should().NotBeNull();
        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache()).Should().BeNull();

        Apply(DirectInterpreter.WithoutEvalCaching(new PineVMParseCache()), entry, PineValue.EmptyList)
            .Should().Be(malformed);
    }

    [Fact]
    public void Recognition_does_not_execute_arbitrary_eval_or_builtin()
    {
        var entry =
            ExpressionEncoding.EncodeExpressionAsValue(
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance));

        var counters = new DirectInterpreterCounters();
        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache(), counters).Should().BeNull();
        counters.Snapshot().DirectInterpreterInvocationCount.Should().Be(0);
        counters.Snapshot().DirectInterpreterEvalCount.Should().Be(0);
    }

    [Fact]
    public void Invariant_arbitrary_terminal_computation_can_follow_a_constant_constructor()
    {
        var terminal = Expression.BuiltinInst("length", Expression.EnvironmentInstance);
        var terminalValue = ExpressionEncoding.EncodeExpressionAsValue(terminal);
        var entry = ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(terminalValue));
        var plan = ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache())!;
        plan.EnvironmentCount.Should().Be(2);
        plan.TerminalExpression.Should().Be(terminal);
        plan.TerminalEnvironmentRecipe.Should().BeSameAs(TemplateValueRecipe.EnvironmentSlot(1));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void An_invariant_constructor_can_be_the_terminal_computation(bool identity)
    {
        var terminal =
            identity
            ?
            Expression.EnvironmentInstance
            :
            Expression.LitralInst(PineValue.EmptyBlob);

        var terminalValue = ExpressionEncoding.EncodeExpressionAsValue(terminal);
        var entry = ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(terminalValue));
        var plan = ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache())!;
        plan.Should().NotBeNull();
        plan.EnvironmentCount.Should().Be(2);
        plan.TerminalExpression.Should().Be(terminal);
        plan.IntermediateRecipes.Count.Should().Be(1);
        plan.TerminalEnvironmentRecipe.Should().BeSameAs(TemplateValueRecipe.EnvironmentSlot(1));
    }

    [Fact]
    public void Recognition_budget_exhaustion_falls_back()
    {
        var expressionItems = new Expression[20_000];
        Array.Fill(expressionItems, Expression.EnvironmentInstance);
        var entry = ExpressionEncoding.EncodeExpressionAsValue(Expression.ListInst(expressionItems));
        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache()).Should().BeNull();
    }

    [Fact]
    public void Deep_unfamiliar_values_fall_back_without_recursive_parsing()
    {
        PineValue entry = PineValue.EmptyList;

        for (var depth = 0; depth < 1_000; ++depth)
            entry = PineValue.List([entry]);

        ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache()).Should().BeNull();
    }

    [Fact]
    public void Slots_are_shared_process_wide_and_return_original_in_process_values()
    {
        var slot = TemplateValueRecipe.EnvironmentSlot(3);
        slot.Should().BeSameAs(TemplateValueRecipe.EnvironmentSlot(3));
        slot.Should().NotBeSameAs(TemplateValueRecipe.EnvironmentSlot(2));
        var lazy = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(27)]);

        slot.Instantiate(
            [
            PineValueInProcess.EmptyList, PineValueInProcess.EmptyList,
            PineValueInProcess.EmptyList, lazy
            ]).Should().BeSameAs(lazy);

        lazy.EvaluatedOrNull.Should().BeNull();
    }

    [Fact]
    public void Materializing_a_saturated_plan_is_not_an_intermediate_value()
    {
        var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(Expression.EnvironmentInstance, 1)!;
        var plan = ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache())!;
        var materialize = () => plan.Materialize([PineValueInProcess.EmptyList], new DirectInterpreterCounters());
        materialize.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void Constant_lists_fold_without_runtime_list_construction()
    {
        var recipe =
            TemplateValueRecipe.BuildList(
                [new TemplateValueRecipe.Constant(PineValue.EmptyBlob), TemplateValueRecipe.EmptyList]);

        recipe.Should().BeOfType<TemplateValueRecipe.Constant>();
        recipe.Instantiate([]).EvaluatedOrNull.Should().Be(PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList]));
    }

    [Fact]
    public void Recipe_list_projection_returns_shared_slot_without_building_enclosing_lists()
    {
        var first = TemplateValueRecipe.EnvironmentSlot(0);
        var second = TemplateValueRecipe.EnvironmentSlot(1);

        var recipe =
            TemplateValueRecipe.BuildList(
                [
                new TemplateValueRecipe.Constant(PineValue.EmptyBlob),
                first,
                TemplateValueRecipe.BuildList([second, first])
                ]);

        recipe.Project([2, 1]).Should().BeSameAs(first);
        recipe.Project([]).Should().BeSameAs(recipe);

        recipe.Instantiate([PineValueInProcess.CreateInteger(11), PineValueInProcess.CreateInteger(13)])
            .Evaluate().Should().Be(
            PineValue.List(
                [
                PineValue.EmptyBlob,
                IntegerEncoding.EncodeSignedInteger(11),
                PineValue.List([IntegerEncoding.EncodeSignedInteger(13), IntegerEncoding.EncodeSignedInteger(11)])
                ]));
    }

    [Fact]
    public void Instantiation_preserves_shared_list_subrecipes()
    {
        var shared =
            TemplateValueRecipe.BuildList(
                [TemplateValueRecipe.EnvironmentSlot(0), TemplateValueRecipe.EnvironmentSlot(1)]);

        var recipe = TemplateValueRecipe.BuildList([shared, shared]);
        var value = recipe.Instantiate([PineValueInProcess.CreateInteger(11), PineValueInProcess.CreateInteger(13)]);
        var items = value.ListItemsOrNull()!;
        items[0].Should().BeSameAs(items[1]);
        var concrete = (PineValue.ListValue)value.Evaluate();
        concrete.Items.Span[0].Should().BeSameAs(concrete.Items.Span[1]);
    }

    [Theory]
    [InlineData(1)]
    [InlineData(23)]
    public void Missing_recipe_fields_follow_Pine_path_semantics(int index)
    {
        var recipe = TemplateValueRecipe.BuildList([TemplateValueRecipe.EnvironmentSlot(0)]);
        recipe.Project([index]).Instantiate([]).Should().BeSameAs(PineValueInProcess.EmptyList);
    }

    [Fact]
    public void Constant_blob_and_nested_missing_projections_return_empty_list()
    {
        new TemplateValueRecipe.Constant(PineValue.Blob([11, 13]))
            .Project([0]).TryGetConstant().Should().Be(PineValue.EmptyList);

        new TemplateValueRecipe.Constant(PineValue.List([PineValue.List([PineValue.EmptyBlob])]))
            .Project([0, 0, 0]).TryGetConstant().Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Negative_projection_indices_clamp_to_zero_for_lists_constants_and_slots()
    {
        var slot = TemplateValueRecipe.EnvironmentSlot(0);
        TemplateValueRecipe.BuildList([slot]).Project([-1]).Should().BeSameAs(slot);

        new TemplateValueRecipe.Constant(PineValue.List([PineValue.EmptyBlob]))
            .Project([int.MinValue]).TryGetConstant().Should().Be(PineValue.EmptyBlob);

        var selected = PineValueInProcess.CreateInteger(29);

        slot.Project([-37]).Instantiate([PineValueInProcess.CreateList([selected])])
            .Should().BeSameAs(selected);
    }

    [Fact]
    public void Slot_projection_composes_paths_and_preserves_unevaluated_siblings()
    {
        var sibling = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(17)]);
        var selected = PineValueInProcess.CreateInteger(29);

        var environment =
            PineValueInProcess.CreateList(
                [PineValueInProcess.CreateList([sibling, selected])]);

        var recipe = TemplateValueRecipe.EnvironmentSlot(0).Project([0]).Project([1]);
        recipe.Instantiate([environment]).Should().BeSameAs(selected);
        sibling.EvaluatedOrNull.Should().BeNull();
        environment.EvaluatedOrNull.Should().BeNull();

        TemplateValueRecipe.EnvironmentSlot(0).Project([0, 2]).Instantiate([environment])
            .Should().BeSameAs(PineValueInProcess.EmptyList);
    }

    private static Expression Tag(string name) => Expression.LitralInst(StringEncoding.ValueFromString(name));

    private static PineValue Apply(DirectInterpreter interpreter, PineValue code, PineValue environment) =>
        interpreter.EvaluateExpressionDefault(
            new Expression.Eval(Expression.LitralInst(code), Expression.LitralInst(environment)),
            PineValue.EmptyList);
}
