using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;
using BindingLayout = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter.PreparedParameterBindingLayout;
using Environment = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter.LocalBindingEnvironment;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class PreparedParameterBindingLayoutTests
{
    [Fact]
    public void Prepared_parameter_slots_preserve_lookup_shadowing_and_dictionary_views()
    {
        var parent =
            Environment.FromBindings(
                ImmutableDictionary<string, PineValueInProcess>.Empty
                .Add("outer", Integer(1))
                .Add("shadowed", Integer(2)));

        var layout =
            BindingLayout.ForPatterns(
                [new Pattern.VarPattern("shadowed"), new Pattern.VarPattern("inner")]);

        var child = layout.BindArguments(parent, [Integer(20), Integer(3)]);

        ElmInterpreter.ToElm(child["outer"]).Should().Be(ElmValue.Integer(1));
        ElmInterpreter.ToElm(child["shadowed"]).Should().Be(ElmValue.Integer(20));
        ElmInterpreter.ToElm(child["inner"]).Should().Be(ElmValue.Integer(3));
        child.Count.Should().Be(3);
        child.Keys.Should().Equal("shadowed", "inner", "outer");

        child.Values.Select(ElmInterpreter.ToElm).Should().Equal(
            ElmValue.Integer(20),
            ElmValue.Integer(3),
            ElmValue.Integer(1));

        child.Select(binding => binding.Key).Should().OnlyHaveUniqueItems();

        child.Cast<KeyValuePair<string, PineValueInProcess>>()
            .Select(binding => binding.Key).Should().Equal(child.Keys);

        child.ContainsKey("Shadowed").Should().BeFalse();
        child.TryGetValue("missing", out var missing).Should().BeFalse();
        missing.Should().BeNull();
        Action lookupMissing = () => _ = child["missing"];

        lookupMissing.Should().Throw<KeyNotFoundException>()
            .WithMessage("The given key 'missing' was not present in the environment.");

        child.Snapshot().Should().BeSameAs(child);
        Environment.FromBindings(child).Should().BeSameAs(child);

        var grandchild =
            BindingLayout.ForPatterns([new Pattern.VarPattern("shadowed"), new Pattern.VarPattern("leaf")])
            .BindArguments(child, [Integer(200), Integer(4)]);

        ElmInterpreter.ToElm(grandchild["shadowed"]).Should().Be(ElmValue.Integer(200));
        ElmInterpreter.ToElm(child["shadowed"]).Should().Be(ElmValue.Integer(20));
        grandchild.Keys.Should().Equal("shadowed", "leaf", "inner", "outer");
        grandchild.Count.Should().Be(4);
    }

    [Fact]
    public void Prepared_parameter_slots_survive_snapshotting_a_mutable_parent()
    {
        var mutableBindings =
            new Dictionary<string, PineValueInProcess>
            {
                ["outer"] = Integer(1),
                ["shadowed"] = Integer(2),
            };

        var parent = Environment.Empty.CreateMutableChild(mutableBindings);
        var layout = BindingLayout.ForPatterns([new Pattern.VarPattern("shadowed")]);
        var child = layout.BindArguments(parent, [Integer(20)]);
        var snapshot = child.Snapshot();

        mutableBindings["outer"] = Integer(10);
        mutableBindings["shadowed"] = Integer(200);
        mutableBindings["later"] = Integer(30);

        ElmInterpreter.ToElm(child["outer"]).Should().Be(ElmValue.Integer(10));
        ElmInterpreter.ToElm(child["shadowed"]).Should().Be(ElmValue.Integer(20));
        ElmInterpreter.ToElm(snapshot["outer"]).Should().Be(ElmValue.Integer(1));
        ElmInterpreter.ToElm(snapshot["shadowed"]).Should().Be(ElmValue.Integer(20));
        snapshot.Count.Should().Be(2);
        snapshot.ContainsKey("later").Should().BeFalse();
        snapshot.Snapshot().Should().BeSameAs(snapshot);
    }

    [Fact]
    public void Mutable_children_of_prepared_parameter_slots_are_snapshotted_independently()
    {
        var layout = BindingLayout.ForPatterns([new Pattern.VarPattern("parameter")]);
        var parent = layout.BindArguments(Environment.Empty, [Integer(1)]);

        var mutableBindings =
            new Dictionary<string, PineValueInProcess>
            {
                ["parameter"] = Integer(2),
                ["local"] = Integer(3),
            };

        var child = parent.CreateMutableChild(mutableBindings);
        var snapshot = child.Snapshot();

        mutableBindings["parameter"] = Integer(20);
        mutableBindings["local"] = Integer(30);
        mutableBindings["later"] = Integer(40);

        ElmInterpreter.ToElm(parent["parameter"]).Should().Be(ElmValue.Integer(1));
        ElmInterpreter.ToElm(child["parameter"]).Should().Be(ElmValue.Integer(20));
        ElmInterpreter.ToElm(snapshot["parameter"]).Should().Be(ElmValue.Integer(2));
        ElmInterpreter.ToElm(snapshot["local"]).Should().Be(ElmValue.Integer(3));
        snapshot.ContainsKey("later").Should().BeFalse();
        snapshot.Keys.Should().BeEquivalentTo("parameter", "local");
        snapshot.Count.Should().Be(2);
    }

    [Fact]
    public void Repeated_parameter_names_overwrite_in_pattern_and_parameter_order()
    {
        Pattern[] patterns =
            [
                new Pattern.AsPattern(
                    new Pattern.TuplePattern(
                        [
                            new Pattern.VarPattern("same"),
                            new Pattern.VarPattern("same"),
                        ]),
                    "same"),
                new Pattern.VarPattern("other"),
                new Pattern.VarPattern("same"),
            ];

        var layout = BindingLayout.ForPatterns(patterns);

        PineValueInProcess[] values =
            [
                Value(ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)])),
                Integer(3),
                Integer(4),
            ];

        var first = layout.BindArguments(Environment.Empty, values);
        values[2] = Integer(40);
        var second = layout.BindArguments(Environment.Empty, values);

        layout.BindingCount.Should().Be(2);
        first.Keys.Should().Equal("same", "other");
        ElmInterpreter.ToElm(first["same"]).Should().Be(ElmValue.Integer(4));
        ElmInterpreter.ToElm(first["other"]).Should().Be(ElmValue.Integer(3));
        ElmInterpreter.ToElm(second["same"]).Should().Be(ElmValue.Integer(40));

        var aliasBindings =
            BindingLayout.ForPatterns([patterns[0]]).BindArguments(Environment.Empty, [values[0]]);

        aliasBindings.Keys.Should().Equal("same");
        aliasBindings["same"].Should().BeSameAs(values[0]);
    }

    [Fact]
    public void Prepared_parameter_slots_bind_nested_tag_record_list_cons_tuple_and_alias_patterns()
    {
        var layout =
            BindingLayout.ForPatterns(
                [
                    new Pattern.AsPattern(
                        new Pattern.TuplePattern(
                            [
                                new Pattern.NamedPattern(
                                    new QualifiedNameRef([], "Pair"),
                                    [
                                        new Pattern.VarPattern("first"),
                                        Pattern.RecordPattern.Create(["field"]),
                                    ]),
                                new Pattern.UnConsPattern(
                                    new Pattern.VarPattern("head"),
                                    new Pattern.ListPattern([new Pattern.VarPattern("last")])),
                            ]),
                        "whole"),
                ]);

        var argument =
            Value(
                ElmValue.ListInstance(
                    [
                        ElmValue.TagInstance(
                            "Pair",
                            [
                                ElmValue.Integer(1),
                                ElmValue.RecordInstance([("field", ElmValue.Integer(2))]),
                            ]),
                        ElmValue.ListInstance([ElmValue.Integer(3), ElmValue.Integer(4)]),
                    ]));

        var bindings = layout.BindArguments(Environment.Empty, [argument]);

        bindings.Count.Should().Be(5);
        ElmInterpreter.ToElm(bindings["first"]).Should().Be(ElmValue.Integer(1));
        ElmInterpreter.ToElm(bindings["field"]).Should().Be(ElmValue.Integer(2));
        ElmInterpreter.ToElm(bindings["head"]).Should().Be(ElmValue.Integer(3));
        ElmInterpreter.ToElm(bindings["last"]).Should().Be(ElmValue.Integer(4));
        bindings["whole"].Should().BeSameAs(argument);
    }

    [Fact]
    public void Nonbinding_parameter_patterns_keep_the_existing_binding_behavior()
    {
        var layout =
            BindingLayout.ForPatterns(
                [
                    new Pattern.AllPattern(),
                    new Pattern.UnitPattern(),
                    new Pattern.CharPattern('a'),
                    new Pattern.StringPattern("text"),
                    new Pattern.IntPattern(1),
                    new Pattern.FloatPattern(1.5),
                    new Pattern.NamedPattern(
                        new QualifiedNameRef([], "True"),
                        [new Pattern.VarPattern("ignoredTrue")]),
                    new Pattern.NamedPattern(
                        new QualifiedNameRef([], "False"),
                        [new Pattern.VarPattern("ignoredFalse")]),
                    new Pattern.VarPattern("after"),
                ]);

        var bindings =
            layout.BindArguments(
                Environment.Empty,
                [.. Enumerable.Repeat(Integer(9), 9)]);

        bindings.Keys.Should().Equal("after");
        ElmInterpreter.ToElm(bindings["after"]).Should().Be(ElmValue.Integer(9));
        BindingLayout.ForPatterns([]).BindArguments(bindings, []).Should().BeSameAs(bindings);

        BindingLayout.ForPatterns([new Pattern.AllPattern()])
            .BindArguments(bindings, [Integer(1)]).Should().BeSameAs(bindings);
    }

    [Fact]
    public void Prepared_parameter_slots_preserve_record_binding_error_messages()
    {
        var layout = BindingLayout.ForPatterns([Pattern.RecordPattern.Create(["present", "missing"])]);
        var argument = Value(ElmValue.RecordInstance([("present", ElmValue.Integer(1))]));

        Action bind = () => layout.BindArguments(Environment.Empty, [argument]);

        bind.Should().Throw<Exception>()
            .WithMessage("Record does not contain a field named 'missing'.");
    }

    [Fact]
    public void Parameter_patterns_without_slots_still_validate_named_tags()
    {
        var layout =
            BindingLayout.ForPatterns([new Pattern.NamedPattern(new QualifiedNameRef([], "Nothing"), [])]);

        var argument = Value(ElmValue.TagInstance("Just", [ElmValue.Integer(1)]));

        Action bind = () => layout.BindArguments(Environment.Empty, [argument]);

        var exception = bind.Should().Throw<InvalidOperationException>().Which;

        exception.Message.Should().Be(
            "Cannot bind named pattern 'Nothing' to value of type " + argument.GetType().FullName + ".");
    }

    [Fact]
    public void Preparing_binding_layouts_does_not_change_prepared_equality_hashing_or_json()
    {
        Pattern[] patterns = [new Pattern.VarPattern("value")];

        var lambda =
            new ElmInterpreter.PreparedExpression.LambdaExpression(
                patterns,
                ElmInterpreter.PreparedExpression.Identifier.Create([], "value"));

        var function = new ElmInterpreter.PreparedFunctionImplementation("identity", patterns, lambda);
        var equalFunction = function with { };
        var equalLambda = lambda with { };
        var originalHash = function.GetHashCode();

        var originalJson =
            JsonSerializer.Serialize(function, ElmSyntaxInterpreterPreparedJson.JsonSerializerOptions);

        var layout = BindingLayout.ForPatterns(patterns);
        layout.BindArguments(Environment.Empty, [Integer(1)]);
        layout.BindArguments(Environment.Empty, [Integer(2)]);

        BindingLayout.ForPatterns(patterns).Should().BeSameAs(layout);
        function.Should().Be(equalFunction);
        lambda.Should().Be(equalLambda);
        function.GetHashCode().Should().Be(originalHash);

        JsonSerializer.Serialize(function, ElmSyntaxInterpreterPreparedJson.JsonSerializerOptions)
            .Should().Be(originalJson);
    }

    [Fact]
    public void Prepared_parameter_binding_allocates_only_a_value_array_and_environment_per_call()
    {
        Pattern[] patterns =
            [.. Enumerable.Range(0, 8).Select(index => (Pattern)new Pattern.VarPattern("arg" + index))];

        var arguments = Enumerable.Repeat(Integer(1), patterns.Length).ToArray();
        var layout = BindingLayout.ForPatterns(patterns);

        for (var index = 0; index < 100; ++index)
            layout.BindArguments(Environment.Empty, arguments);

        var before = GC.GetAllocatedBytesForCurrentThread();

        for (var index = 0; index < 1_000; ++index)
            GC.KeepAlive(BindingLayout.ForPatterns(patterns).BindArguments(Environment.Empty, arguments));

        (GC.GetAllocatedBytesForCurrentThread() - before).Should().BeLessThan(200_000);
    }

    [Theory]
    [InlineData("describe (Pair 1 2) { x = 3 } 4")]
    [InlineData("let partial = describe (Pair 1 2) in partial { x = 3 } 4")]
    [InlineData("(\\((Pair left right) as whole) { x } tail -> ( whole, left, right, x, tail )) (Pair 1 2) { x = 3 } 4")]
    public void Prepared_parameter_patterns_work_with_declared_lambda_partial_and_over_application(
        string expression)
    {
        const string Module =
            """
            module Test exposing (..)


            type Pair
                = Pair Int Int


            describe ((Pair left right) as whole) { x } =
                \tail -> ( whole, left, right, x, tail )
            """;

        var result = InterpreterTestHelper.EvaluateInModuleOrCrash(expression, Module);

        ElmInterpreter.ToElm(result).Should().Be(
            ElmValue.ListInstance(
                [
                    ElmValue.TagInstance("Pair", [ElmValue.Integer(1), ElmValue.Integer(2)]),
                    ElmValue.Integer(1),
                    ElmValue.Integer(2),
                    ElmValue.Integer(3),
                    ElmValue.Integer(4),
                ]));
    }

    private static PineValueInProcess Integer(int value) =>
        Value(ElmValue.Integer(value));

    private static PineValueInProcess Value(ElmValue value) =>
        PineValueInProcess.Create(ElmValueEncoding.ElmValueAsPineValue(value));
}
