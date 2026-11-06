using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;
using Xunit;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class ContinuationStorageTests
{
    [Fact]
    public void Every_compact_continuation_round_trips_its_tag_and_typed_payload()
    {
        var type = typeof(Engine).GetNestedType("Kont", BindingFlags.NonPublic)!;

        var conversions =
            type.GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Where(method => method.Name == "op_Implicit").ToArray();

        var tags = Enum.GetNames(type.GetNestedType("Kind")!).Where(name => name != "None");
        conversions.Select(method => method.GetParameters()[0].ParameterType.Name).Should().BeEquivalentTo(tags);

        foreach (var conversion in conversions)
        {
            var variant = conversion.GetParameters()[0].ParameterType;

            var constructor =
                variant.GetConstructors().OrderByDescending(item => item.GetParameters().Length).FirstOrDefault();

            var original =
                constructor is null
                ?
                Activator.CreateInstance(variant)!
                :
                constructor.Invoke(
                    [
                    .. constructor.GetParameters().Select(parameter => Payload(parameter.ParameterType, parameter.Name!))
                    ]);

            var packed = conversion.Invoke(null, [original])!;
            type.GetProperty("Tag")!.GetValue(packed)!.ToString().Should().Be(variant.Name);

            if (variant.Name == "Negate")
                continue;

            var unpacked = type.GetProperty("As" + variant.Name)!.GetValue(packed)!;

            foreach (var property in variant.GetProperties())
            {
                var expected = property.GetValue(original);
                var actual = property.GetValue(unpacked);

                if (property.PropertyType.IsValueType)
                    actual.Should().Be(expected);

                else
                    actual.Should().BeSameAs(expected);
            }
        }
    }

    private static object Payload(Type type, string name)
    {
        if (type == typeof(int))
            return 17;

        if (type == typeof(string))
            return name;

        if (type.IsArray)
            return Array.CreateInstance(type.GetElementType()!, 0);

        if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(IReadOnlyList<>))
            return Array.CreateInstance(type.GetGenericArguments()[0], 0);

        if (type == typeof(Engine.PreparedExpression))
            return new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.CreateInteger(1));

        if (type == typeof(Engine.PreparedExpression.Identifier))
            return new Engine.PreparedExpression.Identifier(DeclQualifiedName.FromString(name));

        if (type == typeof(Engine.ApplicationContext))
        {
            return
                new Engine.ApplicationContext(
                    DeclQualifiedName.FromString(name),
                    new Dictionary<string, PineValueInProcess>());
        }

        if (type == typeof(Pattern))
            return new Pattern.VarPattern(name);

        if (type == typeof(DeclQualifiedName))
            return DeclQualifiedName.FromString(name);

        if (type == typeof(PineValue))
            return PineValue.Blob([1]);

        if (type == typeof(PineValueInProcess))
            return PineValueInProcess.CreateInteger(1);

        if (type == typeof(Engine.PreparedCaseDispatch))
            return new Engine.PreparedCaseDispatch([]);

        if (type == typeof(Action<PineValueInProcess>))
            return new Action<PineValueInProcess>(_ => GC.KeepAlive(name));

        if (type == typeof(Dictionary<string, PineValueInProcess>))
            return new Dictionary<string, PineValueInProcess>();

        if (type.Name == "Entry" && type.DeclaringType?.Name == "NullaryValueCache")
            return Activator.CreateInstance(type)!;

        throw new NotImplementedException("Payload does not handle " + type);
    }

    private static Engine.Prepared Program(DeclQualifiedName name, Engine.PreparedExpression body) =>
        new(
            ImmutableDictionary<DeclQualifiedName, Engine.PreparedDeclaration>.Empty.Add(
                name,
                new Engine.PreparedDeclaration.FunctionDeclaration(
                    new(
                        null,
                        new(name.DeclName, [], body)))));

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Synthetic_names_are_reused_without_conflating_containing_declarations(bool let)
    {
        Engine.PreparedExpression failure =
            new Engine.PreparedExpression.Negation(new Engine.PreparedExpression.ListExpr([]));

        Engine.PreparedExpression argument =
            new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.CreateInteger(1));

        Engine.PreparedExpression body =
            let
            ?
            new Engine.PreparedExpression.LetExpression(
                [
                new Engine.PreparedLetDeclaration.LetFunction(
                    new(null, new("local", [new Pattern.VarPattern("x")], failure)))
                ],
                new Engine.PreparedExpression.Application(
                    new Engine.PreparedExpression.Identifier(DeclQualifiedName.FromString("local")),
                    [
                    argument
                    ]))
            :
            new Engine.PreparedExpression.Application(
                new Engine.PreparedExpression.LambdaExpression([new Pattern.VarPattern("x")], failure),
                [
                argument
                ]);

        var firstOwner = DeclQualifiedName.FromString("One.root");
        var secondOwner = DeclQualifiedName.FromString("Two.root");
        var firstResolver = Engine.BuildResolvers(Program(firstOwner, body).Declarations);
        var secondResolver = Engine.BuildResolvers(Program(secondOwner, body).Declarations);

        DeclQualifiedName Run(DeclQualifiedName owner, Func<Engine.Application, Engine.ApplicationResolution> resolver) =>
            Engine.Interpret(owner, [], resolver, null).IsErrOrNull()!.CallStack[0].FunctionName;

        var expected = Run(firstOwner, firstResolver);
        expected.FullName.Should().Be(let ? "One.root.<let>.local" : "One.root.<lambda>");
        Parallel.For(0, 8, _ => Run(firstOwner, firstResolver).Should().BeSameAs(expected));
        var other = Run(secondOwner, secondResolver);
        other.FullName.Should().Be(let ? "Two.root.<let>.local" : "Two.root.<lambda>");
        other.Should().NotBeSameAs(expected);
    }

    [Fact]
    public void List_progress_and_observers_do_not_allocate_one_continuation_object_per_element()
    {
        const int count = 1000;
        var literal = new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.CreateInteger(7));
        var owner = DeclQualifiedName.FromString("Test.root");

        var resolver =
            Engine.BuildResolvers(
                Program(
                    owner,
                    new Engine.PreparedExpression.ListExpr(
                        [.. Enumerable.Repeat<Engine.PreparedExpression>(literal, count)])).Declarations);

        var observations = 0;

        var config =
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>(
                    ReferenceEqualityComparer.Instance)
                {
                    [literal] = _ => observations++
                }
            };

        for (var index = 0; index < 3; index++)
            Engine.Interpret(owner, [], resolver, null, config);

        observations = 0;
        var before = GC.GetAllocatedBytesForCurrentThread();
        var result = Engine.Interpret(owner, [], resolver, null, config);
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        result.IsOkOrNull()!.GetLength().Should().Be(count);
        observations.Should().Be(count);
        allocated.Should().BeLessThan(24_000);
    }
}
