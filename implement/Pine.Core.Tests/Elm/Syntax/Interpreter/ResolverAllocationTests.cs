using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections;
using System.Collections.Generic;
using Xunit;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class ResolverAllocationTests
{
    private sealed class IndexOnlyList<T>(params T[] items) : IReadOnlyList<T>
    {
        public int Count => items.Length;

        public T this[int index] => items[index];

        public IEnumerator<T> GetEnumerator() => throw new InvalidOperationException("Must use indexed traversal.");

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }

    [Fact]
    public void Resolver_combination_preserves_order_without_enumerating_or_allocating()
    {
        var calls = new List<int>();
        var resolved = new Engine.ApplicationResolution.Resolved(PineValueInProcess.EmptyList);

        var resolver =
            Engine.CombineResolvers(
                new IndexOnlyList<Func<Engine.Application, Engine.ApplicationResolution?>>(
                    _ =>
                    {
                        calls.Add(1);
                        return null;
                    },
                    _ =>
                    {
                        calls.Add(2);
                        return resolved;
                    },
                    _ => throw new InvalidOperationException("Must stop at first successful resolver.")));

        var application =
            new Engine.Application(
                DeclQualifiedName.Create([], "value"),
                [],
                new Engine.ApplicationContext(
                    DeclQualifiedName.Create([], "root"),
                    new Dictionary<string, PineValueInProcess>()));

        resolver(application).Should().BeSameAs(resolved);
        calls.Should().Equal(1, 2);

        var noAllocation =
            Engine.CombineResolvers(
                new IndexOnlyList<Func<Engine.Application, Engine.ApplicationResolution?>>(_ => null, _ => resolved));

        for (var index = 0; index < 100; index++)
            noAllocation(application);

        var before = GC.GetAllocatedBytesForCurrentThread();

        for (var index = 0; index < 10_000; index++)
            noAllocation(application);

        (GC.GetAllocatedBytesForCurrentThread() - before).Should().BeLessThan(1024);
    }

    [Fact]
    public void Namespace_creation_and_hashing_use_indexed_traversal_and_copy_the_input()
    {
        var namespaces = new[] { "One", "Two" };
        var name = DeclQualifiedName.Create(new IndexOnlyList<string>(namespaces), "value");
        namespaces[0] = "Changed";
        name.FullName.Should().Be("One.Two.value");
        name.Namespaces.Should().Equal("One", "Two");
        name.GetHashCode().Should().Be(DeclQualifiedName.FromString("One.Two.value").GetHashCode());
        var indexedHash = name with { Namespaces = new IndexOnlyList<string>("One", "Two") };
        indexedHash.GetHashCode().Should().Be(name.GetHashCode());
    }

    [Fact]
    public void Qualified_declaration_lookup_does_not_enumerate_namespaces()
    {
        var name = DeclQualifiedName.Create(["Test"], "value") with { Namespaces = new IndexOnlyList<string>("Test") };

        var function =
            new Engine.PreparedFunctionImplementation(
                "value",
                [],
                new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.EmptyList));

        var declarations =
            new Dictionary<DeclQualifiedName, Engine.PreparedDeclaration>
            {
                [name] =
                new Engine.PreparedDeclaration.FunctionDeclaration(new Engine.PreparedFunctionStruct(null, function))
            };

        var resolver = Engine.BuildResolvers(declarations, customFunctionResolvers: null);

        var application =
            new Engine.Application(
                name,
                [],
                new Engine.ApplicationContext(
                    name,
                    new Dictionary<string, PineValueInProcess>()));

        resolver(application).Should().BeOfType<Engine.ApplicationResolution.ContinueWithFunction>();
    }
}
