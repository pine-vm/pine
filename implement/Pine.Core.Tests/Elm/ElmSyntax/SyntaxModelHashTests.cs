using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax;

using Expression = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;
using File = Pine.Core.Elm.ElmSyntax.SyntaxModel.File;
using Module = Pine.Core.Elm.ElmSyntax.SyntaxModel.Module;
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

public class SyntaxModelHashTests
{
    private static readonly Location Position = new(1, 1);

    private static readonly Range Span = new(Position, new Location(1, 8));

    [Fact]
    public void SyntaxModel_hash_history_does_not_affect_equality_and_clones_reset_cache()
    {
        foreach (var original in StructuralSamples())
        {
            var coldEqual = Clone(original);
            original.Equals(coldEqual).Should().BeTrue(original.GetType().FullName);

            var hash = original.GetHashCode();
            CacheField(original.GetType()).GetValue(original).Should().NotBe(0L);
            original.Equals(coldEqual).Should().BeTrue(original.GetType().FullName);
            coldEqual.Equals(original).Should().BeTrue(original.GetType().FullName);

            var clone = Clone(original);
            CacheField(original.GetType()).GetValue(clone).Should().Be(0L);
            original.Equals(clone).Should().BeTrue(original.GetType().FullName);
            clone.GetHashCode().Should().Be(hash);
            coldEqual.GetHashCode().Should().Be(hash);
            original.Equals(coldEqual).Should().BeTrue(original.GetType().FullName);
        }
    }

    [Fact]
    public void SyntaxModel_synthesized_structural_hashes_preserve_compiler_formula()
    {
        foreach (var original in SynthesizedHashSamples())
            VerifySynthesizedHash(original);
    }

    [Fact]
    public void SyntaxModel_primitive_variants_keep_synthesized_hashes_without_memoization()
    {
        object[] primitives =
            [
                new Expression.UnitExpr(),
                new Expression.StringLiteral("value", "source"),
                new Expression.CharLiteral(42),
                new Expression.IntegerLiteral("123"),
                new Expression.FloatLiteral("1.23"),
                new Expression.PrefixOperator("+"),
                new Expression.RecordAccessFunction("field"),
                new Expression.GLSLExpression("shader"),
                new Pattern.AllPattern(),
                new Pattern.VarPattern("a"),
                new Pattern.UnitPattern(),
                new Pattern.CharPattern(42),
                new Pattern.StringPattern("value"),
                new Pattern.IntPattern(123),
                new Pattern.HexPattern(123),
                new Pattern.FloatPattern(1.23f),
                new TypeAnnotation.GenericType("a"),
                new TypeAnnotation.Unit(),
                new Exposing.All(Span),
                new SeparatedSyntaxList<Node<Expression>>.Empty()
            ];

        foreach (var primitive in primitives)
        {
            VerifySynthesizedHash(primitive);
            CacheField(primitive.GetType()).GetValue(primitive).Should().Be(0L);
        }
    }

    [Fact]
    public void SyntaxModel_custom_hashes_preserve_structural_algorithm()
    {
        foreach (var original in CustomHashSamples())
        {
            var expected = new HashCode();
            var constructor = original.GetType().GetConstructors().Single();

            foreach (var parameter in constructor.GetParameters())
            {
                var value = original.GetType().GetProperty(parameter.Name!)!.GetValue(original);

                if (parameter.Name == nameof(Expression.MultilineStringLiteral.SourceLines) && value is null)
                    continue;

                if (value is ValueTuple<IReadOnlyList<string>, string> tuple)
                {
                    foreach (var part in tuple.Item1)
                        expected.Add(part);

                    expected.Add(tuple.Item2);
                }
                else if (value is IEnumerable items &&
                    value.GetType().GetInterfaces().Any(
                        type => type.IsGenericType && type.GetGenericTypeDefinition() == typeof(IReadOnlyList<>)))
                {
                    foreach (var item in items)
                        expected.Add(item);
                }
                else
                {
                    expected.Add(value);
                }
            }

            if (original is File file)
            {
                foreach (var error in file.AdditionalParseErrors)
                    expected.Add(error);
            }

            original.GetHashCode().Should().Be(expected.ToHashCode(), original.GetType().FullName);
        }
    }

    [Fact]
    public void SyntaxModel_repeated_hashes_do_not_enumerate_structural_lists()
    {
        var moduleNames = new CountingList<string>(["Example", "Module"]);
        var nodeNames = new CountingList<string>(["Example", "Module"]);
        var tupleNames = new CountingList<string>(["Example", "Module"]);
        var lines = new CountingList<string>(["one", "two"]);
        var rest = new CountingList<(Location, Node<Expression>)>([(Position, Expr())]);
        var arguments = new CountingList<Node<Expression>>([Expr()]);
        var patternArguments = new CountingList<Node<Pattern>>([Pat()]);
        var namedArguments = new CountingList<Node<Pattern>>([Pat()]);
        var lambdaArguments = new CountingList<Node<Pattern>>([Pat()]);
        var typeArguments = new CountingList<Node<TypeAnnotation>>([TypeNode()]);
        var constructorArguments = new CountingList<Node<TypeAnnotation>>([TypeNode()]);
        var aliasGenerics = new CountingList<Node<string>>([Node("a")]);
        var choiceGenerics = new CountingList<Node<string>>([Node("a")]);
        var declarations = new CountingList<Node<Expression.LetDeclaration>>([]);
        var cases = new CountingList<Case>([new(Pat(), Position, Expr())]);
        var imports = new CountingList<Node<Import>>([]);
        var fileDeclarations = new CountingList<Node<Declaration>>([]);
        var comments = new CountingList<Node<string>>([Node("-- comment")]);
        var incomplete = new CountingList<Node<IncompleteDeclaration>>([]);
        var errors = new CountingList<ElmSyntaxParseError>([]);

        var file =
            SampleFile() with
            {
                Imports = imports,
                Declarations = fileDeclarations,
                Comments = comments,
                IncompleteDeclarations = incomplete,
                AdditionalParseErrors = errors
            };

        (object Model, Func<int> Enumerations)[] samples =
            [
                (new QualifiedNameRef(moduleNames, "value"), () => moduleNames.Enumerations),
                (Node<IReadOnlyList<string>>(nodeNames), () => nodeNames.Enumerations),
                (Node<(IReadOnlyList<string>, string)>((tupleNames, "value")), () => tupleNames.Enumerations),
                (new Expression.Identifier(moduleNames, "value"), () => moduleNames.Enumerations),
                (new Expression.MultilineStringLiteral("one\ntwo", lines), () => lines.Enumerations),
                (new SeparatedSyntaxList<Node<Expression>>.NonEmpty(Expr(), rest), () => rest.Enumerations),
                (new Expression.Application(Expr(), arguments), () => arguments.Enumerations),
                (new FunctionImplementation(Node("value"), patternArguments, Position, Expr()), () => patternArguments.Enumerations),
                (new Pattern.NamedPattern(new QualifiedNameRef([], "Just"), namedArguments), () => namedArguments.Enumerations),
                (new LambdaStruct(Position, lambdaArguments, Position, Expr()), () => lambdaArguments.Enumerations),
                (new TypeAnnotation.Typed(Node<(IReadOnlyList<string>, string)>(([], "Int")), typeArguments), () => typeArguments.Enumerations),
                (new ValueConstructor(Node("Just"), constructorArguments), () => constructorArguments.Enumerations),
                (new TypeAlias(null, Position, Position, Node("Alias"), aliasGenerics, Position, TypeNode()), () => aliasGenerics.Enumerations),
                (new ChoiceTypeStruct(null, Position, Node("Choice"), choiceGenerics, Position, new SeparatedSyntaxList<Node<ValueConstructor>>.Empty()), () => choiceGenerics.Enumerations),
                (new Expression.LetBlock(Position, declarations, Position, Expr()), () => declarations.Enumerations),
                (new CaseBlock(Position, Expr(), Position, cases), () => cases.Enumerations),
                (file, () => imports.Enumerations + fileDeclarations.Enumerations + comments.Enumerations + incomplete.Enumerations + errors.Enumerations)
            ];

        foreach (var (model, enumerations) in samples)
        {
            var before = enumerations();
            var hash = model.GetHashCode();
            (enumerations() > before).Should().BeTrue(model.GetType().FullName);
            var afterFirst = enumerations();

            for (var repeat = 0; repeat < 10; ++repeat)
                model.GetHashCode().Should().Be(hash);

            enumerations().Should().Be(afterFirst);
        }
    }

    [Fact]
    public void SyntaxModel_node_hashes_memoize_child_hashes()
    {
        var probe = new HashProbe(0);
        var node = Node<object>(probe);
        var expected = new HashCode();
        expected.Add(Span);
        expected.Add(0);

        node.GetHashCode().Should().Be(expected.ToHashCode());
        probe.Calls.Should().Be(1);

        for (var repeat = 0; repeat < 10; ++repeat)
            node.GetHashCode();

        probe.Calls.Should().Be(1);
    }

    [Fact]
    public void SyntaxModel_with_changes_recompute_File_node_and_variants()
    {
        var file = SampleFile();
        file.GetHashCode();
        var errors = new ElmSyntaxParseError[] { ParseError() };

        AssertSameAsFresh(
            file with { Comments = [Node("-- changed")] },
            SampleFile() with { Comments = [Node("-- changed")] });

        AssertSameAsFresh(
            file with { AdditionalParseErrors = errors },
            SampleFile() with { AdditionalParseErrors = errors });

        var node = Node<IReadOnlyList<string>>(["Original"]);
        node.GetHashCode();
        AssertSameAsFresh(node with { Value = ["Changed"] }, Node<IReadOnlyList<string>>(["Changed"]));

        AssertSameAsFresh(
            node.WithRange(new(2, 3), new(4, 5)),
            new Node<IReadOnlyList<string>>(new(new(2, 3), new(4, 5)), ["Original"]));

        var identifier = new Expression.Identifier(["Original"], "value");
        identifier.GetHashCode();

        AssertSameAsFresh(
            identifier with { ModuleName = ["Changed"] },
            new Expression.Identifier(["Changed"], "value"));

        var application = new Expression.Application(Expr(), [Expr()]);
        application.GetHashCode();

        AssertSameAsFresh(
            application with { Arguments = [Expr("changed")] },
            new Expression.Application(Expr(), [Expr("changed")]));

        var parenthesized = new Expression.Parenthesized(Expr());
        parenthesized.GetHashCode();

        AssertSameAsFresh(
            parenthesized with { Expression = Expr("changed") },
            new Expression.Parenthesized(Expr("changed")));

        var list = new SeparatedSyntaxList<Node<Expression>>.NonEmpty(Expr(), []);
        list.GetHashCode();

        AssertSameAsFresh(
            list with { First = Expr("changed") },
            new SeparatedSyntaxList<Node<Expression>>.NonEmpty(Expr("changed"), []));

        var module = new Module.NormalModule(Position, ModuleData());
        module.GetHashCode();

        AssertSameAsFresh(
            module with { ModuleData = ModuleData() with { ModuleName = Node<IReadOnlyList<string>>(["Changed"]) } },
            new Module.NormalModule(
                Position,
                ModuleData() with { ModuleName = Node<IReadOnlyList<string>>(["Changed"]) }));

        var pattern = new Pattern.ParenthesizedPattern(Pat());
        pattern.GetHashCode();

        AssertSameAsFresh(
            pattern with { Pattern = Node<Pattern>(new Pattern.VarPattern("changed")) },
            new Pattern.ParenthesizedPattern(Node<Pattern>(new Pattern.VarPattern("changed"))));

        var type = new TypeAnnotation.FunctionTypeAnnotation(TypeNode(), Position, TypeNode());
        type.GetHashCode();

        AssertSameAsFresh(
            type with { ReturnType = Node<TypeAnnotation>(new TypeAnnotation.GenericType("changed")) },
            new TypeAnnotation.FunctionTypeAnnotation(
                TypeNode(),
                Position,
                Node<TypeAnnotation>(new TypeAnnotation.GenericType("changed"))));

        var field = new RecordExprField(Node("field"), Position, Expr());
        field.GetHashCode();

        AssertSameAsFresh(
            field with { ValueExpr = Expr("changed") },
            new RecordExprField(Node("field"), Position, Expr("changed")));
    }

    [Fact]
    public void SyntaxModel_concurrent_hashes_and_clones_never_publish_incorrect_values()
    {
        for (var round = 0; round < 20; ++round)
        {
            var shared = new Expression.Application(Expr(), [Expr("argument"), Expr("other")]);
            var expected = new Expression.Application(Expr(), [Expr("argument"), Expr("other")]).GetHashCode();
            var changedExpected = new Expression.Application(Expr(), [Expr("changed")]).GetHashCode();

            Parallel.For(
                0,
                256,
                _ =>
                {
                    shared.GetHashCode().Should().Be(expected);
                    var changed = shared with { Arguments = [Expr("changed")] };
                    changed.GetHashCode().Should().Be(changedExpected);
                    shared.GetHashCode().Should().Be(expected);
                });
        }
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    [InlineData(int.MinValue)]
    [InlineData(int.MaxValue)]
    public void SyntaxModel_cache_supports_every_int_hash(int hash)
    {
        long cache = 0;
        SyntaxHashCache.TryGet(ref cache, out _).Should().BeFalse();
        SyntaxHashCache.Store(ref cache, hash).Should().Be(hash);
        SyntaxHashCache.TryGet(ref cache, out var actual).Should().BeTrue();
        actual.Should().Be(hash);
    }

    private static void AssertSameAsFresh(object changed, object fresh)
    {
        changed.GetHashCode().Should().Be(fresh.GetHashCode());
        changed.Equals(fresh).Should().BeTrue(changed.GetType().FullName);
        fresh.Equals(changed).Should().BeTrue(changed.GetType().FullName);
    }

    private static void VerifySynthesizedHash(object record)
    {
        // Positional record hashes combine the equality contract and primary-constructor properties in order.
        var constructor = record.GetType().GetConstructors().Single();
        var expected = record.GetType().GetHashCode();

        foreach (var parameter in constructor.GetParameters())
        {
            var value = record.GetType().GetProperty(parameter.Name!)!.GetValue(record);
            expected = unchecked(expected * -1521134295 + (value?.GetHashCode() ?? 0));
        }

        record.GetHashCode().Should().Be(expected);
        record.GetHashCode().Should().Be(expected);
    }

    private static object Clone(object record) =>
        record.GetType().GetMethod("<Clone>$")!.Invoke(record, null)!;

    private static FieldInfo CacheField(Type type) =>
        type.GetField("_cachedHashCode", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.DeclaredOnly) ??
        CacheField(type.BaseType!);

    private static Node<T> Node<T>(T value) => new(Span, value);

    private static Node<Expression> Expr(string name = "value") =>
        Node<Expression>(new Expression.Identifier([], name));

    private static Node<Pattern> Pat() => Node<Pattern>(new Pattern.VarPattern("a"));

    private static Node<TypeAnnotation> TypeNode() => Node<TypeAnnotation>(new TypeAnnotation.GenericType("a"));

    private static DefaultModuleData ModuleData() =>
        new(
            Node<IReadOnlyList<string>>(["Example"]),
            Position,
            Node<Exposing>(new Exposing.All(Span)));

    private static FunctionImplementation Implementation() => new(Node("function"), [Pat()], Position, Expr());

    private static FunctionStruct Function() => new(null, null, Node(Implementation()));

    private static RecordDefinition Record() =>
        new(
            new SeparatedSyntaxList<Node<RecordField>>.NonEmpty(
                Node(new RecordField(Node("field"), Position, TypeNode())),
                []));

    private static ElmSyntaxParseError ParseError() =>
        new(
            Span,
            null,
            new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.AnnotationNameRepeated("a")));

    private static File SampleFile() =>
        new(Node<Module>(new Module.NormalModule(Position, ModuleData())), [], [], [], []);

    private static IEnumerable<object> StructuralSamples() =>
        SynthesizedHashSamples().Concat(CustomHashSamples());

    private static IEnumerable<object> CustomHashSamples()
    {
        yield return SampleFile();
        yield return Node<IReadOnlyList<string>>(["Example"]);
        yield return Node<(IReadOnlyList<string>, string)>((["Example"], "value"));
        yield return new SeparatedSyntaxList<Node<Expression>>.NonEmpty(Expr(), [(Position, Expr())]);
        yield return new QualifiedNameRef(["Example"], "value");
        yield return new TypeAlias(null, Position, Position, Node("Alias"), [Node("a")], Position, TypeNode());
        yield return new ChoiceTypeStruct(null, Position, Node("Choice"), [Node("a")], Position, new SeparatedSyntaxList<Node<ValueConstructor>>.Empty());
        yield return new ValueConstructor(Node("Just"), [TypeNode()]);
        yield return new TypeAnnotation.Typed(Node<(IReadOnlyList<string>, string)>((["Example"], "Type")), [TypeNode()]);
        yield return Implementation();
        yield return new Pattern.NamedPattern(new QualifiedNameRef(["Example"], "Just"), [Pat()]);
        yield return new Expression.MultilineStringLiteral("one\ntwo", ["one", "two"]);
        yield return new Expression.MultilineStringLiteral("one");
        yield return new Expression.Identifier(["Example"], "value");
        yield return new Expression.Application(Expr(), [Expr()]);
        yield return new Expression.LetBlock(Position, [], Position, Expr());
        yield return new LambdaStruct(Position, [Pat()], Position, Expr());
        yield return new CaseBlock(Position, Expr(), Position, [new(Pat(), Position, Expr())]);
    }

    private static IEnumerable<object> SynthesizedHashSamples()
    {
        yield return new IncompleteDeclaration("unfinished =", ParseError());
        yield return new Import(Position, Node<IReadOnlyList<string>>(["Example"]), null, null);

        yield return new Import(Position, Node<IReadOnlyList<string>>(["Example"]), (Position, Node<IReadOnlyList<string>>(["Alias"])),
            (Position, Node<Exposing>(new Exposing.All(Span))));

        yield return ModuleData();

        var effectData =
            new EffectModuleData(
                Node<IReadOnlyList<string>>(["Example"]),
                Position,
                Node<Exposing>(new Exposing.All(Span)),
                Node("command"),
                Node("subscription"));

        yield return effectData;
        yield return new Module.NormalModule(Position, ModuleData());
        yield return new Module.PortModule(Position, Position, ModuleData());
        yield return new Module.EffectModule(Position, Position, effectData);
        yield return new Exposing.Explicit(Position, new SeparatedSyntaxList<Node<TopLevelExpose>>.NonEmpty(Node<TopLevelExpose>(new TopLevelExpose.FunctionExpose("value")), []), Position);
        yield return new Declaration.FunctionDeclaration(Function());

        var choice =
            new ChoiceTypeStruct(
                null,
                Position,
                Node("Choice"),
                [],
                Position,
                new SeparatedSyntaxList<Node<ValueConstructor>>.Empty());

        yield return new Declaration.ChoiceTypeDeclaration(choice);
        yield return new Declaration.AliasDeclaration(new TypeAlias(null, Position, Position, Node("Alias"), [], Position, TypeNode()));
        var signature = new Signature(Node("value"), Position, TypeNode());
        yield return new Declaration.PortDeclaration(Position, signature);
        var infix = new Infix(Position, Node(InfixDirection.Left), Node(5), Node("+"), Position, Node("add"));
        yield return new Declaration.InfixDeclaration(infix);
        yield return infix;
        yield return new TypeAnnotation.Tupled(new SeparatedSyntaxList<Node<TypeAnnotation>>.NonEmpty(TypeNode(), []));
        yield return new TypeAnnotation.Record(Record());
        yield return new TypeAnnotation.GenericRecord(Node("a"), Position, Node(Record()));
        yield return new TypeAnnotation.FunctionTypeAnnotation(TypeNode(), Position, TypeNode());
        yield return Record();
        yield return new RecordField(Node("field"), Position, TypeNode());
        yield return Function();
        yield return signature;
        var patterns = new SeparatedSyntaxList<Node<Pattern>>.NonEmpty(Pat(), []);
        yield return new Pattern.TuplePattern(patterns);
        yield return new Pattern.RecordPattern(new SeparatedSyntaxList<Node<string>>.NonEmpty(Node("field"), []));
        yield return new Pattern.UnConsPattern(Pat(), Position, Pat());
        yield return new Pattern.ListPattern(patterns);
        yield return new Pattern.AsPattern(Pat(), Position, Node("name"));
        yield return new Pattern.ParenthesizedPattern(Pat());
        yield return new Expression.Negation(Expr());
        var expressions = new SeparatedSyntaxList<Node<Expression>>.NonEmpty(Expr(), []);
        yield return new Expression.ListExpr(expressions);
        yield return new Expression.IfBlock(Position, Expr(), Position, Expr(), Position, Expr());
        yield return new Expression.Parenthesized(Expr());
        yield return new Expression.OperatorApplication(Node("+"), InfixDirection.Left, Expr(), Expr());
        yield return new Expression.TupledExpression(expressions);
        yield return new Expression.LambdaExpression(new LambdaStruct(Position, [Pat()], Position, Expr()));
        yield return new Expression.CaseExpression(new CaseBlock(Position, Expr(), Position, []));
        yield return new Expression.LetExpression(new Expression.LetBlock(Position, [], Position, Expr()));
        yield return new Expression.LetDeclaration.LetFunction(Function());
        yield return new Expression.LetDeclaration.LetDestructuring(Pat(), Position, Expr());
        var field = new RecordExprField(Node("field"), Position, Expr());
        var fields = new SeparatedSyntaxList<RecordExprField>.NonEmpty(field, []);
        yield return new Expression.RecordExpr(fields);
        yield return new Expression.RecordAccess(Expr(), Node("field"));
        yield return new Expression.RecordUpdateExpression(Node("record"), Position, fields);
        yield return field;
        yield return new Case(Pat(), Position, Expr());
    }

    private sealed class HashProbe(int hash)
    {
        private int _calls;

        public int Calls => Volatile.Read(ref _calls);

        public override int GetHashCode()
        {
            Interlocked.Increment(ref _calls);
            return hash;
        }
    }

    private sealed class CountingList<T>(IReadOnlyList<T> items) : IReadOnlyList<T>
    {
        private int _enumerations;

        public int Enumerations => Volatile.Read(ref _enumerations);

        public int Count => items.Count;

        public T this[int index] => items[index];

        public IEnumerator<T> GetEnumerator()
        {
            Interlocked.Increment(ref _enumerations);
            return items.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
