using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Concrete;
using System;
using System.Collections;
using System.Collections.Generic;
using Xunit;

using AbstractSyntax = Pine.Core.Elm.Syntax.Abstract;
using ConcreteSyntax = Pine.Core.Elm.Syntax.Concrete;
using ElmFormatter = Pine.Core.Elm.Syntax.Avh4Format;
using LegacySyntax = Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;
using Range = Pine.Core.Elm.Syntax.Concrete.Range;

namespace Pine.Core.Tests.Elm.Syntax;

public class IndexedSyntaxTraversalTests
{
    private static readonly Location s_location = new(1, 1);

    private static readonly Range s_range = new(s_location, s_location);

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(4)]
    public void WithoutSeparatorTokens_copies_nodes_by_index(int count)
    {
        var nodes = new Node<string>[count];

        for (var i = 0; i < count; i++)
            nodes[i] = Node(i.ToString());

        var separated = Separated(nodes);
        var result = separated.WithoutSeparatorTokens();

        result.Count.Should().Be(count);

        for (var i = 0; i < count; i++)
            result[i].Should().BeSameAs(nodes[i]);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(4)]
    public void Syntax_conversions_traverse_application_and_separated_lists_by_index(int count)
    {
        var nodes = new Node<ConcreteSyntax.Expression>[count];

        for (var i = 0; i < count; i++)
            nodes[i] = Node<ConcreteSyntax.Expression>(new ConcreteSyntax.Expression.IntegerLiteral(i.ToString()));

        var function =
            Node<ConcreteSyntax.Expression>(new ConcreteSyntax.Expression.Identifier(["Basics"], "identity"));

        var list = new ConcreteSyntax.Expression.ListExpr(Separated(nodes));

        var application =
            new ConcreteSyntax.Expression.Application(
                function,
                new IndexOnlyList<Node<ConcreteSyntax.Expression>>(nodes));

        var abstractList = AbstractSyntax.ConvertFromConcrete.FromExpression(list);
        var abstractApplication = AbstractSyntax.ConvertFromConcrete.FromExpression(application);

        var abstractElements = abstractList.Should().BeOfType<AbstractSyntax.Expression.ListExpr>().Subject.Elements;

        var abstractArguments =
            abstractApplication.Should().BeOfType<AbstractSyntax.Expression.Application>().Subject.Arguments;

        var restoredAbstractList =
            AbstractSyntax.ConvertToConcrete.ToExpression(
                new AbstractSyntax.Expression.ListExpr(new IndexOnlyList<AbstractSyntax.Expression>(abstractElements)))
            .Should().BeOfType<ConcreteSyntax.Expression.ListExpr>().Subject;

        var restoredAbstractApplication =
            AbstractSyntax.ConvertToConcrete.ToExpression(
                new AbstractSyntax.Expression.Application(
                    AbstractSyntax.Expression.Identifier.Create(["Basics"], "identity"),
                    new IndexOnlyList<AbstractSyntax.Expression>(abstractArguments)))
            .Should().BeOfType<ConcreteSyntax.Expression.Application>().Subject;

        var legacyList =
            LegacySyntax.FromFullSyntaxModel.Convert(list)
            .Should().BeOfType<LegacySyntax.Expression.ListExpr>().Subject;

        var legacyApplication =
            LegacySyntax.FromFullSyntaxModel.Convert(application)
            .Should().BeOfType<LegacySyntax.Expression.Application>().Subject;

        var restoredLegacyList =
            LegacySyntax.ToFullSyntaxModel.Convert(
                new LegacySyntax.Expression.ListExpr(
                    new IndexOnlyList<Node<LegacySyntax.Expression>>(legacyList.Elements)))
            .Should().BeOfType<ConcreteSyntax.Expression.ListExpr>().Subject;

        var restoredLegacyApplication =
            LegacySyntax.ToFullSyntaxModel.Convert(
                new LegacySyntax.Expression.Application(
                    new IndexOnlyList<Node<LegacySyntax.Expression>>(legacyApplication.Arguments)))
            .Should().BeOfType<ConcreteSyntax.Expression.Application>().Subject;

        restoredAbstractList.Elements.Count.Should().Be(count);
        restoredAbstractApplication.Arguments.Count.Should().Be(count);
        restoredLegacyList.Elements.Count.Should().Be(count);
        restoredLegacyApplication.Arguments.Count.Should().Be(count);
        legacyApplication.Arguments.Count.Should().Be(count + 1);

        for (var i = 0; i < count; i++)
        {
            var expected = i.ToString();

            restoredAbstractList.Elements[i].Value.Should().BeOfType<ConcreteSyntax.Expression.IntegerLiteral>()
                .Subject.LiteralText.Should().Be(expected);

            restoredAbstractApplication.Arguments[i].Value.Should().BeOfType<ConcreteSyntax.Expression.IntegerLiteral>()
                .Subject.LiteralText.Should().Be(expected);

            restoredLegacyList.Elements[i].Value.Should().BeOfType<ConcreteSyntax.Expression.IntegerLiteral>()
                .Subject.LiteralText.Should().Be(expected);

            restoredLegacyApplication.Arguments[i].Value.Should().BeOfType<ConcreteSyntax.Expression.IntegerLiteral>()
                .Subject.LiteralText.Should().Be(expected);
        }
    }

    [Fact]
    public void Abstract_record_factories_preserve_equal_key_order_without_enumeration()
    {
        var first = AbstractSyntax.Expression.StringLiteral.Create("first");
        var second = AbstractSyntax.Expression.StringLiteral.Create("second");
        var third = AbstractSyntax.Expression.StringLiteral.Create("third");

        var fields =
            new IndexOnlyList<(string FieldName, AbstractSyntax.Expression FieldExpr)>(
                [("z", third), ("a", first), ("a", second)]);

        var record = AbstractSyntax.Expression.RecordExpr.CreateSorted(fields);
        var update = AbstractSyntax.Expression.RecordUpdateExpression.CreateSorted("record", fields);

        record.Fields[0].Value.Should().BeSameAs(first);
        record.Fields[1].Value.Should().BeSameAs(second);
        record.Fields[2].Value.Should().BeSameAs(third);
        update.Fields[0].Value.Should().BeSameAs(first);
        update.Fields[1].Value.Should().BeSameAs(second);
        update.Fields[2].Value.Should().BeSameAs(third);
    }

    [Fact]
    public void Formatter_and_file_conversions_use_indexed_collections()
    {
        var source =
            """
            module Example exposing
                ( value
                , Choice(..)
                )

            {-| Example.

            @docs value, Choice(..)
            -}

            import Zeta.Module as Z exposing (z, a)
            import Alpha.Module as A

            -- A choice
            type Choice a
                = First a
                | Second

            value =
                [ 1, 2, 3 ]
            """;

        var parsed =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new Exception(error.ToString()));

        var indexed =
            parsed with
            {
                Imports = new IndexOnlyList<Node<ConcreteSyntax.Import>>(parsed.Imports),
                Declarations = new IndexOnlyList<Node<ConcreteSyntax.Declaration>>(parsed.Declarations),
                Comments = new IndexOnlyList<Node<string>>(parsed.Comments),
                IncompleteDeclarations = new IndexOnlyList<Node<IncompleteDeclaration>>(parsed.IncompleteDeclarations)
            };

        var formatted = ElmFormatter.Format(indexed);
        var expected = ElmFormatter.FormatToString(parsed);

        Rendering.ToString(formatted).Should().Be(expected);

        var abstractFile = AbstractSyntax.ConvertFromConcrete.FromFile(indexed);

        var concreteFromAbstract =
            AbstractSyntax.ConvertToConcrete.FromFile(
                abstractFile with
                {
                    Imports = new IndexOnlyList<AbstractSyntax.Import>(abstractFile.Imports),
                    Declarations = new IndexOnlyList<AbstractSyntax.Declaration>(abstractFile.Declarations)
                });

        var legacyFile = LegacySyntax.FromFullSyntaxModel.Convert(indexed);

        var concreteFromLegacy =
            LegacySyntax.ToFullSyntaxModel.Convert(
                legacyFile with
                {
                    Imports = new IndexOnlyList<Node<LegacySyntax.Import>>(legacyFile.Imports),
                    Declarations = new IndexOnlyList<Node<LegacySyntax.Declaration>>(legacyFile.Declarations),
                    Comments = new IndexOnlyList<Node<string>>(legacyFile.Comments)
                });

        concreteFromAbstract.Imports.Count.Should().Be(parsed.Imports.Count);
        concreteFromAbstract.Declarations.Count.Should().Be(parsed.Declarations.Count);
        concreteFromLegacy.Imports.Count.Should().Be(parsed.Imports.Count);
        concreteFromLegacy.Declarations.Count.Should().Be(parsed.Declarations.Count);
        concreteFromLegacy.Comments.Count.Should().Be(parsed.Comments.Count);
    }

    [Fact]
    public void Formatter_preserves_input_order_for_equal_import_sort_keys()
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText("module Example exposing (..)\n\nimport Same.Module\n")
            .Extract(error => throw new Exception(error.ToString()));

        var imports = new Node<ConcreteSyntax.Import>[24];

        for (var i = 0; i < imports.Length; i++)
        {
            imports[i] =
                parsed.Imports[0] with
                {
                    Value =
                    parsed.Imports[0].Value with
                    {
                        ModuleName =
                        parsed.Imports[0].Value.ModuleName with
                        {
                            Value = new IndexOnlyList<string>(parsed.Imports[0].Value.ModuleName.Value)
                        },
                        ModuleAlias = (s_location, Node<IReadOnlyList<string>>(["Alias" + i]))
                    }
                };
        }

        var formatted =
            ElmFormatter.Format(parsed with { Imports = new IndexOnlyList<Node<ConcreteSyntax.Import>>(imports) });

        formatted.Imports.Count.Should().Be(imports.Length);

        for (var i = 0; i < imports.Length; i++)
            formatted.Imports[i].Value.ModuleAlias!.Value.Alias.Value[0].Should().Be("Alias" + i);
    }

    private static Node<T> Node<T>(T value) => new(s_range, value);

    private static SeparatedSyntaxList<T> Separated<T>(IReadOnlyList<T> nodes)
    {
        if (nodes.Count is 0)
            return new SeparatedSyntaxList<T>.Empty();

        var rest = new (Location SeparatorLocation, T Node)[nodes.Count - 1];

        for (var i = 0; i < rest.Length; i++)
            rest[i] = (s_location, nodes[i + 1]);

        return new SeparatedSyntaxList<T>.NonEmpty(nodes[0], new IndexOnlyList<(Location SeparatorLocation, T Node)>(rest));
    }

    private sealed class IndexOnlyList<T>(IReadOnlyList<T> items) : IReadOnlyList<T>
    {
        public int Count => items.Count;

        public T this[int index] => items[index];

        public IEnumerator<T> GetEnumerator() =>
            throw new InvalidOperationException("Syntax traversal must use indexing, not enumeration.");

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
