using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using Shape = Pine.Core.Elm.ElmCompilerInDotnet.StructuralType;
using Direction = Pine.Core.Elm.Syntax.Concrete.InfixDirection;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class CompilerModelHashTests
{
    private static DeclQualifiedName Name(string name) => DeclQualifiedName.Create(["Test"], name);

    private static Model.FunctionId Owner() => new Model.FunctionId.Source(Name("root"));

    private static Model.BindingId BindingId() => new Model.BindingId.Local(Owner(), 0);

    private static Model.Expression Literal(int value = 1) =>
        new Model.Expression.Literal(IntegerEncoding.EncodeSignedInteger(value));

    private static Model.Pattern Pattern() => new Model.Pattern.VarPattern(BindingId());

    private static Model.Function Function() => new([Pattern()], Literal());

    private static Model.CompiledCallable Callable(int value = 1) =>
        new(
            IntegerEncoding.EncodeSignedInteger(value),
            IntegerEncoding.EncodeSignedInteger(value + 1),
            1,
            [
            PineValue.EmptyList
            ]);

    private static Model.SccCompilationInput Request() =>
        new(
            [KeyValuePair.Create(Owner(), Function())],
            ImmutableDictionary<Model.FunctionId, Model.CompiledCallable>.Empty.Add(
                new Model.FunctionId.Source(Name("external")),
                Callable()),
            ImmutableDictionary<DeclQualifiedName, Model.ValueList<string>>.Empty.Add(
                Name("Record"),
                ["first", "second"]),
            ImmutableDictionary<DeclQualifiedName, int>.Empty.Add(Name("Tag"), 1),
            false);

    private static object[] Samples() =>
        [
            new Model.Expression.Literal(IntegerEncoding.EncodeSignedInteger(1)),
            new Model.Expression.ReferenceExpression(new Model.Reference.Local(BindingId())),
            new Model.Expression.List([Literal()]),
            new Model.Expression.Conditional(Literal(), Literal(2), Literal(3)),
            new Model.Expression.Application(Literal(), [Literal(2)]),
            new Model.Expression.Lambda(new Model.FunctionId.Lifted(Owner(), 0), [Pattern()], Literal()),
            new Model.Expression.Let([new(Pattern(), Literal())], Literal(2)),
            new Model.Expression.Match(Literal(), [new(Pattern(), Literal(2))]),
            new Model.Expression.Record([new("field", Literal())]),
            new Model.Expression.RecordAccess(Literal(), "field"),
            new Model.Expression.RecordAccessor("field"),
            new Model.Expression.RecordUpdate(Literal(), [new("field", Literal(2))]),
            new Model.Expression.Project(Literal(), 1),
            new Model.Expression.Operator("+", Direction.Left, Literal(), Literal(2)),
            new Model.Expression.PrefixOperator("+"),
            new Model.Expression.Negate(Literal()),
            new Model.Expression.Shader("shader"),
            new Model.Pattern.AllPattern(),
            Pattern(),
            new Model.Pattern.UnitPattern(),
            new Model.Pattern.CharPattern(65),
            new Model.Pattern.StringPattern("text"),
            new Model.Pattern.IntPattern(BigInteger.One << 100),
            new Model.Pattern.FloatPattern(1.25),
            new Model.Pattern.TuplePattern([Pattern()]),
            new Model.Pattern.ListPattern([Pattern()]),
            new Model.Pattern.UnConsPattern(Pattern(), new Model.Pattern.AllPattern()),
            new Model.Pattern.NamedPattern(Name("Tag"), [Pattern()]),
            new Model.Pattern.AsPattern(Pattern(), new Model.BindingId.Temporary(Owner(), 1)),
            Owner(),
            new Model.FunctionId.Lifted(Owner(), 2),
            Model.FunctionId.Specialized.Create(
                Owner(),
                [new Model.ParameterConstraint.KnownFunction(0, new Model.FunctionId.Lifted(Owner(), 1))]),
            new Model.ParameterConstraint.KnownFunction(1, Owner()),
            new Model.ParameterConstraint.ClosedRecord(
                2,
                Shape.ClosedRecord.Create(ImmutableDictionary<string, Shape>.Empty.Add("field", Shape.IntType.Instance))),
            BindingId(),
            new Model.BindingId.Temporary(Owner(), 2),
            new Model.Reference.Local(BindingId()),
            new Model.Reference.Function(Owner()),
            new Model.Reference.Constructor(Name("Tag")),
            new Model.Reference.Native(DeclQualifiedName.Create(["Pine_kernel"], "length")),
            Function(),
            new Model.Binding(Pattern(), Literal()),
            new Model.Branch(Pattern(), Literal()),
            new Model.Field("field", Literal()),
            Callable(),
            Request(),
            new Shape.IntType(),
            new Shape.FloatType(),
            new Shape.StringType(),
            new Shape.CharType(),
            new Shape.BoolType(),
            new Shape.TypeVariable(0),
            new Shape.ConstrainedVariable(1, Shape.TypeConstraint.Comparable),
            new Shape.FunctionType(new Shape.TypeVariable(0), new Shape.ListType(Shape.IntType.Instance)),
            new Shape.ListType(Shape.StringType.Instance),
            Shape.TupleType.Create([Shape.IntType.Instance, Shape.StringType.Instance]),
            Shape.ClosedRecord.Create(ImmutableDictionary<string, Shape>.Empty.Add("field", Shape.IntType.Instance)),
            new Shape.OpenRecord(ImmutableDictionary<string, Shape>.Empty.Add("field", Shape.IntType.Instance), 0),
            Shape.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<Shape>>.Empty.Add("Tag", [Shape.IntType.Instance])),
            new Shape.Self(),
            new Shape.TypeApplication(new Shape.ListType(new Shape.TypeVariable(0)), [Shape.IntType.Instance])
        ];

    [Fact]
    public void Fixtures_cover_all_transitive_compiler_key_variants()
    {
        Type[] roots =
            [
                typeof(Model.Expression), typeof(Model.Pattern), typeof(Model.FunctionId),
                typeof(Model.ParameterConstraint), typeof(Model.BindingId), typeof(Model.Reference), typeof(Shape)
            ];

        var samples = Samples();

        foreach (var root in roots)
        {
            samples.Where(root.IsInstanceOfType).Select(sample => sample.GetType())
                .Should().BeEquivalentTo(root.GetNestedTypes().Where(type => type.IsSubclassOf(root)));
        }
    }

    [Fact]
    public void Hash_history_never_changes_equality_and_record_copies_reset_memoization()
    {
        foreach (var original in Samples())
        {
            var coldCopy = Clone(original);
            original.Equals(coldCopy).Should().BeTrue(original.GetType().Name);
            var hash = original.GetHashCode();
            original.Equals(coldCopy).Should().BeTrue(original.GetType().Name);
            coldCopy.Equals(original).Should().BeTrue(original.GetType().Name);
            var field = CacheField(original);

            if (HasContents(original))
                field.GetValue(original).Should().NotBe(0L, original.GetType().Name);

            var copy = Clone(original);
            field.GetValue(copy).Should().Be(0L, original.GetType().Name);
            original.Equals(copy).Should().BeTrue(original.GetType().Name);
            copy.GetHashCode().Should().Be(hash, original.GetType().Name);
            coldCopy.GetHashCode().Should().Be(hash, original.GetType().Name);
        }
    }

    [Fact]
    public void Independently_constructed_values_have_equal_cached_hashes()
    {
        var first = Samples();
        var second = Samples();

        for (var index = 0; index < first.Length; ++index)
        {
            first[index].GetHashCode().Should().Be(second[index].GetHashCode(), first[index].GetType().Name);
            first[index].Equals(second[index]).Should().BeTrue(first[index].GetType().Name);
        }
    }

    [Fact]
    public void Changed_record_copies_hash_the_new_semantic_fields()
    {
        var field = new Model.Field("first", Literal());
        var function = Function();
        var binding = new Model.Binding(Pattern(), Literal());
        var branch = new Model.Branch(Pattern(), Literal());
        var conditional = new Model.Expression.Conditional(Literal(), Literal(2), Literal(3));
        var application = new Model.Expression.Application(Literal(), [Literal(2)]);
        var record = new Model.Expression.Record([field]);
        var accessor = new Model.Expression.RecordAccessor("first");
        var access = new Model.Expression.RecordAccess(Literal(), "first");
        var update = new Model.Expression.RecordUpdate(Literal(), [field]);
        var pattern = new Model.Pattern.IntPattern(BigInteger.One << 100);
        var named = new Model.Pattern.NamedPattern(Name("Tag"), [Pattern()]);
        var callable = Callable();
        var request = Request();
        var variable = new Shape.TypeVariable(1);
        var constrained = new Shape.ConstrainedVariable(1, Shape.TypeConstraint.Number);
        var functionType = new Shape.FunctionType(Shape.IntType.Instance, Shape.IntType.Instance);
        var listType = new Shape.ListType(Shape.IntType.Instance);

        var openRecord =
            new Shape.OpenRecord(ImmutableDictionary<string, Shape>.Empty.Add("a", Shape.IntType.Instance), 0);

        var typeApplication =
            new Shape.TypeApplication(new Shape.ListType(new Shape.TypeVariable(0)), [Shape.IntType.Instance]);

        object[] originals =
            [
                field, function, binding, branch, conditional, application, record, accessor, access, update,
                pattern, named, callable, request, variable, constrained, functionType, listType, openRecord, typeApplication
            ];

        foreach (var original in originals)
            original.GetHashCode();

        (object Changed, object Fresh)[] pairs =
            [
                (field with { Name = "second" }, new Model.Field("second", Literal())),
                (function with { Body = Literal(2) }, new Model.Function([Pattern()], Literal(2))),
                (binding with { Value = Literal(2) }, new Model.Binding(Pattern(), Literal(2))),
                (branch with { Body = Literal(2) }, new Model.Branch(Pattern(), Literal(2))),
                (conditional with { Then = Literal(4) }, new Model.Expression.Conditional(Literal(), Literal(4), Literal(3))),
                (application with { Arguments = [Literal(3)] }, new Model.Expression.Application(Literal(), [Literal(3)])),
                (record with { Fields = [new("second", Literal())] }, new Model.Expression.Record([new("second", Literal())])),
                (accessor with { FieldName = "second" }, new Model.Expression.RecordAccessor("second")),
                (access with { FieldName = "second" }, new Model.Expression.RecordAccess(Literal(), "second")),
                (update with { Fields = [] }, new Model.Expression.RecordUpdate(Literal(), [])),
                (pattern with { Value = 2 }, new Model.Pattern.IntPattern(2)),
                (named with { Arguments = [] }, new Model.Pattern.NamedPattern(Name("Tag"), [])),
                (callable with { ParameterCount = 2 }, new Model.CompiledCallable(callable.CompiledValue, callable.EncodedBody, 2, callable.EnvFunctions)),
                (request with { DisableGenericApplicationChainConsolidation = true },
                new Model.SccCompilationInput(
                    request.Members,
                    request.ExternalFunctions,
                    request.RecordConstructors,
                    request.ChoiceConstructors,
                    true)),
                (variable with { Index = 2 }, new Shape.TypeVariable(2)),
                (constrained with { Constraint = Shape.TypeConstraint.Appendable }, new Shape.ConstrainedVariable(1, Shape.TypeConstraint.Appendable)),
                (functionType with { ReturnType = Shape.StringType.Instance }, new Shape.FunctionType(Shape.IntType.Instance, Shape.StringType.Instance)),
                (listType with { ElementType = Shape.StringType.Instance }, new Shape.ListType(Shape.StringType.Instance)),
                (openRecord with { RowVariableIndex = 1 }, new Shape.OpenRecord(openRecord.Fields, 1)),
                (typeApplication with { Arguments = [Shape.StringType.Instance] },
                new Shape.TypeApplication(typeApplication.GenericType, [Shape.StringType.Instance]))
            ];

        foreach (var (changed, fresh) in pairs)
        {
            CacheField(changed).GetValue(changed).Should().Be(0L, changed.GetType().Name);
            changed.Equals(fresh).Should().BeTrue(changed.GetType().Name);
            changed.GetHashCode().Should().Be(fresh.GetHashCode(), changed.GetType().Name);
            new HashSet<object> { changed }.Should().Contain(fresh);
        }
    }

    [Fact]
    public void Scc_hashes_include_map_contents_and_are_independent_of_insertion_order()
    {
        var input = Request();
        input.GetHashCode();
        var external = new Model.FunctionId.Source(Name("other"));

        var first =
            input with
            {
                ExternalFunctions = input.ExternalFunctions.Add(external, Callable(2)),
                RecordConstructors = input.RecordConstructors.Add(Name("OtherRecord"), ["third"]),
                ChoiceConstructors = input.ChoiceConstructors.Add(Name("OtherTag"), 2)
            };

        Model.FunctionId secondMember = new Model.FunctionId.Source(Name("secondMember"));

        var twoMembers =
            new Model.SccCompilationInput(
                [
                    KeyValuePair.Create(
                        Owner(),
                        new Model.Function(
                            [],
                            new Model.Expression.ReferenceExpression(new Model.Reference.Function(secondMember)))),
                    KeyValuePair.Create(
                        secondMember,
                        new Model.Function(
                            [],
                            new Model.Expression.ReferenceExpression(new Model.Reference.Function(Owner()))))
                ],
                first.ExternalFunctions,
                first.RecordConstructors,
                first.ChoiceConstructors,
                false);

        var reordered =
            new Model.SccCompilationInput(
                new(twoMembers.Members.Reverse()),
                first.ExternalFunctions.Reverse().ToImmutableDictionary(),
                first.RecordConstructors.Reverse().ToImmutableDictionary(),
                first.ChoiceConstructors.Reverse().ToImmutableDictionary(),
                false);

        twoMembers.Should().Be(reordered);
        twoMembers.GetHashCode().Should().Be(reordered.GetHashCode());
        var hash = input.GetHashCode();

        var changes =
            new[]
            {
                new Model.SccCompilationInput(
                    [KeyValuePair.Create(Owner(), Function() with { Body = Literal(9) })],
                    input.ExternalFunctions,
                    input.RecordConstructors,
                    input.ChoiceConstructors,
                    false),
                input with { ExternalFunctions = input.ExternalFunctions.SetItem(input.ExternalFunctions.Keys.Single(), Callable(3)) },
                input with { ExternalFunctions = input.ExternalFunctions.Remove(input.ExternalFunctions.Keys.Single()).Add(external, Callable()) },
                input with { RecordConstructors = input.RecordConstructors.SetItem(Name("Record"), ["second", "first"]) },
                input with { ChoiceConstructors = input.ChoiceConstructors.SetItem(Name("Tag"), 2) },
                input with { DisableGenericApplicationChainConsolidation = true }
            };

        foreach (var changed in changes)
        {
            changed.Should().NotBe(input);

            changed.GetHashCode().Should().NotBe(
                hash,
                "member bodies and same-sized maps must contribute their contents");
        }
    }

    [Fact]
    public void Unordered_map_hashes_and_equality_use_the_same_key_comparer()
    {
        var first =
            ImmutableDictionary<string, int>.Empty.WithComparers(StringComparer.OrdinalIgnoreCase).Add("Name", 1);

        var second =
            ImmutableDictionary<string, int>.Empty.WithComparers(StringComparer.OrdinalIgnoreCase).Add("NAME", 1);

        StructuralHash.SameEntries(first, second).Should().BeTrue();
        StructuralHash.UnorderedMap(first).Should().Be(StructuralHash.UnorderedMap(second));

        var differentComparer =
            ImmutableDictionary<string, int>.Empty.WithComparers(StringComparer.Ordinal).Add("Name", 1);

        StructuralHash.SameEntries(first, differentComparer).Should().BeFalse();
        StructuralHash.SameEntries(differentComparer, first).Should().BeFalse();
    }

    [Fact]
    public void Scc_equality_checks_contents_even_when_hashes_collide()
    {
        var comparer = new CountingComparer<DeclQualifiedName>(constantHash: true);

        var tags =
            ImmutableDictionary<DeclQualifiedName, int>.Empty.WithComparers(comparer)
            .Add(Name("First"), 1).Add(Name("Second"), 2);

        var first = Request() with { ChoiceConstructors = tags };
        var second = first with { ChoiceConstructors = tags.SetItem(Name("First"), 2).SetItem(Name("Second"), 1) };
        first.GetHashCode().Should().Be(second.GetHashCode(), "entry-hash multisets collide when key hashes collide");
        first.Should().NotBe(second);
        var cache = new Dictionary<Model.SccCompilationInput, int> { [first] = 1, [second] = 2 };
        cache.Should().HaveCount(2);
        cache[first].Should().Be(1);
        cache[second].Should().Be(2);
    }

    [Fact]
    public void Repeated_Scc_hashes_do_not_enumerate_or_hash_map_entries()
    {
        var functions = new CountingComparer<Model.FunctionId>();
        var records = new CountingComparer<DeclQualifiedName>();
        var choices = new CountingComparer<DeclQualifiedName>();

        var input =
            Request() with
            {
                ExternalFunctions = Request().ExternalFunctions.WithComparers(functions),
                RecordConstructors = Request().RecordConstructors.WithComparers(records),
                ChoiceConstructors = Request().ChoiceConstructors.WithComparers(choices)
            };

        var expression = new ExpressionProbe();

        input =
            new Model.SccCompilationInput(
                [KeyValuePair.Create(Owner(), new Model.Function([], expression))],
                input.ExternalFunctions,
                input.RecordConstructors,
                input.ChoiceConstructors,
                false);

        var before = functions.HashCalls + records.HashCalls + choices.HashCalls;
        var hash = input.GetHashCode();
        var after = functions.HashCalls + records.HashCalls + choices.HashCalls;
        var expressionCalls = expression.Calls;
        expressionCalls.Should().BeGreaterThan(0);
        after.Should().BeGreaterThan(before);

        for (var repeat = 0; repeat < 100; ++repeat)
            input.GetHashCode().Should().Be(hash);

        (functions.HashCalls + records.HashCalls + choices.HashCalls).Should().Be(after);
        expression.Calls.Should().Be(expressionCalls);
    }

    [Fact]
    public void Repeated_parent_hashes_do_not_revisit_child_hashes()
    {
        var expression = new ExpressionProbe();
        var pattern = new PatternProbe();
        var identity = new IdentityProbe();
        var reference = new ReferenceProbe();
        var binding = new BindingProbe();
        var shape = new TypeProbe();

        object[] parents =
            [
                new Model.Expression.Negate(expression),
                new Model.Function([], expression),
                new Model.Binding(pattern, expression),
                new Model.Branch(pattern, expression),
                new Model.Field("field", expression),
                new Model.Expression.ReferenceExpression(reference),
                new Model.Pattern.VarPattern(binding),
                new Model.Pattern.UnConsPattern(pattern, new Model.Pattern.AllPattern()),
                new Model.FunctionId.Lifted(identity, 0),
                new Model.BindingId.Local(identity, 0),
                new Model.BindingId.Temporary(identity, 1),
                new Model.Reference.Function(identity),
                new Model.Reference.Local(binding),
                new Model.ParameterConstraint.KnownFunction(0, identity),
                new Shape.FunctionType(shape, Shape.IntType.Instance),
                new Shape.ListType(shape),
                new Model.ValueList<Model.Expression>([expression])
            ];

        foreach (var parent in parents)
            parent.GetHashCode();

        var before =
            new[] { expression.Calls, pattern.Calls, identity.Calls, reference.Calls, binding.Calls, shape.Calls };

        foreach (var parent in parents)
        {
            for (var repeat = 0; repeat < 100; ++repeat)
                parent.GetHashCode();
        }

        new[] { expression.Calls, pattern.Calls, identity.Calls, reference.Calls, binding.Calls, shape.Calls }.Should()
            .Equal(before);

        before.Should().OnlyContain(calls => calls > 0);
    }

    [Fact]
    public void Hashing_does_not_change_semantic_JSON_or_round_trip_equality()
    {
        var options = Model.CompilerModelJson.CreateOptions();

        foreach (var value in Samples().Where(
            value => value is Model.Expression or Model.Pattern or
            Model.FunctionId or Model.BindingId or Model.Reference or Model.ParameterConstraint or Model.Function or Model.CompiledCallable))
        {
            var type =
                new[]
                {
                    typeof(Model.Expression), typeof(Model.Pattern), typeof(Model.FunctionId),
                    typeof(Model.BindingId), typeof(Model.Reference), typeof(Model.ParameterConstraint)
                }.FirstOrDefault(root => root.IsInstanceOfType(value)) ?? value.GetType();

            var before = JsonSerializer.Serialize(value, type, options);
            value.GetHashCode();
            JsonSerializer.Serialize(value, type, options).Should().Be(before);
            var restored = JsonSerializer.Deserialize(before, type, options)!;
            restored.Should().Be(value);
            restored.GetHashCode().Should().Be(value.GetHashCode());
        }
    }

    [Fact]
    public void Concurrent_hash_publication_and_warmed_hashes_are_allocation_free()
    {
        var samples = Samples();
        var hashes = samples.Select(sample => Clone(sample).GetHashCode()).ToArray();

        Parallel.For(
            0,
            64,
            _ =>
            {
                for (var index = 0; index < samples.Length; ++index)
                    samples[index].GetHashCode().Should().Be(hashes[index]);
            });

        for (var warmup = 0; warmup < 100; ++warmup)
        {
            foreach (var sample in samples)
                sample.GetHashCode();
        }

        var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();
        var combined = 0;

        for (var repeat = 0; repeat < 100; ++repeat)
        {
            foreach (var sample in samples)
                combined ^= sample.GetHashCode();
        }

        var allocated = GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;
        allocated.Should().Be(0);
        combined.Should().Be(0);
    }

    private static object Clone(object value) => value.GetType().GetMethod("<Clone>$")!.Invoke(value, null)!;

    private static FieldInfo CacheField(object value)
    {
        const BindingFlags Flags = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.DeclaredOnly;

        for (var type = value.GetType(); type is not null; type = type.BaseType)
        {
            var field = type.GetField("_cachedHashCode", Flags) ?? type.GetField("cachedHashCode", Flags);

            if (field is not null)
                return field;
        }

        throw new InvalidOperationException("No hash cache on " + value.GetType().Name);
    }

    private static bool HasContents(object value) =>
        value is not (
        Model.Pattern.AllPattern or Model.Pattern.UnitPattern or Shape.IntType or Shape.FloatType or
        Shape.StringType or Shape.CharType or Shape.BoolType or Shape.Self);

    private sealed class CountingComparer<T>(bool constantHash = false) : IEqualityComparer<T> where T : notnull
    {
        public int HashCalls { get; private set; }

        public bool Equals(T? first, T? second) => EqualityComparer<T>.Default.Equals(first, second);

        public int GetHashCode(T value)
        {
            ++HashCalls;
            return constantHash ? 0 : value.GetHashCode();
        }
    }

    private sealed record ExpressionProbe : Model.Expression
    {
        public int Calls { get; private set; }

        public ExpressionProbe() : base(new Literal(PineValue.EmptyList)) { }

        public override int GetHashCode()
        {
            ++Calls;
            return 0;
        }
    }

    private sealed record PatternProbe : Model.Pattern
    {
        public int Calls { get; private set; }

        public PatternProbe() : base(new AllPattern()) { }

        public override int GetHashCode()
        {
            ++Calls;
            return 0;
        }
    }

    private sealed record IdentityProbe : Model.FunctionId
    {
        public int Calls { get; private set; }

        public IdentityProbe() : base(Owner()) { }

        public override int GetHashCode()
        {
            ++Calls;
            return 0;
        }
    }

    private sealed record ReferenceProbe : Model.Reference
    {
        public int Calls { get; private set; }

        public ReferenceProbe() : base(new Function(Owner())) { }

        public override int GetHashCode()
        {
            ++Calls;
            return 0;
        }
    }

    private sealed record BindingProbe : Model.BindingId
    {
        public int Calls { get; private set; }

        public BindingProbe() : base(BindingId()) { }

        public override int GetHashCode()
        {
            ++Calls;
            return 0;
        }
    }

    private sealed record TypeProbe : Shape
    {
        public int Calls { get; private set; }

        public TypeProbe() : base(IntType.Instance) { }

        public override int GetHashCode()
        {
            ++Calls;
            return 0;
        }
    }
}
