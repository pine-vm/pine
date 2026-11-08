using AwesomeAssertions;
using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Elm.Testing;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using Pine.Core.Tests.Elm.LanguageServer;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class SourceCompilationCacheTests
{
    private sealed class TrackingCache<T>(ICache<SourceTransformationInput, T>? backing = null)
        : ICache<SourceTransformationInput, T>
    {
        private readonly ICache<SourceTransformationInput, T> _cache =
            backing ?? new MemoryCache<SourceTransformationInput, T, long>(0);

        public List<SourceTransformationInput> Hits { get; } = [];

        public List<SourceTransformationInput> Misses { get; } = [];

        public List<T> Values { get; } = [];

        public bool TryGetValue(SourceTransformationInput key, [MaybeNullWhen(false)] out T value)
        {
            var hit = _cache.TryGetValue(key, out value);
            (hit ? Hits : Misses).Add(key);
            return hit;
        }

        public void Set(SourceTransformationInput key, T value)
        {
            Values.Add(value);
            _cache.Set(key, value);
        }

        public void Reset()
        {
            Hits.Clear();
            Misses.Clear();
            Values.Clear();
        }
    }

    private sealed class CountingDeclarations(
        IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations)
        : IReadOnlyDictionary<DeclQualifiedName, Declaration>
    {
        public int Enumerations { get; private set; }

        public int EntriesVisited { get; private set; }

        public Declaration this[DeclQualifiedName key] => declarations[key];

        public IEnumerable<DeclQualifiedName> Keys => declarations.Keys;

        public IEnumerable<Declaration> Values => declarations.Values;

        public int Count => declarations.Count;

        public bool ContainsKey(DeclQualifiedName key) => declarations.ContainsKey(key);

        public bool TryGetValue(DeclQualifiedName key, [MaybeNullWhen(false)] out Declaration value) =>
            declarations.TryGetValue(key, out value);

        public IEnumerator<KeyValuePair<DeclQualifiedName, Declaration>> GetEnumerator()
        {
            Enumerations++;

            foreach (var entry in declarations)
            {
                EntriesVisited++;
                yield return entry;
            }
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();
    }

    [Theory]
    [InlineData(32)]
    [InlineData(256)]
    public void Shared_projection_context_scans_constructor_owners_once(int functionCount)
    {
        var declarations =
            Parse(
                "type Choice = Choice Int | Empty\n" +
                string.Join("\n", Enumerable.Range(0, functionCount).Select(index => $"f{index} n = Choice n")));

        var counted = new CountingDeclarations(declarations);
        var context = new SourceLoweringProjection.Context(counted, CancellationToken.None);
        context.Constructors[Name("Choice")].Should().Be((Name("Choice"), 0));
        context.Constructors[Name("Empty")].Should().Be((Name("Choice"), 1));

        foreach (var function in declarations.Keys.Where(name => name.DeclName.StartsWith("f", StringComparison.Ordinal)))
        {
            var projection = new SourceLoweringProjection(function, context);
            var input = projection.Input("shared-context-test", includeFunctionTypes: false);
            projection.Rebind(input.Syntax).Should().Be(declarations[function]);
        }

        counted.Enumerations.Should().Be(1);
        counted.EntriesVisited.Should().Be(declarations.Count);
    }

    [Theory]
    [InlineData(32)]
    [InlineData(256)]
    public void Cached_semantic_pass_shares_the_index_between_inference_and_lowering(int functionCount)
    {
        var declarations =
            Parse(
                "type Choice = Choice Int | Empty\n" +
                string.Join("\n", Enumerable.Range(0, functionCount).Select(index => $"f{index} n = Choice n")));

        var cache = new TrackingCache<LoweredSourceFunction>();

        for (var round = 0; round < 2; round++)
        {
            var counted = new CountingDeclarations(declarations);
            var actual = Model.FromCanonicalSyntax.Lower(counted, cache);
            AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(declarations));
            counted.Enumerations.Should().Be(6, "the two projection loops must share one constructor scan");
            counted.EntriesVisited.Should().Be(6 * declarations.Count);
            actual.Program.Functions.Should().HaveCount(functionCount);
            cache.Reset();
        }
    }

    [Fact]
    public void Projection_contexts_keep_constructor_indexes_bound_to_their_declaration_snapshot()
    {
        var original = Parse("type Choice = Choice Int | Empty\nroot n = Choice n");
        var edited = Parse("type Choice = Empty | Choice Float\nroot n = Choice n");
        var first = new SourceLoweringProjection.Context(original, CancellationToken.None);
        var second = new SourceLoweringProjection.Context(edited, CancellationToken.None);
        first.Constructors[Name("Choice")].Should().Be((Name("Choice"), 0));
        second.Constructors[Name("Choice")].Should().Be((Name("Choice"), 1));

        var before =
            new SourceLoweringProjection(Name("root"), first)
            .Input("snapshot-test", includeFunctionTypes: false);

        var after =
            new SourceLoweringProjection(Name("root"), second)
            .Input("snapshot-test", includeFunctionTypes: false);

        after.ComputeHash().ToArray().Should().NotEqual(before.ComputeHash().ToArray());

        new SourceLoweringProjection(Name("root"), first)
            .Input("snapshot-test", includeFunctionTypes: false)
            .ComputeHash().ToArray().Should().Equal(before.ComputeHash().ToArray());
    }

    [Fact]
    public void Cancelled_projection_context_does_not_scan_declarations()
    {
        var declarations = new CountingDeclarations(Parse("type Choice = Choice Int\nroot n = Choice n"));
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        Action construct = () => _ = new SourceLoweringProjection.Context(declarations, cancellation.Token);
        construct.Should().Throw<OperationCanceledException>();
        declarations.Enumerations.Should().Be(0);
    }

    [Fact]
    public void Equivalent_anonymous_roles_share_syntax_inputs_and_digests()
    {
        var declarations = Parse("type Choice = Choice Int\nfirst n = Choice n\nsecond n = Choice n");
        var context = new SourceLoweringProjection.Context(declarations, CancellationToken.None);
        var first = new SourceLoweringProjection(Name("first"), context);
        var second = new SourceLoweringProjection(Name("second"), context);
        second.Syntax.Should().BeSameAs(first.Syntax);

        foreach (var (name, declaration) in first.Declarations)
            second.Declarations[name].Should().BeSameAs(declaration);

        var input = first.Input("anonymous-role-reuse-test", includeFunctionTypes: false);
        first.Input("anonymous-role-reuse-test", includeFunctionTypes: false).Should().BeSameAs(input);
        second.Input("anonymous-role-reuse-test", includeFunctionTypes: false).Should().BeSameAs(input);
        input.ComputeHash().Equals(input.ComputeHash()).Should().BeTrue("the digest itself is memoized");
        second.Rebind(input.Syntax).Should().Be(declarations[Name("second")]);
    }

    [Fact]
    public void Canonical_documents_repair_copied_AST_hashes_without_reencoding_normalized_syntax()
    {
        var original = ((Declaration.FunctionDeclaration)Parse("root n = n")[Name("root")]).Function;
        var copied = original.Declaration with { Name = "renamed" };
        var declaration = new Declaration.FunctionDeclaration(new(null, copied));
        var expected = Parse("renamed n = n")[Name("renamed")];
        var normalized = SourceCompilationCaches.NormalizeSyntax(declaration);
        normalized.Should().Be(expected);
        normalized.GetHashCode().Should().Be(expected.GetHashCode());
        SourceCompilationCaches.NormalizeSyntax(normalized).Should().BeSameAs(normalized);
        SourceCompilationCaches.NormalizeSyntax(declaration).Should().BeSameAs(normalized);

        var context =
            new SourceLoweringProjection.Context(
                ImmutableDictionary<DeclQualifiedName, Declaration>.Empty,
                CancellationToken.None);

        var shared = context.InternSyntax(expected);
        context.InternSyntax(declaration).Should().BeSameAs(shared, "copied hashes must not prevent canonical sharing");

        (new SourceTransformationInput(Name("renamed"), normalized, "environment", "hash-repair-test"))
            .ComputeHash().ToArray().Should().Equal(
            new SourceTransformationInput(Name("renamed"), expected, "environment", "hash-repair-test")
            .ComputeHash().ToArray());
    }

    [Fact]
    public void Compositional_fingerprints_reuse_equal_shapes_and_distinguish_all_type_facts()
    {
        const string source = "type Choice a = Choice a\nroot n = n";
        var declarations = Parse(source);
        var cache = new SourceCompilationCaches.EnvironmentFingerprintCache();
        var noTypes = cache.Compute(declarations, null);
        cache.Compute(Parse(source).Reverse().ToImmutableDictionary(), null).Should().BeSameAs(noTypes);
        var empty = cache.Compute(declarations, ImmutableDictionary<DeclQualifiedName, FunctionTypeInfo>.Empty);
        empty.Should().NotBe(noTypes, "absent and explicitly empty inference inputs have different identities");

        var types =
            ImmutableDictionary<DeclQualifiedName, FunctionTypeInfo>.Empty.Add(
                Name("root"),
                new(new TypeInference.InferredType.IntType(), [new TypeInference.InferredType.IntType()]));

        var first = cache.Compute(declarations, types);

        cache.Compute(
            Parse(source),
            types.SetItem(
                Name("root"),
                new(new TypeInference.InferredType.IntType(), [new TypeInference.InferredType.IntType()])))
            .Should().BeSameAs(first, "parameter list identity is not semantic");

        var facts =
            new TypeInference.InferredType[]
            {
                new TypeInference.InferredType.FloatType(),
                new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Comparable),
                new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Number),
                new TypeInference.InferredType.ChoiceType(["PineLowering", "R1"], "Symbol", []),
                new TypeInference.InferredType.ChoiceType(["PineLowering", "R2"], "Symbol", [])
            };

        var hashes =
            facts.Select(
                type => cache.Compute(
                    declarations,
                    types.SetItem(
                        Name("root"),
                        new(type, [new TypeInference.InferredType.IntType()])))).Append(first).ToArray();

        hashes.Distinct().Should().HaveCount(hashes.Length);

        cache.Compute(
            declarations,
            types.SetItem(
                Name("root"),
                new(new TypeInference.InferredType.IntType(), [new TypeInference.InferredType.FloatType()])))
            .Should().NotBe(first);
    }

    [Fact]
    public void Unit_stubs_skip_operator_cache_work_but_retain_annotated_and_semantic_functions()
    {
        var declarations = Parse("stub = ()\nreal n = n + 1\nannotated : ()\nannotated = ()");
        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var lowered = LowerOperators(declarations, operators);
        lowered[Name("stub")].Should().BeSameAs(declarations[Name("stub")]);
        operators.Misses.Should().HaveCount(2);
        var actual = Model.FromCanonicalSyntax.Lower(lowered, semantic);
        actual.Program.Functions.Should().HaveCount(3);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(LowerOperators(declarations)));
    }

    [Fact]
    public void Exported_runner_bodies_bypass_stub_lowering_without_merging_captures_or_recursive_closures()
    {
        const string source =
            """
            make expected =
                let
                    even n = if n == 0 then expected else odd (n - 1)
                    odd n = if n == 0 then expected + 1 else even (n - 1)
                    check = Expect.equal expected
                    state = { wrap = Just, read = .answer, value = { answer = expected } }
                in
                \_ ->
                    Expect.all
                        [ \_ -> check (even 4)
                        , \_ -> Expect.equal (Just expected) (state.wrap (state.read state.value))
                        ]
                        ()
            suite =
                Test.describe "captures"
                    [ Test.test "one" (make 3)
                    , Test.test "two" (make 7)
                    , Test.fuzz Fuzz.int "fuzz" (\n -> Expect.equal n n)
                    ]
            """;

        var discovery =
            ElmTestDiscovery.DiscoverTests(
                LanguageServerTestingTests.Tree(source),
                executionSettings: new(123456, 10));

        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var caches = new SourceCompilationCaches(operators, semantic);
        using var session = new ElmTestRunCacheSession();

        for (var round = 0; round < 2; round++)
        {
            var entries = discovery.PrepareExecution(compilationCaches: caches);
            entries.Should().HaveCount(3);

            foreach (var test in discovery.Items.Where(item => entries.ContainsKey(item.Id)))
            {
                ElmTestRunner.ExecuteDiscoveredTest(test, entries[test.Id], session, default)
                    .Kind.Should().Be(CompletedTestKind.Passed);
            }

            var inputs =
                operators.Misses.Concat(operators.Hits).Concat(semantic.Misses).Concat(semantic.Hits).ToArray();

            inputs.Should().NotBeEmpty();

            inputs.Where(
                input => input.Syntax is Declaration.FunctionDeclaration
                {
                    Function.Signature: null,
                    Function.Declaration.Arguments.Count: 0,
                    Function.Declaration.Expression: Core.Elm.Syntax.Abstract.Expression.UnitExpr
                }).Should().BeEmpty();

            if (round is 1)
            {
                operators.Misses.Should().BeEmpty();
                semantic.Misses.Should().BeEmpty();
            }

            operators.Reset();
            semantic.Reset();
        }
    }

    [Fact]
    public void Body_only_edits_reuse_unmodified_functions_and_preserve_fresh_results()
    {
        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var original = Parse("helper n = n + 1\nroot n = helper n\nunrelated n = n + 100");
        var lowered = LowerOperators(original, operators);
        _ = Model.FromCanonicalSyntax.Lower(lowered, semantic);
        operators.Reset();
        semantic.Reset();

        var edited = Parse("helper n = n + 2\nroot n = helper n\nunrelated n = n + 100");
        var editedLowered = LowerOperators(edited, operators);
        var actual = Model.FromCanonicalSyntax.Lower(editedLowered, semantic);
        var expected = Model.FromCanonicalSyntax.Lower(LowerOperators(edited));
        AssertEquivalent(actual, expected);
        operators.Hits.Should().HaveCount(2);
        semantic.Hits.Should().HaveCount(2);
        semantic.Misses.Should().ContainSingle();
    }

    [Theory]
    [InlineData("helper n = { x = n }", "helper n = { x = n, y = 0 }", "root n = (helper n).x")]
    [InlineData("helper : Int -> Int\nhelper n = n", "helper : Float -> Float\nhelper n = n", "root n = helper n")]
    [InlineData("type alias Shape = { x : Int }", "type alias Shape = { x : Int, y : Int }",
        "root : Shape -> Int\nroot record = record.x")]
    [InlineData("type Shape = Shape Int", "type Shape = Shape Float", "root n = Shape n")]
    public void Consulted_semantic_environment_changes_invalidate_unchanged_callers(
        string before, string after, string root)
    {
        var cache = new TrackingCache<LoweredSourceFunction>();

        _ =
            Model.FromCanonicalSyntax.Lower(
                Parse(before + "\n" + root),
                cache,
                new HashSet<DeclQualifiedName> { Name("root") });

        cache.Reset();
        var edited = Parse(after + "\n" + root);
        var selected = new HashSet<DeclQualifiedName> { Name("root") };
        var actual = Model.FromCanonicalSyntax.Lower(edited, cache, selected);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(edited, selectedFunctions: selected));
        cache.Misses.Should().ContainSingle();
        cache.Hits.Should().BeEmpty();
    }

    [Fact]
    public void Unrelated_roots_and_type_metadata_do_not_invalidate_selected_functions()
    {
        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        const string source = "helper : Int -> Int\nhelper n = n + 1\nroot n = helper n";
        var first = LowerOperators(Parse(source), operators);
        var selected = new HashSet<DeclQualifiedName> { Name("root") };
        _ = Model.FromCanonicalSyntax.Lower(first, semantic, selected);
        var input = semantic.Misses.Single();
        operators.Reset();
        semantic.Reset();
        var extended = LowerOperators(Parse(source + "\nnewFunction n = n\ntype Other = Other Int"), operators);
        var actual = Model.FromCanonicalSyntax.Lower(extended, semantic, selected);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(extended, selectedFunctions: selected));
        operators.Hits.Should().HaveCount(2);
        semantic.Misses.Should().BeEmpty();
        semantic.Hits.Single().Should().BeSameAs(input);
    }

    [Fact]
    public void Namespace_and_binding_renames_share_anonymous_inputs_and_rebind_closures_and_evidence()
    {
        const string Source =
            """
            type Choice = Present Int | Absent
            type alias Shape = { x : Choice }
            helper : Shape -> Choice
            helper r = r.x
            build = Shape (Present 3)
            match : Choice -> Int
            match choice =
                case choice of
                    Present n -> n
                    Absent -> 0
            root record other = ( \n -> helper record, other.unknown, Present 3 )
            """;

        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var original = Parse(Source);
        _ = Model.FromCanonicalSyntax.Lower(LowerOperators(original, operators), semantic);
        var originalInputs = semantic.Misses.ToArray();
        var values = semantic.Values.ToArray();
        var operatorValues = operators.Values.ToArray();
        operators.Reset();
        semantic.Reset();

        DeclQualifiedName Rename(DeclQualifiedName name) =>
            name.Namespaces is ["Test"]
            ?
            DeclQualifiedName.Create(
                ["Fork", "DifferentPackage", "Version42"],
                name.DeclName is "root" ? "entry" : name.DeclName is "helper" ? "callee" : name.DeclName)
            :
            name;

        var renamed =
            original.ToImmutableDictionary(
                entry => Rename(entry.Key),
                entry =>
                {
                    var declaration =
                        Core.Elm.ElmCompilerInDotnet.DeclarationUnits.Canonicalize.MapNames(entry.Value, Rename);

                    if (declaration is Declaration.FunctionDeclaration function)
                    {
                        var name = Rename(entry.Key).DeclName;

                        return
                            (Declaration)new Declaration.FunctionDeclaration(
                                new(
                                    function.Function.Signature is { } signature
                                    ?
                                    signature with { Name = name }
                                    :
                                    null,
                                    function.Function.Declaration with { Name = name }));
                    }

                    return declaration;
                });

        var actual = Model.FromCanonicalSyntax.Lower(LowerOperators(renamed, operators), semantic);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(LowerOperators(renamed)));
        operators.Misses.Should().BeEmpty();
        semantic.Misses.Should().BeEmpty();
        semantic.Hits.Should().OnlyContain(input => originalInputs.Any(previous => ReferenceEquals(previous, input)));

        foreach (var input in originalInputs.Concat(semantic.Hits).Concat(operators.Hits))
        {
            var document = JsonSerializer.Serialize(input, ElmSyntaxAbstractJson.JsonSerializerOptions);

            document.Should()
                .NotContain("Test").And.NotContain("Fork").And.NotContain("DifferentPackage").And
                .NotContain("Version42");
        }

        foreach (var value in values)
        {
            var document =
                System.Text.Encoding.UTF8.GetString(SourceCompilationCaches.SemanticCodec.Encode(value).Span);

            document.Should()
                .NotContain("Test").And.NotContain("Fork").And.NotContain("DifferentPackage").And
                .NotContain("Version42");
        }

        foreach (var value in operatorValues)
        {
            var document = ElmSyntaxAbstractJson.ToJsonString(value);

            document.Should()
                .NotContain("Test").And.NotContain("Fork").And.NotContain("DifferentPackage").And
                .NotContain("Version42");
        }

        var nodes = new HashSet<Model.Expression>(ReferenceEqualityComparer.Instance);

        foreach (var function in actual.Program.Functions.Values)
            Visit(function.Body);

        actual.GenericAccessEvidence.Keys.Should().OnlyContain(node => nodes.Contains(node));

        void Visit(Model.Expression expression)
        {
            nodes.Add(expression);

            foreach (var child in Model.ExpressionTransform.Children(expression))
                Visit(child);
        }
    }

    [Fact]
    public void Equal_nominal_shapes_keep_distinct_relative_bindings_and_identity_relationships()
    {
        const string Types = "type First = First Int\ntype Second = Second Int\n";
        const string Body = "\nroot a b = ( a.x, b.x )";
        var cache = new TrackingCache<LoweredSourceFunction>();
        var selected = new HashSet<DeclQualifiedName> { Name("root") };
        var declarations = Parse(Types + "root : First -> Second -> ( Int, Int )" + Body);
        var actual = Model.FromCanonicalSyntax.Lower(declarations, cache, selected);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(declarations, selectedFunctions: selected));

        var nominalTypes =
            cache.Values.Single().Evidence.Values
            .Select(item => (TypeInference.InferredType.ChoiceType)item.SubjectType).ToArray();

        nominalTypes.Should().HaveCount(2);
        nominalTypes.Select(type => string.Join(".", type.ModuleName)).Distinct().Should().HaveCount(2);
        nominalTypes.Should().OnlyContain(type => type.TypeName == "Symbol");

        cache.Reset();
        var unified = Parse(Types + "root : First -> First -> ( Int, Int )" + Body);
        var changed = Model.FromCanonicalSyntax.Lower(unified, cache, selected);
        AssertEquivalent(changed, Model.FromCanonicalSyntax.Lower(unified, selectedFunctions: selected));
        cache.Misses.Should().ContainSingle();
        cache.Hits.Should().BeEmpty();
    }

    [Theory]
    [InlineData("String")]
    [InlineData("Char")]
    public void Native_constructor_bindings_remain_intrinsic_in_inputs_models_and_returned_metadata(string module)
    {
        var source = Parse("type Native = Native Int\nroot n = n");
        var choice = ((Declaration.ChoiceTypeDeclaration)source[Name("Native")]).TypeDeclaration;
        var constructor = DeclQualifiedName.Create([module], module);

        var metadata =
            new Declaration.ChoiceTypeDeclaration(
                new(
                    module,
                    choice.Generics,
                    [ValueConstructor.Create(module, choice.Constructors[0].Arguments)]));

        var root = ((Declaration.FunctionDeclaration)source[Name("root")]).Function;

        var function =
            new Declaration.FunctionDeclaration(
                new(
                    root.Signature,
                    new FunctionImplementation(
                        root.Declaration.Name,
                        root.Declaration.Arguments,
                        new Core.Elm.Syntax.Abstract.Expression.Application(
                            new Core.Elm.Syntax.Abstract.Expression.Identifier(constructor),
                            [
                            new Core.Elm.Syntax.Abstract.Expression.Identifier(DeclQualifiedName.FromString("n"))
                            ]))));

        var declarations =
            ImmutableDictionary<DeclQualifiedName, Declaration>.Empty
            .Add(constructor, metadata).Add(Name("root"), function);

        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var lowered = LowerOperators(declarations, operators);
        var actual = Model.FromCanonicalSyntax.Lower(lowered, semantic);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(LowerOperators(declarations)));
        lowered[constructor].Should().BeSameAs(metadata);
        actual.Program.SourceDeclarations[constructor].Should().BeSameAs(metadata);

        var application = (Model.Expression.Application)semantic.Values.Single().Function.Body;
        var reference = (Model.Expression.ReferenceExpression)application.Function;
        ((Model.Reference.Constructor)reference.Reference).Name.Should().Be(constructor);
        var syntax = (Declaration.FunctionDeclaration)operators.Misses.Single().Syntax;

        var syntaxApplication =
            (Core.Elm.Syntax.Abstract.Expression.Application)syntax.Function.Declaration.Expression;

        ((Core.Elm.Syntax.Abstract.Expression.Identifier)syntaxApplication.Function).QualifiedName.Should().Be(
            constructor);
    }

    [Fact]
    public void Unused_nested_let_bindings_preserve_callee_constraints_for_anonymous_function_slots()
    {
        var declarations =
            Parse(
                """
                root x =
                    let
                        xa = beta (x + 7)
                        xb = beta (x + 13)
                    in
                    let
                        xc = beta (xa + xb)
                    in
                    x * 21

                beta : Int -> Int
                beta x = x
                """);

        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var lowered = LowerOperators(declarations, operators);
        var implementation = ((Declaration.FunctionDeclaration)lowered[Name("root")]).Function.Declaration.Expression;
        var outer = (Core.Elm.Syntax.Abstract.Expression.LetExpression)implementation;
        var inner = (Core.Elm.Syntax.Abstract.Expression.LetExpression)outer.Expression;
        var multiplication = (Core.Elm.Syntax.Abstract.Expression.Application)inner.Expression;

        ((Core.Elm.Syntax.Abstract.Expression.Identifier)multiplication.Function).QualifiedName
            .Should().Be(DeclQualifiedName.Create(["Pine_builtin"], "int_mul"));

        var actual = Model.FromCanonicalSyntax.Lower(lowered, semantic);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(LowerOperators(declarations)));

        References(actual.Program.Functions[new Model.FunctionId.Source(Name("root"))].Body)
            .Should().Contain(DeclQualifiedName.Create(["Pine_builtin"], "int_mul"));

        semantic.Misses.Should().OnlyContain(input => input.Name.DeclName == "symbol");

        operators.Reset();
        semantic.Reset();
        var reused = Model.FromCanonicalSyntax.Lower(LowerOperators(declarations, operators), semantic);
        AssertEquivalent(reused, actual);
        operators.Misses.Should().BeEmpty();
        semantic.Misses.Should().BeEmpty();

        static IEnumerable<DeclQualifiedName> References(Model.Expression expression)
        {
            if (expression is Model.Expression.ReferenceExpression { Reference: Model.Reference.Native native })
                yield return native.Name;

            foreach (var child in Model.ExpressionTransform.Children(expression))
            {
                foreach (var name in References(child))
                    yield return name;
            }
        }
    }

    [Fact]
    public void Local_signatures_and_lifted_binding_constraints_survive_name_normalization()
    {
        var declarations =
            Parse(
                """
                root record =
                    let
                        local : Int -> Int
                        local n = n * 21
                    in
                    ( local 2, record.x )
                """);

        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var actual = Model.FromCanonicalSyntax.Lower(LowerOperators(declarations, operators), semantic);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(LowerOperators(declarations)));

        var syntax = ((Declaration.FunctionDeclaration)operators.Misses.Single().Syntax).Function;
        var let = (Core.Elm.Syntax.Abstract.Expression.LetExpression)syntax.Declaration.Expression;
        ((LetDeclaration.LetFunction)let.Declarations.Single()).Function.Signature!.Name.Should().Be("local");

        var body = (Model.Expression.Let)actual.Program.Functions[new Model.FunctionId.Source(Name("root"))].Body;
        var lambda = (Model.Expression.Lambda)body.Bindings.Single().Value;
        var application = (Model.Expression.Application)lambda.Body;
        var reference = (Model.Expression.ReferenceExpression)application.Function;

        ((Model.Reference.Native)reference.Reference).Name.Should().Be(
            DeclQualifiedName.Create(["Pine_builtin"], "int_mul"));

        lambda.Id.SourceDeclaration.Should().Be(Name("root"));
    }

    [Fact]
    public void Type_and_constructor_bindings_with_the_same_spelling_use_distinct_relative_slots()
    {
        var declarations =
            Parse(
                """
                type Runner = Runnable
                type Runnable = Thunk Int

                root : Runnable -> Runner -> Int
                root runnable runner =
                    case runnable of
                        Thunk n ->
                            case runner of
                                Runnable -> n
                """);

        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var actual = Model.FromCanonicalSyntax.Lower(LowerOperators(declarations, operators), semantic);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(LowerOperators(declarations)));
        var constructors = new List<DeclQualifiedName>();
        Visit(semantic.Values.Single().Function.Body);
        constructors.Should().HaveCount(2);
        constructors.Select(name => string.Join(".", name.Namespaces)).Distinct().Should().HaveCount(2);
        constructors.Should().OnlyContain(name => name.DeclName == "C0");

        foreach (var input in operators.Misses.Concat(semantic.Misses))
        {
            ElmSyntaxAbstractJson.ToJsonString(input.Syntax).Should().NotContain("PineLoweringType").And.NotContain(
                "Test.");
        }

        operators.Reset();
        semantic.Reset();
        var reused = Model.FromCanonicalSyntax.Lower(LowerOperators(declarations, operators), semantic);
        AssertEquivalent(reused, actual);
        operators.Misses.Should().BeEmpty();
        semantic.Misses.Should().BeEmpty();

        void Visit(Model.Expression expression)
        {
            if (expression is Model.Expression.Match match)
            {
                foreach (var branch in match.Branches)
                {
                    if (branch.Pattern is Model.Pattern.NamedPattern named)
                        constructors.Add(named.Name);
                }
            }

            foreach (var child in Model.ExpressionTransform.Children(expression))
                Visit(child);
        }
    }

    [Fact]
    public void Internal_type_questions_do_not_shadow_legal_source_module_names()
    {
        var source =
            Parse(
                """
                type Container = Symbol Int
                root : Container -> Int
                root value =
                    case value of
                        Symbol n -> n
                """);

        DeclQualifiedName Rename(DeclQualifiedName name) =>
            name.Namespaces is ["Test"] ? DeclQualifiedName.Create(["PineLoweringType", "R0"], name.DeclName) : name;

        var declarations =
            source.ToImmutableDictionary(
                entry => Rename(entry.Key),
                entry => Core.Elm.ElmCompilerInDotnet.DeclarationUnits.Canonicalize.MapNames(entry.Value, Rename));

        var actual =
            Model.FromCanonicalSyntax.Lower(
                LowerOperators(declarations, SourceCompilationCaches.CreateInMemory().OperatorLowering),
                SourceCompilationCaches.CreateInMemory().SemanticFunctions);

        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(LowerOperators(declarations)));
    }

    [Theory]
    [InlineData("Tree Int Int")]
    [InlineData("Wrapper")]
    public void Runtime_collection_tags_preserve_nonprimitive_equality_without_source_names(string collection)
    {
        const string types =
            """
            type Colour = Red | Black
            type Tree k v = RBEmpty_elm_builtin | RBNode_elm_builtin Colour k v (Tree k v) (Tree k v)
            type Wrapper = Set_elm_builtin (Tree Int Int)
            """;

        var root = $"\nroot : List ({collection}) -> List ({collection}) -> Bool\nroot a b = a == b";
        var cache = new TrackingCache<Declaration>();
        var original = LowerOperators(Parse(types + root), cache);
        FunctionReference(original[Name("root")]).Should().Be(DeclQualifiedName.Create(["Basics"], "eq"));

        var ordinaryTypes =
            types.Replace("RBEmpty_elm_builtin", "Empty", StringComparison.Ordinal)
            .Replace("RBNode_elm_builtin", "Node", StringComparison.Ordinal)
            .Replace("Set_elm_builtin", "Wrap", StringComparison.Ordinal);

        cache.Reset();
        var ordinary = LowerOperators(Parse(ordinaryTypes + root), cache);
        FunctionReference(ordinary[Name("root")]).Should().Be(DeclQualifiedName.Create(["Pine_builtin"], "equal"));
        cache.Misses.Should().ContainSingle();
        cache.Hits.Should().BeEmpty();

        static DeclQualifiedName FunctionReference(Declaration declaration)
        {
            var function = ((Declaration.FunctionDeclaration)declaration).Function;
            var application = (Core.Elm.Syntax.Abstract.Expression.Application)function.Declaration.Expression;
            return ((Core.Elm.Syntax.Abstract.Expression.Identifier)application.Function).QualifiedName;
        }
    }

    [Fact]
    public void Ordinary_types_named_Dict_have_no_native_equality_privileges_and_share_lowering_inputs()
    {
        var original =
            Parse("type Collection = Ordinary Int\nroot : Collection -> Collection -> Bool\nroot a b = a == b");

        var cache = new TrackingCache<Declaration>();
        var lowered = LowerOperators(original, cache);
        var input = cache.Misses.Single();

        DeclQualifiedName Rename(DeclQualifiedName name) =>
            name == Name("Collection")
            ?
            DeclQualifiedName.Create(["Dict"], "Dict")
            :
            name == Name("Ordinary") ? DeclQualifiedName.Create(["Dict"], "Ordinary") : name;

        var renamed =
            original.ToImmutableDictionary(
                entry => Rename(entry.Key),
                entry =>
                {
                    var declaration =
                        Core.Elm.ElmCompilerInDotnet.DeclarationUnits.Canonicalize.MapNames(entry.Value, Rename);

                    return
                        declaration is Declaration.ChoiceTypeDeclaration choice
                        ?
                        new Declaration.ChoiceTypeDeclaration(
                            new(
                                "Dict",
                                choice.TypeDeclaration.Generics,
                                choice.TypeDeclaration.Constructors))
                        :
                        declaration;
                });

        cache.Reset();
        var reused = LowerOperators(renamed, cache);
        cache.Misses.Should().BeEmpty();
        cache.Hits.Single().Should().BeSameAs(input);

        ((Declaration.FunctionDeclaration)reused[Name("root")]).Function.Declaration.Expression
            .Should().Be(((Declaration.FunctionDeclaration)lowered[Name("root")]).Function.Declaration.Expression);

        var application =
            (Core.Elm.Syntax.Abstract.Expression.Application)
            ((Declaration.FunctionDeclaration)reused[Name("root")]).Function.Declaration.Expression;

        ((Core.Elm.Syntax.Abstract.Expression.Identifier)application.Function).QualifiedName
            .Should().Be(DeclQualifiedName.Create(["Pine_builtin"], "equal"));
    }

    [Fact]
    public void Semantic_pruning_keeps_all_type_metadata_and_original_reference_classification()
    {
        var declarations =
            Parse(
                """
                type alias Person = { age : Int }
                type Choice = Yes | No
                helper n = n
                unused n = n
                root n = helper n
                """);

        var full = Model.FromCanonicalSyntax.Lower(declarations);

        var selected =
            Model.FromCanonicalSyntax.Lower(
                declarations,
                selectedFunctions:
                new HashSet<DeclQualifiedName>
                {
                    Name("root")
                });

        selected.Program.Functions.Should().ContainSingle();

        selected.Program.Functions[new Model.FunctionId.Source(Name("root"))]
            .Should().Be(full.Program.Functions[new Model.FunctionId.Source(Name("root"))]);

        selected.Program.SourceDeclarations.Should().Equal(full.Program.SourceDeclarations);
    }

    [Fact]
    public void Lowering_configuration_and_every_input_component_change_cryptographic_addresses()
    {
        var declarations = Parse("root a b = a + b");
        var cache = new TrackingCache<Declaration>();
        _ = BuiltinOperatorLowering.Apply(declarations, new(true, false), cache);
        var first = cache.Misses.Single();
        cache.Reset();
        _ = BuiltinOperatorLowering.Apply(declarations, new(false, true), cache);
        var second = cache.Misses.Single();

        var clonedSyntax =
            ElmSyntaxAbstractJson.FromJsonString<Declaration>(
                ElmSyntaxAbstractJson.ToJsonString(first.Syntax));

        first.ComputeHash().Length.Should().Be(32);
        (first with { Syntax = clonedSyntax }).ComputeHash().ToArray().Should().Equal(first.ComputeHash().ToArray());

        var hashes =
            new[]
            {
                first,
                second,
                first with { Name = Name("other") },
                first with { EnvironmentHash = "different" },
                first with { Syntax = Parse("root a b = a - b")[Name("root")] }
            }.Select(input => Convert.ToHexString(input.ComputeHash().Span)).ToArray();

        hashes.Distinct().Should().HaveCount(hashes.Length);
    }

    [Fact]
    public void Dictionary_order_and_reparsed_equal_syntax_do_not_change_input_hashes()
    {
        const string Source = "type Choice a = First a | Last\nhelper n = ( n, First n )\nroot r = r.x";
        var first = new TrackingCache<LoweredSourceFunction>();
        var second = new TrackingCache<LoweredSourceFunction>();
        _ = Model.FromCanonicalSyntax.Lower(Parse(Source), first);
        _ = Model.FromCanonicalSyntax.Lower(Parse(Source).Reverse().ToImmutableDictionary(), second);

        first.Misses.Select(input => Convert.ToHexString(input.ComputeHash().Span))
            .Should().Equal(second.Misses.Select(input => Convert.ToHexString(input.ComputeHash().Span)));
    }

    [Fact]
    public void Persistent_layers_restore_both_models_in_fresh_cache_instances()
    {
        var store = new FileStoreFromConcurrentDictionary();

        const string Source =
            """
            helper : Int -> Int
            helper n = n + 1
            root record = { x = record.x, call = \n -> helper n }
            """;

        var first = SourceCompilationCaches.CreateInMemory(store);

        var expected =
            Model.FromCanonicalSyntax.Lower(
                LowerOperators(Parse(Source), first.OperatorLowering),
                first.SemanticFunctions);

        var fresh = SourceCompilationCaches.CreateInMemory(store);
        var operators = new TrackingCache<Declaration>(fresh.OperatorLowering);
        var semantic = new TrackingCache<LoweredSourceFunction>(fresh.SemanticFunctions);
        var actual = Model.FromCanonicalSyntax.Lower(LowerOperators(Parse(Source), operators), semantic);
        operators.Misses.Should().BeEmpty();
        semantic.Misses.Should().BeEmpty();
        operators.Hits.Should().HaveCount(2);
        semantic.Hits.Should().HaveCount(2);
        AssertEquivalent(actual, expected);
        actual.GenericAccessEvidence.Should().NotBeEmpty();

        var nodes = new HashSet<Model.Expression>(ReferenceEqualityComparer.Instance);

        foreach (var function in actual.Program.Functions.Values)
            Visit(function.Body);

        actual.GenericAccessEvidence.Keys.Should().OnlyContain(node => nodes.Contains(node));

        Model.Expression Visit(Model.Expression expression)
        {
            nodes.Add(expression);
            _ = Model.ExpressionTransform.MapChildren(expression, Visit);
            return expression;
        }
    }

    [Fact]
    public void Persistent_fingerprint_format_does_not_read_previous_version_directories()
    {
        var store = new FileStoreFromConcurrentDictionary();
        var declarations = Parse("root n = n + 1");
        var original = SourceCompilationCaches.CreateInMemory(store);

        _ =
            Model.FromCanonicalSyntax.Lower(
                LowerOperators(declarations, original.OperatorLowering),
                original.SemanticFunctions);

        var paths = store.ListFilesInDirectory(ImmutableList<string>.Empty).ToArray();
        paths.Should().HaveCount(2);
        paths.Should().OnlyContain(path => path[0] == "operator-lowering-v6" || path[0] == "semantic-functions-v6");

        foreach (var path in paths)
        {
            store.SetFileContent(
                path.SetItem(0, path[0].Replace("-v6", "-v5", StringComparison.Ordinal)),
                store.GetFileContent(path) ?? throw new InvalidOperationException("Missing cached file."));

            store.DeleteFile(path);
        }

        var fresh = SourceCompilationCaches.CreateInMemory(store);
        var operators = new TrackingCache<Declaration>(fresh.OperatorLowering);
        var semantic = new TrackingCache<LoweredSourceFunction>(fresh.SemanticFunctions);
        _ = Model.FromCanonicalSyntax.Lower(LowerOperators(declarations, operators), semantic);
        operators.Misses.Should().ContainSingle();
        semantic.Misses.Should().ContainSingle();
    }

    [Fact]
    public void Semantic_codec_round_trips_nested_inferred_types_and_reference_bound_evidence()
    {
        var owner = new Model.FunctionId.Source(Name("root"));
        var binding = new Model.BindingId.Local(owner, 0);

        var expression =
            new Model.Expression.RecordAccess(
                new Model.Expression.ReferenceExpression(new Model.Reference.Local(binding)),
                "answer");

        TypeInference.InferredType type =
            new TypeInference.InferredType.OpenRecordType(
                "row",
                [
                    ("answer",
                    new TypeInference.InferredType.TupleType(
                        [
                        new TypeInference.InferredType.ChoiceType(
                            ["Module"],
                            "Choice",
                            [new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Comparable)]),
                        new TypeInference.InferredType.RecordType([("x", new TypeInference.InferredType.IntType())])
                        ]))
                ]);

        var value =
            new LoweredSourceFunction(
                new(new([new Model.Pattern.VarPattern(binding)]), expression),
                ImmutableDictionary<Model.BindingId, string>.Empty.Add(binding, "record"),
                ImmutableDictionary.Create<Model.Expression, GenericRecordAccessEvidence>(
                    ReferenceEqualityComparer.Instance).Add(
                    expression,
                    new(type, RecordOperationProvenance.NonIdentifierRecordExpression)));

        var codec = SourceCompilationCaches.SemanticCodec;
        var bytes = codec.Encode(value);
        var restored = codec.Decode(bytes);
        codec.Encode(restored).ToArray().Should().Equal(bytes.ToArray());
        restored.Evidence.Keys.Single().Should().BeSameAs(restored.Function.Body);
        restored.Function.Should().Be(value.Function);
    }

    [Fact]
    public void Corrupt_persistent_transformation_entries_are_explicit()
    {
        var store = new FileStoreFromConcurrentDictionary();
        var caches = SourceCompilationCaches.CreateInMemory(store);
        _ = LowerOperators(Parse("root n = n + 1"), caches.OperatorLowering);
        var path = store.ListFilesInDirectory(ImmutableList<string>.Empty).Single();
        store.SetFileContent(path, new byte[3]);
        var fresh = SourceCompilationCaches.CreateInMemory(store);
        Action read = () => LowerOperators(Parse("root n = n + 1"), fresh.OperatorLowering);
        read.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public async Task Session_trims_only_above_three_working_sets_to_two_including_configuration_variants()
    {
        var caches = SourceCompilationCaches.CreateInMemory();

        await using var session =
            new ElmTestRunCacheSession(
                precompiledLeaves: ImmutableDictionary<PineValue, Core.Runtime.PrecompiledLeaf>.Empty,
                compilationCaches: caches);

        var syntax = Parse("root n = n")[Name("root")];

        var inputs =
            Enumerable.Range(0, 7)
            .Select(index => new SourceTransformationInput(Name("root"), syntax, "environment", "variant-" + index))
            .ToArray();

        foreach (var input in inputs.Take(6))
            caches.OperatorLowering.Set(input, syntax);

        using (session.BeginCompilationInvocation())
        {
            caches.OperatorLowering.TryGetValue(inputs[0], out _).Should().BeTrue();
            caches.OperatorLowering.TryGetValue(inputs[1], out _).Should().BeTrue();
        }

        await session.WaitForCompilationMaintenanceAsync();
        caches.OperatorLifetime!.Count.Should().Be(6, "the trigger is strictly greater than 3 * used");
        caches.OperatorLowering.Set(inputs[6], syntax);

        using (session.BeginCompilationInvocation())
        {
            caches.OperatorLowering.TryGetValue(inputs[0], out _).Should().BeTrue();
            caches.OperatorLowering.TryGetValue(inputs[1], out _).Should().BeTrue();
        }

        await session.WaitForCompilationMaintenanceAsync();
        caches.OperatorLifetime.Count.Should().Be(4);
        caches.OperatorLowering.TryGetValue(inputs[0], out _).Should().BeTrue();
        caches.OperatorLowering.TryGetValue(inputs[1], out _).Should().BeTrue();
    }

    private sealed record FixedUsage(int EntryCount) : ICacheUsage;

    private sealed class BlockingLifetime : IMarkerCacheLifetime<long>
    {
        public int Count => 100;

        public int _workingSet = 10;

        public List<int> Budgets { get; } = [];

        public List<Func<long, CacheRetention>> Policies { get; } = [];

        public TaskCompletionSource Release { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);

        public long _marker;

        public ICacheUsage SetMarker(long marker)
        {
            _marker = marker;
            return new FixedUsage(_workingSet);
        }

        public Task TrimAsync(
            int budget,
            Func<long, CacheRetention> retention,
            CancellationToken cancellationToken = default)
        {
            Budgets.Add(budget);
            Policies.Add(retention);
            return Budgets.Count == 1 ? Release.Task : Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Concurrent_cleanup_coalesces_latest_budget_and_pins_active_invocations()
    {
        var lifetime = new BlockingLifetime();

        var caches =
            SourceCompilationCaches.CreateInMemory() with
            {
                OperatorLifetime = lifetime,
                SemanticLifetime = null
            };

        await using var session =
            new ElmTestRunCacheSession(
                precompiledLeaves: ImmutableDictionary<PineValue, Core.Runtime.PrecompiledLeaf>.Empty,
                compilationCaches: caches);

        using (session.BeginCompilationInvocation())
        {
        }

        lifetime.Budgets.Should().Equal(20);

        try
        {
            lifetime._workingSet = 2;

            using (session.BeginCompilationInvocation())
            {
                lifetime._marker.Should().Be(2);
                lifetime.Policies.Single()(lifetime._marker).Should().BeOfType<CacheRetention.Retain>();
            }

            lifetime.Budgets.Should().Equal([20], "cleanup remains single-flight");
            lifetime.Policies.Single()(2).Should().Be(new CacheRetention.Ranked(2));
            var completed = session.WaitForCompilationMaintenanceAsync();
            lifetime.Release.SetResult();
            await completed.WaitAsync(TimeSpan.FromSeconds(10));
            lifetime.Budgets.Should().Equal(20, 4);
        }
        finally
        {
            lifetime.Release.TrySetResult();
        }
    }

    [Fact]
    public void Cancelled_transformation_does_not_publish_a_result_and_can_retry()
    {
        var declarations = Parse("root n = n");
        var cache = new TrackingCache<LoweredSourceFunction>();
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        Action run = () => Model.FromCanonicalSyntax.Lower(declarations, cache, cancellationToken: cancellation.Token);
        run.Should().Throw<OperationCanceledException>();
        cache.Misses.Should().BeEmpty();
        _ = Model.FromCanonicalSyntax.Lower(declarations, cache);
        cache.Misses.Should().ContainSingle();
    }

    private static DeclQualifiedName Name(string name) => DeclQualifiedName.Create(["Test"], name);

    private static ImmutableDictionary<DeclQualifiedName, Declaration> Parse(string declarations)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText("module Test exposing (..)\n" + declarations + "\n")
            .Extract(error => throw new Exception(error.ToString()));

        var canonical =
            Canonicalization.CanonicalizeOrThrow([parsed])
            .Extract(error => throw new Exception(error));

        return
            ElmCompiler.FlattenModulesToDeclarationDictionary(
                [
                    .. canonical.Values.Select(
                        module =>
                        ConvertFromConcrete.FromFile(module.Extract(error => throw new Exception(error))))
                ]);
    }

    private static ImmutableDictionary<DeclQualifiedName, Declaration> LowerOperators(
        ImmutableDictionary<DeclQualifiedName, Declaration> declarations,
        ICache<SourceTransformationInput, Declaration>? cache = null) =>
        BuiltinOperatorLowering.Apply(declarations, cache).Extract(error => throw new Exception(error));

    private static void AssertEquivalent(Model.SourceLoweringResult actual, Model.SourceLoweringResult expected)
    {
        var options = Model.CompilerModelJson.CreateOptions();

        JsonSerializer.Serialize(actual.Program, options).Should().Be(
            JsonSerializer.Serialize(expected.Program, options));

        actual.GenericAccessEvidence.Count.Should().Be(expected.GenericAccessEvidence.Count);
    }
}
