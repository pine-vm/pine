using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using Shape = Pine.Core.Elm.ElmCompilerInDotnet.StructuralType;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class StructuredIdentityTests
{
    private static DeclQualifiedName Name(string module, string name) => DeclQualifiedName.Create([module], name);

    [Fact]
    public void Structured_identity_specializations_are_normalized_without_numbering_or_callers()
    {
        var original = new FunctionId.Source(Name("Main", "map"));
        var callback = new FunctionId.Lifted(new FunctionId.Source(Name("Main", "caller")), 0);
        ParameterConstraint first = new ParameterConstraint.KnownFunction(0, callback);

        ParameterConstraint second =
            new ParameterConstraint.ClosedRecord(
                1,
                Shape.ClosedRecord.Create(
                    ImmutableDictionary<string, Shape>.Empty
                    .Add("age", Shape.IntType.Instance).Add("name", Shape.StringType.Instance)));

        var combined = FunctionId.Specialized.Create(original, [first, second]);
        var nested = FunctionId.Specialized.Create(FunctionId.Specialized.Create(original, [second]), [first, first]);
        nested.Should().Be(combined);
        nested.GetHashCode().Should().Be(combined.GetHashCode());
        IdentityComparer.Instance.Compare(nested, combined).Should().Be(0);
        FunctionId.Specialized.Create(original, []).Should().Be(original);

        var conflicting =
            () => FunctionId.Specialized.Create(
                combined,
                [new ParameterConstraint.KnownFunction(0, new FunctionId.Source(Name("Main", "different")))]);

        conflicting.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Structured_identity_lifting_cannot_collide_with_source_names()
    {
        var (value, pipeline) =
            Evaluate(
                """
                module Main exposing (root)
                apply f x = f x
                root =
                    ( apply (\x -> x + 1) 40
                    , root__lifted__lambda1 1
                    )
                root__lifted__lambda1 x = x + 100
                """);

        value.Should().Be(
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(41),
                IntegerEncoding.EncodeSignedInteger(101)
                ]));

        var functions = pipeline.CompilerProgram!.Functions;
        functions.Keys.Should().Contain(new FunctionId.Source(Name("Main", "root__lifted__lambda1")));
        functions.Keys.OfType<FunctionId.Lifted>().Should().ContainSingle();

        foreach (var function in functions.Values)
        {
            IR.ExpressionTraversal.Enumerate(ToIntermediateRepresentation.Convert(function.Body))
                .OfType<IR.Expression.Identifier>().Should().OnlyContain(expression => expression.Reference != null);
        }
    }

    [Fact]
    public void Structured_identity_same_spelling_in_different_modules_has_independent_lifted_sites()
    {
        var sources =
            new[]
            {
                """
                module Main exposing (root)
                import First
                import Second
                root = ( First.make 10 2, Second.make 20 3 )
                """,
                """
                module First exposing (make)
                make captured = \argument -> captured + argument
                """,
                """
                module Second exposing (make)
                make captured = \argument -> captured * argument
                """
            };

        var (value, pipeline) = Evaluate(sources);

        value.Should().Be(
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(12),
                IntegerEncoding.EncodeSignedInteger(60)
                ]));

        var lifted = pipeline.CompilerProgram!.Functions.Keys.OfType<FunctionId.Lifted>().ToArray();
        lifted.Should().HaveCount(2);

        lifted.Select(id => id.SourceDeclaration).Should().BeEquivalentTo(
            [
            Name("First", "make"),
            Name("Second", "make")
            ]);
    }

    [Fact]
    public void Structured_identity_mutually_recursive_local_functions_forward_transitive_captures()
    {
        var (value, _) =
            Evaluate(
                """
                module Main exposing (root)
                calculate seed =
                    let
                        even n =
                            if n == 0 then seed else odd (n - 1)
                        odd n =
                            if n == 0 then 0 else even (n - 1)
                    in
                    odd 5
                root = calculate 42
                """);

        value.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Structured_identity_nested_closures_preserve_multiple_captures_and_case_bindings()
    {
        var (value, _) =
            Evaluate(
                """
                module Main exposing (root)
                make first second =
                    \argument ->
                        case argument of
                            Just extra ->
                                (\another -> first + second + extra + another) 4
                            Nothing ->
                                0
                root = make 10 20 (Just 8)
                """);

        value.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Structured_identity_lifting_is_idempotent_and_unrelated_declarations_do_not_renumber_bindings()
    {
        var first =
            Lower(
                """
                module Main exposing (root)
                root captured = \argument -> captured + argument
                """);

        var second =
            Lower(
                """
                module Main exposing (root, unrelated)
                unrelated value = (\x -> x * 3) value
                root captured = \argument -> captured + argument
                """,
                [Name("Main", "root"), Name("Main", "unrelated")]);

        foreach (var (id, function) in first.Functions)
            second.Functions[id].Should().Be(function);

        LambdaLifting.Lift(first).Functions.Should().BeEquivalentTo(first.Functions);

        LambdaLifting.Lift(first with { BindingNames = [] })
            .Functions.Should().BeEquivalentTo(first.Functions);
    }

    [Fact]
    public void Structured_identity_conversion_resolves_lexical_shadowing_by_binding_not_spelling()
    {
        var file =
            ElmSyntaxParser.ParseModuleText(
                """
                module Main exposing (root)
                root x = (\x -> x) x
                """).Extract(error => throw new InvalidOperationException(error.ToString()));

        var declaration = ConvertFromConcrete.FromDeclaration(file.Declarations.Single().Value);

        var converted =
            FromCanonicalSyntax.Convert(
                ImmutableDictionary<DeclQualifiedName, Declaration>.Empty.Add(Name("Main", "root"), declaration));

        var root = converted.Functions[new FunctionId.Source(Name("Main", "root"))];
        var outer = ((Model.Pattern.VarPattern)root.Parameters.Single()).Binding;
        var application = (Model.Expression.Application)root.Body;
        var lambda = (Model.Expression.Lambda)application.Function;
        var inner = ((Model.Pattern.VarPattern)lambda.Parameters.Single()).Binding;
        inner.Should().NotBe(outer);
        ((Reference.Local)((Model.Expression.ReferenceExpression)lambda.Body).Reference).Binding.Should().Be(inner);

        ((Reference.Local)((Model.Expression.ReferenceExpression)application.Arguments.Single()).Reference).Binding
            .Should().Be(outer);
    }

    [Fact]
    public void Structured_identity_expression_values_compare_structurally_across_independent_conversion()
    {
        const string Source =
            """
            module Main exposing (root)
            root captured = \argument -> ( captured, argument, [ captured ] )
            """;

        var first = Lower(Source);
        var second = Lower(Source);

        foreach (var (id, function) in first.Functions)
        {
            second.Functions[id].Should().Be(function);
            second.Functions[id].GetHashCode().Should().Be(function.GetHashCode());
        }
    }

    [Fact]
    public void Structured_identity_SCC_memoization_misses_only_changed_declaration_and_dependents()
    {
        const string Source =
            """
            module Main exposing (root, unrelated)
            shared x = x + 1
            left x = shared x
            right x = x * 2
            root x = ( left x, right x )
            unrelated x = x - 3
            """;

        var roots = new[] { Name("Main", "root"), Name("Main", "unrelated") };
        var first = Lower(Source, roots);
        var second = Lower(Source.Replace("x + 1", "x + 2", StringComparison.Ordinal), roots);
        var cache = new Dictionary<SccCompilationInput, ImmutableDictionary<FunctionId, CompiledCallable>>();
        var misses = new List<string>();
        var hits = new List<string>();

        Result<CompilationError, ImmutableDictionary<FunctionId, CompiledCallable>> Worker(SccCompilationInput input)
        {
            var name = input.Members.Single().Key.SourceDeclaration.DeclName;

            if (cache.TryGetValue(input, out var cached))
            {
                hits.Add(name);
                return cached;
            }

            misses.Add(name);
            var result = Compilation.CompileScc(input);

            if (result.IsOkOrNull() is { } output)
                cache.Add(input, output);

            return result;
        }

        Compilation.Compile(first, compileScc: Worker)
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        misses.Should().HaveCount(5);
        misses.Clear();

        Compilation.Compile(second, compileScc: Worker)
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        misses.Should().BeEquivalentTo(["shared", "left", "root"]);
        hits.Should().BeEquivalentTo(["right", "unrelated"]);
    }

    [Fact]
    public void Structured_identity_repeated_semantic_AST_reuses_every_SCC_even_when_source_locations_move()
    {
        const string Source =
            """
            module Main exposing (root)
            root captured = \argument -> captured + argument
            """;

        var first = Lower(Source);
        var second = Lower(Source.Replace("root captured", "\n-- moved\nroot captured", StringComparison.Ordinal));
        var requests = new HashSet<SccCompilationInput>();
        var repeated = 0;

        Result<CompilationError, ImmutableDictionary<FunctionId, CompiledCallable>> Worker(SccCompilationInput input)
        {
            if (!requests.Add(input))
                repeated++;

            return Compilation.CompileScc(input);
        }

        Compilation.Compile(first, compileScc: Worker)
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        Compilation.Compile(second, compileScc: Worker)
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        repeated.Should().Be(first.Functions.Count);
    }

    [Fact]
    public void Structured_identity_binding_spellings_are_diagnostic_provenance_not_semantic_inputs()
    {
        var first = Lower("module Main exposing (root)\nroot captured = \\argument -> captured + argument");
        var second = Lower("module Main exposing (root)\nroot outside = \\inside -> outside + inside");

        foreach (var (id, function) in first.Functions)
            second.Functions[id].Should().Be(function);

        first.BindingNames.Values.Should().NotBeEquivalentTo(second.BindingNames.Values);
    }

    [Fact]
    public void Structured_record_temporaries_cannot_collide_with_source_names_and_survive_lifting()
    {
        var (value, pipeline) =
            Evaluate(
                """
                module Main exposing (root)
                select : Int -> { name : Int } -> Int
                select record__projection_0 { name } =
                    (\extra -> record__projection_0 + name + extra) 2
                root = select 10 { name = 30 }
                """);

        value.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
        var function = pipeline.CompilerProgram.Functions[new FunctionId.Source(Name("Main", "select"))];
        var pattern = function.Parameters[1].Should().BeOfType<Model.Pattern.VarPattern>().Subject;
        pattern.Binding.Should().BeOfType<BindingId.Temporary>();
        typeof(Model.Pattern.VarPattern).GetProperty("Name").Should().BeNull();
        ProgramValidation.Validate(pipeline.CompilerProgram);
    }

    [Theory]
    [InlineData("let\n        _ = \\x -> x\n    in\n    42")]
    [InlineData("let\n        (count as again) = \\n -> if n == 0 then 42 else again (n - 1)\n    in\n    count 3")]
    public void Lambda_values_in_wildcard_and_alias_bindings_do_not_require_a_single_named_slot(string body)
    {
        var (value, _) = Evaluate("module Main exposing (root)\nroot =\n    " + body);
        value.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Output_names_and_rendering_are_separate_from_semantic_identity_and_compilation()
    {
        var program =
            Lower(
                """
                module Main exposing (root)
                root captured = \argument -> captured + argument
                """);

        var names = DisplayNames.Create(program);
        names.Functions.Values.Distinct(StringComparer.Ordinal).Should().HaveCount(program.Functions.Count);

        names.Functions.Values.Concat(names.Bindings.Values).Should()
            .OnlyContain(name => name.All(character => char.IsAsciiLetterOrDigit(character) || character == '_'));

        names.Functions.Values.Intersect(names.Bindings.Values, StringComparer.Ordinal).Should().BeEmpty();
        var rendered = Rendering.Render(program, names);
        Rendering.Render(program).Should().Be(rendered);
        rendered.Should().Contain("function_Main_root").And.Contain("lifted").And.Contain("binding_captured");

        var renamed =
            program with { BindingNames = program.BindingNames.ToImmutableDictionary(entry => entry.Key, _ => "const") };

        Rendering.Render(renamed).Should().NotBe(rendered);

        var first =
            Compilation.Compile(program).Extract(error => throw new InvalidOperationException(error.ToString()));

        var second =
            Compilation.Compile(renamed).Extract(error => throw new InvalidOperationException(error.ToString()));

        foreach (var (id, artifact) in first)
            second[id].Should().Be(artifact);
    }

    [Fact]
    public void Output_names_are_unique_even_when_source_names_and_generated_labels_overlap()
    {
        var program =
            Lower(
                """
                module Main exposing (root, lifted, root_1)
                lifted x = x
                root_1 x = x
                root temporary = \another -> lifted (root_1 (temporary + another))
                """,
                [Name("Main", "root"), Name("Main", "lifted"), Name("Main", "root_1")]);

        var names = DisplayNames.Create(program);
        var all = names.Functions.Values.Concat(names.Bindings.Values).ToArray();
        all.Distinct(StringComparer.Ordinal).Should().HaveCount(all.Length);
    }

    private static Program Lower(string source, IReadOnlyList<DeclQualifiedName>? roots = null) =>
        ElmCompiler.LowerToCompilerModel(
            TestCase.DefaultAppWithoutPackages([source]).AsFileTree(),
            roots ?? [Name("Main", "root")])
        .Extract(error => throw new InvalidOperationException(error)).CompilerProgram!;

    private static (PineValue Value, CompilationPipelineStageResults Pipeline) Evaluate(params string[] sources)
    {
        var compilation =
            ElmCompiler.CompileInteractiveEnvironment(
                TestCase.DefaultAppWithoutPackages(sources).AsFileTree(),
                [Name("Main", "root")])
            .Extract(error => throw new InvalidOperationException(error));

        var evaluated =
            ElmSourceCompilation.EvaluateZeroParameterRoots(
                compilation.compiledEnvValue,
                DirectInterpreter.WithLocalEvalCache(new PineVMParseCache()))
            .Extract(error => throw new InvalidOperationException(error));

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(evaluated)
            .Extract(error => throw new InvalidOperationException(error));

        return (environment.Modules.Single().moduleContent.FunctionDeclarations["root"], compilation.pipelineStageResults);
    }
}
