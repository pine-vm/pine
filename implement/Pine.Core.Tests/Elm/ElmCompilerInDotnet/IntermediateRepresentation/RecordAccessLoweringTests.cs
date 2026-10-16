using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class RecordAccessLoweringTests
{
    [Fact]
    public void Closed_record_updates_reconstruct_the_sorted_physical_tuple()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                select : { z : Int, a : Int } -> { z : Int, a : Int }
                select record = { record | z = record.a }
                """);

        var body =
            ((IR.Declaration.FunctionDeclaration)lowered.Declarations[Name("select")]).Function.Declaration.Expression;

        body.Should().BeOfType<IR.Expression.TupledExpression>();
        Expressions(lowered, "select").OfType<IR.Expression.RecordUpdateExpression>().Should().BeEmpty();

        Expressions(lowered, "select").OfType<IR.Expression.ProjectItem>().Select(projection => projection.Index)
            .Should().Equal(0, 1, 2, 3, 2);
    }

    [Theory]
    [InlineData(true, true)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(false, false)]
    public void Runtime_record_updates_agree_with_explicit_IR_in_both_optimization_modes(
        bool closed,
        bool disableOptimization)
    {
        var type = closed ? "{ age : Int, name : String }" : "{ r | name : String }";

        var (environment, pipeline) =
            ElmCompilerTestHelper.CompileElmModules(
                [
                "module Test exposing (..)\nselect : " + type + " -> " + type +
                "\nselect record = { record | name = \"Bob\" }"
                ],
                disableInlining: disableOptimization);

        var function =
            environment.Modules.Single(module => module.moduleName is "Test").moduleContent.FunctionDeclarations["select"];

        var updateExpression =
            new PineVMParseCache().ParseExpression(RecordRuntime.PineFunctionForRecordUpdateAsValue)
            .Extract(error => throw new InvalidOperationException(error));

        var vm =
            ElmCompilerTestHelper.PineVMForProfiling(
                _ => { },
                skipInlineForExpression: expression => expression == updateExpression);

        var count = 0;

        void Entered(in EnteredStackFrame frame)
        {
            if (frame.FrameExpression == updateExpression)
                count++;
        }

        var (result, _) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                function,
                new ElmValue.ElmRecord([("age", ElmValue.Integer(42)), ("name", ElmValue.StringInstance("Ada"))]),
                vm,
                Entered);

        result.Should().Be(
            new ElmValue.ElmRecord([("age", ElmValue.Integer(42)), ("name", ElmValue.StringInstance("Bob"))]));

        var declaration = (IR.Declaration.FunctionDeclaration)pipeline.DeclarationsForEmission[Name("select")];

        var updates =
            IR.ExpressionTraversal
            .Enumerate(declaration.Function.Declaration.Expression).OfType<IR.Expression.RecordUpdateExpression>();

        updates.Any().Should().Be(!closed);
        (count > 0).Should().Be(!closed);
    }

    [Fact]
    public void Record_update_replacements_keep_the_original_bindings()
    {
        var (environment, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [
                """
                module Test exposing (..)
                type Pair = Pair { a : Int, z : Int }
                select (Pair record) = { record | z = record.a, a = record.z }
                apply record = select (Pair record)
                """
                ],
                disableInlining: true);

        var function =
            environment.Modules.Single(module => module.moduleName is "Test").moduleContent.FunctionDeclarations["apply"];

        CoreLibraryTestHelper.ApplyAndProfileUnary(
            function,
            new ElmValue.ElmRecord([("a", ElmValue.Integer(3)), ("z", ElmValue.Integer(7))]),
            ElmCompilerTestHelper.PineVMForProfiling(_ => { })).value
            .Should().Be(new ElmValue.ElmRecord([("a", ElmValue.Integer(7)), ("z", ElmValue.Integer(3))]));
    }

    [Fact]
    public void Closed_record_reads_use_sorted_physical_value_slots()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                select : { z : a, a : b } -> ( b, a )
                select record = ( record.a, record.z )
                """);

        Expressions(lowered, "select").OfType<IR.Expression.ProjectItem>().Select(item => item.Index).Should().Equal(
            2,
            4);

        Generic(lowered).Should().BeEmpty();
    }

    [Theory]
    [InlineData("{ name = \"Ada\", age = 42 }.name")]
    [InlineData("(if True then { name = \"Ada\", age = 42 } else { name = \"Bob\", age = 40 }).name")]
    [InlineData("(makeRecord 42).name")]
    [InlineData(".name { age = 42, name = \"Ada\" }")]
    public void Closed_non_identifier_receivers_are_lowered(string expression)
    {
        var lowered =
            Lower(
                "module Test exposing (..)\n" +
                "makeRecord : Int -> { age : Int, name : String }\n" +
                "makeRecord age = { age = age, name = \"Ada\" }\n" +
                "select = " + expression);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Nested_named_aliases_are_expanded_at_each_read()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                type alias Person = { age : Int, name : String }
                type alias Wrapper = { inner : Person, z : Int }
                select : Wrapper -> String
                select wrapper = wrapper.inner.name
                """);

        Expressions(lowered, "select").OfType<IR.Expression.ProjectItem>().Select(item => item.Index).Should().Equal(
            4,
            2);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Imported_closed_aliases_do_not_depend_on_the_defining_modules_other_declarations()
    {
        var lowered =
            Lower(
                """
                module Types exposing (..)
                type alias Person = { age : Int, name : String }
                type alias Other = { other : Int }
                unrelated = 0
                """,
                """
                module Test exposing (..)
                import Types
                select : Types.Person -> String
                select person = person.name
                """);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Nested_imported_aliases_resolve_in_their_defining_module_not_the_callers_module()
    {
        var lowered =
            Lower(
                """
                module Types exposing (..)
                type alias Inner = { age : Int, name : String }
                type alias Outer = { inner : Inner }
                unrelated = 0
                """,
                """
                module Test exposing (..)
                import Types
                type alias Inner = { name : String, z : Int }
                select : Types.Outer -> String
                select outer = outer.inner.name
                """);

        Expressions(lowered, "select").OfType<IR.Expression.ProjectItem>().Select(projection => projection.Index)
            .Should().Equal(4, 2);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Open_imported_aliases_remain_generic_for_different_concrete_callers()
    {
        var lowered =
            Lower(
                """
                module Types exposing (..)
                type alias Named row = { row | name : String }
                type alias Other = { name : String }
                unrelated = 0
                """,
                """
                module Test exposing (..)
                import Types
                select : Types.Named row -> String
                select record = record.name
                first = select { name = "Ada" }
                second = select { age = 42, name = "Bob" }
                """);

        Expressions(lowered, "select").OfType<IR.Expression.ProjectItem>().Should().BeEmpty();

        Generic(lowered).Should().ContainSingle().Which.RecordOperationProvenance
            .Should().Be(RecordOperationProvenance.OpenRecordSourceType);
    }

    [Fact]
    public void Generic_lookup_results_preserve_closed_record_types_through_canonical_list_annotations()
    {
        var lowered =
            Lower(
                """
                module Maybe exposing (..)
                type Maybe a = Just a | Nothing
                """,
                """
                module Lookup exposing (..)
                get : key -> List.List ( key, value ) -> Maybe.Maybe value
                get key entries = Maybe.Nothing
                """,
                """
                module Test exposing (..)
                import Lookup
                type alias Person = { age : Int, name : String }
                select : List.List ( String, Person ) -> String
                select entries =
                    case Lookup.get "key" entries of
                        Maybe.Just person -> person.name
                        Maybe.Nothing -> ""
                """);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Implicit_record_constructors_do_not_exclude_imported_alias_only_modules()
    {
        var lowered =
            Lower(
                """
                module Types exposing (..)
                type alias Person = { age : Int, name : String }
                """,
                """
                module Test exposing (..)
                import Types
                select : Types.Person -> String
                select person = person.name
                """);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void An_inferred_record_producer_and_forward_let_binding_supply_layouts()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                makeRecord age = { age = age, name = "Ada" }
                select =
                    let
                        selected = record.name
                        record = makeRecord 42
                    in
                    selected
                """);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Closed_callers_do_not_close_a_polymorphic_record_helper()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                getName record = record.name
                first = getName { name = "Ada" }
                second = getName { age = 42, name = "Bob" }
                """);

        Expressions(lowered, "getName").Should().ContainSingle(expression => expression is IR.Expression.RecordAccess);

        Generic(lowered).Should().ContainSingle().Which.RecordOperationProvenance.Should().Be(
            RecordOperationProvenance.OpenRecordSourceType);
    }

    [Fact]
    public void Qualified_callees_without_signatures_do_not_borrow_another_modules_closed_alias()
    {
        var lowered =
            Lower(
                """
                module Other exposing (..)
                type alias Person = { name : String }
                consume : Person -> String
                consume person = person.name
                """,
                """
                module Producer exposing (..)
                consume record = record.name
                """,
                """
                module Test exposing (..)
                import Producer
                select record = ( Producer.consume record, record.name )
                first = select { name = "Ada" }
                second = select { age = 42, name = "Bob" }
                """);

        Expressions(lowered, "select").OfType<IR.Expression.ProjectItem>().Should().BeEmpty();

        Generic(lowered).Where(opportunity => opportunity.ContainingDecl == Name("select"))
            .Should().ContainSingle().Which.RecordOperationProvenance.Should().Be(RecordOperationProvenance.OpenRecordSourceType);
    }

    [Fact]
    public void Local_bindings_shadow_outer_record_types()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                select : { age : Int, name : String } -> String
                select record =
                    let
                        record = { name = "Ada" }
                    in
                    record.name
                """);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(2);
    }

    [Theory]
    [InlineData("select : { age : Int, name : String } -> String\nselect { name } = name")]
    [InlineData("select : { age : Int, name : String } -> String\nselect record = let { name } = record in name")]
    [InlineData("select : { age : Int, name : String } -> String\nselect record = case record of\n    { name } -> name")]
    [InlineData("select : ( Int, { age : Int, name : String } ) -> String\nselect ( _, { name } ) = name")]
    [InlineData("select : List { age : Int, name : String } -> String\nselect records = case records of\n    { name } :: _ -> name\n    [] -> \"\"")]
    [InlineData("select : { age : Int, name : String } -> String\nselect ({ name } as person) = name")]
    public void Closed_record_patterns_become_explicit_projections(string declaration)
    {
        var lowered = Lower("module Test exposing (..)\n" + declaration);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Nested_generic_constructor_patterns_preserve_record_layouts()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                type alias Person = { age : Int, name : String }
                type Node a = Node Int a
                type Wrapper = Wrapper (Node Person)
                select wrapper =
                    case wrapper of
                        Wrapper (Node _ { name }) -> name
                """);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Theory]
    [InlineData("select (Types.Wrapper (Types.Node _ { name })) = name")]
    [InlineData("select wrapper = case wrapper of\n    Types.Wrapper (Types.Node _ { name }) -> name")]
    public void Constructor_pattern_aliases_are_resolved_in_their_defining_module(string declaration)
    {
        var lowered =
            Lower(
                """
                module Types exposing (..)
                type alias Person = { age : Int, name : String }
                type alias Other = { other : Int }
                type Node a = Node Int a
                type Wrapper = Wrapper (Node Person)
                unrelated = 0
                """,
                "module Test exposing (..)\nimport Types\n" + declaration);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Constructor_applications_preserve_the_emitters_parameter_context()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                type alias Person = { age : Int, name : String }
                type Wrapper = Wrapper Person
                select person = ( Wrapper person, person.name )
                """);

        Expressions(lowered, "select")
            .OfType<IR.Expression.ProjectItem>().Should().ContainSingle().Which.Index.Should().Be(4);

        Generic(lowered).Should().BeEmpty();
    }

    [Theory]
    [InlineData("select { name } = name")]
    [InlineData("select record = let { name } = record in name")]
    [InlineData("select record = case record of\n    { name } -> name")]
    public void Generic_pattern_reads_remain_explicit_and_reportable(string declaration)
    {
        var lowered = Lower("module Test exposing (..)\n" + declaration);

        Expressions(lowered, "select").Should().ContainSingle(expression => expression is IR.Expression.RecordAccess);
        Generic(lowered).Should().ContainSingle().Which.Description.Should().Be("name");
    }

    [Fact]
    public void Record_pattern_temporaries_do_not_capture_user_names()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                select : Int -> { age : Int, name : String } -> ( Int, String )
                select record__projection_0 { name } = ( record__projection_0, name )
                """);

        var function = (IR.Declaration.FunctionDeclaration)lowered.Declarations[Name("select")];

        function.Function.Declaration.Arguments.OfType<IR.Pattern.VarPattern>().Select(pattern => pattern.Name)
            .Should().Equal("record__projection_0", "record__projection_1");

        Generic(lowered).Should().BeEmpty();
    }

    [Fact]
    public void Escaped_closed_type_accessors_are_reported_until_their_function_bodies_are_specialized()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                select : { age : Int, name : String } -> String
                select = .name
                """);

        Generic(lowered).Should().ContainSingle().Which.RecordOperationProvenance
            .Should().Be(RecordOperationProvenance.RecordAccessFunctionEscaped);
    }

    [Fact]
    public void The_finder_reports_an_unlowered_IR_read_even_when_its_receiver_is_a_closed_literal()
    {
        var receiver = IR.Expression.RecordExpr.CreateSorted([("name", IR.Expression.StringLiteral.Create("Ada"))]);

        var read =
            new IR.Expression.RecordAccess(
                receiver,
                "name",
                Core.CommonEncodings.StringEncoding.ValueFromString("name"));

        var declaration = new IR.Declaration.FunctionDeclaration(new(null, new("select", [], read)));

        var declarations =
            ImmutableDictionary<DeclQualifiedName, IR.Declaration>.Empty.Add(Name("select"), declaration);

        OptimizationOpportunityFinder.FindGenericRecordAccessOpportunities(declarations).Should().ContainSingle();
        Generic(Lower("module Test exposing (..)\nselect = { name = \"Ada\" }.name")).Should().BeEmpty();
    }

    [Fact]
    public void Lowering_is_deterministic_and_rebuilt_functions_have_consistent_hashes()
    {
        const string source =
            """
                module Test exposing (..)
                select : { age : Int, name : String } -> String
                select { name } = name
                """;

        var lowered = Lower(source);
        var second = Lower(source);
        var firstFunction = (IR.Declaration.FunctionDeclaration)lowered.Declarations[Name("select")];
        var secondFunction = (IR.Declaration.FunctionDeclaration)second.Declarations[Name("select")];

        firstFunction.Should().Be(secondFunction);
        firstFunction.GetHashCode().Should().Be(secondFunction.GetHashCode());
    }

    [Fact]
    public void Missing_fields_in_a_closed_layout_are_explicit_errors()
    {
        Action lower =
            () => Lower(
                """
                module Test exposing (..)
                select : { age : Int } -> String
                select record = record.name
                """);

        lower.Should().Throw<InvalidOperationException>().WithMessage("*does not contain field 'name'*");
    }

    [Fact]
    public void Reachability_tracks_callees_inside_projected_receivers_without_following_local_shadows()
    {
        var lowered =
            Lower(
                """
                module Test exposing (..)
                read record = record.name
                root = read { name = "Ada" }
                cold record = record.unused
                """);

        OptimizationOpportunityFinder.FindGenericRecordAccessOpportunities(
            lowered.Declarations,
            [Name("root")],
            lowered.GenericAccessEvidence)
            .Should().ContainSingle().Which.ContainingDecl.Should().Be(Name("read"));
    }

    [Theory]
    [InlineData("select person = person.name", true)]
    [InlineData("select person = .name person", true)]
    [InlineData("select { name } = name", true)]
    [InlineData("select person = let { name } = person in name", true)]
    [InlineData("select person = case person of\n    { name } -> name", true)]
    [InlineData("select : { r | name : String } -> String\nselect person = person.name", false)]
    [InlineData("select : { r | name : String } -> String\nselect { name } = name", false)]
    public void Runtime_generic_lookup_invocations_agree_with_the_lowered_IR(string declaration, bool closed)
    {
        var source =
            "module Test exposing (..)\n" +
            (closed ? "select : { age : Int, name : String } -> String\n" : "") + declaration;

        var (environment, pipeline) = ElmCompilerTestHelper.CompileElmModules([source], disableInlining: true);

        var function =
            environment.Modules.Single(module => module.moduleName == "Test").moduleContent.FunctionDeclarations["select"];

        var accessExpression =
            new PineVMParseCache().ParseExpression(RecordRuntime.PineFunctionForRecordAccessAsValue)
            .Extract(error => throw new InvalidOperationException(error));

        var vm =
            ElmCompilerTestHelper.PineVMForProfiling(
                _ => { },
                skipInlineForExpression: expression => expression == accessExpression);

        var count = 0;

        void Entered(in EnteredStackFrame frame)
        {
            if (frame.FrameExpression == accessExpression)
                count++;
        }

        var (result, _) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                function,
                new ElmValue.ElmRecord([("age", ElmValue.Integer(42)), ("name", ElmValue.StringInstance("Ada"))]),
                vm,
                Entered);

        result.Should().Be(ElmValue.StringInstance("Ada"));

        var opportunities =
            OptimizationOpportunityFinder.FindGenericRecordAccessOpportunities(pipeline.DeclarationsForEmission);

        if (closed)
        {
            count.Should().Be(0);
            opportunities.Should().BeEmpty();
        }
        else
        {
            count.Should().BeGreaterThan(0);
            opportunities.Should().ContainSingle();
        }
    }

    private static DeclQualifiedName Name(string name) => DeclQualifiedName.Create(["Test"], name);

    private static IEnumerable<IR.Expression> Expressions(
        IntermediateRepresentationLoweringResult lowered,
        string name) =>
        IR.ExpressionTraversal.Enumerate(
            ((IR.Declaration.FunctionDeclaration)lowered.Declarations[Name(name)]).Function.Declaration.Expression);

    private static ImmutableHashSet<Opportunity> Generic(IntermediateRepresentationLoweringResult lowered) =>
        OptimizationOpportunityFinder.FindGenericRecordAccessOpportunities(
            lowered.Declarations,
            typeEvidence: lowered.GenericAccessEvidence);

    private static IntermediateRepresentationLoweringResult Lower(params string[] sources)
    {
        var declarations = ImmutableDictionary.CreateBuilder<DeclQualifiedName, Abstract.Declaration>();

        foreach (var source in sources)
        {
            var syntax =
                ElmSyntaxParser.ParseModuleText(source).Extract(
                    error => throw new InvalidOperationException(error.ToString()));

            var module = Abstract.ConvertFromConcrete.FromFile(syntax);
            var moduleName = Abstract.Module.GetModuleName(module.ModuleDefinition);

            foreach (var declaration in module.Declarations)
            {
                var name =
                    declaration switch
                    {
                        Abstract.Declaration.FunctionDeclaration function => function.Function.Declaration.Name,
                        Abstract.Declaration.AliasDeclaration alias => alias.TypeAlias.Name,
                        Abstract.Declaration.ChoiceTypeDeclaration choice => choice.TypeDeclaration.Name,
                        Abstract.Declaration.PortDeclaration port => port.Signature.Name,
                        Abstract.Declaration.InfixDeclaration infix => infix.Infix.Operator,

                        _ =>
                        throw new NotImplementedException(
                            $"{nameof(Lower)} does not handle declaration variant: {declaration.GetType().Name}")
                    };

                declarations.Add(DeclQualifiedName.Create(moduleName, name), declaration);
            }
        }

        return LowerToIntermediateRepresentation.Lower(declarations.ToImmutable());
    }
}
