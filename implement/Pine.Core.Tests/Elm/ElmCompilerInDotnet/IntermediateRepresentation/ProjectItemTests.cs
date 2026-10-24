using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using Xunit;

using Abstract = Pine.Core.Elm.Syntax.Abstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class ProjectItemTests
{
    private static readonly Model.FunctionId Owner =
        new Model.FunctionId.Source(DeclQualifiedName.Create(["Test"], "value"));

    private static Model.BindingId Id(int slot) => new Model.BindingId.Local(Owner, slot);

    private static IR.Expression Local(int slot) => new IR.Expression.Identifier(new Model.Reference.Local(Id(slot)));

    private static IR.Expression String(string value) =>
        new IR.Expression.LiteralValue(ElmValueEncoding.StringAsPineValue(value));

    private static IR.Expression Integer(int value) =>
        new IR.Expression.LiteralValue(IntegerEncoding.EncodeSignedInteger(value));

    [Fact]
    public void ProjectItem_has_structural_equality_and_hashing()
    {
        var first = new IR.Expression.ProjectItem(new IR.Expression.ListExpr([Integer(1), Integer(2)]), 1);
        var equal = new IR.Expression.ProjectItem(new IR.Expression.ListExpr([Integer(1), Integer(2)]), 1);

        first.Should().Be(equal);
        first.GetHashCode().Should().Be(equal.GetHashCode());
        first.Should().NotBe(new IR.Expression.ProjectItem(first.Value, 0));

        first.Should().NotBe(
            new IR.Expression.ProjectItem(new IR.Expression.ListExpr([Integer(1), Integer(3)]), 1));
    }

    [Fact]
    public void ProjectItem_rejects_negative_indices_and_null_operands()
    {
        Action negative = () => new IR.Expression.ProjectItem(Integer(1), -1);
        Action missing = () => new IR.Expression.ProjectItem(null!, 0);

        negative.Should().Throw<ArgumentOutOfRangeException>().Which.ParamName.Should().Be("index");
        missing.Should().Throw<ArgumentNullException>().Which.ParamName.Should().Be("value");
    }

    [Fact]
    public void ProjectItem_JSON_roundtrip_preserves_operand_and_index()
    {
        IR.Expression projection =
            new IR.Expression.ProjectItem(
                new IR.Expression.ListExpr([Integer(1), String("second")]),
                1);

        var options = Model.CompilerModelJson.CreateOptions();
        var json = JsonSerializer.Serialize(projection, options);

        json.Should().Contain("\"ProjectItem\"");
        JsonSerializer.Deserialize<IR.Expression>(json, options).Should().Be(projection);
    }

    [Theory]
    [InlineData(0, 11)]
    [InlineData(1, 22)]
    [InlineData(2, 33)]
    public void ProjectItem_emits_a_static_projection(int index, int expected)
    {
        var projection =
            new IR.Expression.ProjectItem(
                new IR.Expression.ListExpr([Integer(11), Integer(22), Integer(33)]),
                index);

        Evaluate(projection).Should().Be(IntegerEncoding.EncodeSignedInteger(expected));
    }

    [Fact]
    public void ProjectItem_uses_physical_indices_for_the_existing_record_encoding()
    {
        var record =
            IR.Expression.RecordExpr.CreateSorted(
                [("name", String("Ada")), ("age", Integer(42))]);

        Evaluate(new IR.Expression.ProjectItem(record, 4)).Should().Be(ElmValueEncoding.StringAsPineValue("Ada"));
    }

    [Fact]
    public void ProjectItem_supports_nested_physical_projections()
    {
        var tuple =
            new IR.Expression.ListExpr(
                [
                Integer(1),
                new IR.Expression.ListExpr([String("second"), Integer(3)])
                ]);

        var projection = new IR.Expression.ProjectItem(new IR.Expression.ProjectItem(tuple, 1), 0);

        Evaluate(projection).Should().Be(ElmValueEncoding.StringAsPineValue("second"));
    }

    [Fact]
    public void ProjectItem_tracks_references_for_out_of_order_let_bindings()
    {
        var expression =
            new IR.Expression.LetExpression(
                [
                    new(new Model.Pattern.VarPattern(Id(0)), new IR.Expression.ProjectItem(Local(1), 1)),
                    new(new Model.Pattern.VarPattern(Id(1)), new IR.Expression.ListExpr([Integer(1), Integer(2)]))
                ],
                Local(0));

        Evaluate(expression).Should().Be(IntegerEncoding.EncodeSignedInteger(2));
    }

    [Fact]
    public void ProjectItem_propagates_operand_compilation_errors()
    {
        var result =
            ExpressionCompiler.Compile(
                new IR.Expression.ProjectItem(Local(99), 0),
                CompilationContext());

        result.IsErrOrNull().Should().BeOfType<CompilationError.UnresolvedReference>();
    }

    [Fact]
    public void ProjectItem_operand_contributes_to_function_dependencies()
    {
        var callerName = DeclQualifiedName.Create(["Test"], "caller");
        var calleeName = DeclQualifiedName.Create(["Test"], "callee");

        var caller = new Model.FunctionId.Source(callerName);
        var callee = new Model.FunctionId.Source(calleeName);

        var functions =
            System.Collections.Immutable.ImmutableDictionary<Model.FunctionId, Model.Function>.Empty
            .Add(
                caller,
                new(
                    [],
                    new Model.Expression.Project(
                        new Model.Expression.ReferenceExpression(new Model.Reference.Function(callee)),
                        0)))
            .Add(
                callee,
                new(
                    [],
                    new Model.Expression.List([new Model.Expression.Literal(IntegerEncoding.EncodeSignedInteger(1))])));

        var program =
            new Model.Program(
                functions,
                []);

        var requests = new List<Model.SccCompilationInput>();

        Model.Compilation.Compile(
            program,
            compileScc:
            request =>
            {
                requests.Add(request);
                return Model.Compilation.CompileScc(request);
            }).Extract(error => throw new InvalidOperationException(error.ToString()));

        requests.Single(request => request.Members.Any(member => member.Key == caller))
            .ExternalFunctions.Should().ContainKey(callee);
    }

    [Fact]
    public void Elm_compiler_exposes_lowered_record_reads_while_preserving_tuple_patterns()
    {
        var (environment, pipeline) =
            ElmCompilerTestHelper.CompileElmModules(
                [
                """
                module Test exposing (..)

                type alias Person = { age : Int, name : String }

                select : Person -> ( Int, String )
                select person =
                    let
                        ( first, second ) = ( person.age, person.name )
                    in
                    ( first, second )
                """
                ],
                disableInlining: true);

        environment.Modules.Should().Contain(module => module.moduleName == "Test");

        var options = Model.CompilerModelJson.CreateOptions();
        var declarations = pipeline.FunctionsForEmission;

        declarations.Should().HaveCount(pipeline.CompilerProgram.Functions.Count);

        var select =
            declarations[new Model.FunctionId.Source(
            DeclQualifiedName.Create(["Test"], "select"))];

        var json = JsonSerializer.Serialize(select, options);
        json.Should().NotContain("\"RecordAccess\"");
        json.Should().Contain("\"TuplePattern\"");
        json.Should().Contain("\"ProjectItem\"");
    }

    [Fact]
    public void Pipeline_IR_uses_qualified_declarations_without_source_file_containers()
    {
        var modules =
            new[] { "First", "Second" }
            .Select(
                name =>
                new Abstract.File(
                    new Module.NormalModule(
                        new Abstract.DefaultModuleData([name], new Exposing.All())),
                    [new Abstract.Import(["Unused"], null, null)],
                    [
                    new Declaration.FunctionDeclaration(
                        new Abstract.FunctionStruct(
                            null,
                            new Abstract.FunctionImplementation("value", [], Abstract.Expression.UnitExpr.Instance)))
                    ]))
            .Append(
                new Abstract.File(
                    new Module.NormalModule(
                        new Abstract.DefaultModuleData(["Empty"], new Exposing.All())),
                    [],
                    []))
            .ToArray();

        var canonical = ElmCompiler.FlattenModulesToDeclarationDictionary(modules);
        var program = Model.FromCanonicalSyntax.Convert(canonical);
        var pipeline = new CompilationPipelineStageResults([], canonical, program);
        var declarations = pipeline.FunctionsForEmission;

        declarations.Keys.Should().BeEquivalentTo(
            [
            new Model.FunctionId.Source(DeclQualifiedName.Create(["First"], "value")),
            new Model.FunctionId.Source(DeclQualifiedName.Create(["Second"], "value"))
            ]);

        declarations.Values.Should().AllBeOfType<IR.FunctionStruct>();
        pipeline.CompilerProgram.Should().BeSameAs(program);
        typeof(IR.Expression).Assembly.GetType(typeof(IR.Expression).Namespace + ".File").Should().BeNull();
    }

    private static PineValue Evaluate(IR.Expression expression)
    {
        var compiled =
            ExpressionCompiler.Compile(expression, CompilationContext())
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        return
            ElmCompilerTestHelper.PineVMForProfiling(_ => { })
            .EvaluateExpression(compiled, PineValue.EmptyList)
            .Extract(error => throw new InvalidOperationException(error));
    }

    private static ExpressionCompilationContext CompilationContext() =>
        new(
            new Dictionary<Model.BindingId, int>(),
            new Dictionary<Model.BindingId, Core.PineLanguage.Expression>(),
            new Model.SccCompilationContext(
                [],
                [],
                []),
            new ModuleCompilationContext(
                FrozenSet.Create(["Pine_builtin", "Pine_kernel"]),
                [],
                []),
            "Test",
            new PineVMParseCache(),
            null);
}
