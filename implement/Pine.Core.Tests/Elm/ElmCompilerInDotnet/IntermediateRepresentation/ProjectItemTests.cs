using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class ProjectItemTests
{
    private static IR.Expression Integer(int value) =>
        new IR.Expression.IntegerLiteral(value, IntegerEncoding.EncodeSignedInteger(value));

    [Fact]
    public void ProjectItem_has_structural_equality_and_hashing()
    {
        var first = new IR.Expression.ProjectItem(new IR.Expression.TupledExpression([Integer(1), Integer(2)]), 1);
        var equal = new IR.Expression.ProjectItem(new IR.Expression.TupledExpression([Integer(1), Integer(2)]), 1);

        first.Should().Be(equal);
        first.GetHashCode().Should().Be(equal.GetHashCode());
        first.Should().NotBe(new IR.Expression.ProjectItem(first.Value, 0));

        first.Should().NotBe(
            new IR.Expression.ProjectItem(new IR.Expression.TupledExpression([Integer(1), Integer(3)]), 1));
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
                new IR.Expression.TupledExpression([Integer(1), IR.Expression.StringLiteral.Create("second")]),
                1);

        var options = Abstract.ElmSyntaxAbstractJson.JsonSerializerOptions;
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
                new IR.Expression.TupledExpression([Integer(11), Integer(22), Integer(33)]),
                index);

        Evaluate(projection).Should().Be(IntegerEncoding.EncodeSignedInteger(expected));
    }

    [Fact]
    public void ProjectItem_uses_physical_indices_for_the_existing_record_encoding()
    {
        var record =
            IR.Expression.RecordExpr.CreateSorted(
                [("name", IR.Expression.StringLiteral.Create("Ada")), ("age", Integer(42))]);

        Evaluate(new IR.Expression.ProjectItem(record, 4)).Should().Be(ElmValueEncoding.StringAsPineValue("Ada"));
    }

    [Fact]
    public void ProjectItem_supports_nested_physical_projections()
    {
        var tuple =
            new IR.Expression.TupledExpression(
                [
                Integer(1),
                new IR.Expression.TupledExpression([IR.Expression.StringLiteral.Create("second"), Integer(3)])
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
                new IR.LetDeclaration.LetFunction(
                    new IR.FunctionStruct(
                        null,
                        new IR.FunctionImplementation(
                            "selected",
                            [],
                            new IR.Expression.ProjectItem(IR.Expression.Identifier.Create([], "tuple"), 1)))),
                new IR.LetDeclaration.LetFunction(
                    new IR.FunctionStruct(
                        null,
                        new IR.FunctionImplementation(
                            "tuple",
                            [],
                            new IR.Expression.TupledExpression([Integer(1), Integer(2)]))))
                ],
                IR.Expression.Identifier.Create([], "selected"));

        Evaluate(expression).Should().Be(IntegerEncoding.EncodeSignedInteger(2));
    }

    [Fact]
    public void ProjectItem_propagates_operand_compilation_errors()
    {
        var result =
            ExpressionCompiler.Compile(
                new IR.Expression.ProjectItem(IR.Expression.Identifier.Create([], "missing"), 0),
                CompilationContext());

        result.IsErrOrNull().Should().BeOfType<CompilationError.UnresolvedReference>();
    }

    [Fact]
    public void ProjectItem_operand_contributes_to_function_dependencies()
    {
        var callerName = DeclQualifiedName.Create(["Test"], "caller");
        var calleeName = DeclQualifiedName.Create(["Test"], "callee");

        var functions =
            new Dictionary<DeclQualifiedName, (string moduleName, string functionName, IR.Declaration.FunctionDeclaration declaration)>
            {
                [callerName] =
                ("Test",
                "caller",
                new IR.Declaration.FunctionDeclaration(
                    new IR.FunctionStruct(
                        null,
                        new IR.FunctionImplementation(
                            "caller",
                            [],
                            new IR.Expression.ProjectItem(IR.Expression.Identifier.Create(["Test"], "callee"), 0))))),
                [calleeName] =
                ("Test",
                "callee",
                new IR.Declaration.FunctionDeclaration(
                    new IR.FunctionStruct(
                        null,
                        new IR.FunctionImplementation(
                            "callee",
                            [],
                            new IR.Expression.TupledExpression([Integer(1), Integer(2)])))))
            };

        var context = CompilationContext().ModuleCompilationContext with { AllFunctions = functions };
        var (_, functionToScc, _) = ElmCompiler.ComputeDependencyLayoutsAndSccs(functions, context);

        functionToScc["Test.caller"].AdditionalDependencies.Should().Contain("Test.callee");
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

        var options = Abstract.ElmSyntaxAbstractJson.JsonSerializerOptions;
        var declarations = pipeline.DeclarationsForEmission;

        declarations.Should().HaveCount(pipeline.ModulesForCompilation.Sum(module => module.Declarations.Count));

        var select = declarations[DeclQualifiedName.Create(["Test"], "select")];
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
                    new Abstract.Module.NormalModule(
                        new Abstract.DefaultModuleData([name], new Abstract.Exposing.All())),
                    [new Abstract.Import(["Unused"], null, null)],
                    [
                    new Abstract.Declaration.FunctionDeclaration(
                        new Abstract.FunctionStruct(
                            null,
                            new Abstract.FunctionImplementation("value", [], Abstract.Expression.UnitExpr.Instance)))
                    ]))
            .Append(
                new Abstract.File(
                    new Abstract.Module.NormalModule(
                        new Abstract.DefaultModuleData(["Empty"], new Abstract.Exposing.All())),
                    [],
                    []))
            .ToArray();

        var pipeline = new CompilationPipelineStageResults<int>([], 0, modules);
        var declarations = pipeline.DeclarationsForEmission;

        declarations.Keys.Should().BeEquivalentTo(
            [
            DeclQualifiedName.Create(["First"], "value"),
            DeclQualifiedName.Create(["Second"], "value")
            ]);

        declarations.Values.Should().AllBeOfType<IR.Declaration.FunctionDeclaration>();
        pipeline.ModulesForCompilation.Should().BeSameAs(modules);
        typeof(IR.Declaration).Assembly.GetType(typeof(IR.Declaration).Namespace + ".File").Should().BeNull();
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
            ParameterNames: new Dictionary<string, int>(),
            CurrentModuleName: "Test",
            CurrentFunctionName: "value",
            LocalBindings: null,
            DependencyLayout: [],
            ModuleCompilationContext:
            new ModuleCompilationContext(
                AllFunctions:
                new Dictionary<DeclQualifiedName, (string moduleName, string functionName, IR.Declaration.FunctionDeclaration declaration)>(),
                CompiledFunctionsCache: [],
                PineKernelModuleNames: FrozenSet.Create(["Pine_builtin", "Pine_kernel"])),
            ParseCache: new PineVMParseCache(),
            ReducedExpressionCache: null);
}
