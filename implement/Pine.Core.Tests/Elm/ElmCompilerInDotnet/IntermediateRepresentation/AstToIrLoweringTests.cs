using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using AST = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using InfixDirection = Pine.Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class AstToIrLoweringTests
{
    [Fact]
    public void Canonical_list_annotations_do_not_lose_element_types_or_conflate_other_modules()
    {
        var canonical =
            TypeInference.TypeAnnotationToInferredType(
                new AST.TypeAnnotation.Typed(["List"], "List", [new AST.TypeAnnotation.Unit()]));

        canonical.Should().BeOfType<TypeInference.InferredType.ListType>();

        var unrelated =
            TypeInference.TypeAnnotationToInferredType(
                new AST.TypeAnnotation.Typed(["Other"], "List", [new AST.TypeAnnotation.Unit()]));

        unrelated.Should().BeOfType<TypeInference.InferredType.ChoiceType>().Which.ModuleName.Should().Equal("Other");
    }

    [Fact]
    public void Type_inference_has_no_IR_entry_points_and_emission_requires_IR()
    {
        var irNamespace = typeof(IR.Expression).Namespace;

        foreach (var method in typeof(TypeInference).GetMethods(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static))
        {
            foreach (var type in method.GetParameters()
                .Select(parameter => parameter.ParameterType).Append(method.ReturnType)
                .SelectMany(TypeComponents))
            {
                type.Namespace.Should().NotBe(irNamespace);
            }
        }

        foreach (var method in typeof(ExpressionCompiler).GetMethods().Where(method => method.Name == nameof(ExpressionCompiler.Compile)))
            method.GetParameters()[0].ParameterType.Should().Be(typeof(IR.Expression));
    }

    [Fact]
    public void Emission_rejects_unlowered_record_patterns_instead_of_hiding_generic_reads()
    {
        Action analyze =
            () => PatternCompiler.AnalyzePattern(
                IR.Pattern.RecordPattern.Create(["name"]),
                Expression.EnvironmentInstance);

        analyze.Should().Throw<InvalidOperationException>().WithMessage("*must be lowered*");
    }

    private static IEnumerable<Type> TypeComponents(Type type)
    {
        yield return type;

        foreach (var argument in type.GetGenericArguments())
            foreach (var component in TypeComponents(argument))
                yield return component;
    }

    [Theory]
    [MemberData(nameof(ConvertFromAbstractSyntaxTests.Expressions), MemberType = typeof(ConvertFromAbstractSyntaxTests))]
    public void AST_to_IR_lowering_handles_every_expression_variant(AST.Expression expression) =>
        Lower(expression, new AST.TypeAnnotation.GenericType("a")).Should().NotBeNull();

    [Theory]
    [InlineData("+", "add", "int_add")]
    [InlineData("-", "sub", "int_add")]
    [InlineData("*", "mul", "int_mul")]
    public void AST_operators_prefix_operators_and_direct_calls_select_integer_intrinsics(
        string operation, string functionName, string intrinsic)
    {
        var left = AST.Expression.Identifier.Create([], "left");
        var right = AST.Expression.Identifier.Create([], "right");

        AST.Expression[] expressions =
            [
            new AST.Expression.OperatorApplication(operation, InfixDirection.Left, left, right),
            new AST.Expression.Application(new AST.Expression.PrefixOperator(operation), [left, right]),
            new AST.Expression.Application(AST.Expression.Identifier.Create(["Basics"], functionName), [left, right])
            ];

        var integer = new AST.TypeAnnotation.Typed(["Basics"], "Int", []);

        foreach (var expression in expressions)
        {
            var body = Lower(expression, integer);
            References(body).Should().Contain("Pine_builtin." + intrinsic);
            References(body).Should().NotContain("Basics." + functionName);
            IR.ExpressionTraversal.Enumerate(body).OfType<IR.Expression.OperatorApplication>().Should().BeEmpty();
        }
    }

    [Theory]
    [InlineData("String", true)]
    [InlineData("List", true)]
    [InlineData("appendable", false)]
    public void AST_append_selection_preserves_genuinely_polymorphic_operations(string typeName, bool specialized)
    {
        AST.TypeAnnotation type =
            typeName is "appendable"
            ?
            new AST.TypeAnnotation.GenericType(typeName)
            :
            new AST.TypeAnnotation.Typed(
                [typeName],
                typeName,
                typeName is "List" ? [new AST.TypeAnnotation.GenericType("a")] : []);

        var left = AST.Expression.Identifier.Create([], "left");
        var right = AST.Expression.Identifier.Create([], "right");

        AST.Expression[] expressions =
            [
            new AST.Expression.OperatorApplication("++", InfixDirection.Left, left, right),
            new AST.Expression.Application(new AST.Expression.PrefixOperator("++"), [left, right]),
            new AST.Expression.Application(AST.Expression.Identifier.Create(["Basics"], "append"), [left, right])
            ];

        foreach (var expression in expressions)
        {
            var body = Lower(expression, type);
            References(body).Contains("Pine_builtin.concat").Should().Be(specialized);

            if (typeName is "String")
                References(body).Should().Contain("String.String");
        }
    }

    [Theory]
    [InlineData("Float")]
    [InlineData("number")]
    public void Non_integer_numeric_operations_remain_generic(string typeName)
    {
        AST.TypeAnnotation type =
            typeName is "number"
            ?
            new AST.TypeAnnotation.GenericType(typeName)
            :
            new AST.TypeAnnotation.Typed(["Basics"], typeName, []);

        var expression =
            new AST.Expression.Application(
                AST.Expression.Identifier.Create(["Basics"], "add"),
                [AST.Expression.Identifier.Create([], "left"), AST.Expression.Identifier.Create([], "right")]);

        var body = Lower(expression, type);
        References(body).Should().Contain("Basics.add");
        References(body).Should().NotContain("Pine_builtin.int_add");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Compiler_selects_numeric_and_append_implementations_in_both_optimization_modes(
        bool disableOptimization)
    {
        var (environment, pipeline) =
            ElmCompilerTestHelper.CompileElmModules(
                [
                """
                module Test exposing (..)

                add : Int -> Int -> Int
                add left right = left + right
                sub : Int -> Int -> Int
                sub left right = (-) left right
                mul : Int -> Int -> Int
                mul left right = (*) left right
                appendString : String -> String -> String
                appendString left right = left ++ right
                appendList : List Int -> List Int -> List Int
                appendList left right = (++) left right
                fromRecord : { amount : Int } -> Int -> Int
                fromRecord record right = record.amount + right
                """
                ],
                disableInlining: disableOptimization);

        var declarations = pipeline.DeclarationsForEmission;

        var functions =
            environment.Modules.Single(module => module.moduleName is "Test").moduleContent.FunctionDeclarations;

        foreach (var name in new[] { "add", "sub", "mul", "appendString", "appendList", "fromRecord" })
        {
            var declaration =
                (IR.Declaration.FunctionDeclaration)declarations[DeclQualifiedName.Create(["Test"], name)];

            References(declaration.Function.Declaration.Expression).Should().NotContain(
                reference => reference == "Basics.add" || reference == "Basics.sub" ||
                    reference == "Basics.mul" || reference == "Basics.append");
        }

        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        CoreLibraryTestHelper.ApplyAndProfileBinary(functions["add"], ElmValue.Integer(7), ElmValue.Integer(3), vm)
            .value
            .Should().Be(ElmValue.Integer(10));

        CoreLibraryTestHelper.ApplyAndProfileBinary(functions["sub"], ElmValue.Integer(7), ElmValue.Integer(3), vm)
            .value
            .Should().Be(ElmValue.Integer(4));

        CoreLibraryTestHelper.ApplyAndProfileBinary(functions["mul"], ElmValue.Integer(7), ElmValue.Integer(3), vm)
            .value
            .Should().Be(ElmValue.Integer(21));

        CoreLibraryTestHelper.ApplyAndProfileBinary(
            functions["appendString"],
            ElmValue.StringInstance("ab"),
            ElmValue.StringInstance("cd"),
            vm).value
            .Should().Be(ElmValue.StringInstance("abcd"));

        CoreLibraryTestHelper.ApplyAndProfileBinary(
            functions["appendList"],
            new ElmValue.ElmList([ElmValue.Integer(1)]),
            new ElmValue.ElmList([ElmValue.Integer(2)]),
            vm).value.Should().Be(new ElmValue.ElmList([ElmValue.Integer(1), ElmValue.Integer(2)]));

        CoreLibraryTestHelper.ApplyAndProfileBinary(
            functions["fromRecord"],
            new ElmValue.ElmRecord([("amount", ElmValue.Integer(7))]),
            ElmValue.Integer(3),
            vm).value
            .Should().Be(ElmValue.Integer(10));
    }

    [Theory]
    [InlineData("+", "add")]
    [InlineData("-", "sub")]
    [InlineData("*", "mul")]
    [InlineData("++", "append")]
    public void Emission_compiles_generic_operations_without_semantic_type_contexts(
        string operation,
        string functionName)
    {
        var left = IR.Expression.Identifier.Create([], "left");
        var right = IR.Expression.Identifier.Create([], "right");

        IR.Expression[] expressions =
            [
            new IR.Expression.OperatorApplication(operation, InfixDirection.Left, left, right),
            new IR.Expression.Application(IR.Expression.Identifier.Create(["Basics"], functionName), [left, right])
            ];

        foreach (var expression in expressions)
        {
            ExpressionCompiler.Compile(expression, Context()).IsOkOrNull().Should().NotBeNull();
        }
    }

    [Fact]
    public void Polymorphic_bodies_remain_generic_and_accept_multiple_runtime_representations()
    {
        var (environment, pipeline) =
            ElmCompilerTestHelper.CompileElmModules(
                [
                """
                module Test exposing (..)
                add left right = left + right
                append left right = left ++ right
                """
                ],
                disableInlining: true);

        var functions =
            environment.Modules.Single(module => module.moduleName is "Test").moduleContent.FunctionDeclarations;

        var add =
            (IR.Declaration.FunctionDeclaration)pipeline.DeclarationsForEmission[DeclQualifiedName.Create(["Test"], "add")];

        var append =
            (IR.Declaration.FunctionDeclaration)pipeline.DeclarationsForEmission[DeclQualifiedName.Create(["Test"], "append")];

        References(add.Function.Declaration.Expression).Should().Contain("Basics.add");
        References(add.Function.Declaration.Expression).Should().NotContain("Pine_builtin.int_add");
        References(append.Function.Declaration.Expression).Should().Contain("Basics.append");
        References(append.Function.Declaration.Expression).Should().NotContain("Pine_builtin.concat");

        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        CoreLibraryTestHelper.ApplyAndProfileBinary(functions["add"], ElmValue.Integer(2), ElmValue.Integer(3), vm)
            .value
            .Should().Be(ElmValue.Integer(5));

        CoreLibraryTestHelper.ApplyAndProfileBinary(
            functions["add"],
            ElmValue.ElmFloat.Normalized(1, 2),
            ElmValue.ElmFloat.Normalized(1, 3),
            vm).value
            .Should().Be(ElmValue.ElmFloat.Normalized(5, 6));

        CoreLibraryTestHelper.ApplyAndProfileBinary(
            functions["append"],
            ElmValue.StringInstance("a"),
            ElmValue.StringInstance("b"),
            vm).value
            .Should().Be(ElmValue.StringInstance("ab"));

        CoreLibraryTestHelper.ApplyAndProfileBinary(
            functions["append"],
            new ElmValue.ElmList([ElmValue.Integer(1)]),
            new ElmValue.ElmList([ElmValue.Integer(2)]),
            vm).value
            .Should().Be(new ElmValue.ElmList([ElmValue.Integer(1), ElmValue.Integer(2)]));
    }

    private static IR.Expression Lower(AST.Expression expression, AST.TypeAnnotation type)
    {
        var declaration =
            new AST.Declaration.FunctionDeclaration(
                new AST.FunctionStruct(
                    new AST.Signature(
                        "select",
                        new AST.TypeAnnotation.FunctionTypeAnnotation(
                            type,
                            new AST.TypeAnnotation.FunctionTypeAnnotation(type, type))),
                    new AST.FunctionImplementation(
                        "select",
                        [
                        new AST.Pattern.VarPattern("left"),
                        new AST.Pattern.VarPattern("right")
                        ],
                        expression)));

        var name = DeclQualifiedName.Create(["Test"], "select");

        var lowered =
            LowerToIntermediateRepresentation.Lower(
                ImmutableDictionary<DeclQualifiedName, AST.Declaration>.Empty.Add(name, declaration));

        return ((IR.Declaration.FunctionDeclaration)lowered.Declarations[name]).Function.Declaration.Expression;
    }

    private static IEnumerable<string> References(IR.Expression expression) =>
        IR.ExpressionTraversal.Enumerate(expression).OfType<IR.Expression.Identifier>()
        .Select(identifier => identifier.QualifiedName.FullName);

    private static ExpressionCompilationContext Context() =>
        new(
            ParameterNames: new Dictionary<string, int> { ["left"] = 0, ["right"] = 1 },
            CurrentModuleName: "Test",
            CurrentFunctionName: "select",
            LocalBindings: null,
            DependencyLayout: [],
            ModuleCompilationContext: new ModuleCompilationContext(
                AllFunctions:
                new Dictionary<DeclQualifiedName, (string moduleName, string functionName, IR.Declaration.FunctionDeclaration declaration)>(),
                CompiledFunctionsCache: [],
                PineKernelModuleNames: FrozenSet.Create(["Pine_builtin", "Pine_kernel"])),
            ParseCache: new PineVMParseCache(),
            ReducedExpressionCache: null);
}
