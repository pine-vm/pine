using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using Xunit;

using AST = Pine.Core.Elm.Syntax.Abstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;
using FunctionId = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel.FunctionId;
using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class AstToIrLoweringTests
{
    private static readonly FunctionId s_owner = new FunctionId.Source(DeclQualifiedName.Create(["Test"], "select"));

    private static Model.BindingId Id(int slot) => new Model.BindingId.Local(s_owner, slot);

    [Fact]
    public void Canonical_list_annotations_do_not_lose_element_types_or_conflate_other_modules()
    {
        var canonical =
            TypeInference.TypeAnnotationToInferredType(
                new TypeAnnotation.Typed(["List"], "List", [new TypeAnnotation.Unit()]));

        canonical.Should().BeOfType<TypeInference.InferredType.ListType>();

        var unrelated =
            TypeInference.TypeAnnotationToInferredType(
                new TypeAnnotation.Typed(["Other"], "List", [new TypeAnnotation.Unit()]));

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
    public void Resolved_patterns_cannot_represent_unlowered_record_patterns() =>
        typeof(Model.Pattern).GetNestedType("RecordPattern").Should().BeNull();

    private static IEnumerable<Type> TypeComponents(Type type)
    {
        yield return type;

        foreach (var argument in type.GetGenericArguments())
        {
            foreach (var component in TypeComponents(argument))
                yield return component;
        }
    }

    [Theory]
    [MemberData(nameof(FromCanonicalSyntaxTests.Expressions), MemberType = typeof(FromCanonicalSyntaxTests))]
    public void AST_to_IR_lowering_handles_every_expression_variant(Core.Elm.Syntax.Abstract.Expression expression) =>
        Lower(expression, new TypeAnnotation.GenericType("a")).Should().NotBeNull();

    public static IEnumerable<object[]> ExactFloatFractions()
    {
        var huge = BigInteger.Pow(10, 400);
        var beyondDoublePrecision = (BigInteger.One << 53) + 1;
        yield return [new BigInteger(29), new BigInteger(100), new BigInteger(29), new BigInteger(100)];
        yield return [BigInteger.One, new BigInteger(3), BigInteger.One, new BigInteger(3)];
        yield return [beyondDoublePrecision, BigInteger.One, beyondDoublePrecision, BigInteger.One];
        yield return [huge, BigInteger.One, huge, BigInteger.One];
        yield return [BigInteger.One, huge, BigInteger.One, huge];
        yield return [huge + 1, huge + 3, huge + 1, huge + 3];
        yield return [new BigInteger(6), new BigInteger(-8), new BigInteger(-3), new BigInteger(4)];
        yield return [new BigInteger(-6), new BigInteger(-8), new BigInteger(3), new BigInteger(4)];
        yield return [BigInteger.Zero, BigInteger.One, BigInteger.Zero, BigInteger.One];
        yield return [new BigInteger(-29), new BigInteger(100), new BigInteger(-29), new BigInteger(100)];
        yield return [-huge - 1, huge + 3, -huge - 1, huge + 3];
    }

    [Theory]
    [MemberData(nameof(ExactFloatFractions))]
    public void Float_literal_lowering_preserves_exact_rational_values(
        BigInteger numerator, BigInteger denominator, BigInteger expectedNumerator, BigInteger expectedDenominator)
    {
        var body =
            Lower(
                new Core.Elm.Syntax.Abstract.Expression.FloatLiteral(numerator, denominator),
                new TypeAnnotation.Typed(["Basics"], "Float", []));

        body.Should().BeOfType<IR.Expression.LiteralValue>().Which.Value.Should().Be(
            ElmValueEncoding.TagAsPineValue(
                ElmValue.ElmFloatTypeTagName,
                [
                IntegerEncoding.EncodeSignedInteger(expectedNumerator),
                IntegerEncoding.EncodeSignedInteger(expectedDenominator)
                ]));
    }

    public static IEnumerable<object[]> ExactFloatSources()
    {
        (string Literal, string Numerator, string Denominator)[] decimals =
            [
                ("0.29", "29", "100"),
                ("-0.29", "-29", "100"),
                ("9007199254740993.0", "9007199254740993", "1"),
                ("0.12345678901234567890123456789", "12345678901234567890123456789", "100000000000000000000000000000"),
                ("6.022e23", "602200000000000000000000", "1"),
                ("1.6e-19", "1", "6250000000000000000")
            ];

        foreach (var (literal, numerator, denominator) in decimals)
        {
            yield return [literal, BigInteger.Parse(numerator, System.Globalization.CultureInfo.InvariantCulture),
                BigInteger.Parse(denominator, System.Globalization.CultureInfo.InvariantCulture)];
        }

        yield return ["1.0e400", BigInteger.Pow(10, 400), BigInteger.One];
        yield return ["1.0e-400", BigInteger.One, BigInteger.Pow(10, 400)];
    }

    [Theory]
    [MemberData(nameof(ExactFloatSources))]
    public void Compiled_float_literals_preserve_exact_source_values(
        string literal, BigInteger numerator, BigInteger denominator)
    {
        var (environment, _) =
            ElmCompilerTestHelper.CompileElmModules(
                ["module Test exposing (value)\nvalue = " + literal],
                disableInlining: false);

        environment.Modules.Single().moduleContent.FunctionDeclarations["value"].Should().Be(
            ElmValueEncoding.TagAsPineValue(
                ElmValue.ElmFloatTypeTagName,
                [IntegerEncoding.EncodeSignedInteger(numerator), IntegerEncoding.EncodeSignedInteger(denominator)]));
    }

    [Theory]
    [InlineData("+", "add", "int_add")]
    [InlineData("-", "sub", "int_add")]
    [InlineData("*", "mul", "int_mul")]
    public void AST_operators_prefix_operators_and_direct_calls_select_integer_intrinsics(
        string operation, string functionName, string intrinsic)
    {
        var left = Expression.Identifier.Create([], "left");
        var right = Expression.Identifier.Create([], "right");

        Core.Elm.Syntax.Abstract.Expression[] expressions =
            [
            new Core.Elm.Syntax.Abstract.Expression.OperatorApplication(operation, InfixDirection.Left, left, right),
            new Core.Elm.Syntax.Abstract.Expression.Application(
                new Core.Elm.Syntax.Abstract.Expression.PrefixOperator(operation),
                [
                left,
                right
                ]),
            new Core.Elm.Syntax.Abstract.Expression.Application(
                Expression.Identifier.Create(["Basics"], functionName),
                [
                left,
                right
                ])
            ];

        var integer = new TypeAnnotation.Typed(["Basics"], "Int", []);

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
        TypeAnnotation type =
            typeName is "appendable"
            ?
            new Pine.Core.Elm.Syntax.Abstract.TypeAnnotation.GenericType(typeName)
            :
            new Pine.Core.Elm.Syntax.Abstract.TypeAnnotation.Typed(
                [typeName],
                typeName,
                typeName is "List" ? [new Pine.Core.Elm.Syntax.Abstract.TypeAnnotation.GenericType("a")] : []);

        var left = Expression.Identifier.Create([], "left");
        var right = Expression.Identifier.Create([], "right");

        Core.Elm.Syntax.Abstract.Expression[] expressions =
            [
            new Core.Elm.Syntax.Abstract.Expression.OperatorApplication("++", InfixDirection.Left, left, right),
            new Core.Elm.Syntax.Abstract.Expression.Application(
                new Core.Elm.Syntax.Abstract.Expression.PrefixOperator("++"),
                [
                left,
                right
                ]),
            new Core.Elm.Syntax.Abstract.Expression.Application(
                Expression.Identifier.Create(["Basics"], "append"),
                [
                left,
                right
                ])
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
        TypeAnnotation type =
            typeName is "number"
            ?
            new Pine.Core.Elm.Syntax.Abstract.TypeAnnotation.GenericType(typeName)
            :
            new Pine.Core.Elm.Syntax.Abstract.TypeAnnotation.Typed(["Basics"], typeName, []);

        var expression =
            new Core.Elm.Syntax.Abstract.Expression.Application(
                Expression.Identifier.Create(["Basics"], "add"),
                [Expression.Identifier.Create([], "left"), Expression.Identifier.Create([], "right")]);

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

        var declarations = pipeline.FunctionsForEmission;

        var functions =
            environment.Modules.Single(module => module.moduleName is "Test").moduleContent.FunctionDeclarations;

        foreach (var name in new[] { "add", "sub", "mul", "appendString", "appendList", "fromRecord" })
        {
            var declaration =
                declarations[new FunctionId.Source(DeclQualifiedName.Create(["Test"], name))];

            References(declaration.Expression).Should().NotContain(
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
        var left = new IR.Expression.Identifier(new Model.Reference.Local(Id(0)));
        var right = new IR.Expression.Identifier(new Model.Reference.Local(Id(1)));

        IR.Expression[] expressions =
            [
            new IR.Expression.OperatorApplication(operation, InfixDirection.Left, left, right),
            new IR.Expression.Application(
                new IR.Expression.Identifier(
                    new Model.Reference.Native(DeclQualifiedName.Create(["Basics"], functionName))),
                [
                left,
                right
                ])
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
            pipeline.FunctionsForEmission[new FunctionId.Source(DeclQualifiedName.Create(["Test"], "add"))];

        var append =
            pipeline.FunctionsForEmission[new FunctionId.Source(DeclQualifiedName.Create(["Test"], "append"))];

        References(add.Expression).Should().Contain("Basics.add");
        References(add.Expression).Should().NotContain("Pine_builtin.int_add");
        References(append.Expression).Should().Contain("Basics.append");
        References(append.Expression).Should().NotContain("Pine_builtin.concat");

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

    private static IR.Expression Lower(Core.Elm.Syntax.Abstract.Expression expression, TypeAnnotation type)
    {
        var declaration =
            new Declaration.FunctionDeclaration(
                new AST.FunctionStruct(
                    new AST.Signature(
                        "select",
                        new TypeAnnotation.FunctionTypeAnnotation(
                            type,
                            new TypeAnnotation.FunctionTypeAnnotation(type, type))),
                    new AST.FunctionImplementation(
                        "select",
                        [
                        new Pattern.VarPattern("left"),
                        new Pattern.VarPattern("right")
                        ],
                        expression)));

        var name = DeclQualifiedName.Create(["Test"], "select");

        var lowered =
            LowerToIntermediateRepresentation.Lower(
                ImmutableDictionary<DeclQualifiedName, Declaration>.Empty.Add(name, declaration));

        return lowered[new FunctionId.Source(name)].Expression;
    }

    private static IEnumerable<string> References(IR.Expression expression) =>
        IR.ExpressionTraversal.Enumerate(expression).OfType<IR.Expression.Identifier>()
        .Select(identifier => identifier.Reference).OfType<Model.Reference.Native>().Select(native => native.Name.FullName);

    private static ExpressionCompilationContext Context() =>
        new(
            new Dictionary<Model.BindingId, int> { [Id(0)] = 0, [Id(1)] = 1 },
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
