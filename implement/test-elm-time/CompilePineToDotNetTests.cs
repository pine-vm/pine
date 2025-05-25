using ElmTime.ElmInteractive;
using FluentAssertions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.CompilePineToDotNet;
using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using Pine.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace TestElmTime;

[TestClass]
public class CompilePineToDotNetTests
{
    public static readonly PineValue value_299b7decef = StringEncoding.ValueFromString("List");

    public static readonly PineValue value_d597fb92e5 = PineValue.List([value_299b7decef, PineValue.EmptyList]);

    [TestMethod]
    public void Test_sort_pine_value_for_declaration()
    {
        value_d597fb92e5.ContainsInListTransitive(value_299b7decef).Should().BeTrue();

        var listBeforeOrdering =
            new[]
            {
                StringEncoding.ValueFromString("Err"),
                value_d597fb92e5,
                StringEncoding.ValueFromString("Ok"),
                value_299b7decef
            };

        var listWithHashes =
            listBeforeOrdering
            .Select(value => (value, hash: System.Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span)))
            .ToImmutableList();

        var orderedValues =
            CSharpDeclarationOrder.OrderValuesForDeclaration(listBeforeOrdering)
            .ToImmutableList();

        orderedValues.Should().Equal(
            new[]
            {
                StringEncoding.ValueFromString("Ok"),
                StringEncoding.ValueFromString("Err"),
                value_299b7decef,
                value_d597fb92e5
            });
    }

    [TestMethod]
    public void Test_compile_syntax_for_type_declared_in_type()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(EvalExprDelegate),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.EvalExprDelegate");
    }

    [TestMethod]
    public void Test_compile_syntax_for_generic_type()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Result<string, int>),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.Result<System.String,System.Int32>");
    }

    [TestMethod]
    public void Test_compile_syntax_for_generic_IReadOnlyDictionary()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(IReadOnlyDictionary<PineValue, string>),
                usings: []);

        syntax.ToFullString().Should().Be("System.Collections.Generic.IReadOnlyDictionary<Pine.Core.PineValue,System.String>");
    }

    [TestMethod]
    [Ignore("Inlining disabled for head")]
    public void Test_compile_specialized_for_kernel_head()
    {
        var pineExpression =
            new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.Environment());

        var compiledFormattedExpression =
            CompiledFormattedCSharp(
                pineExpression,
                new FunctionCompilationEnv(
                    new ExprFunctionCompilationInterface(
                        EnvItemsParamNames: [([], "environment")],
                        ArgumentEvalGenericName: "eval"),
                    CompilationUnit: new CompilationUnitEnv(
                        AvailableExpr: ImmutableDictionary<Expression, CompilationUnitEnvExprEntry>.Empty,
                        DefaultInterface: new ExprFunctionCompilationInterface(EnvItemsParamNames: [], ArgumentEvalGenericName: "eval"))));

        var expectedSyntaxText = """
            environment switch
            {
                PineValue.ListValue listValue =>
                listValue.Elements switch
                {
                    [var head, ..] =>
                    head,

                    _ =>
                    PineValue.EmptyList
                },

                _ =>
                PineValue.EmptyList
            }
            """;

        var expectedSyntaxNormalized =
            CSharpSyntaxTree.ParseText(expectedSyntaxText)
            .GetRoot()
            .NormalizeWhitespace();

        compiledFormattedExpression.Syntax.ToFullString().Should().Be(expectedSyntaxNormalized.ToFullString());
    }

    static CompiledExpression CompiledFormattedCSharp(
        Expression expression,
        FunctionCompilationEnv environment)
    {
        var compiledExpression =
            CompileToCSharp.CompileToCSharpExpression(
                expression,
                new ExpressionCompilationEnvironment(
                    environment,
                    LetBindings: CompiledExpression.NoLetBindings,
                    ParentEnvironment: null,
                    EnvConstraint: null),
                createLetBindingsForCse: false)
            .Extract(err => throw new System.Exception(err));

        return
            compiledExpression
            with
            {
                Syntax =
                FormatCSharpSyntaxRewriter.FormatSyntaxTree(compiledExpression.Syntax)
                .NormalizeWhitespace()
            };
    }

    [TestMethod]
    public void Test_TryParseExpressionAsIndexPathFromEnv()
    {
        var testCases = new[]
        {
            ((Expression)new Expression.Environment(),
            (ExprMappedToParentEnv?)new ExprMappedToParentEnv.PathInParentEnv([])),

            (Expression.ListInstance([]),
            null),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.Environment()),
                new ExprMappedToParentEnv.PathInParentEnv([0])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: nameof(KernelFunction.head),
                    input: new Expression.Environment())),
                new ExprMappedToParentEnv.PathInParentEnv([0, 0])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: "skip",
                    input: Expression.ListInstance(
                        [ Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                        Expression.EnvironmentInstance
                        ]))),
                new ExprMappedToParentEnv.PathInParentEnv([13])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: "skip",
                    input: Expression.ListInstance(
                        [ new Expression.Literal(IntegerEncoding.EncodeSignedInteger(21)),
                        new Expression.KernelApplication(
                            function: nameof(KernelFunction.head),
                            input: new Expression.KernelApplication(
                                function: "skip",
                                input: Expression.ListInstance(
                                    [ new Expression.Literal(IntegerEncoding.EncodeSignedInteger(17)),
                                    new Expression.Environment()
                                    ])))
                        ]))),
                new ExprMappedToParentEnv.PathInParentEnv([17,21])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: "skip",
                    input: Expression.ListInstance(
                        [ new Expression.Literal(IntegerEncoding.EncodeSignedInteger(23)),
                        new Expression.KernelApplication(
                            function: nameof(KernelFunction.head),
                            input: new Expression.Environment())
                        ]))),
                new ExprMappedToParentEnv.PathInParentEnv([0,23])),
        };

        foreach (var (expression, expected) in testCases)
        {
            var result = CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression);

            result.Should().Be(expected);
        }
    }

    [TestMethod]
    public void Test_ExprMappedToParentEnv_PathInParentEnv_equality()
    {
        new ExprMappedToParentEnv.PathInParentEnv([])
            .Should().Be(new ExprMappedToParentEnv.PathInParentEnv([]));

        new ExprMappedToParentEnv.PathInParentEnv([1, 3])
            .Should().Be(new ExprMappedToParentEnv.PathInParentEnv([1, 3]));
    }

    [TestMethod]
    public void Test_CompileEnvItemsPathsForExprFunction()
    {
        var testCases = new[]
        {
            new
            {
                expr =
                (Expression)
                new Expression.Environment(),

                envConstraint =
                EnvConstraintId.Create(
                    envClass: new ExpressionEnvClass.ConstrainedEnv([]),
                    PineValue.EmptyList,
                    skipUnavailableItems: false),

                expectedPaths = (IReadOnlyList<IReadOnlyList<int>>)[[]]
            },
            new
            {
                expr =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.head),
                    input: new Expression.Environment()),

                envConstraint =
                EnvConstraintId.Create(
                    envClass: new ExpressionEnvClass.ConstrainedEnv([]),
                    PineValue.EmptyList,
                    skipUnavailableItems: false),

                expectedPaths = (IReadOnlyList<IReadOnlyList<int>>)[[0]]
            },
            new
            {
                expr =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.head),
                    input:
                    new Expression.KernelApplication(
                        function:"skip",
                        input: Expression.ListInstance(
                            [
                            new Expression.Literal(IntegerEncoding.EncodeSignedInteger(13)),
                            new Expression.Environment()
                            ]))),

                envConstraint =
                EnvConstraintId.Create(
                    envClass: new ExpressionEnvClass.ConstrainedEnv([]),
                    PineValue.EmptyList,
                    skipUnavailableItems: false),

                expectedPaths = (IReadOnlyList<IReadOnlyList<int>>)[[13]]
            },
        };

        foreach (var testCase in testCases)
        {
            var paths =
                FunctionCompilationEnv.CompileEnvItemsPathsForExprFunction(
                    testCase.expr,
                    testCase.envConstraint);

            paths.Count.Should().Be(testCase.expectedPaths.Count, "Paths count");

            for (var i = 0; i < testCase.expectedPaths.Count; i++)
            {
                paths[i].Should().Equal(testCase.expectedPaths[i], "Path " + i);
            }
        }
    }

    [TestMethod]
    public void Compile_from_Elm_to_CSharp()
    {
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/bytes": "1.0.8",
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                    }
                }
            }
            """;

        IReadOnlyList<TestCase> testCases =
        [
            new TestCase
            (
                InputModuleText:
                """
                module Common exposing (..)


                simpleFunction : Int -> Int
                simpleFunction n =
                    n + 1

                """,

                ExpectedText:
                """
                using Pine.Core;
                using Pine.PineVM;
                using System;
                using System.Collections.Generic;
                using System.Collections.Immutable;
                using System.Linq;

                public static class Common
                {
                    public static PineValue simpleFunction(PineValue env)
                    {
                        var env_1 =
                            KernelFunction.head(
                                KernelFunction.skip(
                                    1,
                                    env));

                        var env_1_0 =
                            KernelFunction.head(
                                env_1);

                        return simpleFunction_uparam(
                                env_1_0);
                    }


                    public static PineValue simpleFunction_uparam(PineValue env_1_0)
                    {
                        var env_1_0_as_int =
                            KernelFunction.SignedIntegerFromValueRelaxed(
                                env_1_0);

                        var stack_3_as_int =
                            env_1_0_as_int + 1L;

                        PineValue stack_3 =
                            PineValue.EmptyList;

                        if (stack_3_as_int is { } stack_3_as_int_not_null)
                        {
                            stack_3 = IntegerEncoding.EncodeSignedInteger(
                                stack_3_as_int_not_null);
                        }

                        return stack_3;
                    }
                }
                """
            ),

            new TestCase
            (
                InputModuleText:
                """
                module Common exposing (..)


                simpleFunction : Int -> Int
                simpleFunction n =
                    n * 3 + 1

                """,

                ExpectedText:
                """
                using Pine.Core;
                using Pine.PineVM;
                using System;
                using System.Collections.Generic;
                using System.Collections.Immutable;
                using System.Linq;

                public static class Common
                {
                    public static PineValue simpleFunction(PineValue env)
                    {
                        var env_1 =
                            KernelFunction.head(
                                KernelFunction.skip(
                                    1,
                                    env));

                        var env_1_0 =
                            KernelFunction.head(
                                env_1);

                        return simpleFunction_uparam(
                                env_1_0);
                    }


                    public static PineValue simpleFunction_uparam(PineValue env_1_0)
                    {
                        var env_1_0_as_int =
                            KernelFunction.SignedIntegerFromValueRelaxed(
                                env_1_0);

                        var stack_3_as_int =
                            env_1_0_as_int * 3L;

                        var stack_4_as_int =
                            stack_3_as_int + 1L;

                        PineValue stack_4 =
                            PineValue.EmptyList;

                        if (stack_4_as_int is { } stack_4_as_int_not_null)
                        {
                            stack_4 = IntegerEncoding.EncodeSignedInteger(
                                stack_4_as_int_not_null);
                        }

                        return stack_4;
                    }
                }
                """
            ),

        ];

        for (int testCaseIndex = 0; testCaseIndex < testCases.Count; ++testCaseIndex)
        {
            var testCase = testCases[testCaseIndex];

            try
            {
                var appCodeTree =
                    TreeNodeWithStringPath.EmptyTree
                    .SetNodeAtPathSorted(
                        ["elm.json"],
                        TreeNodeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
                    .SetNodeAtPathSorted(
                        ["src", "Common.elm"],
                        TreeNodeWithStringPath.Blob(Encoding.UTF8.GetBytes(testCase.InputModuleText)));

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        appCodeTree,
                        rootFilePaths: [["src", "Common.elm"]],
                        skipLowering: true,
                        skipFilteringForSourceDirs: false)
                    .Extract(err => throw new System.Exception(err));

                var parsedEnv =
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new System.Exception(err));

                var compiledModuleCommon =
                    parsedEnv.Modules.Single(m => m.moduleName is "Common");

                var compiledModuleCommonCSharp =
                    Pine.Pine.CompilePineToDotNet.CompileModuleToCSharp.BuildCSharpClassStringFromModule(
                        compiledModuleCommon.moduleValue,
                        containerConfig:
                        new SyntaxContainerConfig(
                            ContainerTypeName: "Common",
                            DictionaryMemberName: ""));

                var compilationUnitSyntax =
                    SyntaxFactory.CompilationUnit()
                    .WithUsings([.. compiledModuleCommonCSharp.UsingDirectives])
                    .WithMembers(
                    SyntaxFactory.List<MemberDeclarationSyntax>(
                        [compiledModuleCommonCSharp.ClassDeclarationSyntax]));

                var formattedNode =
                    FormatCSharpSyntaxRewriter.FormatSyntaxTree(
                        compilationUnitSyntax.NormalizeWhitespace(eol: "\n"));

                var formattedNodeText =
                    formattedNode.ToFullString()
                    .Trim();

                formattedNodeText.Should().Be(testCase.ExpectedText.Trim());
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    "Failed for test case " + testCaseIndex + ": " + e.Message +
                    "\nTest case module text:\n" +
                    testCase.InputModuleText,
                    e);
            }
        }
    }

    private record TestCase(
        string InputModuleText,
        string ExpectedText);
}
