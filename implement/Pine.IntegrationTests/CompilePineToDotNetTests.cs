using AwesomeAssertions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.CompilePineToDotNet;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests;

public class CompilePineToDotNetTests
{
    public static readonly PineValue Value_299b7decef = StringEncoding.ValueFromString("List");

    public static readonly PineValue Value_d597fb92e5 = PineValue.List([Value_299b7decef, PineValue.EmptyList]);

    [Fact]
    public void Test_sort_pine_value_for_declaration()
    {
        Value_d597fb92e5.ContainsInListTransitive(Value_299b7decef).Should().BeTrue("value should contain value_299b7decef in list transitive");

        var listBeforeOrdering =
            new[]
            {
                StringEncoding.ValueFromString("Err"),
                Value_d597fb92e5,
                StringEncoding.ValueFromString("Ok"),
                Value_299b7decef
            };

        var listWithHashes =
            listBeforeOrdering
            .Select(value => (value, hash: System.Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span)))
            .ToImmutableList();

        var orderedValues =
            CSharpDeclarationOrder.OrderValuesForDeclaration(listBeforeOrdering)
            .ToImmutableList();

        orderedValues.Should().BeEquivalentTo(
            [
                StringEncoding.ValueFromString("Ok"),
                StringEncoding.ValueFromString("Err"),
                Value_299b7decef,
                Value_d597fb92e5
            ], options => options.WithStrictOrdering());
    }

    [Fact]
    public void Test_compile_syntax_for_type_declared_in_type()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(EvalExprDelegate),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.EvalExprDelegate");
    }

    [Fact]
    public void Test_compile_syntax_for_generic_type()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Result<string, int>),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.Result<System.String,System.Int32>");
    }

    [Fact]
    public void Test_compile_syntax_for_generic_IReadOnlyDictionary()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(IReadOnlyDictionary<PineValue, string>),
                usings: []);

        syntax.ToFullString().Should().Be("System.Collections.Generic.IReadOnlyDictionary<Pine.Core.PineValue,System.String>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_using_alias()
    {
        // Create a using alias: using MyDict = System.Collections.Generic.Dictionary<string, int>;
        var usingAlias =
            SyntaxFactory.UsingDirective(
                alias: SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("MyDict")),
                name: SyntaxFactory.ParseName("System.Collections.Generic.Dictionary"));

        var context = new DeclarationSyntaxContext([usingAlias]);

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Dictionary<string, int>),
                context);

        syntax.ToFullString().Should().Be("MyDict<System.String,System.Int32>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_using_alias_with_global_prefix()
    {
        // Create a using alias: using MyDict = global::System.Collections.Generic.Dictionary<string, int>;
        var usingAlias =
            SyntaxFactory.UsingDirective(
                alias: SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("MyDict")),
                name: SyntaxFactory.ParseName("global::System.Collections.Generic.Dictionary"));

        var context = new DeclarationSyntaxContext([usingAlias]);

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Dictionary<string, int>),
                context);

        syntax.ToFullString().Should().Be("MyDict<System.String,System.Int32>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_current_namespace_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine.Core");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_current_namespace_partial_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("Core.PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_no_shortening_when_different_namespace()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "SomeOther.Namespace");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("Pine.Core.PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_using_directive_takes_precedence_over_namespace()
    {
        var usingDirective =
            SyntaxFactory.UsingDirective(
                SyntaxFactory.ParseName("Pine.Core"));

        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [usingDirective],
                CurrentNamespace: "DifferentNamespace");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_generic_type_with_namespace_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine.Core");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Result<string, PineValue>),
                context);

        syntax.ToFullString().Should().Be("Result<System.String,PineValue>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_backwards_compatibility()
    {
        // Test that the old method signature still works
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.PineValue");
    }

    [Fact]
    public void Test_ShortestRelativeNamespace_with_current_namespace_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine.Core");

        var result =
            CompileTypeSyntax.ShortestRelativeNamespace(
                "Pine.Core.CodeAnalysis.SomeType",
                context);

        result.Should().BeEquivalentTo(["CodeAnalysis", "SomeType"]);
    }

    [Fact]
    public void Test_ShortestRelativeNamespace_backwards_compatibility()
    {
        // Test that the old method signature still works
        var result =
            CompileTypeSyntax.ShortestRelativeNamespace(
                "Pine.Core",
                usings: []);

        result.Should().BeEquivalentTo(["Pine", "Core"]);
    }

    [Fact(Skip = "Inlining disabled for head")]
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
                        DefaultInterface: new ExprFunctionCompilationInterface(EnvItemsParamNames: [], ArgumentEvalGenericName: "eval")),
                    DeclarationSyntaxContext: new DeclarationSyntaxContext([])));

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

    [Fact]
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
            var result = Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression);

            result.Should().Be(expected);
        }
    }

    [Fact]
    public void Test_ExprMappedToParentEnv_PathInParentEnv_equality()
    {
        new ExprMappedToParentEnv.PathInParentEnv([])
            .Should().Be(new ExprMappedToParentEnv.PathInParentEnv([]));

        new ExprMappedToParentEnv.PathInParentEnv([1, 3])
            .Should().Be(new ExprMappedToParentEnv.PathInParentEnv([1, 3]));
    }

    [Fact]
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
                PineValueClass.Create(
                    observedPart: new ExpressionEnvClass.ConstrainedEnv([]),
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
                PineValueClass.Create(
                    observedPart: new ExpressionEnvClass.ConstrainedEnv([]),
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
                PineValueClass.Create(
                    observedPart: new ExpressionEnvClass.ConstrainedEnv([]),
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

            paths.Count.Should().Be(testCase.expectedPaths.Count, "Paths count should match");

            for (var i = 0; i < testCase.expectedPaths.Count; i++)
            {
                paths[i].Should().BeEquivalentTo(testCase.expectedPaths[i], options => options.WithStrictOrdering(), $"Path {i} should match expected path");
            }
        }
    }

    [Fact(Skip = "Switched to alternative compilation path")]
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
                using Pine.Core.CodeAnalysis;
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
                                KernelFunction.skip(1, env));

                        var env_1_0 =
                            KernelFunction.head(env_1);

                        return simpleFunction_uparam(env_1_0);
                    }


                    public static PineValue simpleFunction_uparam(PineValue env_1_0)
                    {
                        var env_1_0_as_int =
                            KernelFunction.SignedIntegerFromValueRelaxed(env_1_0);

                        var stack_3_as_int =
                            env_1_0_as_int + 1L;

                        PineValue stack_3 =
                            PineValue.EmptyList;

                        if (stack_3_as_int is { } stack_3_as_int_not_null)
                        {
                            stack_3 =
                                IntegerEncoding.EncodeSignedInteger(stack_3_as_int_not_null);
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
                using Pine.Core.CodeAnalysis;
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
                                KernelFunction.skip(1, env));

                        var env_1_0 =
                            KernelFunction.head(env_1);

                        return simpleFunction_uparam(env_1_0);
                    }


                    public static PineValue simpleFunction_uparam(PineValue env_1_0)
                    {
                        var env_1_0_as_int =
                            KernelFunction.SignedIntegerFromValueRelaxed(env_1_0);

                        var stack_3_as_int =
                            env_1_0_as_int * 3L;

                        var stack_4_as_int =
                            stack_3_as_int + 1L;

                        PineValue stack_4 =
                            PineValue.EmptyList;

                        if (stack_4_as_int is { } stack_4_as_int_not_null)
                        {
                            stack_4 =
                                IntegerEncoding.EncodeSignedInteger(stack_4_as_int_not_null);
                        }

                        return stack_4;
                    }
                }
                """
            ),

        ];

        for (var testCaseIndex = 0; testCaseIndex < testCases.Count; ++testCaseIndex)
        {
            var testCase = testCases[testCaseIndex];

            try
            {
                var appCodeTree =
                    BlobTreeWithStringPath.EmptyTree
                    .SetNodeAtPathSorted(
                        ["elm.json"],
                        BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
                    .SetNodeAtPathSorted(
                        ["src", "Common.elm"],
                        BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(testCase.InputModuleText)));

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
                    Pine.CompilePineToDotNet.CompileModuleToCSharp.BuildCSharpClassStringFromModule(
                        compiledModuleCommon.moduleValue,
                        containerConfig:
                        new SyntaxContainerConfig(
                            ContainerTypeName: "Common",
                            DictionaryMemberName: ""),
                        CodeAnalysis.CodeAnalysisTestHelper.DeclarationSyntaxContext);

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
