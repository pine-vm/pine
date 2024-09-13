using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class CompilePineToDotNetTests
{
    public static readonly PineValue value_299b7decef = PineValueAsString.ValueFromString("List");

    public static readonly PineValue value_d597fb92e5 = PineValue.List([value_299b7decef, PineValue.EmptyList]);

    [TestMethod]
    public void Test_sort_pine_value_for_declaration()
    {
        Assert.IsTrue(value_d597fb92e5.ContainsInListTransitive(value_299b7decef));

        var listBeforeOrdering =
            new[]
            {
                PineValueAsString.ValueFromString("Err"),
                value_d597fb92e5,
                PineValueAsString.ValueFromString("Ok"),
                value_299b7decef
            };

        var listWithHashes =
            listBeforeOrdering
            .Select(value => (value, hash: CommonConversion.StringBase16(PineValueHashTree.ComputeHash(value))))
            .ToImmutableList();

        var orderedValues =
            Pine.CompilePineToDotNet.CSharpDeclarationOrder.OrderValuesForDeclaration(listBeforeOrdering)
            .ToImmutableList();

        CollectionAssert.AreEqual(
            new[]
            {
                PineValueAsString.ValueFromString("Ok"),
                PineValueAsString.ValueFromString("Err"),
                value_299b7decef,
                value_d597fb92e5
            },
            orderedValues);
    }

    [TestMethod]
    public void Test_compile_syntax_for_type_declared_in_type()
    {
        var syntax =
            Pine.CompilePineToDotNet.CompileTypeSyntax.TypeSyntaxFromType(
                typeof(EvalExprDelegate),
                usings: []);

        Assert.AreEqual(
            "Pine.Core.EvalExprDelegate",
            syntax.ToFullString());
    }

    [TestMethod]
    public void Test_compile_syntax_for_generic_type()
    {
        var syntax =
            Pine.CompilePineToDotNet.CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Result<string, int>),
                usings: []);

        Assert.AreEqual(
            "Pine.Core.Result<System.String,System.Int32>",
            syntax.ToFullString());
    }

    [TestMethod]
    public void Test_compile_syntax_for_generic_IReadOnlyDictionary()
    {
        var syntax =
            Pine.CompilePineToDotNet.CompileTypeSyntax.TypeSyntaxFromType(
                typeof(IReadOnlyDictionary<PineValue, string>),
                usings: []);

        Assert.AreEqual(
            "System.Collections.Generic.IReadOnlyDictionary<Pine.Core.PineValue,System.String>",
            syntax.ToFullString());
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
                new Pine.CompilePineToDotNet.FunctionCompilationEnv(
                    new Pine.CompilePineToDotNet.ExprFunctionCompilationInterface(
                        EnvItemsParamNames: [([], "environment")],
                        ArgumentEvalGenericName: "eval"),
                    CompilationUnit: new Pine.CompilePineToDotNet.CompilationUnitEnv(
                        AvailableExpr: ImmutableDictionary<Expression, Pine.CompilePineToDotNet.CompilationUnitEnvExprEntry>.Empty,
                        DefaultInterface: new Pine.CompilePineToDotNet.ExprFunctionCompilationInterface(EnvItemsParamNames: [], ArgumentEvalGenericName: "eval"))));

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

        Assert.AreEqual(
            expectedSyntaxNormalized.ToFullString(),
            compiledFormattedExpression.Syntax.ToFullString());
    }

    static Pine.CompilePineToDotNet.CompiledExpression CompiledFormattedCSharp(
        Expression expression,
        Pine.CompilePineToDotNet.FunctionCompilationEnv environment)
    {
        var compiledExpression =
            Pine.CompilePineToDotNet.CompileToCSharp.CompileToCSharpExpression(
                expression,
                new Pine.CompilePineToDotNet.ExpressionCompilationEnvironment(
                    environment,
                    LetBindings: Pine.CompilePineToDotNet.CompiledExpression.NoLetBindings,
                    ParentEnvironment: null,
                    EnvConstraint: null),
                createLetBindingsForCse: false)
            .Extract(err => throw new System.Exception(err));

        return
            compiledExpression
            with
            {
                Syntax =
                Pine.CompilePineToDotNet.FormatCSharpSyntaxRewriter.FormatSyntaxTree(compiledExpression.Syntax)
                .NormalizeWhitespace()
            };
    }

    [TestMethod]
    public void Test_TryParseExpressionAsIndexPathFromEnv()
    {
        var testCases = new[]
        {
            ((Expression)new Expression.Environment(),
            (Pine.PineVM.ExprMappedToParentEnv?)new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([])),

            (Expression.ListInstance([]),
            null),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.Environment()),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([0])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: nameof(KernelFunction.head),
                    input: new Expression.Environment())),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([0, 0])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: "skip",
                    input: Expression.ListInstance(
                        [ Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(13)),
                        Expression.EnvironmentInstance
                        ]))),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([13])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: "skip",
                    input: Expression.ListInstance(
                        [ new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(21)),
                        new Expression.KernelApplication(
                            function: nameof(KernelFunction.head),
                            input: new Expression.KernelApplication(
                                function: "skip",
                                input: Expression.ListInstance(
                                    [ new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
                                    new Expression.Environment()
                                    ])))
                        ]))),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([17,21])),

            (new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: new Expression.KernelApplication(
                    function: "skip",
                    input: Expression.ListInstance(
                        [ new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(23)),
                        new Expression.KernelApplication(
                            function: nameof(KernelFunction.head),
                            input: new Expression.Environment())
                        ]))),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([0,23])),
        };

        foreach (var (expression, expected) in testCases)
        {
            var result = Pine.PineVM.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression);

            Assert.AreEqual(expected, result);
        }
    }

    [TestMethod]
    public void Test_ExprMappedToParentEnv_PathInParentEnv_equality()
    {
        Assert.AreEqual(
            new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([]),
            new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([]));

        Assert.AreEqual(
            new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([1, 3]),
            new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([1, 3]));
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
                Pine.PineVM.EnvConstraintId.Create(
                    envClass: new Pine.PineVM.ExpressionEnvClass.ConstrainedEnv([]),
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
                Pine.PineVM.EnvConstraintId.Create(
                    envClass: new Pine.PineVM.ExpressionEnvClass.ConstrainedEnv([]),
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
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                            new Expression.Environment()
                            ]))),

                envConstraint =
                Pine.PineVM.EnvConstraintId.Create(
                    envClass: new Pine.PineVM.ExpressionEnvClass.ConstrainedEnv([]),
                    PineValue.EmptyList,
                    skipUnavailableItems: false),

                expectedPaths = (IReadOnlyList<IReadOnlyList<int>>)[[13]]
            },
        };

        foreach (var testCase in testCases)
        {
            var paths =
                Pine.CompilePineToDotNet.FunctionCompilationEnv.CompileEnvItemsPathsForExprFunction(
                    testCase.expr,
                    testCase.envConstraint);

            Assert.AreEqual(testCase.expectedPaths.Count, paths.Count, "Paths count");

            for (var i = 0; i < testCase.expectedPaths.Count; i++)
            {
                Assert.IsTrue(paths[i].SequenceEqual(testCase.expectedPaths[i]), "Path " + i);
            }
        }
    }
}
