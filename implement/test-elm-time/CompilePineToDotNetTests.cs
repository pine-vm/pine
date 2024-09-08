using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
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
                typeof(Pine.PineVM.EvalExprDelegate),
                usings: []);

        Assert.AreEqual(
            "Pine.PineVM.EvalExprDelegate",
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
            "Pine.Result<System.String,System.Int32>",
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
            "System.Collections.Generic.IReadOnlyDictionary<Pine.PineValue,System.String>",
            syntax.ToFullString());
    }

    [TestMethod]
    [Ignore("Inlining disabled for head")]
    public void Test_compile_specialized_for_kernel_head()
    {
        var pineExpression =
            new Pine.PineVM.Expression.KernelApplication(
                function: nameof(Pine.PineVM.KernelFunction.head),
                input: new Pine.PineVM.Expression.Environment());

        var compiledFormattedExpression =
            CompiledFormattedCSharp(
                pineExpression,
                new Pine.CompilePineToDotNet.FunctionCompilationEnv(
                    new Pine.CompilePineToDotNet.ExprFunctionCompilationInterface(
                        EnvItemsParamNames: [([], "environment")],
                        ArgumentEvalGenericName: "eval"),
                    CompilationUnit: new Pine.CompilePineToDotNet.CompilationUnitEnv(
                        AvailableExpr: ImmutableDictionary<Pine.PineVM.Expression, Pine.CompilePineToDotNet.CompilationUnitEnvExprEntry>.Empty,
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
        Pine.PineVM.Expression expression,
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
            ((Pine.PineVM.Expression)new Pine.PineVM.Expression.Environment(),
            (Pine.PineVM.ExprMappedToParentEnv?)new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([])),

            (Pine.PineVM.Expression.ListInstance([]),
            null),

            (new Pine.PineVM.Expression.KernelApplication(
                function: nameof(Pine.PineVM.KernelFunction.head),
                input: new Pine.PineVM.Expression.Environment()),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([0])),

            (new Pine.PineVM.Expression.KernelApplication(
                function: nameof(Pine.PineVM.KernelFunction.head),
                input: new Pine.PineVM.Expression.KernelApplication(
                    function: nameof(Pine.PineVM.KernelFunction.head),
                    input: new Pine.PineVM.Expression.Environment())),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([0, 0])),

            (new Pine.PineVM.Expression.KernelApplication(
                function: nameof(Pine.PineVM.KernelFunction.head),
                input: new Pine.PineVM.Expression.KernelApplication(
                    function: "skip",
                    input: Pine.PineVM.Expression.ListInstance(
                        [ Pine.PineVM.Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(13)),
                        Pine.PineVM.Expression.EnvironmentInstance
                        ]))),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([13])),

            (new Pine.PineVM.Expression.KernelApplication(
                function: nameof(Pine.PineVM.KernelFunction.head),
                input: new Pine.PineVM.Expression.KernelApplication(
                    function: "skip",
                    input: Pine.PineVM.Expression.ListInstance(
                        [ new Pine.PineVM.Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(21)),
                        new Pine.PineVM.Expression.KernelApplication(
                            function: nameof(Pine.PineVM.KernelFunction.head),
                            input: new Pine.PineVM.Expression.KernelApplication(
                                function: "skip",
                                input: Pine.PineVM.Expression.ListInstance(
                                    [ new Pine.PineVM.Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
                                    new Pine.PineVM.Expression.Environment()
                                    ])))
                        ]))),
                new Pine.PineVM.ExprMappedToParentEnv.PathInParentEnv([17,21])),

            (new Pine.PineVM.Expression.KernelApplication(
                function: nameof(Pine.PineVM.KernelFunction.head),
                input: new Pine.PineVM.Expression.KernelApplication(
                    function: "skip",
                    input: Pine.PineVM.Expression.ListInstance(
                        [ new Pine.PineVM.Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(23)),
                        new Pine.PineVM.Expression.KernelApplication(
                            function: nameof(Pine.PineVM.KernelFunction.head),
                            input: new Pine.PineVM.Expression.Environment())
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
                (Pine.PineVM.Expression)
                new Pine.PineVM.Expression.Environment(),

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
                (Pine.PineVM.Expression)
                new Pine.PineVM.Expression.KernelApplication(
                    function: nameof(Pine.PineVM.KernelFunction.head),
                    input: new Pine.PineVM.Expression.Environment()),

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
                (Pine.PineVM.Expression)
                new Pine.PineVM.Expression.KernelApplication(
                    function: nameof(Pine.PineVM.KernelFunction.head),
                    input:
                    new Pine.PineVM.Expression.KernelApplication(
                        function:"skip",
                        input: Pine.PineVM.Expression.ListInstance(
                            [
                            new Pine.PineVM.Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                            new Pine.PineVM.Expression.Environment()
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
