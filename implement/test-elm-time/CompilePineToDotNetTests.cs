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
                typeof(Pine.PineVM.PineVM.EvalExprDelegate),
                usings: []);

        Assert.AreEqual(
            "Pine.PineVM.PineVM.EvalExprDelegate",
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
    public void Test_compile_specialized_for_kernel_list_head()
    {
        var pineExpression =
            new Pine.PineVM.Expression.KernelApplicationExpression(
                functionName: nameof(Pine.PineVM.KernelFunction.list_head),
                argument: new Pine.PineVM.Expression.EnvironmentExpression(),
                function: null);

        var compiledFormattedExpression =
            CompiledFormattedCSharp(
                pineExpression,
                new Pine.CompilePineToDotNet.FunctionCompilationEnvironment(
                    ArgumentEnvironmentName: "environment",
                    ArgumentEvalGenericName: "eval"));

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
        Pine.CompilePineToDotNet.FunctionCompilationEnvironment environment)
    {
        var compiledExpression =
            Pine.CompilePineToDotNet.CompileToCSharp.CompileToCSharpExpression(
                expression,
                new Pine.CompilePineToDotNet.ExpressionCompilationEnvironment(
                    environment,
                    LetBindings: Pine.CompilePineToDotNet.CompiledExpression.NoLetBindings,
                    ParentEnvironment: null),
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
}
