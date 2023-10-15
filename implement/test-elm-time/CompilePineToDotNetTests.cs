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
}
