using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.PineVM;

namespace TestElmTime;

[TestClass]
public class FormatCSharpSyntaxRewriterTests
{
    [TestMethod]
    public void Formats_argument_list_in_invocation_expression()
    {
        var inputSyntaxText =
            """
            Result<string, PineValue>.ok(Pine.PineVM.KernelFunction.list_head(pine_environment).WithDefault(PineValue.EmptyList));
            """.Trim();

        var expectedFormattedText =
            """
            Result<string, PineValue>.ok(
                Pine.PineVM.KernelFunction.list_head(
                    pine_environment).WithDefault(
                    PineValue.EmptyList));
            """.Trim();

        var inputSyntaxTree = SyntaxFactory.ParseSyntaxTree(
            inputSyntaxText,
            options: new CSharpParseOptions().WithKind(SourceCodeKind.Script));

        var formattedSyntaxTree = new FormatCSharpSyntaxRewriter().Visit(inputSyntaxTree.GetRoot());

        var formattedSyntaxText = formattedSyntaxTree.ToFullString();

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }
}
