using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.CompilePineToDotNet;

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
                    pine_environment)
                .WithDefault(
                    PineValue.EmptyList));
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Formats_argument_list_in_constructor_invocation()
    {
        var inputSyntaxText =
            """
            new Expression.DecodeAndEvaluateExpression(arg_a, arg_b);
            """.Trim();

        var expectedFormattedText =
            """
            new Expression.DecodeAndEvaluateExpression(
                arg_a,
                arg_b);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Adds_newlines_between_statements_in_method_declaration()
    {
        var inputSyntaxText =
            """
            int method_name()
            {
                var local =
                    0;
                return local;
            }
            """.Trim();

        var expectedFormattedText =
            """
            int method_name()
            {
                var local =
                    0;

                return local;
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }


    [TestMethod]
    public void Indents_in_arrow_expression_clause()
    {
        var inputSyntaxText =
            """
            int method_declaration() =>
            1;
            """.Trim();

        var expectedFormattedText =
            """
            int method_declaration() =>
                1;
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Adds_newlines_in_conditional_expression()
    {
        var inputSyntaxText =
            """
            bool is_less_than_million(int integer) => integer < 1_000_000 ? true : false;
            """.Trim();

        var expectedFormattedText =
            """
            bool is_less_than_million(int integer) =>
                integer < 1_000_000
                ?
                true
                :
                false;
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Adds_newlines_between_member_declarations()
    {
        var inputSyntaxText =
            """
            class MyClass
            {
                public int MyField;
                public int MyFunction()
                {
                    return 1;
                }
            }
            """.Trim();

        var expectedFormattedText =
            """
            class MyClass
            {
                public int MyField;

                public int MyFunction()
                {
                    return 1;
                }
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Adds_newlines_before_method_calls()
    {
        var inputSyntaxText =
            """
            string method_declaration()
            {
                return
                    1.ToString().Trim().ToLower();
            }
            """.Trim();

        var expectedFormattedText =
            """
            string method_declaration()
            {
                return
                    1.ToString()
                    .Trim()
                    .ToLower();
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Adds_newlines_after_assignment()
    {
        var inputSyntaxText =
            """
            class MyClass
            {
                public int myField = 1;

                public int MyProperty { get; set; } = 1;

                void method_declaration()
                {
                    var local = 1;
                }
            }
            """.Trim();

        var expectedFormattedText =
            """
            class MyClass
            {
                public int myField =
                    1;

                public int MyProperty { get; set; } =
                    1;

                void method_declaration()
                {
                    var local =
                        1;
                }
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Formats_switch_expression()
    {
        var inputSyntaxText =
            """
            environment switch
            {
                PineValue.ListValue listValue => 11,
                _ =>
                13 }
            """.Trim();

        var expectedFormattedText =
            """
            environment switch
            {
                PineValue.ListValue listValue =>
                11,

                _ =>
                13
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    [TestMethod]
    public void Formats_nested_switch_expression()
    {
        var inputSyntaxText =
            """
            environment switch
            {
                PineValue.ListValue listValue =>
                listValue switch
                {
                [] =>17,

                _ =>
                    
                19   }
            
                _ =>
                13
            }
            """.Trim();

        var expectedFormattedText =
            """
            environment switch
            {
                PineValue.ListValue listValue =>
                listValue switch
                {
                    [] =>
                    17,

                    _ =>
                    19
                }

                _ =>
                13
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        StringAssert.Contains(formattedSyntaxText, expectedFormattedText);
    }

    static string FormatCSharpScript(string inputSyntaxText) =>
        FormatCSharpSyntaxRewriter.FormatSyntaxTree(ParseAsCSharpScript(inputSyntaxText))
        .GetRoot().ToFullString();

    static SyntaxTree ParseAsCSharpScript(string inputSyntaxText) =>
        SyntaxFactory.ParseSyntaxTree(
            inputSyntaxText,
            options: new CSharpParseOptions().WithKind(SourceCodeKind.Script));
}
