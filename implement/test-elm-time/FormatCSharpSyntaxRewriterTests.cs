using FluentAssertions;
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
            Result<string, PineValue>.ok(Pine.PineVM.KernelFunction.head(pine_environment).WithDefault(PineValue.EmptyList));
            """.Trim();

        var expectedFormattedText =
            """
            Result<string, PineValue>.ok(
                Pine.PineVM.KernelFunction.head(
                    pine_environment)
                .WithDefault(
                    PineValue.EmptyList));
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Contain(expectedFormattedText);
    }

    [TestMethod]
    public void Formats_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """
            new Expression.ParseAndEvalExpression(arg_a, arg_b);
            """.Trim();

        var expectedFormattedText =
            """
            new Expression.ParseAndEvalExpression(
                arg_a,
                arg_b);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Contain(expectedFormattedText);
    }

    [TestMethod]
    public void Does_not_line_break_empty_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """
            new Expression.EnvironmentExpression();
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Contain(inputSyntaxText);
    }

    [TestMethod]
    public void Formats_nested_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """
            new Expression.ParseAndEvalExpression(new Expression.ParseAndEvalExpression(arg_a_a, arg_a_b), arg_b);
            """.Trim();

        var expectedFormattedText =
            """
            new Expression.ParseAndEvalExpression(
                new Expression.ParseAndEvalExpression(
                    arg_a_a,
                    arg_a_b),
                arg_b);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Contain(expectedFormattedText);
    }

    [TestMethod]
    public void Formats_nested_argument_list_in_obj_creation_expression_containing_invocation()
    {
        var inputSyntaxText =
            """
            new Expression.ParseAndEvalExpression(new Expression.ParseAndEvalExpression(PineVM.ParseKernelApplicationExpressionThrowOnUnknownName("head",arg_a_a), arg_a_b), arg_b);
            """.Trim();

        var expectedFormattedText =
            """
            new Expression.ParseAndEvalExpression(
                new Expression.ParseAndEvalExpression(
                    PineVM.ParseKernelApplicationExpressionThrowOnUnknownName(
                        "head",
                        arg_a_a),
                    arg_a_b),
                arg_b);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
    }

    [TestMethod]
    public void Indents_statements_in_if_statement_block()
    {
        var inputSyntaxText =
            """
            if (condition)
            {
                    Console.WriteLine(
                        "Entering expr_function_240b663fa0_env_6acb138b");

            return expr_function_240b663fa0_env_6acb138b(
                    eval_generic,
                    pine_environment);
            }
            """.Trim();

        var expectedFormattedText =
            """
            if (condition)
            {
                Console.WriteLine(
                    "Entering expr_function_240b663fa0_env_6acb138b");
            
                return expr_function_240b663fa0_env_6acb138b(
                        eval_generic,
                        pine_environment);
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
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

        formattedSyntaxText.Should().Contain(expectedFormattedText);
    }

    [TestMethod]
    public void Formats_parameter_list_in_method_declaration()
    {
        var inputSyntaxText =
            """
            void method_name(int arg_a, int arg_b)
            {
            }
            """.Trim();

        var expectedFormattedText =
            """
            void method_name(
                int arg_a,
                int arg_b)
            {
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Contain(expectedFormattedText);
    }


    static string FormatCSharpScript(string inputSyntaxText) =>
        FormatCSharpSyntaxRewriter.FormatSyntaxTree(ParseAsCSharpScript(inputSyntaxText))
        .GetRoot().ToFullString();

    static SyntaxTree ParseAsCSharpScript(string inputSyntaxText) =>
        SyntaxFactory.ParseSyntaxTree(
            inputSyntaxText,
            options: new CSharpParseOptions().WithKind(SourceCodeKind.Script));
}
