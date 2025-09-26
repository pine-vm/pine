using AwesomeAssertions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Pine.CompilePineToDotNet;
using Xunit;

namespace Pine.IntegrationTests;

public class FormatCSharpSyntaxRewriterTests
{
    [Fact]
    public void Formats_argument_list_in_invocation_expression()
    {
        var inputSyntaxText =
            """
            Result<string, PineValue>.ok(Pine.PineVM.KernelFunction.head(pine_environment).WithDefault(PineValue.EmptyList));
            """.Trim();

        var expectedFormattedText =
            """
            Result<string, PineValue>.ok(
                Pine.PineVM.KernelFunction.head(pine_environment)
                .WithDefault(PineValue.EmptyList));
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """
            new Expression.ParseAndEvalExpression(arg_a     , arg_b);
            """.Trim();

        var expectedFormattedText =
            """
            new Expression.ParseAndEvalExpression(arg_a, arg_b);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Does_not_line_break_empty_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """
            new Expression.EnvironmentExpression();
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(inputSyntaxText);
    }

    [Fact]
    public void Formats_nested_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """
            new Expression.ParseAndEvalExpression(new Expression.ParseAndEvalExpression(arg_a_a, arg_a_b), arg_b);
            """.Trim();

        var expectedFormattedText =
            """
            new Expression.ParseAndEvalExpression(
                new Expression.ParseAndEvalExpression(arg_a_a, arg_a_b),
                arg_b);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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
                    PineVM.ParseKernelApplicationExpressionThrowOnUnknownName("head", arg_a_a),
                    arg_a_b),
                arg_b);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }


    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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
                Console.WriteLine("Entering expr_function_240b663fa0_env_6acb138b");
            
                return expr_function_240b663fa0_env_6acb138b(eval_generic, pine_environment);
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_member_equals()
    {
        var inputSyntaxText =
            """
            class MyClass
            {
                public int myField =
                
                1;

                public int MyProperty { get; set; } =
                    1;

                void method_declaration()
                {
                    var local =     1;
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
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

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_function_arguments_simple_case_on_single_line()
    {
        var inputSyntaxText =
            """
            parseInt_specialized(param_1_0, param_1_1);
            """.Trim();

        var expectedFormattedText =
            """
            parseInt_specialized(param_1_0, param_1_1);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_function_arguments_complex_case_on_separate_lines()
    {
        var inputSyntaxText =
            """
            parseInt_specialized(Common.ValueFromPathInValueOrEmptyList(env, [1, 0]), param_1_1);
            """.Trim();

        var expectedFormattedText =
            """
            parseInt_specialized(
                Common.ValueFromPathInValueOrEmptyList(
                    env,
                    [1, 0]),
                param_1_1);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_collection_expression_simple_case_on_single_line()
    {
        var inputSyntaxText =
            """
            var list = [param_1_0, param_1_1, 42];
            """.Trim();

        var expectedFormattedText =
            """
            var list =
                [param_1_0, param_1_1, 42];
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_collection_expression_complex_case_on_separate_lines()
    {
        var inputSyntaxText =
            """
            var list = [PineValue.List([CommonValues.Tag_name_value_Ok, param_1_0]), param_1_1];
            """.Trim();

        var expectedFormattedText =
            """
            var list =
                [
                    PineValue.List(
                        [CommonValues.Tag_name_value_Ok, param_1_0]),
                    param_1_1
                ];
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_return_statement_simple_case_on_same_line()
    {
        var inputSyntaxText =
            """
            return parseInt_specialized(param_1_0, param_1_1);
            """.Trim();

        var expectedFormattedText =
            """
            return parseInt_specialized(param_1_0, param_1_1);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_return_statement_complex_case_on_new_line()
    {
        var inputSyntaxText =
            """
            return PineValue.List([PineValue.List([CommonValues.Tag_name_value_Ok, local_003]), local_004]);
            """.Trim();

        var expectedFormattedText =
            """
            return
                PineValue.List(
                    [
                        PineValue.List(
                            [CommonValues.Tag_name_value_Ok, local_003]),
                        local_004
                    ]);
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }


    [Fact]
    public void Adds_empty_line_between_blocks()
    {
        var inputSyntaxText =
            """
            if (condition1)
            {
                DoSomething();
            } {
                DoSomethingElse();
            }
            while (running)
            {
                Process();
                }
            """.Trim();

        var expectedFormattedText =
            """
            if (condition1)
            {
                DoSomething();
            }

            {
                DoSomethingElse();
            }

            while (running)
            {
                Process();
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Adds_empty_line_between_switch_cases()
    {
        var inputSyntaxText =
            """
            switch (value)
            {
                case 1:
                    DoOne();
                    break;
                case 2:
                    DoTwo();
                    break;
                default:
                    DoDefault();
                    break;
            }
            """.Trim();

        var expectedFormattedText =
            """
            switch (value)
            {
                case 1:
                    DoOne();
                    break;

                case 2:
                    DoTwo();
                    break;

                default:
                    DoDefault();
                    break;
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_simple_if_block()
    {
        var inputSyntaxText =
            """
            if(xyz) { DoSomething(); }
            """.Trim();

        var expectedFormattedText =
            """
            if (xyz)
            {
                DoSomething();
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_if_else_blocks()
    {
        var inputSyntaxText =
            """
            if(xyz) { DoSomething(); } else { DoSomethingElse(); }  
            """.Trim();

        var expectedFormattedText =
            """
            if (xyz)
            {
                DoSomething();
            }
            else
            {
                DoSomethingElse();
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_nested_if_else_blocks()
    {
        var inputSyntaxText =
            """
            if (abc)
            {
            if(xyz) { DoSomething(); } else { DoSomethingElse(); }
            }
            else
            {
            if (abcd)
            {
                DoSomething();
            }
            else
            {
                DoSomethingElse();
            }
            }            
            """.Trim();

        var expectedFormattedText =
            """
            if (abc)
            {
                if (xyz)
                {
                    DoSomething();
                }
                else
                {
                    DoSomethingElse();
                }
            }
            else
            {
                if (abcd)
                {
                    DoSomething();
                }
                else
                {
                    DoSomethingElse();
                }
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_nested_blocks()
    {
        var inputSyntaxText =
            """
            if (condition1)
            {
                DoSomething();
            } {
                DoSomethingElse();

            if (abc)
            {
                DoSomething();
            }
            while (running)
            {
                Process();

                if(abcd)
                {
            DoSomethingElse();
                }
                }
            }
            
            """.Trim();

        var expectedFormattedText =
            """
            if (condition1)
            {
                DoSomething();
            }

            {
                DoSomethingElse();

                if (abc)
                {
                    DoSomething();
                }

                while (running)
                {
                    Process();

                    if (abcd)
                    {
                        DoSomethingElse();
                    }
                }
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_nested_while_loop_with_collection_expressions()
    {
        var inputSyntaxText =
            """
            while (running)
            {
                var items = [GetItemFromDatabase(connectionString, [1, 2]), ProcessItem(currentItem)];
                while (items.Any())
                {
                    ProcessItems(items);
                    items = [GetNextBatch(query, [item.Id, item.Category]), ValidateItems(items)];
                }
            }
            """.Trim();

        var expectedFormattedText =
            """
            while (running)
            {
                var items =
                    [
                        GetItemFromDatabase(
                            connectionString,
                            [1, 2]),
                        ProcessItem(currentItem)
                    ];

                while (items.Any())
                {
                    ProcessItems(items);

                    items =
                        [
                            GetNextBatch(
                                query,
                                [item.Id, item.Category]),
                            ValidateItems(items)
                        ];
                }
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_nested_foreach_loop_with_method_invocations_in_collections()
    {
        var inputSyntaxText =
            """
            foreach (var category in categories)
            {
                var results = [ProcessCategory(category, [option1, option2]), ValidateCategory(category)];
                foreach (var result in results)
                {
                    UpdateDatabase(connectionString, [result.Id, TransformResult(result, [param1, param2])]);
                }
            }
            """.Trim();

        var expectedFormattedText =
            """
            foreach (var category in categories)
            {
                var results =
                    [
                        ProcessCategory(
                            category,
                            [option1, option2]),
                        ValidateCategory(category)
                    ];

                foreach (var result in results)
                {
                    UpdateDatabase(
                        connectionString,
                        [
                            result.Id,
                            TransformResult(
                                result,
                                [param1, param2])
                        ]);
                }
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_deeply_nested_collection_with_multiple_method_invocations()
    {
        var inputSyntaxText =
            """
            var simpleNested = [ProcessData(GetRawData([source1, source2])), TransformData(inputData)];
            """.Trim();

        var expectedFormattedText =
            """
            var simpleNested =
                [
                    ProcessData(
                        GetRawData(
                            [source1, source2])),
                    TransformData(inputData)
                ];
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }

    [Fact]
    public void Formats_while_loop_with_nested_collection_and_method_chain()
    {
        var inputSyntaxText =
            """
            while (condition)
            {
                var processedItems = [item.Transform([config1, config2]), GetDefaultItem()];
                DoSomething(processedItems);
            }
            """.Trim();

        var expectedFormattedText =
            """
            while (condition)
            {
                var processedItems =
                    [
                        item.Transform(
                            [config1, config2]),
                        GetDefaultItem()
                    ];

                DoSomething(processedItems);
            }
            """.Trim();

        var formattedSyntaxText = FormatCSharpScript(inputSyntaxText);

        formattedSyntaxText.Should().Be(expectedFormattedText);
    }


    static string FormatCSharpScript(string inputSyntaxText) =>
        FormatCSharpSyntaxRewriter.FormatSyntaxTree(ParseAsCSharpScript(inputSyntaxText))
        .GetRoot().ToFullString().TrimEnd();

    static SyntaxTree ParseAsCSharpScript(string inputSyntaxText) =>
        SyntaxFactory.ParseSyntaxTree(
            inputSyntaxText,
            options: new CSharpParseOptions().WithKind(SourceCodeKind.Script));
}
