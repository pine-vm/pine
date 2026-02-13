using AwesomeAssertions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Pine.Core.DotNet;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using Xunit;

namespace Pine.Core.Tests.DotNet;

/// <summary>
/// Test for the specifications from 'csharp-coding-guidelines.md'
/// </summary>
public class CSharpFormatTests
{
    [Fact]
    public void Preserves_argument_list_in_invocation_expression()
    {
        var inputSyntaxText =
            """"
            Result<string, PineValue>.ok(Pine.PineVM.KernelFunction.head(pine_environment).WithDefault(PineValue.EmptyList));
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """"
            new Expression.ParseAndEvalExpression(arg_a     , arg_b);
            """";

        var expectedFormattedText =
            """"
            new Expression.ParseAndEvalExpression(arg_a, arg_b);
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Does_not_line_break_empty_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """"
            new Expression.EnvironmentExpression();
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_nested_argument_list_in_obj_creation_expression()
    {
        var inputSyntaxText =
            """"
            new Expression.ParseAndEvalExpression(new Expression.ParseAndEvalExpression(arg_a_a, arg_a_b), arg_b);
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_nested_argument_list_in_obj_creation_expression_containing_invocation_exceeding_line_length()
    {
        var inputSyntaxText =
            """"
            new Expression.ParseAndEvalExpression(new Expression.ParseAndEvalExpression(PineVM.ParseKernelApplicationExpressionThrowOnUnknownName("head",arg_a_a), arg_a_b), arg_b);
            """";

        var expectedFormattedText =
            """"
            new Expression.ParseAndEvalExpression(
                new Expression.ParseAndEvalExpression(
                    PineVM.ParseKernelApplicationExpressionThrowOnUnknownName("head", arg_a_a),
                    arg_a_b),
                arg_b);
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Adds_newlines_between_statements_in_method_declaration()
    {
        var inputSyntaxText =
            """"
            int method_name()
            {
                var local =
                    0;
                return local;
            }
            """";

        var expectedFormattedText =
            """"
            int method_name()
            {
                var local =
                    0;

                return local;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Indents_in_arrow_expression_clause()
    {
        var inputSyntaxText =
            """"
            int method_declaration() =>
            1;
            """";

        var expectedFormattedText =
            """"
            int method_declaration() =>
                1;
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Indents_statements_in_if_statement_block()
    {
        var inputSyntaxText =
            """"
            if (condition)
            {
                    Console.WriteLine(
                        "Entering expr_function_240b663fa0_env_6acb138b");

            return expr_function_240b663fa0_env_6acb138b(
                    eval_generic,
                    pine_environment);
            }
            """";

        var expectedFormattedText =
            """"
            if (condition)
            {
                Console.WriteLine(
                    "Entering expr_function_240b663fa0_env_6acb138b");
            
                return
                    expr_function_240b663fa0_env_6acb138b(
                        eval_generic,
                        pine_environment);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Preserves_conditional_expression_on_single_line()
    {
        var inputSyntaxText =
            """"
            bool is_less_than_million(int integer) => integer < 1_000_000 ? true : false;
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Adds_newlines_between_member_declarations()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                public int MyField;
                public int MyFunction()
                {
                    return 1;
                }
            }
            """";

        var expectedFormattedText =
            """"
            class MyClass
            {
                public int MyField;


                public int MyFunction()
                {
                    return 1;
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Preserves_nested_invocations_single_line()
    {
        var inputSyntaxText =
            """"
            string method_declaration()
            {
                return
                    1.ToString().Trim().ToLower();
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_member_equals()
    {
        var inputSyntaxText =
            """"
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
            """";

        var expectedFormattedText =
            """"
            class MyClass
            {
                public int myField =
                    1;


                public int MyProperty { get; set; } =
                    1;


                void method_declaration()
                {
                    var local = 1;
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_switch_expression()
    {
        var inputSyntaxText =
            """"
            environment switch
            {
                PineValue.ListValue listValue => 11,
                _ =>
                13 }
            """";

        var expectedFormattedText =
            """"
            environment switch
            {
                PineValue.ListValue listValue => 11,

                _ =>
                13
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_nested_switch_expression()
    {
        var inputSyntaxText =
            """"
            environment switch
            {
                PineValue.ListValue listValue =>
                listValue switch
                {
                [] =>17,

                _ =>
                    
                19   },
            
                _ =>
                13
            }
            """";

        var expectedFormattedText =
            """"
            environment switch
            {
                PineValue.ListValue listValue =>
                listValue switch
                {
                    [] => 17,

                    _ =>
                    19
                },

                _ =>
                13
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_parameter_list_single_line_in_method_declaration()
    {
        var inputSyntaxText =
            """"
            void method_name(int arg_a, int    arg_b,  int arg_c)
            {
            }
            """";

        var expectedFormattedText =
            """
            void method_name(int arg_a, int arg_b, int arg_c)
            {
            }
            """.Trim();

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_parameter_list_multiline_in_method_declaration()
    {
        var inputSyntaxText =
            """"
            void method_name(int arg_a, int arg_b,
            int arg_c)
            {
            }
            """";

        var expectedFormattedText =
            """"
            void method_name(
                int arg_a,
                int arg_b,
                int arg_c)
            {
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Preserves_function_arguments_simple_case_on_single_line()
    {
        var inputSyntaxText =
            """"
            parseInt_specialized(param_1_0, param_1_1);
            """";

        var expectedFormattedText =
            """
            parseInt_specialized(param_1_0, param_1_1);
            """.Trim();

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Preserves_function_arguments_simple_case_on_multiple_line()
    {
        var inputSyntaxText =
            """"
            parseInt_specialized(
                param_1_0,
                param_1_1);
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_function_arguments_complex_case_on_separate_lines()
    {
        var inputSyntaxText =
            """"
            parseInt_specialized(Common.ValueFromPathInValueOrEmptyList(env, [1, 0]), param_1_1);
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_collection_expression_simple_case_on_single_line()
    {
        var inputSyntaxText =
            """"
            var list = [param_1_0, param_1_1, 42];
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_collection_expression_complex_case_on_separate_lines()
    {
        var inputSyntaxText =
            """"
            var list = [PineValue.List([CommonValues.Tag_name_value_Ok, param_1_0]), param_1_1];
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_return_statement_simple_case_on_same_line()
    {
        var inputSyntaxText =
            """"
            return parseInt_specialized(param_1_0, param_1_1);
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_return_statement_complex_case_on_new_line()
    {
        var inputSyntaxText =
            """"
            return PineValue.List([PineValue.List([CommonValues.Tag_name_value_Ok, local_003]), local_004]);
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Adds_empty_line_between_blocks()
    {
        var inputSyntaxText =
            """"
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
            """";

        var expectedFormattedText =
            """"
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
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Adds_empty_line_between_switch_cases()
    {
        var inputSyntaxText =
            """"
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
            """";

        var expectedFormattedText =
            """"
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
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_simple_if_block()
    {
        var inputSyntaxText =
            """"
            if(xyz) { DoSomething(); }
            """";

        var expectedFormattedText =
            """"
            if (xyz)
            {
                DoSomething();
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_if_else_blocks()
    {
        var inputSyntaxText =
            """"
            if(xyz) { DoSomething(); } else { DoSomethingElse(); }  
            """";

        var expectedFormattedText =
            """"
            if (xyz)
            {
                DoSomething();
            }
            else
            {
                DoSomethingElse();
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_nested_if_else_blocks()
    {
        var inputSyntaxText =
            """"
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
            """";

        var expectedFormattedText =
            """"
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
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_nested_blocks()
    {
        var inputSyntaxText =
            """"
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
            
            """";

        var expectedFormattedText =
            """"
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
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
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
                var items = [GetItemFromDatabase(connectionString, [1, 2]), ProcessItem(currentItem)];

                while (items.Any())
                {
                    ProcessItems(items);
                    items = [GetNextBatch(query, [item.Id, item.Category]), ValidateItems(items)];
                }
            }
            """.Trim();

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
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
                var results = [ProcessCategory(category, [option1, option2]), ValidateCategory(category)];

                foreach (var result in results)
                {
                    UpdateDatabase(connectionString, [result.Id, TransformResult(result, [param1, param2])]);
                }
            }
            """.Trim();

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_deeply_nested_collection_with_multiple_method_invocations()
    {
        var inputSyntaxText =
            """
            var simpleNested = [ProcessData(GetRawData([source1, source2])), TransformData(inputData)];
            """.Trim();

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_while_loop_with_nested_collection_and_method_chain()
    {
        var inputSyntaxText =
            """"
            while (condition)
            {
                var processedItems = [item.Transform([config1, config2]), GetDefaultItem()];
                DoSomething(processedItems);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_indexer_setter_assignments()
    {
        var inputSyntaxText =
            """"
            void setup(Dictionary<string, object> dict)
            {
                dict[CommonReusedValues.List_9a1e26cb] =
                Dispatch_9a1e26cb;

                dict[CommonReusedValues.List_6a38b355] = Dispatch_6a38b355;

                dict[simple_key] = method_call(param1, param2);
            }
            """";

        var expectedFormattedText =
            """"
            void setup(Dictionary<string, object> dict)
            {
                dict[CommonReusedValues.List_9a1e26cb] =
                    Dispatch_9a1e26cb;

                dict[CommonReusedValues.List_6a38b355] = Dispatch_6a38b355;

                dict[simple_key] = method_call(param1, param2);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_file_scoped_namespace_with_two_empty_lines_before_class()
    {
        var inputSyntaxText =
            """"
            namespace MyNamespace;
            public class MyClass
            {
                public int MyField;
            }
            """";

        var expectedFormattedText =
            """"
            namespace MyNamespace;

            public class MyClass
            {
                public int MyField;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: false);
    }


    [Fact]
    public void Formats_using_directives_with_static_usings_separated()
    {
        var inputSyntaxText =
            """"
            using Zebra;
            using static System.Math;
            using System.Collections.Generic;
            using static System.Console;
            using Apple;
            namespace MyNamespace;
            public class MyClass
            {
            }
            """";

        var expectedFormattedText =
            """"
            using Apple;
            using System.Collections.Generic;
            using Zebra;

            using static System.Console;
            using static System.Math;

            namespace MyNamespace;

            public class MyClass
            {
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: false);
    }


    [Fact]
    public void Formats_using_directives_followed_by_namespace_with_two_empty_lines()
    {
        var inputSyntaxText =
            """"
            using System;
            using System.Linq;
            namespace MyNamespace
            {
                public class MyClass
                {
                }
            }
            """";

        var expectedFormattedText =
            """"
            using System;
            using System.Linq;

            namespace MyNamespace
            {
                public class MyClass
                {
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: false);
    }


    [Fact]
    public void Sorts_using_directives_case_insensitive()
    {
        var inputSyntaxText =
            """"
            using Pine.Core.IO;
            using Pine.Core.Http;
            using Pine.Core.Interpreter.IntermediateVM;
            namespace MyNamespace;
            public class MyClass
            {
            }
            """";

        var expectedFormattedText =
            """"
            using Pine.Core.Http;
            using Pine.Core.Interpreter.IntermediateVM;
            using Pine.Core.IO;

            namespace MyNamespace;

            public class MyClass
            {
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: false);
    }


    [Fact]
    public void Preserves_named_argument_layout()
    {
        var inputSyntaxText =
            """"
            KernelFunctionFused.TakeLast(
                takeCountValue: Global_Anonymous.zzz_anon_627f403e_dca18c16(
                    KernelFunction.length(local_000),
                    local_000),
                argument: local_000);
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_single_line_declarations_without_blank_line()
    {
        var inputSyntaxText =
            """"
            void method_name()
            {
                var a = 1;
                var b = 2;
                var c = 3;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Indentation_depends_only_on_content_not_on_existing_indent()
    {
        var inputSyntaxText =
            """"
            if (condition)
            {
                        DoSomething();
                                    DoSomethingElse();
            }
            """";

        var expectedFormattedText =
            """"
            if (condition)
            {
                DoSomething();
                DoSomethingElse();
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Preserves_single_line_comments_in_method_body()
    {
        var inputSyntaxText =
            """"
            void method_name()
            {
                // This is a comment about x
                var x = 1;
                // This is a comment about y
                var y = 2;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_multi_line_comments_in_method_body()
    {
        var inputSyntaxText =
            """"
            void method_name()
            {
                /* This is a
                   multi-line comment */
                var x = 1;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_trailing_single_line_comment()
    {
        var inputSyntaxText =
            """"
            void method_name()
            {
                var x = 1; // inline comment
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_xml_doc_comments_on_class()
    {
        var inputSyntaxText =
            """"
            /// <summary>
            /// This is a doc comment on the class.
            /// </summary>
            class MyClass
            {
                public int MyField;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_xml_doc_comments_on_method()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                /// <summary>
                /// This is a doc comment on the method.
                /// </summary>
                /// <param name="x">The parameter.</param>
                /// <returns>The result.</returns>
                int MyMethod(int x)
                {
                    return x;
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_xml_doc_comments_on_file_scoped_class()
    {
        var inputSyntaxText =
            """"
            using System;

            namespace MyNamespace;

            /// <summary>
            /// This is a doc comment on the class.
            /// </summary>
            public class MyClass
            {
                /// <summary>
                /// This is a doc comment on the method.
                /// </summary>
                public int MyMethod()
                {
                    return 1;
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Stable_method_with_tuple_return_type_and_single_param_on_new_line()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                public static (ReadOnlyMemory<byte> hashBytes, int encodingBytesLength)
                    ComputeHashForValue(
                    PineValue value)
                {
                    using var stream = new System.IO.MemoryStream();

                    ValueEncodingFlatDeterministic.Encode(stream, value);

                    var encodingBytes = stream.ToArray();

                    var hashBytes = SHA256.HashData(encodingBytes);

                    return (hashBytes, encodingBytes.Length);
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Aligns_indentation_in_multiline_invocation()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
               public void MyMethod()
               {
                  var hash =
                       PineValueHashTree.ComputeHash(
                           pineValue,
                              delegateGetHashOfComponent: TryGetCached,
                            reportComputedHash: RecordComputed);
                }
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                public void MyMethod()
                {
                    var hash =
                        PineValueHashTree.ComputeHash(
                            pineValue,
                            delegateGetHashOfComponent: TryGetCached,
                            reportComputedHash: RecordComputed);
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Consolidates_spaces_in_else_if()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                public void MyMethod()
                {
                    if (charSpaceIndex is 0)
                    {
                        if (currentByte is 32)
                        {
                            charSpaceIndex = i;
                        }
                    }
                    else   if  (currentByte is 0)
                    {
                        charNullIndex = i;
                        break;
                    }
                }
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                public void MyMethod()
                {
                    if (charSpaceIndex is 0)
                    {
                        if (currentByte is 32)
                        {
                            charSpaceIndex = i;
                        }
                    }
                    else if (currentByte is 0)
                    {
                        charNullIndex = i;
                        break;
                    }
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Consolidates_indentation_in_switch_block()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                public void MyMethod()
                {
                    switch(node)
                    {
                       case   PineValue.BlobValue:
                          stack.Push((node, true));
                          break;

                        case PineValue.ListValue listValue:
                              stack.Push((node, true));

                           var itemsSpan = listValue.Items.Span;

                            for(var i = itemsSpan.Length - 1; i >= 0; i--)
                            {
                                  stack.Push((itemsSpan[i], false)  );
                            }

                            break;

                       default :
                            throw new NotImplementedException(
                                  "Not implemented for value type: " + node.GetType().FullName);
                    }
                }
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                public void MyMethod()
                {
                    switch (node)
                    {
                        case PineValue.BlobValue:
                            stack.Push((node, true));
                            break;
            
                        case PineValue.ListValue listValue:
                            stack.Push((node, true));
            
                            var itemsSpan = listValue.Items.Span;
            
                            for (var i = itemsSpan.Length - 1; i >= 0; i--)
                            {
                                stack.Push((itemsSpan[i], false));
                            }
            
                            break;
            
                        default:
                            throw new NotImplementedException(
                                "Not implemented for value type: " + node.GetType().FullName);
                    }
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_single_line_invocation_and_assignment()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                public void MyMethod()
                {
                    if (!computedHashes.TryGetValue(child, out var childHash))
                    {
                        throw new InvalidOperationException("Missing hash for child value.");
                    }

                    elementHashes[i] = childHash;
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_various_single_line_statements_and_expressions()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                public void MyMethod()
                {
                    var asciiStringUpToFirstSpace =
                        System.Text.Encoding.ASCII.GetString(serializedValue.Span[..charSpaceIndex]);

                    var asciiStringDigits =
                        System.Text.Encoding.ASCII.GetString(
                            serializedValue.Span[(charSpaceIndex + 1)..charNullIndex]);

                    if (loadResult.IsOkOrNull() is not { } ok)
                        throw new Exception("Unexpected result: " + loadResult);

                    if (ComputeHash(pineValue).Span.SequenceEqual(hash.Span))
                        return pineValue;

                    if (asciiStringUpToFirstSpace is "blob")
                    {
                        var expectedCount = serializedValue.Length - charNullIndex - 1;

                        var count = int.Parse(asciiStringDigits);

                        if (count != expectedCount)
                            return "Unexpected count: got " + count + ", but I expected " + expectedCount;

                        return PineValue.Blob(serializedValue[(charNullIndex + 1)..]);
                    }
                }


                public static ReadOnlyMemory<byte> ComputeHashSorted(FileTree treeNode) =>
                    ComputeHashNotSorted(FileTree.Sort(treeNode));


                public ReadOnlyMemory<byte> GetHash(PineValue pineValue) =>
                    GetHash(pineValue, shouldCache: _ => true);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_record_declaration_members()
    {
        var inputSyntaxText =
            """"
            internal readonly record struct HashComputationResult(
                ReadOnlyMemory<byte> RootHash,
                ReadOnlyMemory<byte> RootSerialization,
                IReadOnlyCollection<PineValue> RootDependencies);


            public record CacheByFileName(IFileStore FileStore)
            {
                public ReadOnlyMemory<byte> GetOrUpdate(string fileName, Func<ReadOnlyMemory<byte>> getNew) =>
                    GetOrTryAdd(fileName, () => getNew())!.Value;


                public ReadOnlyMemory<byte>? GetOrTryAdd(string fileName, Func<ReadOnlyMemory<byte>?> tryBuild)
                {
                    var entryPath = ImmutableList.Create(fileName);

                    var fromCache = FileStore.GetFileContent(entryPath);

                    if (fromCache is not null)
                        return fromCache;

                    var file = tryBuild();

                    if (file.HasValue)
                    {
                        FileStore.SetFileContent(entryPath, file.Value.ToArray());
                    }

                    return file;
                }


                public record InnerR(
                    int A,
                    int B)
                {
                    public int Sum(
                        int x,
                        int y)
                    {
                        return A + B + x + y;
                    }
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Preserves_interface_declaration_members()
    {
        var inputSyntaxText =
            """"
            public interface IConsole
            {
                void WriteLine(string text) =>
                    WriteLine(text, TextColor.Default);


                void WriteLine(string text, TextColor color) =>
                    Write(text + "\n", color);


                void Write(string text, TextColor color) =>
                    Write([(text, color)]);


                void Write(IReadOnlyList<(string text, TextColor color)> coloredTexts);


                public record struct ColoredText(string text, TextColor color);


                public enum TextColor
                {
                    Default,
                    Green,
                    Red
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Preserves_try_catch_block()
    {
        var inputSyntaxText =
            """"
            try
            {
                var json = JsonSerializer.Deserialize(lineText, EchoJsonSerializerContext.Default.JsonElement);

                Console.WriteLine(JsonSerializer.Serialize(json, EchoJsonSerializerContext.Default.JsonElement) + "\n");
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine("Failed with runtime exception:\n" + ex);
                return 1;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_lambda_in_invocation_argument()
    {
        var inputSyntaxText =
            """"
            echoJsonCommand.SetHandler(
                () =>
                {
                    EchoJson.EchoJsonLoop();
                });
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_class_parameters()
    {
        var inputSyntaxText =
            """"
            public class FormatCSharpSyntaxRewriter(
                char indentChar,
                int indentCharsPerLevel)
                : CSharpSyntaxRewriter
            {
                public FormatCSharpSyntaxRewriter()
                    : this(
                          indentChar: ' ',
                          indentCharsPerLevel: 4)
                {
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Preserves_pipeline_with_lambda()
    {
        var inputSyntaxText =
            """"
            var reformattedArgs =
                node.Arguments
                .Select((argumentSyntax, index) =>
                {
                    var trailingComments = StripWhitespaceTrivia(argumentSyntax.GetTrailingTrivia());

                    if (index is 0)
                        return argumentSyntax.WithLeadingTrivia().WithTrailingTrivia(trailingComments);
                    else
                        return argumentSyntax.WithLeadingTrivia(s_singleSpace).WithTrailingTrivia(trailingComments);

                    return
                        element
                        .WithLeadingTrivia(leadingTrivia)
                        .WithTrailingTrivia(trailingComments);
                }).ToList();

            var reformattedMultiLineArgs =
                node.Arguments
                .Select((argumentSyntax, index) =>
                {
                    var originalArg = originalNode.Arguments[index];
                    var trailingComments = StripWhitespaceTrivia(originalArg.GetTrailingTrivia());
            
                    // Preserve leading comments on the argument, with LineFeed prefix for multi-line layout
                    var leadingTrivia =
                        HasPreservableTrivia(originalArg.GetLeadingTrivia())
                        ?
                        new SyntaxTriviaList(SyntaxFactory.LineFeed)
                        .AddRange(BuildLeadingTriviaPreservingComments(
                            originalArg.GetLeadingTrivia(),
                            argumentIndentationTrivia))
                        :
                        new SyntaxTriviaList(SyntaxFactory.LineFeed, argumentIndentationTrivia);
            
                    return
                        argumentSyntax
                        .WithLeadingTrivia(leadingTrivia)
                        .WithTrailingTrivia(trailingComments);
                }).ToList();
            
            """";

        AssertFormattingIsStable(inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_pipeline_with_multiple_invocations_and_named_arguments()
    {
        var inputSyntaxText =
            """"
            var responseSlice =
                responseChunk
                .PadRight(totalWidth: expectedChunk.Length, ' ')
                .Substring(startIndex: sliceStartIndex, length: sliceLength);
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_empty_lines_between_comments()
    {
        var inputSyntaxText =
            """"
            class MyClass
                : IMyInterface
            {
                void Method() =>
                    DoSomething();


                // This is a comment.
                // Another comment.

                /// <inheritdoc/>
                void MyMethod()
                {
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_comments_after_return_statement()
    {
        var inputSyntaxText =
            """"
            void A()
            {
                return; // This is a comment after a return statement.
            }


            void B()
            {
                return; /* This is a comment after a return statement. */
            }


            int C()
            {
                return 13;
                // This is a comment after a return statement.
            }

            
            int D()
            {
                return 17;

                // This is a comment after a return statement.
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_switch_expression_in_initializer()
    {
        var inputSyntaxText =
            """"
            System.Console.ForegroundColor =
               color   switch
               {
                    IConsole.TextColor.Default =>
                        InitialForegroundColor,
                    IConsole.TextColor.Green => System.ConsoleColor.Green,
                    IConsole.TextColor.Red => System.ConsoleColor.Red,
                    _ => throw new System.NotImplementedException(),
               };
            """";

        var expectedSyntaxText =
            """"
            System.Console.ForegroundColor =
                color switch
                {
                    IConsole.TextColor.Default =>
                    InitialForegroundColor,

                    IConsole.TextColor.Green => System.ConsoleColor.Green,
                    IConsole.TextColor.Red => System.ConsoleColor.Red,

                    _ =>
                    throw new System.NotImplementedException(),
                };
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_conditional_in_arrow_body_expression()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                private static ReadOnlyMemory<byte> CountEncoding(uint count) =>
                    count < s_preencodedCounts.Length
                        ? s_preencodedCounts[(int)count]
                        : System.Text.Encoding.ASCII.GetBytes(count.ToString());
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                private static ReadOnlyMemory<byte> CountEncoding(uint count) =>
                    count < s_preencodedCounts.Length
                    ?
                    s_preencodedCounts[(int)count]
                    :
                    System.Text.Encoding.ASCII.GetBytes(count.ToString());
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_single_line_and_multi_line_lists()
    {
        var inputSyntaxText =
            """"
            int[] alfa =
                [1, 2,
                3];

            var beta =
                func(1, 2, 3);

            var gamma =
                func(1, 2,
                3);

            """";

        var expectedSyntaxText =
            """"
            int[] alfa =
                [
                1,
                2,
                3
                ];

            var beta =
                func(1, 2, 3);

            var gamma =
                func(
                    1,
                    2,
                    3);

            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_initializer_multi_line_containing_object_initializer()
    {
        var inputSyntaxText =
            """"
            var mutatedDict = new Dictionary<string, PineValue>
            {
                [expectedInCompilerKey] =
                PineValue.List(
                    [
                        .. source.ValuesExpectedInCompilerBlobs
                        .Cast<PineValue>(),
                        .. source.ValuesExpectedInCompilerLists
                    ])
            };

            """";

        var expectedSyntaxText =
            """"
            var mutatedDict =
                new Dictionary<string, PineValue>
                {
                    [expectedInCompilerKey] =
                    PineValue.List(
                        [
                        .. source.ValuesExpectedInCompilerBlobs
                        .Cast<PineValue>(),
                        .. source.ValuesExpectedInCompilerLists
                        ])
                };

            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_indentation_in_pipeline()
    {
        var inputSyntaxText =
            """"
            normalizedParam =
                normalizedParam
            .WithIdentifier(normalizedParam.Identifier.WithLeadingTrivia());

            """";

        var expectedSyntaxText =
            """"
            normalizedParam =
                normalizedParam
                .WithIdentifier(normalizedParam.Identifier.WithLeadingTrivia());

            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_indentation_in_local_method_and_pipeline()
    {
        var inputSyntaxText =
            """"
            void M()
            {
                LocalDeclarationStatementSyntax ParamDeclarationStatement(IReadOnlyList<int> paramPath)
                {
                    var localName = paramToLocalMap[paramPath];

                    var (declType, initExpr) = ParamDeclarationInitExpr(paramPath);

                    return
                        SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(
                                CompileTypeSyntax.TypeSyntaxFromType(declType, declarationSyntaxContext))
                            .WithVariables(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.VariableDeclarator(
                                        SyntaxFactory.Identifier(localName))
                                    .WithInitializer(
                                        SyntaxFactory.EqualsValueClause(
                                            initExpr)))));
                }
            }

            return
                classDeclarations
                .ToFrozenDictionary(
                    cd => FilePathFromClassDeclaration(cd.declSyntax, [.. namespacePrefix, .. cd.namespaces]),
                    cd =>
                    (ReadOnlyMemory<byte>)
                    Encoding.UTF8.GetBytes(
                        BuildCompilationUnitSyntax(
                            cd.declSyntax,
                            staticProgram.DeclarationSyntaxContext,
                            [.. namespacePrefix, .. cd.namespaces]).ToFullString())
                    .AsMemory(),
                    comparer:
                    EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Switches_argument_list_exceeding_length_to_multi_line()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                void M()
                {
                    func("123456789", "123456789", "123456789", "123456789", "123456789", 123456789, "123456789", "123456789", decl_name);
                }
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                void M()
                {
                    func(
                        "123456789",
                        "123456789",
                        "123456789",
                        "123456789",
                        "123456789",
                        123456789,
                        "123456789",
                        "123456789",
                        decl_name);
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Breaks_initializer_to_new_line_when_exceeding_line_length()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                void M()
                {
                    var very_long_variable_name_that_exceeds_the_maximum_line_length_limit = CreateWithVeryLongMethodName("alpha", "bravo", "charlie");
                }
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                void M()
                {
                    var very_long_variable_name_that_exceeds_the_maximum_line_length_limit =
                        CreateWithVeryLongMethodName("alpha", "bravo", "charlie");
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Separates_type_declarations_by_two_empty_lines()
    {
        var inputSyntaxText =
            """"
            class FirstClass
            {
                public int Field;
            }
            class SecondClass
            {
                public int Field;
            }
            """";

        var expectedSyntaxText =
            """"
            class FirstClass
            {
                public int Field;
            }


            class SecondClass
            {
                public int Field;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Breaks_expression_body_to_new_line_when_exceeding_line_length()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                public ReadOnlyMemory<byte> GetVeryLongMethodNameForHashComputation(PineValue pineValue) => ComputeHashWithLongMethodName(pineValue);
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                public ReadOnlyMemory<byte> GetVeryLongMethodNameForHashComputation(PineValue pineValue) =>
                    ComputeHashWithLongMethodName(pineValue);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Breaks_member_access_dot_to_new_line_when_exceeding_line_length()
    {
        var inputSyntaxText =
            """"
            class MyClass
            {
                void M()
                {
                    SomeObject.GetItemsFromDatabaseWithAVeryLongMethodName(connectionString).TransformAllResultsWithAnotherVeryLongName(parameters);
                }
            }
            """";

        var expectedSyntaxText =
            """"
            class MyClass
            {
                void M()
                {
                    SomeObject.GetItemsFromDatabaseWithAVeryLongMethodName(connectionString)
                        .TransformAllResultsWithAnotherVeryLongName(parameters);
                }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Breaks_switch_arm_expression_to_new_line_when_exceeding_line_length()
    {
        var inputSyntaxText =
            """"
            var result = value switch
            {
                SomeVeryLongPatternName => SomeVeryLongMethodCallName(parameterAlpha, parameterBravo, parameterCharlie, parameterDelta),

                _ => throw new System.NotImplementedException()
            };
            """";

        var expectedSyntaxText =
            """"
            var result =
                value switch
                {
                    SomeVeryLongPatternName =>
                    SomeVeryLongMethodCallName(parameterAlpha, parameterBravo, parameterCharlie, parameterDelta),

                    _ =>
                    throw new System.NotImplementedException()
                };
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_return_statement_multi_line_expression_on_new_line()
    {
        var inputSyntaxText =
            """"
            string method_declaration()
            {
                return SomeFunction(
                    arg1,
                    arg2);
            }
            """";

        var expectedFormattedText =
            """"
            string method_declaration()
            {
                return
                    SomeFunction(
                        arg1,
                        arg2);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedFormattedText, scriptMode: true);
    }


    [Fact]
    public void Formats_multi_line_conditional_with_tokens_on_own_lines()
    {
        var inputSyntaxText =
            """"
            var x = condition
                ? trueValue
                : falseValue;
            """";

        var expectedSyntaxText =
            """"
            var x =
                condition
                ?
                trueValue
                :
                falseValue;
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_conditional_expression_when_line_exceeds_maximum_length()
    {
        var inputSyntaxText =
            """"
            var alfa = someConditionCheckingExpression.IsTrue ? firstAlternativeValueExpressionResult : secondAlternativeValueExpressionResult;
            """";

        var expectedSyntaxText =
            """"
            var alfa =
                someConditionCheckingExpression.IsTrue
                ?
                firstAlternativeValueExpressionResult
                :
                secondAlternativeValueExpressionResult;
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_lambda_block_in_invocation()
    {
        var inputSyntaxText =
            """"
            var filteredDecls =
                declarations.Where(d =>
                {
                    var variable = d.Declaration.Variables.FirstOrDefault();
                    return variable is not null && mutatedUsed.Contains(variable.Identifier.ValueText);
                }).ToImmutableArray();
            """";

        var expectedSyntaxText =
            """"
            var filteredDecls =
                declarations.Where(
                    d =>
                    {
                        var variable = d.Declaration.Variables.FirstOrDefault();
                        return variable is not null && mutatedUsed.Contains(variable.Identifier.ValueText);
                    }).ToImmutableArray();
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Stable_conditional_expression_inside_lambda_argument()
    {
        // Reproduces NameMapper.cs instability: conditional expression inside a lambda
        // argument gets different indentation between first and second format passes.
        var inputSyntaxText =
            """"
            namespace MyNamespace;

            public class MyClass
            {
                public static string MapNames(
                    string file,
                    Func<string, string> mapFunc) => file;


                public static string MapNames(
                    string file,
                    Dictionary<string, string> namesMap)
                {
                    return MapNames(file, originalName =>
                        namesMap.TryGetValue(originalName, out var mappedName)
                        ? mappedName
                        : originalName);
                }
            }
            """";

        AssertFormattingIsStable(inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Stable_lambda_block_body_inside_return_argument()
    {
        // Reproduces CodeAnalysis.cs instability: lambda with block body inside
        // a return statement's argument list gets different indentation.
        var inputSyntaxText =
            """"
            namespace MyNamespace;

            public class MyClass
            {
                public static string Process(string input)
                {
                    return Transform(input, path =>
                    {
                        if (path.Length > 10)
                        {
                            return null;
                        }

                        return "default";
                    });
                }


                public static string Transform(string s, Func<string, string> f) => s;
            }
            """";

        AssertFormattingIsStable(inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Stable_catch_clause_with_empty_block()
    {
        // Reproduces CommonMappings.cs instability: catch clause formatting
        // with trailing whitespace before opening brace.
        var inputSyntaxText =
            """"
            void M()
            {
                try
                {
                    DoSomething();
                }
                catch
                {
                }

                if (true)
                    return;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Stable_multiline_comment_followed_by_doc_comment()
    {
        // Reproduces FileEvent.cs instability: blank line between multiline comment
        // and doc comment is lost or changed between format passes.
        var inputSyntaxText =
            """"
            namespace MyNamespace;

            /**
             * A file event.
            interface FileEvent
            {
                uri: string;
            }
             */

            /// <summary>
            /// Represents a file event.
            /// </summary>
            public record FileEvent(
                string Uri);
            """";

        AssertFormattingIsStable(inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Stable_switch_expression_keyword_whitespace()
    {
        // Reproduces StaticProgramCSharpClass.cs instability: trailing whitespace
        // before 'switch' keyword is inconsistent between format passes.
        var inputSyntaxText =
            """"
            string M(string kind)
            {
                var result = kind switch
                {
                    "a" => "alpha",
                    "b" => "beta",
                    _ => null,
                };

                return result;
            }
            """";

        AssertFormattingIsStable(inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Stable_collection_inside_argument_list()
    {
        // Reproduces FunctionValueBuilder.cs / EncodeAsElmValue.cs instability: collection
        // expression inside argument list stays single-line on first format but breaks to
        // multi-line on second format due to indentation changes.
        var inputSyntaxText =
            """"
            namespace MyNamespace;

            public class MyClass
            {
                public static object Build(string tag, object[] args)
                {
                    return
                        CreateInstance(
                            [CreateInstance([LiteralTag(tag), ..args.Select(a => Encode(a))])]);
                }


                public static object CreateInstance(object[] items) => items;


                public static object LiteralTag(string tag) => tag;


                public static object Encode(object a) => a;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Stable_parameter_list_near_line_length_limit()
    {
        // Reproduces EnumerableExtensions.cs instability: parameter list just under
        // MaximumLineLength stays single-line first but breaks on second pass.
        var inputSyntaxText =
            """"
            namespace MyNamespace;

            public static class Extensions
            {
                public static IEnumerable<string> OrderByNatural(this IEnumerable<string> items, StringComparer? cmp = null) =>
                    items.OrderBy(s => s, cmp);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: false);
    }


    [Fact]
    public void Collection_expression_single_line_exceeding_line_length()
    {
        var inputSyntaxText =
            """"
            return
                PineValue.List([
                    CommonReusedValues.Blob_Str_String,
                    PineValue.List([KernelFunctionFused.TakeAndSkip(skipCountValue: Global_Anonymous.zzz_anon_7b433b8b_d4fe90b2(CommonReusedValues.Blob_Int_0, local_000), takeCountValue: Global_Anonymous.zzz_anon_627f403e_dca18c16(KernelFunction.length(local_000), local_000), argument: local_000)])
                ]);
            """";

        var expectedSyntaxText =
            """"
            return
                PineValue.List(
                    [
                    CommonReusedValues.Blob_Str_String,
                    PineValue.List(
                        [
                        KernelFunctionFused.TakeAndSkip(
                            skipCountValue: Global_Anonymous.zzz_anon_7b433b8b_d4fe90b2(CommonReusedValues.Blob_Int_0, local_000),
                            takeCountValue: Global_Anonymous.zzz_anon_627f403e_dca18c16(KernelFunction.length(local_000), local_000),
                            argument: local_000)
                        ])
                    ]);
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_initializer_on_new_line_in_object_creation()
    {
        var inputSyntaxText =
            """"
            var versionOption = new Option<bool>("-v")
            {
                Description = "Show version information"
            };
            """";

        var expectedSyntaxText =
            """"
            var versionOption =
                new Option<bool>("-v")
                {
                    Description = "Show version information"
                };
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Formats_argument_list_containing_lambda_containing_try_catch_block()
    {
        var inputSyntaxText =
            """"
            Parallel.ForEach(files, (filePath) =>
            {
                try
                {
                    var originalContent = File.ReadAllText(filePath);
                    var result = formatFile(originalContent);

                    switch (result)
                    {
                        case FormatFileResult.Error errorResult:
                            parseErrors.Add((filePath, errorResult.ErrorText));
                            break;

                        case FormatFileResult.Stable:
                            alreadyFormatted.Add(filePath);
                            break;

                        case FormatFileResult.Changed changedResult:
                            needsFormatting.Add((filePath, changedResult.FormattedText));
                            break;
                    }
                }
                catch (Exception ex)
                {
                    parseErrors.Add((filePath, ex.Message));
                }
            });
            """";

        var expectedSyntaxText =
            """"
            Parallel.ForEach(
                files,
                (filePath) =>
                {
                    try
                    {
                        var originalContent = File.ReadAllText(filePath);
                        var result = formatFile(originalContent);

                        switch (result)
                        {
                            case FormatFileResult.Error errorResult:
                                parseErrors.Add((filePath, errorResult.ErrorText));
                                break;

                            case FormatFileResult.Stable:
                                alreadyFormatted.Add(filePath);
                                break;

                            case FormatFileResult.Changed changedResult:
                                needsFormatting.Add((filePath, changedResult.FormattedText));
                                break;
                        }
                    }
                    catch (Exception ex)
                    {
                        parseErrors.Add((filePath, ex.Message));
                    }
                });
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Preserves_record_constructor_empty_single_line()
    {
        var inputSyntaxText =
            """"
            public abstract record FormatFileResult
            {
                private FormatFileResult() { }
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, inputSyntaxText, scriptMode: true);
    }


    [Fact]
    public void Switches_argument_list_containing_multi_line_argument_to_multi_line()
    {
        var inputSyntaxText =
            """"
            decl =
                alfa(beta(
                    gamma(delta(a, b))));
            """";

        var expectedSyntaxText =
            """"
            decl =
                alfa(
                    beta(
                        gamma(delta(a, b))));
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: true);
    }

    [Fact]
    public void Formats_member_declarations_containing_collections()
    {
        var inputSyntaxText =
            """"
            public static class MyClass
            {
                public static readonly PineValue List_Single_List_Single_List_c6f65d71 =
                    PineValue.List([List_Single_List_c6f65d71]);

                public static readonly PineValue List_13572e94 = PineValue.List([Blob_Str_List, List_Single_List_Single_List_c6f65d71]);

                public static readonly PineValue List_Single_List_Single_List_c6f65d71 =
                    PineValue.List([List_Single_List_c6f65d71]);

                public static readonly PineValue List_f3328233 =
                    PineValue.List([List_976730e9, List_b43468c9, List_4af1f0ec, List_2a07a877]);
            }
            """";

        var expectedSyntaxText =
            """"
            public static class MyClass
            {
                public static readonly PineValue List_Single_List_Single_List_c6f65d71 =
                    PineValue.List([List_Single_List_c6f65d71]);

                public static readonly PineValue List_13572e94 =
                    PineValue.List([Blob_Str_List, List_Single_List_Single_List_c6f65d71]);

                public static readonly PineValue List_Single_List_Single_List_c6f65d71 =
                    PineValue.List([List_Single_List_c6f65d71]);

                public static readonly PineValue List_f3328233 =
                    PineValue.List([List_976730e9, List_b43468c9, List_4af1f0ec, List_2a07a877]);
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: false);
    }

    [Fact]
    public void Formats_nested_infix_operator_and_conditionals_exceeding_line_length()
    {
        var inputSyntaxText =
            """"
            if ((local_001 == PineKernelValues.TrueValue ? false : true) == (local_003 == PineKernelValues.TrueValue ? false : true))
            {
                return local_008;
            }
            """";

        var expectedSyntaxText =
            """"
            if ((local_001 == PineKernelValues.TrueValue ? false : true) ==
                (local_003 == PineKernelValues.TrueValue ? false : true))
            {
                return local_008;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: false);
    }

    [Fact]
    public void Formats_named_argument_on_line_exceeding_length_limit()
    {
        var inputSyntaxText =
            """"
            return
                KernelFunctionFused.ListPrependItem(
                    itemToPrepend: CommonReusedValues.Blob_Char_hyphen,
                    notItemToPrepend: Global_Anonymous.zzz_anon_12af8dcc_24a48553(KernelFunction.negate(param_1_0), someother(param_1_0)));
            """";

        var expectedSyntaxText =
            """"
            return
                KernelFunctionFused.ListPrependItem(
                    itemToPrepend: CommonReusedValues.Blob_Char_hyphen,
                    notItemToPrepend:
                    Global_Anonymous.zzz_anon_12af8dcc_24a48553(KernelFunction.negate(param_1_0), someother(param_1_0)));
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: false);
    }

    [Fact]
    public void Formats_if_condition_exceeding_line_length_and_nested_condition()
    {
        var inputSyntaxText =
            """"            
            if ((local_002 == PineKernelValues.TrueValue) && (CommonReusedValues.Blob_Str_Elm_Float == PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [0])))
            {
                PineValue local_003 = PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1]);
                PineValue local_004 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1]);

                if (KernelFunctionSpecialized.int_mul(
                    PineValueExtension.ValueFromPathOrEmptyList(local_003, [0]),
                    PineValueExtension.ValueFromPathOrEmptyList(local_004, [1])) == KernelFunctionSpecialized.int_mul(
                    PineValueExtension.ValueFromPathOrEmptyList(local_004, [0]),
                    PineValueExtension.ValueFromPathOrEmptyList(local_003, [1])))
                {
                    return CommonReusedValues.List_ac855cb8;
                }

                if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(
                    KernelFunctionSpecialized.int_mul(
                        PineValueExtension.ValueFromPathOrEmptyList(local_003, [0]),
                        PineValueExtension.ValueFromPathOrEmptyList(local_004, [1])),
                    KernelFunctionSpecialized.int_mul(
                        PineValueExtension.ValueFromPathOrEmptyList(local_004, [0]),
                        PineValueExtension.ValueFromPathOrEmptyList(local_003, [1]))))
                {
                    return CommonReusedValues.List_af0e3cad;
                }

                return CommonReusedValues.List_50724673;
            }
            """";

        var expectedSyntaxText =
            """"
            if ((local_002 == PineKernelValues.TrueValue) &&
                (CommonReusedValues.Blob_Str_Elm_Float == PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [0])))
            {
                PineValue local_003 = PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1]);
                PineValue local_004 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1]);

                if (KernelFunctionSpecialized.int_mul(
                    PineValueExtension.ValueFromPathOrEmptyList(local_003, [0]),
                    PineValueExtension.ValueFromPathOrEmptyList(local_004, [1])) ==
                    KernelFunctionSpecialized.int_mul(
                        PineValueExtension.ValueFromPathOrEmptyList(local_004, [0]),
                        PineValueExtension.ValueFromPathOrEmptyList(local_003, [1])))
                {
                    return CommonReusedValues.List_ac855cb8;
                }

                if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(
                    KernelFunctionSpecialized.int_mul(
                        PineValueExtension.ValueFromPathOrEmptyList(local_003, [0]),
                        PineValueExtension.ValueFromPathOrEmptyList(local_004, [1])),
                    KernelFunctionSpecialized.int_mul(
                        PineValueExtension.ValueFromPathOrEmptyList(local_004, [0]),
                        PineValueExtension.ValueFromPathOrEmptyList(local_003, [1]))))
                {
                    return CommonReusedValues.List_af0e3cad;
                }

                return CommonReusedValues.List_50724673;
            }
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: false);
    }

    [Fact]
    public void Formats_boolean_operation_chain_exceeding_line_length()
    {
        var inputSyntaxText =
            """"            
            var putOnNewLine = node.Expression is ThrowExpressionSyntax || SpansMultipleLines(node) || node.Pattern is DiscardPatternSyntax || node.Pattern is DeclarationPatternSyntax;
            """";

        var expectedSyntaxText =
            """"
            var putOnNewLine =
                node.Expression is ThrowExpressionSyntax || SpansMultipleLines(node) || node.Pattern is DiscardPatternSyntax ||
                node.Pattern is DeclarationPatternSyntax;
            """";

        AssertFormattedSyntax(inputSyntaxText, expectedSyntaxText, scriptMode: false);
    }

    [Fact]
    public void Formats_simple_while_statement()
    {
        var whileStatementSyntax =
            SyntaxFactory.WhileStatement(
                SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression),
                SyntaxFactory.Block(
                    SyntaxFactory.ExpressionStatement(
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.IdentifierName("DoSomething")))));

        var expectedSyntaxText =
            """"
            while (true)
            {
                DoSomething();
            }
            """";

        AssertFormattedSyntax(whileStatementSyntax, expectedSyntaxText);
    }

    [Fact]
    public void Formatting_only_changes_whitespace_in_Pine_Core_files()
    {
        var pineCoreDir = FindPineCoreDirectory();

        var csFiles = Directory.EnumerateFiles(pineCoreDir, "*.cs", SearchOption.AllDirectories).ToList();

        csFiles.Should().NotBeEmpty();

        var failures = new List<string>();
        var skippedFiles = new List<(string path, string reason)>();

        foreach (var filePath in csFiles)
        {
            var relativePath = Path.GetRelativePath(pineCoreDir, filePath);
            var originalText = File.ReadAllText(filePath);

            // Skip files in bin/obj directories (build artifacts)
            var sep = Path.DirectorySeparatorChar;

            if (relativePath.Contains(sep + "bin" + sep) ||
                relativePath.Contains(sep + "obj" + sep))
            {
                continue;
            }

            string formattedText;
            string formattedTwice;

            try
            {
                formattedText = CSharpFormat.FormatCSharpFile(originalText);
                formattedTwice = CSharpFormat.FormatCSharpFile(formattedText);
            }
            catch (Exception ex)
            {
                failures.Add($"{relativePath}: formatter threw {ex.GetType().Name}: {ex.Message}");
                continue;
            }

            // Assert formatting is stable: formatting a second time must return the same result.
            if (formattedText != formattedTwice)
            {
                var stableMinLen = Math.Min(formattedText.Length, formattedTwice.Length);
                var stableDiffIndex = 0;

                for (var i = 0; i < stableMinLen; i++)
                {
                    if (formattedText[i] != formattedTwice[i])
                    {
                        stableDiffIndex = i;
                        break;
                    }
                }

                if (stableDiffIndex is 0 && formattedText.Length != formattedTwice.Length)
                    stableDiffIndex = stableMinLen;

                var stableContextStart = Math.Max(0, stableDiffIndex - 40);

                var expectedStable =
                    formattedText[stableContextStart..Math.Min(formattedText.Length, stableDiffIndex + 40)];

                var actualStable =
                    formattedTwice[stableContextStart..Math.Min(formattedTwice.Length, stableDiffIndex + 40)];

                failures.Add(
                    $"{relativePath}: formatting is NOT stable (second pass differs at index {stableDiffIndex}).\n" +
                    $"  Expected (first format): \"{EscapeNewlines(expectedStable)}\"\n" +
                    $"  Actual   (second format): \"{EscapeNewlines(actualStable)}\"");

                continue;
            }

            var originalNonWhitespace = StripWhitespace(originalText);
            var formattedNonWhitespace = StripWhitespace(formattedText);

            if (originalNonWhitespace == formattedNonWhitespace)
                continue;

            // The formatter sorts using directives alphabetically, which reorders non-whitespace
            // characters without losing any content. Detect this by checking if the sorted
            // character sequences are identical (same characters, just reordered).
            var originalSorted = new string([.. originalNonWhitespace.OrderBy(c => c)]);
            var formattedSorted = new string([.. formattedNonWhitespace.OrderBy(c => c)]);

            if (originalSorted == formattedSorted)
            {
                skippedFiles.Add((relativePath, "using directive sorting (reorders but preserves all content)"));
                continue;
            }

            // If we get here, the formatter changed or lost non-whitespace content.
            // Find the first difference to help debugging.
            var minLen = Math.Min(originalNonWhitespace.Length, formattedNonWhitespace.Length);
            var diffIndex = 0;

            for (var i = 0; i < minLen; i++)
            {
                if (originalNonWhitespace[i] != formattedNonWhitespace[i])
                {
                    diffIndex = i;
                    break;
                }
            }

            if (diffIndex is 0 && originalNonWhitespace.Length != formattedNonWhitespace.Length)
                diffIndex = minLen;

            var contextStart = Math.Max(0, diffIndex - 40);

            var expectedContext =
                originalNonWhitespace[contextStart..Math.Min(originalNonWhitespace.Length, diffIndex + 40)];

            var actualContext =
                formattedNonWhitespace[contextStart..Math.Min(formattedNonWhitespace.Length, diffIndex + 40)];

            failures.Add(
                $"{relativePath}: non-whitespace characters differ at position {diffIndex}.\n" +
                $"  Expected (original, length {originalNonWhitespace.Length}): \"{expectedContext}\"\n" +
                $"  Actual   (formatted, length {formattedNonWhitespace.Length}): \"{actualContext}\"");
        }

        failures.Should().BeEmpty(
            $"Formatting changed non-whitespace characters in {failures.Count} file(s):\n" +
            string.Join("\n", failures) +
            (skippedFiles.Count > 0
            ?
            $"\n\nSkipped {skippedFiles.Count} file(s):\n" +
                  string.Join("\n", skippedFiles.Select(s => $"  {s.path}: {s.reason}"))
            :
            ""));
    }

    private static string EscapeNewlines(string text) =>
        text.Replace("\r", "\\r").Replace("\n", "\\n");


    private static string StripWhitespace(string text) =>
        new([.. text.Where(c => !char.IsWhiteSpace(c))]);


    private static string FindPineCoreDirectory([CallerFilePath] string? callerFilePath = null)
    {
        // This test file is at implement/Pine.Core.Tests/DotNet/CSharpFormatTests.cs
        // Pine.Core is at implement/Pine.Core/
        var testFileDir = Path.GetDirectoryName(callerFilePath)!;
        var pineCoreDir = Path.GetFullPath(Path.Combine(testFileDir, "..", "..", "Pine.Core"));

        Directory.Exists(pineCoreDir).Should().BeTrue(
            $"Expected Pine.Core directory at {pineCoreDir}");

        return pineCoreDir;
    }

    private static void AssertFormattedSyntax(
        string inputSyntaxText,
        string expectedFormattedText,
        bool scriptMode)
    {
        Func<string, string> formatOnce =
            scriptMode
            ?
            CSharpFormat.FormatCSharpScript
            :
            CSharpFormat.FormatCSharpFile;

        var formattedSyntaxText = formatOnce(inputSyntaxText);

        formattedSyntaxText.Trim().Should().Be(expectedFormattedText.Trim());

        if (inputSyntaxText != expectedFormattedText)
        {
            // Assert stability: Formatting a second time should yield the same result

            var formattedTwice = formatOnce(formattedSyntaxText);

            formattedTwice.Trim().Should().Be(expectedFormattedText.Trim());
        }
    }

    private static void AssertFormattedSyntax(
        SyntaxNode syntaxNode,
        string expectedFormattedText)
    {
        var formattedSyntaxText =
            FormatCSharpFile.FormatSyntaxTree(SyntaxFactory.SyntaxTree(syntaxNode)).ToString();

        formattedSyntaxText.Trim().Should().Be(expectedFormattedText.Trim());
    }


    /// <summary>
    /// Asserts that formatting is idempotent: formatting once and formatting twice
    /// produce the same result (format(format(x)) == format(x)).
    /// Does NOT assert any particular expected output.
    /// </summary>
    private static void AssertFormattingIsStable(string inputSyntaxText, bool scriptMode)
    {
        Func<string, string> formatOnce =
            scriptMode
            ?
            CSharpFormat.FormatCSharpScript
            :
            CSharpFormat.FormatCSharpFile;

        var formattedOnce = formatOnce(inputSyntaxText);
        var formattedTwice = formatOnce(formattedOnce);

        formattedTwice.Trim().Should().Be(formattedOnce.Trim());
    }


    /// <summary>
    /// Asserts that formatting only changes whitespace characters,
    /// and that the result is stable (idempotent).
    /// </summary>
    private static void AssertFormattingOnlyChangesWhitespace(string inputSyntaxText, bool scriptMode)
    {
        Func<string, string> formatOnce =
            scriptMode
            ?
            CSharpFormat.FormatCSharpScript
            :
            CSharpFormat.FormatCSharpFile;

        var formattedOnce = formatOnce(inputSyntaxText);
        var formattedTwice = formatOnce(formattedOnce);

        // Assert stability
        formattedTwice.Trim().Should().Be(formattedOnce.Trim(),
            "Formatting must be stable (second pass must match first)");

        // Assert only whitespace changed
        var origNonWs = StripWhitespace(inputSyntaxText);
        var fmtNonWs = StripWhitespace(formattedOnce);

        fmtNonWs.Should().Be(origNonWs,
            "Formatting must only change whitespace characters");
    }


    [Fact]
    public void Preserves_trailing_comment_on_field_declaration()
    {
        var input =
            """
            class C
            {
                private const int RandomSweepDenominator = 100_000; // ~0.01% chance per new bucket
                private const int RandomSweepSampleSize = 100_000;
            }
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: false);
    }


    [Fact]
    public void Preserves_comment_in_member_access_chain()
    {
        var input =
            """
            var specialized =
                specializations
                // Order to prefer more specific constraints when selecting at runtime.
                .OrderDescending(comparer);
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_between_chained_method_calls()
    {
        var input =
            """
            var operatorToFunction =
                ImmutableDictionary<string, (string ModuleName, string FunctionName)>.Empty
                // Basics operators
                .Add("+", ("Basics", "add"))
                .Add("-", ("Basics", "sub"));
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_trailing_comment_on_argument()
    {
        var input =
            """
            var result = Func(
                updatesExpr,
                Expression.EmptyList, // processed fields (initially empty)
                recordFieldsExpr);
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_inline_comment_after_condition_in_else_if()
    {
        var input =
            """
            if (x)
            {
                Row++;
            }
            else if (ch is not '\r') // Skip CR when counting position
            {
                CurrentColumn++;
            }
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_in_switch_expression_arm()
    {
        var input =
            """
            return type switch
            {
                ValueType.Generic =>
                    // boolean == PineKernelValues.TrueValue
                    SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, left, right),
                ValueType.Integer =>
                    left,
            };
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_after_switch_expression_arm_expression()
    {
        var input =
            """
            return pattern switch
            {
                Pattern.AllPattern => null, // Wildcard pattern contains no constant value
                Pattern.VarPattern => null,
                Pattern.IntPattern intPattern =>
                    intPattern.Value,
            };
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_between_named_arguments()
    {
        var input =
            """
            var result = Func(
                trueBranch: floatFloatEqual, // Both floats - compare cross-products
                falseBranch: floatIntEqual);
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_block_comment_in_expression()
    {
        var input =
            """
            var firstArgExpr = inputList.Items[0];

            /* if (firstArgExpr is Expression.ListExpression innerList)
            {
                return innerList;
            } */

            {
                var nonEmptyItems = new List<int>(capacity);
            }
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_after_arrow_expression_clause()
    {
        var input =
            """
            class C
            {
                public static string CacheDirectory =>
                    // https://stackoverflow.com/questions/39224518
                    Path.Combine(
                        Environment.GetEnvironmentVariable("HOME")!,
                        ".cache");
            }
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: false);
    }


    [Fact]
    public void Preserves_comment_in_conditional_expression()
    {
        var input =
            """
            return
                charValue is 39
                ? "\\'" // single quote must be escaped
                : CharDefaultEscaping(charValue);
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void While_statement_is_stable_with_internal_whitespace()
    {
        var input =
            """
            while (                        NextTokenMatches(peek => peek.Kind is TokenKind.Comma))
            {
                Advance();
            }
            """;

        AssertFormattingIsStable(input, scriptMode: true);
    }


    [Fact]
    public void Switch_expression_arm_when_clause_spacing_is_stable()
    {
        var input =
            """
            return expr switch
            {
                FuncOrValue funcOrValue when funcOrValue.ModuleName.Count is 0 &&
                    replacements.TryGetValue(funcOrValue.Name, out var replacement)  =>
                    replacement.Value,

                _ =>
                    expr,
            };
            """;

        AssertFormattingIsStable(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_on_conditional_expression_colon()
    {
        var input =
            """
            return
                charValue is 39
                ? "\\'"
                : // single quote must be escaped in Elm char literals
                CharDefaultEscaping(charValue) ??
                char.ConvertFromUtf32(charValue);
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_on_binary_operator()
    {
        var input =
            """
            var result = allComments.Any(c =>
                !c.Lexeme.StartsWith("{-|") && // not a doc comment
                c.Start.Row > commentToken.End.Row);
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_before_else_statement()
    {
        var input =
            """
            if (hex.Length <= 4)
                targetLength = 4;
            else if (hex.Length <= 8)
                targetLength = 8;
            else
                // Round up to next multiple of 8
                targetLength = (hex.Length + 7) / 8 * 8;
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_comment_on_collection_element()
    {
        var input =
            """
            var result = Func([
                updatesExpr,
                Expression.EmptyList, // processed fields (initially empty)
                recordFieldsExpr // remaining fields
            ]);
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Case_pattern_when_clause_whitespace_is_stable()
    {
        var input =
            """
            switch (expr)
            {
                case Expression.FunctionOrValue funcOrValue when funcOrValue.ModuleName.Count is 0 && replacements.TryGetValue(funcOrValue.Name, out var replacement):
                    return replacement.Value;
            }
            """;

        AssertFormattingIsStable(input, scriptMode: true);
    }


    [Fact]
    public void Preserves_trailing_comment_on_continue_statement()
    {
        var input =
            """
            foreach (var item in items)
            {
                if (visited.Contains(item))
                {
                    continue; // Skip to avoid infinite loop
                }

                Process(item);
            }
            """;

        AssertFormattingOnlyChangesWhitespace(input, scriptMode: true);
    }


    [Fact]
    public void Case_pattern_when_clause_multiline_is_stable()
    {
        var input =
            """
            switch (expr)
            {
                case Expression.FunctionOrValue funcOrValue when
                    funcOrValue.ModuleName.Count is 0 &&
                    substitutions.TryGetValue(funcOrValue.Name, out var replacement):
                    return replacement.Value;
            }
            """;

        AssertFormattingIsStable(input, scriptMode: true);
    }
}
