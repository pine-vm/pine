using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// For more details on how we compile Elm code using records, see:
/// <see href="https://github.com/pine-vm/pine/blob/aa5acc2131910f90a8b61437f8a590036fb7f097/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md#record-access-and-record-update"></see>
/// </summary>
public class RecordExpressionTests
{
    [Fact]
    public void Module_level_declaration_record_value()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa =
                { x = 13, z = 17, y = 23 }

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        // Module-level declarations are compiled as zero-parameter functions
        // Parse and evaluate to get the actual value
        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing function: " + err));

        functionRecord.ParameterCount.Should().Be(0);

        var evalResult =
            ElmCompilerTestHelper.EvaluateWithProfiling(functionRecord.InnerFunction, PineValue.EmptyBlob);

        var actualValue =
            evalResult.evalReport.ReturnValue.Evaluate();

        actualValue.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                ("x", IntegerEncoding.EncodeSignedInteger(13)),
                ("y", IntegerEncoding.EncodeSignedInteger(23)),
                ("z", IntegerEncoding.EncodeSignedInteger(17))
                ]));
    }

    [Fact]
    public void Module_level_function_returning_record_value()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b c =
                { x = a, z = c, y = b }

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        // Parsing and rendering not implemented yet, therefore no snapshot test here.


        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArguments(
            PineValue a,
            PineValue b,
            PineValue c)
        {
            var (applyRunResult, _) = invokeFunction([a, b, c]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        ElmValue ApplyForElmArguments(
            ElmValue a,
            ElmValue b,
            ElmValue c)
        {
            var pineValue =
                ApplyForArguments(
                    ElmValueEncoding.ElmValueAsPineValue(a),
                    ElmValueEncoding.ElmValueAsPineValue(b),
                    ElmValueEncoding.ElmValueAsPineValue(c));

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.Integer(41),
                    ElmValue.Integer(47),
                    ElmValue.Integer(49));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.Integer(41)),
                    ("y", ElmValue.Integer(47)),
                    ("z", ElmValue.Integer(49))
                    ]));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.ElmFloat.Convert(1.3),
                    ElmValue.ElmFloat.Convert(2.7),
                    ElmValue.ElmFloat.Convert(0));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.ElmFloat.Convert(1.3)),
                    ("y", ElmValue.ElmFloat.Convert(2.7)),
                    ("z", ElmValue.ElmFloat.Convert(0))
                    ]));
        }
    }

    [Fact]
    public void Module_level_function_updating_concrete_record_value()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { x : Int, y : Int } -> Int -> { x : Int, y : Int }
            alfa r b =
                { r | y = b }

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        // Parsing and rendering not implemented yet, therefore no snapshot test here.


        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArguments(
            PineValue r,
            PineValue b)
        {
            var (applyRunResult, _) = invokeFunction([r, b]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        ElmValue ApplyForElmArguments(
            ElmValue r,
            ElmValue b)
        {
            var pineValue =
                ApplyForArguments(
                    ElmValueEncoding.ElmValueAsPineValue(r),
                    ElmValueEncoding.ElmValueAsPineValue(b));

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.Integer(41)),
                        ("y", ElmValue.Integer(47))
                        ]),
                    ElmValue.Integer(49));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.Integer(41)),
                    ("y", ElmValue.Integer(49))
                    ]));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.Integer(17)),
                        ("y", ElmValue.Integer(19))
                        ]),
                    ElmValue.Integer(13));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.Integer(17)),
                    ("y", ElmValue.Integer(13))
                    ]));
        }
    }

    [Fact]
    public void Module_level_function_updating_generic_record_value()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { x : a, y : b } -> b -> { x : a, y : b }
            alfa r b =
                { r | y = b }

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        // Parsing and rendering not implemented yet, therefore no snapshot test here.


        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArguments(
            PineValue r,
            PineValue b)
        {
            var (applyRunResult, _) = invokeFunction([r, b]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        ElmValue ApplyForElmArguments(
            ElmValue r,
            ElmValue b)
        {
            var pineValue =
                ApplyForArguments(
                    ElmValueEncoding.ElmValueAsPineValue(r),
                    ElmValueEncoding.ElmValueAsPineValue(b));

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.Integer(41)),
                        ("y", ElmValue.Integer(47))
                        ]),
                    ElmValue.Integer(49));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.Integer(41)),
                    ("y", ElmValue.Integer(49))
                    ]));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.ElmFloat.Convert(1.3)),
                        ("y", ElmValue.StringInstance("test")),
                        ]),
                    ElmValue.StringInstance("updated"));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.ElmFloat.Convert(1.3)),
                    ("y", ElmValue.StringInstance("updated"))
                    ]));
        }
    }

    [Fact]
    public void Module_level_function_accessing_record_field()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { x : a, y : b, z : c } -> b
            alfa r =
                r.y

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArguments(PineValue r)
        {
            var (applyRunResult, _) = invokeFunction([r]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        ElmValue ApplyForElmArguments(ElmValue r)
        {
            var pineValue =
                ApplyForArguments(
                    ElmValueEncoding.ElmValueAsPineValue(r));

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer in field y
        {
            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.Integer(41)),
                        ("y", ElmValue.Integer(47)),
                        ("z", ElmValue.Integer(49))
                        ]));

            resultValue.Should().Be(ElmValue.Integer(47));
        }

        // Test with string in field y
        {
            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.ElmFloat.Convert(1.3)),
                        ("y", ElmValue.StringInstance("hello")),
                        ("z", ElmValue.Integer(100))
                        ]));

            resultValue.Should().Be(ElmValue.StringInstance("hello"));
        }

        // Test with float in field y
        {
            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.StringInstance("test")),
                        ("y", ElmValue.ElmFloat.Convert(3.14)),
                        ("z", ElmValue.Integer(0))
                        ]));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert(3.14));
        }

        // Test with nested record in field y
        {
            var innerRecord = new ElmValue.ElmRecord(
                [
                ("a", ElmValue.Integer(1)),
                ("b", ElmValue.Integer(2))
                ]);

            var resultValue =
                ApplyForElmArguments(
                    new ElmValue.ElmRecord(
                        [
                        ("x", ElmValue.Integer(0)),
                        ("y", innerRecord),
                        ("z", ElmValue.Integer(0))
                        ]));

            resultValue.Should().Be(innerRecord);
        }
    }

    /// <summary>
    /// Tests record access on the penultimate field of a 9-field record.
    /// Verifies that invocation count is less than 7, proving compile-time field index computation.
    /// </summary>
    [Fact]
    public void Record_access_on_penultimate_field_of_nine_field_record_uses_compile_time_index()
    {
        // Record with 9 fields: a, b, c, d, e, f, g, h, i (sorted alphabetically)
        // Penultimate field is 'h' (index 7)
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int, i : Int } -> Int
            alfa r =
                r.h

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        // Test with a 9-field record, accessing field 'h' (penultimate)
        var testRecord = new ElmValue.ElmRecord(
            [
            ("a", ElmValue.Integer(1)),
            ("b", ElmValue.Integer(2)),
            ("c", ElmValue.Integer(3)),
            ("d", ElmValue.Integer(4)),
            ("e", ElmValue.Integer(5)),
            ("f", ElmValue.Integer(6)),
            ("g", ElmValue.Integer(7)),
            ("h", ElmValue.Integer(888)),  // This is the penultimate field we're accessing
            ("i", ElmValue.Integer(9))
            ]);

        var (evalReport, _) = invokeFunction([ElmValueEncoding.ElmValueAsPineValue(testRecord)]);

        var resultValue = evalReport.ReturnValue.Evaluate();

        // Verify correct result
        resultValue.Should().Be(IntegerEncoding.EncodeSignedInteger(888));

        // Verify invocation count is less than 6
        // If we compute the index at compile time, we don't need to iterate through fields
        evalReport.InvocationCount.Should().BeLessThan(6,
            "Record access should use compile-time index computation, not runtime iteration through fields");
    }

    /// <summary>
    /// Tests record update on the penultimate field of a 9-field record.
    /// Verifies that invocation count is less than 7, proving compile-time field index computation.
    /// </summary>
    [Fact]
    public void Record_update_on_penultimate_field_of_nine_field_record_uses_compile_time_index()
    {
        // Record with 9 fields: a, b, c, d, e, f, g, h, i (sorted alphabetically)
        // Updating penultimate field 'h' (index 7)
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int, i : Int } -> Int -> { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int, i : Int }
            alfa r newH =
                { r | h = newH }

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        // Test with a 9-field record, updating field 'h' (penultimate)
        var testRecord = new ElmValue.ElmRecord(
            [
            ("a", ElmValue.Integer(1)),
            ("b", ElmValue.Integer(2)),
            ("c", ElmValue.Integer(3)),
            ("d", ElmValue.Integer(4)),
            ("e", ElmValue.Integer(5)),
            ("f", ElmValue.Integer(6)),
            ("g", ElmValue.Integer(7)),
            ("h", ElmValue.Integer(8)),
            ("i", ElmValue.Integer(9))
            ]);

        var newValue = ElmValue.Integer(999);

        var (evalReport, _) = invokeFunction(
            [ElmValueEncoding.ElmValueAsPineValue(testRecord),
             ElmValueEncoding.ElmValueAsPineValue(newValue)]);

        var resultPineValue = evalReport.ReturnValue.Evaluate();
        var resultValue = ElmValueEncoding.PineValueAsElmValue(resultPineValue, null, null)
            .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));

        // Verify correct result - all fields same except 'h' which is updated to 999
        resultValue.Should().Be(
            new ElmValue.ElmRecord(
                [
                ("a", ElmValue.Integer(1)),
                ("b", ElmValue.Integer(2)),
                ("c", ElmValue.Integer(3)),
                ("d", ElmValue.Integer(4)),
                ("e", ElmValue.Integer(5)),
                ("f", ElmValue.Integer(6)),
                ("g", ElmValue.Integer(7)),
                ("h", ElmValue.Integer(999)),  // Updated value
                ("i", ElmValue.Integer(9))
                ]));

        // Verify invocation count is less than 6
        // If we compute the index at compile time, we don't need to iterate through fields
        evalReport.InvocationCount.Should().BeLessThan(6,
            "Record update should use compile-time index computation, not runtime iteration through fields");
    }
}
