using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for record type alias constructors in Elm.
/// A type alias declaration for a record type implicitly creates a constructor function.
/// The order of arguments matches the order of fields in the type alias declaration,
/// NOT the alphabetical order used for the record's runtime representation.
/// 
/// For more details on how we compile Elm code using records, see:
/// <see href="https://github.com/pine-vm/pine/blob/main/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md#records"></see>
/// </summary>
public class RecordConstructorTests
{
    /// <summary>
    /// Tests a simple record constructor where fields are declared in alphabetical order.
    /// type alias Point = { x : Int, y : Int }
    /// Point 10 20 should produce { x = 10, y = 20 }
    /// </summary>
    [Fact]
    public void Record_constructor_with_alphabetical_field_order()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }


            makePoint : Int -> Int -> Point
            makePoint a b =
                Point a b

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
            .FirstOrDefault(decl => decl.Key is "makePoint");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArguments(ElmValue a, ElmValue b)
        {
            var (applyRunResult, _) = invokeFunction(
                [ElmValueEncoding.ElmValueAsPineValue(a),
                 ElmValueEncoding.ElmValueAsPineValue(b)]);

            var pineValue = applyRunResult.ReturnValue.Evaluate();

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Point 10 20 should produce { x = 10, y = 20 }
        var result = ApplyForElmArguments(ElmValue.Integer(10), ElmValue.Integer(20));

        result.Should().Be(
            new ElmValue.ElmRecord(
                [
                ("x", ElmValue.Integer(10)),
                ("y", ElmValue.Integer(20))
                ]));
    }

    /// <summary>
    /// Tests a record constructor where fields are declared in reverse alphabetical order.
    /// type alias Point = { y : Int, x : Int }
    /// Point 10 20 should produce { x = 20, y = 10 } (fields sorted alphabetically in result)
    /// The first argument 10 goes to 'y' (first in declaration), second argument 20 goes to 'x'.
    /// </summary>
    [Fact]
    public void Record_constructor_with_reverse_alphabetical_field_order()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Point =
                { y : Int, x : Int }


            makePoint : Int -> Int -> Point
            makePoint a b =
                Point a b

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
            .FirstOrDefault(decl => decl.Key is "makePoint");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArguments(ElmValue a, ElmValue b)
        {
            var (applyRunResult, _) = invokeFunction(
                [ElmValueEncoding.ElmValueAsPineValue(a),
                 ElmValueEncoding.ElmValueAsPineValue(b)]);

            var pineValue = applyRunResult.ReturnValue.Evaluate();

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Point 10 20 should produce { x = 20, y = 10 }
        // First argument 10 goes to 'y' (first in declaration)
        // Second argument 20 goes to 'x' (second in declaration)
        // Result record has fields sorted alphabetically: x, y
        var result = ApplyForElmArguments(ElmValue.Integer(10), ElmValue.Integer(20));

        result.Should().Be(
            new ElmValue.ElmRecord(
                [
                ("x", ElmValue.Integer(20)),
                ("y", ElmValue.Integer(10))
                ]));
    }

    /// <summary>
    /// Tests a record constructor with 3 fields in non-alphabetical order.
    /// type alias Rec = { z : Int, a : Int, m : Int }
    /// Rec 1 2 3 should produce { a = 2, m = 3, z = 1 }
    /// </summary>
    [Fact]
    public void Record_constructor_with_three_fields_non_alphabetical()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Rec =
                { z : Int, a : Int, m : Int }


            makeRec : Int -> Int -> Int -> Rec
            makeRec p q r =
                Rec p q r

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
            .FirstOrDefault(decl => decl.Key is "makeRec");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArguments(ElmValue p, ElmValue q, ElmValue r)
        {
            var (applyRunResult, _) = invokeFunction(
                [ElmValueEncoding.ElmValueAsPineValue(p),
                 ElmValueEncoding.ElmValueAsPineValue(q),
                 ElmValueEncoding.ElmValueAsPineValue(r)]);

            var pineValue = applyRunResult.ReturnValue.Evaluate();

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Rec 1 2 3 should produce { a = 2, m = 3, z = 1 }
        // First argument 1 goes to 'z' (first in declaration)
        // Second argument 2 goes to 'a' (second in declaration)
        // Third argument 3 goes to 'm' (third in declaration)
        // Result has fields sorted alphabetically: a, m, z
        var result = ApplyForElmArguments(ElmValue.Integer(1), ElmValue.Integer(2), ElmValue.Integer(3));

        result.Should().Be(
            new ElmValue.ElmRecord(
                [
                ("a", ElmValue.Integer(2)),
                ("m", ElmValue.Integer(3)),
                ("z", ElmValue.Integer(1))
                ]));
    }

    /// <summary>
    /// Tests that two different type aliases for the same record type produce different constructors.
    /// type alias PointXY = { x : Int, y : Int }
    /// type alias PointYX = { y : Int, x : Int }
    /// Both have the same record type, but PointXY takes x first, PointYX takes y first.
    /// </summary>
    [Fact]
    public void Different_type_aliases_same_record_type_different_constructors()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias PointXY =
                { x : Int, y : Int }


            type alias PointYX =
                { y : Int, x : Int }


            makePointXY a b =
                PointXY a b


            makePointYX a b =
                PointYX a b

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test PointXY constructor
        {
            var declValue =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "makePointXY");

            var declParsed =
                FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
                .Extract(err => throw new Exception("Failed parsing makePointXY: " + err));

            var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

            var (applyRunResult, _) = invokeFunction(
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(100)),
                 ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(200))]);

            var pineValue = applyRunResult.ReturnValue.Evaluate();
            var result = ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            // PointXY 100 200 -> { x = 100, y = 200 }
            result.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.Integer(100)),
                    ("y", ElmValue.Integer(200))
                    ]));
        }

        // Test PointYX constructor
        {
            var declValue =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "makePointYX");

            var declParsed =
                FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
                .Extract(err => throw new Exception("Failed parsing makePointYX: " + err));

            var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

            var (applyRunResult, _) = invokeFunction(
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(100)),
                 ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(200))]);

            var pineValue = applyRunResult.ReturnValue.Evaluate();
            var result = ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            // PointYX 100 200 -> { x = 200, y = 100 }
            // First arg 100 goes to 'y', second arg 200 goes to 'x'
            result.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.Integer(200)),
                    ("y", ElmValue.Integer(100))
                    ]));
        }
    }

    /// <summary>
    /// Tests record constructor with polymorphic field types.
    /// type alias Pair a b = { first : a, second : b }
    /// </summary>
    [Fact]
    public void Record_constructor_with_polymorphic_fields()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Pair a b =
                { first : a, second : b }


            makePair : a -> b -> Pair a b
            makePair a b =
                Pair a b

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
            .FirstOrDefault(decl => decl.Key is "makePair");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        // Test with Int and String
        {
            var (applyRunResult, _) = invokeFunction(
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(42)),
                 ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("hello"))]);

            var pineValue = applyRunResult.ReturnValue.Evaluate();
            var result = ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            // Pair 42 "hello" -> { first = 42, second = "hello" }
            result.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("first", ElmValue.Integer(42)),
                    ("second", ElmValue.StringInstance("hello"))
                    ]));
        }
    }
}
