using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Unit-like tests for RecordRuntime methods, testing them directly without embedding into module compilation.
/// Tests the Pine functions for record access and record update at runtime.
/// </summary>
public class RecordRuntimeTests
{
    /// <summary>
    /// Tests that PineFunctionForRecordAccessAsValue correctly accesses a field from a simple 2-field record.
    /// </summary>
    [Fact]
    public void RecordAccess_simple_two_field_record_accesses_first_field()
    {
        // Create a record: { x = 41, y = 47 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("x", IntegerEncoding.EncodeSignedInteger(41)),
                ("y", IntegerEncoding.EncodeSignedInteger(47))
            ]);

        var fieldName = StringEncoding.ValueFromString("x");

        // Environment for record access function: [record, fieldName]
        var environment = PineValue.List([record, fieldName]);

        var result = EvaluateRecordAccessFunction(environment);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(41));
    }

    /// <summary>
    /// Tests that PineFunctionForRecordAccessAsValue correctly accesses a field from a simple 2-field record.
    /// </summary>
    [Fact]
    public void RecordAccess_simple_two_field_record_accesses_second_field()
    {
        // Create a record: { x = 41, y = 47 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("x", IntegerEncoding.EncodeSignedInteger(41)),
                ("y", IntegerEncoding.EncodeSignedInteger(47))
            ]);

        var fieldName = StringEncoding.ValueFromString("y");

        // Environment for record access function: [record, fieldName]
        var environment = PineValue.List([record, fieldName]);

        var result = EvaluateRecordAccessFunction(environment);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(47));
    }

    /// <summary>
    /// Tests that PineFunctionForRecordAccessAsValue works with 3-field records.
    /// </summary>
    [Fact]
    public void RecordAccess_three_field_record_accesses_middle_field()
    {
        // Create a record: { a = 10, b = 20, c = 30 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("a", IntegerEncoding.EncodeSignedInteger(10)),
                ("b", IntegerEncoding.EncodeSignedInteger(20)),
                ("c", IntegerEncoding.EncodeSignedInteger(30))
            ]);

        var fieldName = StringEncoding.ValueFromString("b");

        // Environment for record access function: [record, fieldName]
        var environment = PineValue.List([record, fieldName]);

        var result = EvaluateRecordAccessFunction(environment);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(20));
    }

    /// <summary>
    /// Tests that PineFunctionForRecordAccessAsValue works with string values.
    /// </summary>
    [Fact]
    public void RecordAccess_with_string_value()
    {
        // Create a record: { name = "test", value = 123 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("name", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("test"))),
                ("value", IntegerEncoding.EncodeSignedInteger(123))
            ]);

        var fieldName = StringEncoding.ValueFromString("name");

        // Environment for record access function: [record, fieldName]
        var environment = PineValue.List([record, fieldName]);

        var result = EvaluateRecordAccessFunction(environment);

        result.Should().Be(ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("test")));
    }

    /// <summary>
    /// Tests that PineFunctionForRecordUpdateAsValue correctly updates a single field.
    /// </summary>
    [Fact]
    public void RecordUpdate_single_field_update()
    {
        // Create a record: { x = 41, y = 47 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("x", IntegerEncoding.EncodeSignedInteger(41)),
                ("y", IntegerEncoding.EncodeSignedInteger(47))
            ]);

        // Update y to 99
        var updates = PineValue.List(
            [
                PineValue.List([StringEncoding.ValueFromString("y"), IntegerEncoding.EncodeSignedInteger(99)])
            ]);

        // Environment for record update function: [record, updates]
        var environment = PineValue.List([record, updates]);

        var result = EvaluateRecordUpdateFunction(environment);

        // Expected result: { x = 41, y = 99 }
        result.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("x", IntegerEncoding.EncodeSignedInteger(41)),
                    ("y", IntegerEncoding.EncodeSignedInteger(99))
                ]));
    }

    /// <summary>
    /// Tests that PineFunctionForRecordUpdateAsValue correctly updates the first field.
    /// </summary>
    [Fact]
    public void RecordUpdate_first_field_update()
    {
        // Create a record: { x = 41, y = 47 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("x", IntegerEncoding.EncodeSignedInteger(41)),
                ("y", IntegerEncoding.EncodeSignedInteger(47))
            ]);

        // Update x to 100
        var updates = PineValue.List(
            [
                PineValue.List([StringEncoding.ValueFromString("x"), IntegerEncoding.EncodeSignedInteger(100)])
            ]);

        // Environment for record update function: [record, updates]
        var environment = PineValue.List([record, updates]);

        var result = EvaluateRecordUpdateFunction(environment);

        // Expected result: { x = 100, y = 47 }
        result.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("x", IntegerEncoding.EncodeSignedInteger(100)),
                    ("y", IntegerEncoding.EncodeSignedInteger(47))
                ]));
    }

    /// <summary>
    /// Tests that PineFunctionForRecordUpdateAsValue correctly updates multiple fields at once.
    /// </summary>
    [Fact]
    public void RecordUpdate_multiple_fields_update()
    {
        // Create a record: { a = 1, b = 2, c = 3 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("a", IntegerEncoding.EncodeSignedInteger(1)),
                ("b", IntegerEncoding.EncodeSignedInteger(2)),
                ("c", IntegerEncoding.EncodeSignedInteger(3))
            ]);

        // Update a to 10 and c to 30 (updates must be sorted alphabetically!)
        var updates = PineValue.List(
            [
                PineValue.List([StringEncoding.ValueFromString("a"), IntegerEncoding.EncodeSignedInteger(10)]),
                PineValue.List([StringEncoding.ValueFromString("c"), IntegerEncoding.EncodeSignedInteger(30)])
            ]);

        // Environment for record update function: [record, updates]
        var environment = PineValue.List([record, updates]);

        var result = EvaluateRecordUpdateFunction(environment);

        // Expected result: { a = 10, b = 2, c = 30 }
        result.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("a", IntegerEncoding.EncodeSignedInteger(10)),
                    ("b", IntegerEncoding.EncodeSignedInteger(2)),
                    ("c", IntegerEncoding.EncodeSignedInteger(30))
                ]));
    }

    /// <summary>
    /// Tests that PineFunctionForRecordUpdateAsValue works with a larger record.
    /// </summary>
    [Fact]
    public void RecordUpdate_five_field_record_updates_middle_field()
    {
        // Create a record: { a = 1, b = 2, c = 3, d = 4, e = 5 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("a", IntegerEncoding.EncodeSignedInteger(1)),
                ("b", IntegerEncoding.EncodeSignedInteger(2)),
                ("c", IntegerEncoding.EncodeSignedInteger(3)),
                ("d", IntegerEncoding.EncodeSignedInteger(4)),
                ("e", IntegerEncoding.EncodeSignedInteger(5))
            ]);

        // Update c to 333
        var updates = PineValue.List(
            [
                PineValue.List([StringEncoding.ValueFromString("c"), IntegerEncoding.EncodeSignedInteger(333)])
            ]);

        // Environment for record update function: [record, updates]
        var environment = PineValue.List([record, updates]);

        var result = EvaluateRecordUpdateFunction(environment);

        // Expected result: { a = 1, b = 2, c = 333, d = 4, e = 5 }
        result.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("a", IntegerEncoding.EncodeSignedInteger(1)),
                    ("b", IntegerEncoding.EncodeSignedInteger(2)),
                    ("c", IntegerEncoding.EncodeSignedInteger(333)),
                    ("d", IntegerEncoding.EncodeSignedInteger(4)),
                    ("e", IntegerEncoding.EncodeSignedInteger(5))
                ]));
    }

    /// <summary>
    /// Tests that record access works on a 5-field record accessing the last field.
    /// </summary>
    [Fact]
    public void RecordAccess_five_field_record_accesses_last_field()
    {
        // Create a record: { a = 1, b = 2, c = 3, d = 4, e = 555 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("a", IntegerEncoding.EncodeSignedInteger(1)),
                ("b", IntegerEncoding.EncodeSignedInteger(2)),
                ("c", IntegerEncoding.EncodeSignedInteger(3)),
                ("d", IntegerEncoding.EncodeSignedInteger(4)),
                ("e", IntegerEncoding.EncodeSignedInteger(555))
            ]);

        var fieldName = StringEncoding.ValueFromString("e");

        // Environment for record access function: [record, fieldName]
        var environment = PineValue.List([record, fieldName]);

        var result = EvaluateRecordAccessFunction(environment);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(555));
    }

    /// <summary>
    /// Tests record update with string value update.
    /// </summary>
    [Fact]
    public void RecordUpdate_with_string_value()
    {
        // Create a record: { name = "old", value = 123 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("name", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("old"))),
                ("value", IntegerEncoding.EncodeSignedInteger(123))
            ]);

        // Update name to "new"
        var updates = PineValue.List(
            [
                PineValue.List([StringEncoding.ValueFromString("name"),
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("new"))])
            ]);

        // Environment for record update function: [record, updates]
        var environment = PineValue.List([record, updates]);

        var result = EvaluateRecordUpdateFunction(environment);

        // Expected result: { name = "new", value = 123 }
        result.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("name", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("new"))),
                    ("value", IntegerEncoding.EncodeSignedInteger(123))
                ]));
    }

    /// <summary>
    /// Tests that record update preserves all fields when updating the last field.
    /// </summary>
    [Fact]
    public void RecordUpdate_last_field_preserves_all_other_fields()
    {
        // Create a record: { a = 1, b = 2, c = 3, d = 4 }
        var record = ElmValueEncoding.ElmRecordAsPineValue(
            [
                ("a", IntegerEncoding.EncodeSignedInteger(1)),
                ("b", IntegerEncoding.EncodeSignedInteger(2)),
                ("c", IntegerEncoding.EncodeSignedInteger(3)),
                ("d", IntegerEncoding.EncodeSignedInteger(4))
            ]);

        // Update d to 400
        var updates = PineValue.List(
            [
                PineValue.List([StringEncoding.ValueFromString("d"), IntegerEncoding.EncodeSignedInteger(400)])
            ]);

        // Environment for record update function: [record, updates]
        var environment = PineValue.List([record, updates]);

        var result = EvaluateRecordUpdateFunction(environment);

        // Expected result: { a = 1, b = 2, c = 3, d = 400 }
        result.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("a", IntegerEncoding.EncodeSignedInteger(1)),
                    ("b", IntegerEncoding.EncodeSignedInteger(2)),
                    ("c", IntegerEncoding.EncodeSignedInteger(3)),
                    ("d", IntegerEncoding.EncodeSignedInteger(400))
                ]));
    }

    #region Helper Methods

    /// <summary>
    /// Evaluates the RecordRuntime.PineFunctionForRecordAccessAsValue with the given environment.
    /// </summary>
    private static PineValue EvaluateRecordAccessFunction(PineValue environment)
    {
        var parseCache = new PineVMParseCache();

        var expression =
            parseCache.ParseExpression(RecordRuntime.PineFunctionForRecordAccessAsValue)
            .Extract(err => throw new Exception("Failed to parse record access function: " + err));

        var evalResult = ElmCompilerTestHelper.EvaluateWithProfiling(expression, environment);

        return evalResult.evalReport.ReturnValue.Evaluate();
    }

    /// <summary>
    /// Evaluates the RecordRuntime.PineFunctionForRecordUpdateAsValue with the given environment.
    /// </summary>
    private static PineValue EvaluateRecordUpdateFunction(PineValue environment)
    {
        var parseCache = new PineVMParseCache();

        var expression = parseCache.ParseExpression(RecordRuntime.PineFunctionForRecordUpdateAsValue)
            .Extract(err => throw new Exception("Failed to parse record update function: " + err));

        var evalResult = ElmCompilerTestHelper.EvaluateWithProfiling(expression, environment);

        return evalResult.evalReport.ReturnValue.Evaluate();
    }

    #endregion
}
