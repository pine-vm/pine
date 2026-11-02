using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    private static ApplicationResolution? DebugResolver(Application application)
    {
        if (application.FunctionName.Namespaces is not ["Debug"])
            return null;

        var name = application.FunctionName.DeclName;

        var parameters =
            name switch
            {
                "log" => new[] { "message", "value" },
                "toString" => ["value"],
                "todo" => ["message"],

                _ =>
                null,
            };

        if (parameters is null)
            return null;

        if (application.Arguments.Count != parameters.Length)
        {
            // Native Debug functions also need callable values for currying and higher-order use.
            return
                new ApplicationResolution.ContinueWithFunction(
                    PrepareNativeFunction(application.FunctionName, parameters));
        }

        return name switch
        {
            "log" => new ApplicationResolution.Resolved(application.Arguments[1]),

            "toString" =>
            new ApplicationResolution.Resolved(
                MakeElmString(StringEncoding.BlobValueFromString(RenderDebugValue(application.Arguments[0])))),

            "todo" =>
            throw new InvalidOperationException(
                "Debug.todo: " + RenderDebugValue(application.Arguments[0])),

            _ =>
            throw new NotImplementedException($"{nameof(DebugResolver)} does not handle Debug function: {name}"),
        };
    }

    private static string RenderDebugValue(PineValueInProcess value)
    {
        if (IsOpaque(value))
            return "<function>";

        if (PineValueInProcess.AreEqual(value, PineKernelValues.TrueValue))
            return "True";

        if (PineValueInProcess.AreEqual(value, PineKernelValues.FalseValue))
            return "False";

        if (value.IsBlob())
        {
            if (value.AsInteger() is { } integer)
                return integer.ToString(CultureInfo.InvariantCulture);

            if (value.Evaluate() is PineValue.BlobValue { Bytes.Length: 4 } blob &&
                blob.Bytes.Span[0] is 0)
            {
                var code = IntegerEncoding.ParseUnsignedInteger(blob.Bytes.Span);

                if (code <= 0x10ffff && !(code >= 0xd800 && code <= 0xdfff))
                    return "'" + char.ConvertFromUtf32((int)code) + "'";
            }

            return "<blob>";
        }

        var length = value.GetLength();

        if (length >= 3 && !IsOpaque(value.GetElementAt(0)) &&
            ElmValue.IsFlatChoiceTypeTag(value.GetElementAt(0).Evaluate()))
        {
            var tag = value.GetElementAt(1).Evaluate();

            if (tag == ElmValue.ElmStringTypeTagNameAsValue)
                return "\"" + DebugText(value.GetElementAt(2)) + "\"";

            if (tag == ElmValue.ElmFloatTypeTagNameAsValue && length is 4)
            {
                var numerator =
                    value.GetElementAt(2).AsInteger()
                    ?? throw new InvalidOperationException("Debug.toString: invalid float numerator.");

                var denominator =
                    value.GetElementAt(3).AsInteger()
                    ?? throw new InvalidOperationException("Debug.toString: invalid float denominator.");

                return numerator.IsZero && denominator.IsZero ? "NaN" : FromFloatDecimal(16, numerator, denominator);
            }
        }

        if (length >= 1 && !IsOpaque(value.GetElementAt(0)) &&
            (PineValueInProcess.AreEqual(value.GetElementAt(0), ElmValue.ElmRecordTypeTagNameAsValue) ||
            PineValueInProcess.AreEqual(value.GetElementAt(0), ElmValue.ElmRecordTypeTagNameAsValue_2026)))
        {
            var fields = new List<string>();

            for (var index = 1; index + 1 < length; index += 2)
            {
                fields.Add(
                    DebugText(value.GetElementAt(index)) + " = " + RenderDebugValue(value.GetElementAt(index + 1)));
            }

            return "{ " + string.Join(", ", fields) + " }";
        }

        if (length >= 2 && !IsOpaque(value.GetElementAt(0)) &&
            ElmValue.IsFlatChoiceTypeTag(value.GetElementAt(0).Evaluate()) &&
            value.GetElementAt(1).Evaluate() != ElmValue.ElmBytesTypeTagNameAsValue)
        {
            var arguments = new List<string> { DebugText(value.GetElementAt(1)) };

            for (var index = 2; index < length; index++)
            {
                var argument = value.GetElementAt(index);
                var rendered = RenderDebugValue(argument);

                var needsParens =
                    !IsOpaque(argument) &&
                    argument.IsList() && argument.GetLength() > 2 &&
                    !IsOpaque(argument.GetElementAt(0)) &&
                    ElmValue.IsFlatChoiceTypeTag(argument.GetElementAt(0).Evaluate()) &&
                    argument.GetElementAt(1).Evaluate() != ElmValue.ElmStringTypeTagNameAsValue &&
                    argument.GetElementAt(1).Evaluate() != ElmValue.ElmFloatTypeTagNameAsValue &&
                    argument.GetElementAt(1).Evaluate() != ElmValue.ElmBytesTypeTagNameAsValue;

                arguments.Add(needsParens ? "(" + rendered + ")" : rendered);
            }

            return string.Join(" ", arguments);
        }

        return
            "[" +
            string.Join(",", Enumerable.Range(0, length).Select(index => RenderDebugValue(value.GetElementAt(index)))) +
            "]";
    }

    private static string DebugText(PineValueInProcess value) =>
        StringEncoding.StringFromValue(value.Evaluate())
        .Extract(error => throw new InvalidOperationException("Debug.toString: " + error));
}
