using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using AST = Pine.Core.Elm.Syntax.Abstract;
using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.CLI;

internal static class RunValues
{
    internal sealed record TextEncoding(Encoding Encoding, bool Bom);

    public static string Render(ElmValue value) =>
        ElmValue.RenderAsElmExpression(
            value,
            item => item is ElmValue.ElmBytes bytes
            ?
            ($"Bytes {bytes.Value.Length} 0x{Convert.ToHexStringLower(bytes.Value.Span)}", false)
            :
            ((string, bool)?)null).expressionString;

    public static (ElmValue Value, TextEncoding? Encoding) FromFile(ReadOnlyMemory<byte> bytes, Type type)
    {
        if (IsBytes(type) || type is Type.TypeVariable)
            return (new ElmValue.ElmBytes(bytes), null);

        if (type is not Type.StringType)
            throw new ArgumentException("File input requires a String or Bytes parameter.");

        var encoding = DetectEncoding(bytes.Span);
        var preambleLength = encoding.Bom ? encoding.Encoding.GetPreamble().Length : 0;
        return (ElmValue.StringInstance(encoding.Encoding.GetString(bytes.Span[preambleLength..])), encoding);
    }

    public static byte[] ToFile(ElmValue value, TextEncoding? inputEncoding)
    {
        if (value is ElmValue.ElmBytes bytes)
            return bytes.Value.ToArray();

        var encoding = inputEncoding ?? new TextEncoding(new UTF8Encoding(false, true), false);
        var text = value is ElmValue.ElmString str ? str.Value : Render(value);
        return [.. encoding.Bom ? encoding.Encoding.GetPreamble() : [], .. encoding.Encoding.GetBytes(text)];
    }

    private static TextEncoding DetectEncoding(ReadOnlySpan<byte> bytes)
    {
        if (bytes.StartsWith(new byte[] { 0, 0, 0xfe, 0xff }))
            return new(new UTF32Encoding(true, true, true), true);

        if (bytes.StartsWith(new byte[] { 0xff, 0xfe, 0, 0 }))
            return new(new UTF32Encoding(false, true, true), true);

        if (bytes.StartsWith(new byte[] { 0xef, 0xbb, 0xbf }))
            return new(new UTF8Encoding(true, true), true);

        if (bytes.StartsWith(new byte[] { 0xfe, 0xff }))
            return new(new UnicodeEncoding(true, true, true), true);

        if (bytes.StartsWith(new byte[] { 0xff, 0xfe }))
            return new(new UnicodeEncoding(false, true, true), true);

        return new(new UTF8Encoding(false, true), false);
    }

    public static bool IsBytes(Type type) =>
        type is Type.ChoiceType { TypeName: "Bytes" } choice &&
        (choice.ModuleName.Count is 0 || choice.ModuleName[^1] is "Bytes");

    public static ElmValue ParseInput(string input, Type type)
    {
        if (type is Type.StringType && !input.TrimStart().StartsWith('"'))
            return ElmValue.StringInstance(input);

        var expression =
            ElmSyntaxParser.ParseExpression(input)
            .Extract(error => throw new ArgumentException("Invalid --input expression: " + error));

        return Literal(AST.ConvertFromConcrete.FromExpression(expression));
    }

    private static ElmValue Literal(AST.Expression expression)
    {
        switch (expression)
        {
            case AST.Expression.UnitExpr:
                return ElmValue.ListInstance([]);

            case AST.Expression.StringLiteral str:
                return ElmValue.StringInstance(str.Value);

            case AST.Expression.CharLiteral character:
                return ElmValue.CharInstance(character.Value);

            case AST.Expression.IntegerLiteral integer:
                return new ElmValue.ElmInteger(integer.Value);

            case AST.Expression.FloatLiteral number:
                return ElmValue.ElmFloat.Normalized(number.Numerator, number.Denominator);

            case AST.Expression.Negation { Expression: AST.Expression.IntegerLiteral integer }:
                return new ElmValue.ElmInteger(-integer.Value);

            case AST.Expression.Negation { Expression: AST.Expression.FloatLiteral number }:
                return ElmValue.ElmFloat.Normalized(-number.Numerator, number.Denominator);

            case AST.Expression.ListExpr list:
                var items = list.Elements.Select(Literal).ToArray();

                if (ElmValue.AreElmValueListItemTypesEqual(items) is false)
                    throw new ArgumentException("--input lists must have consistently typed elements.");

                return ElmValue.ListInstance(items);

            case AST.Expression.TupledExpression tuple:
                return ElmValue.ListInstance([.. tuple.Elements.Select(Literal)]);

            case AST.Expression.RecordExpr record:
                return
                    ElmValue.RecordInstance(
                        [.. record.Fields.Select(field => (field.FieldName, Literal(field.Value)))]);

            case AST.Expression.Identifier identifier:
                return Constructor(identifier, []);

            case AST.Expression.Application { Function: AST.Expression.Identifier identifier } application:
                return Constructor(identifier, [.. application.Arguments.Select(Literal)]);

            case AST.Expression.Negation:
            case AST.Expression.Application:
            case AST.Expression.IfBlock:
            case AST.Expression.PrefixOperator:
            case AST.Expression.OperatorApplication:
            case AST.Expression.LambdaExpression:
            case AST.Expression.CaseExpression:
            case AST.Expression.LetExpression:
            case AST.Expression.RecordAccess:
            case AST.Expression.RecordAccessFunction:
            case AST.Expression.RecordUpdateExpression:
            case AST.Expression.GLSLExpression:
                throw new ArgumentException(
                    "--input supports only literals, lists, tuples, records, and choice constructors.");

            default:
                throw new NotImplementedException(
                    nameof(Literal) + " does not handle expression variant: " + expression.GetType().Name);
        }
    }

    private static ElmValue Constructor(AST.Expression.Identifier identifier, ElmValue[] arguments)
    {
        if (!char.IsUpper(identifier.QualifiedName.DeclName[0]))
            throw new ArgumentException("--input cannot reference functions or variables.");

        return ElmValue.TagInstance(identifier.QualifiedName.DeclName, arguments);
    }

    public static string RenderInputPreview(ElmValue value)
    {
        const int maxLength = 160;

        if (value is ElmValue.ElmString text && text.Value.Length > maxLength)
        {
            var length = char.IsHighSurrogate(text.Value[maxLength - 1]) ? maxLength - 1 : maxLength;
            return Render(ElmValue.StringInstance(text.Value[..length])) + $" ... ({text.Value.Length} characters)";
        }

        if (value is ElmValue.ElmBytes bytes)
        {
            return
                $"Bytes {bytes.Value.Length} 0x{Convert.ToHexStringLower(bytes.Value.Span[..Math.Min(32, bytes.Value.Length)])}" +
                (bytes.Value.Length > 32 ? "..." : "");
        }

        var rendered = Render(value);
        return rendered.Length > maxLength ? rendered[..maxLength] + " ... (preview)" : rendered;
    }

    public static void Validate(
        ElmValue value, Type type, RunCompilation.TypeInformation information, Dictionary<string, Type> variables) =>
        Validate(value, type, information.Choices, variables);

    private static void Validate(
        ElmValue value, Type type, Dictionary<string, TypeInference.ChoiceTypeDefinition> choices, Dictionary<string, Type> variables)
    {
        void Check(bool condition)
        {
            if (!condition)
            {
                throw new ArgumentException(
                    "--input value does not match the function's parameter type (" + type.GetType().Name + ").");
            }
        }

        switch (type)
        {
            case Type.StringType:
                Check(value is ElmValue.ElmString);
                break;

            case Type.IntType:
                Check(value is ElmValue.ElmInteger);
                break;

            case Type.FloatType:
                Check(value is ElmValue.ElmFloat or ElmValue.ElmInteger);
                break;

            case Type.NumberType:
                Check(value is ElmValue.ElmFloat or ElmValue.ElmInteger);
                break;

            case Type.CharType:
                Check(value is ElmValue.ElmChar);
                break;

            case Type.BoolType:
                Check(value is ElmValue.ElmTag { TagName: "True" or "False", Arguments.Count: 0 });
                break;

            case Type.ListType list:
                Check(value is ElmValue.ElmList);

                foreach (var item in ((ElmValue.ElmList)value).Items)
                {
                    Validate(item, list.ElementType, choices, variables);
                }

                break;

            case Type.TupleType tuple:
                Check(value is ElmValue.ElmList items && items.Items.Count == tuple.ElementTypes.Count);

                for (var i = 0; i < tuple.ElementTypes.Count; ++i)
                {
                    Validate(((ElmValue.ElmList)value).Items[i], tuple.ElementTypes[i], choices, variables);
                }

                break;

            case Type.RecordType record:
                ValidateRecord(value, record.Fields, open: false, choices, variables);
                break;

            case Type.OpenRecordType record:
                ValidateRecord(value, record.KnownFields, open: true, choices, variables);
                break;

            case Type.ChoiceType choice:
                if (IsBytes(type))
                {
                    Check(value is ElmValue.ElmBytes);
                    break;
                }

                var name = string.Join(".", choice.ModuleName.Append(choice.TypeName));

                if (!choices.TryGetValue(name, out var definition))
                {
                    throw new ArgumentException(
                        "Cannot resolve parameter type '" + name + "'. Add an explicit type annotation.");
                }

                Check(value is ElmValue.ElmTag);
                var tag = (ElmValue.ElmTag)value;
                var constructor = definition.Constructors.SingleOrDefault(item => item.TagName == tag.TagName);
                Check(constructor is not null && constructor.ArgumentTypes.Count == tag.Arguments.Count);
                var parameters = new Type.TupleType(constructor!.ArgumentTypes);

                var instantiated =
                    TypeInference.ExpandTypeAliases(
                        new Type.ChoiceType([], "RunArguments", choice.TypeArguments),
                        new Dictionary<DeclQualifiedName, TypeInference.TypeAliasDefinition>
                        {
                            [DeclQualifiedName.Create([], "RunArguments")] = new(definition.TypeParameters, parameters)
                        });

                Validate(ElmValue.ListInstance(tag.Arguments), instantiated, choices, variables);
                break;

            case Type.TypeVariable variable:
                Check(
                    variable.Constraint switch
                    {
                        TypeVariableConstraint.None => true,
                        TypeVariableConstraint.Number => value is ElmValue.ElmInteger or ElmValue.ElmFloat,
                        TypeVariableConstraint.Appendable => value is ElmValue.ElmString or ElmValue.ElmList,
                        TypeVariableConstraint.Comparable => IsComparable(value),

                        TypeVariableConstraint.CompAppend =>
                        value is ElmValue.ElmString ||
                        value is ElmValue.ElmList comparableList && comparableList.Items.All(IsComparable),

                        _ =>
                        throw new NotImplementedException("Unknown type-variable constraint: " + variable.Constraint)
                    });

                var actual = ValueType(value);

                if (variables.TryGetValue(variable.Name, out var previous))
                {
                    var unified = TypeInference.TryUnify(previous, actual);
                    Check(unified.IsOk());
                    variables[variable.Name] = unified.Extract(error => throw new ArgumentException(error));
                }
                else
                    variables.Add(variable.Name, actual);

                break;

            case Type.UnknownType:
                throw new ArgumentException("Cannot infer the parameter type. Add an explicit type annotation.");

            case Type.FunctionType:
                throw new ArgumentException("Function-valued inputs are not supported.");

            default:
                throw new NotImplementedException(nameof(Validate) + " does not handle type variant: " + type.GetType().Name);
        }
    }

    private static void ValidateRecord(
        ElmValue value,
        IReadOnlyList<(string FieldName, Type FieldType)> fields,
        bool open,
        Dictionary<string, TypeInference.ChoiceTypeDefinition> choices,
        Dictionary<string, Type> variables)
    {
        if (value is not ElmValue.ElmRecord record ||
            (!open && record.Fields.Count != fields.Count))
        {
            throw new ArgumentException("--input record does not match the parameter type.");
        }

        foreach (var (name, fieldType) in fields)
        {
            var field = record.Fields.SingleOrDefault(item => item.FieldName == name);

            if (field.Value is null)
                throw new ArgumentException("--input record is missing field '" + name + "'.");

            Validate(field.Value, fieldType, choices, variables);
        }
    }

    private static bool IsComparable(ElmValue value) =>
        value is ElmValue.ElmInteger or ElmValue.ElmFloat or ElmValue.ElmChar or ElmValue.ElmString ||
        value is ElmValue.ElmList list && list.Items.All(IsComparable);

    private static Type ValueType(ElmValue value)
    {
        switch (value)
        {
            case ElmValue.ElmInteger:
                return new Type.NumberType();

            case ElmValue.ElmFloat:
                return new Type.FloatType();

            case ElmValue.ElmString:
                return new Type.StringType();

            case ElmValue.ElmChar:
                return new Type.CharType();

            case ElmValue.ElmBytes:
                return new Type.ChoiceType(["Bytes"], "Bytes", []);

            case ElmValue.ElmRecord record:
                return
                    new Type.RecordType(
                        [.. record.Fields.Select(field => (field.FieldName, ValueType(field.Value)))]);

            case ElmValue.ElmList list:
                if (ElmValue.ElmListItemsLookLikeTupleItems(list.Items) is true)
                    return new Type.TupleType([.. list.Items.Select(ValueType)]);

                Type element = new Type.TypeVariable("_empty");

                foreach (var item in list.Items)
                {
                    element =
                        TypeInference.TryUnify(element, ValueType(item))
                        .Extract(error => throw new ArgumentException("Inconsistent list elements: " + error));
                }

                return new Type.ListType(element);

            case ElmValue.ElmTag { TagName: "True" or "False", Arguments.Count: 0 }:
                return new Type.BoolType();

            case ElmValue.ElmTag:
                return new Type.UnknownType();

            case ElmValue.ElmPineBlob:
            case ElmValue.ElmInternal:
                throw new ArgumentException("Unsupported input value.");

            default:
                throw new NotImplementedException(
                    nameof(ValueType) + " does not handle value variant: " + value.GetType().Name);
        }
    }
}
