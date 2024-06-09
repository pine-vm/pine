using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.ElmInteractive;

/// <summary>
/// Corresponding to the type ElmValue in ElmInteractive.elm
/// </summary>
public abstract record ElmValue
{
    /*

    type ElmValue
        = ElmList (List ElmValue)
        | ElmChar Char
        | ElmInteger BigInt.BigInt
        | ElmString String
        | ElmTag String (List ElmValue)
        | ElmRecord (List ( String, ElmValue ))
        | ElmInternal String

     * */

    public static readonly ElmValue TrueValue = new ElmTag("True", []);

    public static readonly ElmValue FalseValue = new ElmTag("False", []);

    override public string ToString() =>
        ElmValueAsExpression(this).expressionString;

    public const string ElmRecordTypeTagName = "Elm_Record";

    public const string ElmStringTypeTagName = "String";

    public static readonly PineValue ElmRecordTypeTagNameAsValue =
        PineValueAsString.ValueFromString(ElmRecordTypeTagName);

    public static readonly PineValue ElmStringTypeTagNameAsValue =
        PineValueAsString.ValueFromString(ElmStringTypeTagName);

    public static ElmValue Integer(System.Numerics.BigInteger Value) =>
        ReusedIntegerInstances?.TryGetValue(Value, out var reusedInstance) ?? false && reusedInstance is not null ?
        reusedInstance
        :
        new ElmInteger(Value);

    public static ElmValue String(string Value) =>
        ReusedStringInstances?.TryGetValue(Value, out var reusedInstance) ?? false && reusedInstance is not null ?
        reusedInstance
        :
        new ElmString(Value);

    public static ElmValue Char(int Value) =>
        Value < ReusedCharInstances?.Count && 0 <= Value ?
        ReusedCharInstances[Value]
        :
        new CharObject(Value);

    private static readonly FrozenDictionary<System.Numerics.BigInteger, ElmValue> ReusedIntegerInstances =
        Enumerable.Range(-100, 400)
        .ToFrozenDictionary(
            keySelector: i => (System.Numerics.BigInteger)i,
            elementSelector: i => Integer(i));

    private static readonly FrozenDictionary<string, ElmValue> ReusedStringInstances =
        PopularValues.PopularStrings
        .ToFrozenDictionary(
            keySelector: s => s,
            elementSelector: String);

    private static readonly IReadOnlyList<ElmValue> ReusedCharInstances =
        [..Enumerable.Range(0, 4000)
        .Select(Char)];

    public record ElmInteger(System.Numerics.BigInteger Value)
        : ElmValue;

    public record ElmTag(string TagName, IReadOnlyList<ElmValue> Arguments)
        : ElmValue
    {
        public virtual bool Equals(ElmTag? otherTag)
        {
            if (otherTag is null)
                return false;

            if (TagName != otherTag.TagName)
                return false;

            if (Arguments.Count != otherTag.Arguments.Count)
                return false;

            for (int i = 0; i < Arguments.Count; i++)
            {
                if (!Arguments[i].Equals(otherTag.Arguments[i]))
                    return false;
            }

            return true;
        }

        override public int GetHashCode() =>
            TagName.GetHashCode() ^
            Arguments.Select(a => a.GetHashCode()).Aggregate((a, b) => a ^ b);
    }

    public record ElmList(IReadOnlyList<ElmValue> Elements)
        : ElmValue
    {
        public virtual bool Equals(ElmList? otherList)
        {
            if (otherList is null)
                return false;

            if (Elements.Count != otherList.Elements.Count)
                return false;

            for (int i = 0; i < Elements.Count; i++)
            {
                if (!Elements[i].Equals(otherList.Elements[i]))
                    return false;
            }

            return true;
        }

        public override int GetHashCode() =>
            Elements.Select(e => e.GetHashCode()).Aggregate((a, b) => a ^ b);

        public override string ToString() =>
            ElmValueAsExpression(this).expressionString;
    }

    public record ElmString(string Value)
        : ElmValue;

    public record CharObject(int Value)
        : ElmValue;

    public record ElmRecord(IReadOnlyList<(string FieldName, ElmValue Value)> Fields)
        : ElmValue
    {
        public virtual bool Equals(ElmRecord? otherRecord)
        {
            if (otherRecord is null)
                return false;

            if (Fields.Count != otherRecord.Fields.Count)
                return false;

            for (int i = 0; i < Fields.Count; i++)
            {
                if (Fields[i].FieldName != otherRecord.Fields[i].FieldName)
                    return false;

                if (!Fields[i].Value.Equals(otherRecord.Fields[i].Value))
                    return false;
            }

            return true;
        }

        public override int GetHashCode() =>
            Fields.Select(f => f.GetHashCode()).Aggregate((a, b) => a ^ b);

        public override string ToString() =>
            ElmValueAsExpression(this).expressionString;

        public ElmValue? this[string fieldName] =>
            Fields.FirstOrDefault(field => field.FieldName == fieldName).Value;
    }

    public record ElmInternal(string Value)
        : ElmValue;

    public static Maybe<string> TryMapElmValueToString(ElmList elmValues) =>
        elmValues.Elements.Select(TryMapElmValueToChar).ListCombine()
        .Map(chars => string.Join("", chars.Select(char.ConvertFromUtf32)));

    public static Maybe<int> TryMapElmValueToChar(ElmValue elmValue) =>
        elmValue switch
        {
            CharObject elmChar =>
            elmChar.Value,

            _ =>
            Maybe<int>.nothing()
        };

    public static (string expressionString, bool needsParens) ElmValueAsExpression(ElmValue elmValue)
    {
        return
            elmValue switch
            {
                ElmInteger integer =>
                (integer.Value.ToString(), needsParens: false),

                CharObject charValue =>
                ("'" + char.ConvertFromUtf32(charValue.Value) + "'", needsParens: false),

                ElmList list =>
                ElmListItemsLookLikeTupleItems(list.Elements).WithDefault(false)
                ?
                ("(" + string.Join(",", list.Elements.Select(item => ElmValueAsExpression(item).expressionString)) + ")",
                needsParens: false)
                :
                ("[" + string.Join(",", list.Elements.Select(item => ElmValueAsExpression(item).expressionString)) + "]",
                needsParens: false),

                ElmString stringValue =>
                ("\"" + stringValue.Value.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"", needsParens: false),

                ElmRecord record =>
                (record.Fields.Count < 1)
                ?
                ("{}", needsParens: false)
                :
                ("{ " + string.Join(", ", record.Fields.Select(field =>
                field.FieldName + " = " + ElmValueAsExpression(field.Value).expressionString)) + " }",
                needsParens: false),

                ElmTag tag =>
                ElmTagAsExpression(tag),

                ElmInternal internalValue =>
                ("<" + internalValue.Value + ">", needsParens: false),

                _ =>
                throw new NotImplementedException(
                    "Not implemented for value type: " + elmValue.GetType().FullName)
            };
    }

    public static Result<string, ElmRecord> ElmValueAsElmRecord(ElmValue elmValue)
    {
        return elmValue switch
        {
            ElmList recordFieldList =>
            recordFieldList.Elements.Select(TryMapToRecordField).ListCombine()
            .AndThen(recordFields =>
            {
                var recordFieldsNames = recordFields.Select(field => field.fieldName).ToImmutableArray();

                var recordFieldsNamesOrdered =
                    recordFieldsNames.OrderBy(name => name, StringComparer.Ordinal).ToImmutableArray();

                if (recordFieldsNamesOrdered.SequenceEqual(recordFieldsNames))
                    return new ElmRecord(recordFields);
                else
                    return
                    (Result<string, ElmRecord>)
                    ("Unexpected order of " + recordFieldsNames.Length + " fields: " + string.Join(", ", recordFieldsNames));
            }),

            _ =>
            "Value is not a list."
        };
    }

    private static Result<string, (string fieldName, ElmValue fieldValue)> TryMapToRecordField(ElmValue fieldElmValue)
    {
        if (fieldElmValue is ElmList fieldListItems)
        {
            if (fieldListItems.Elements.Count is 2)
            {
                var fieldNameValue = fieldListItems.Elements[0];
                var fieldValue = fieldListItems.Elements[1];

                Result<string, (string, ElmValue)> continueWithFieldName(string fieldName) =>
                    (0 < fieldName.Length && !char.IsUpper(fieldName[0]))
                    ?
                    (fieldName, fieldValue)
                    :
                    "Field name does start with uppercase: '" + fieldName + "'";

                return fieldNameValue switch
                {
                    ElmList fieldNameValueList =>
                    TryMapElmValueToString(fieldNameValueList)
                    .Map(continueWithFieldName)
                    .WithDefault((Result<string, (string, ElmValue)>)
                        "Failed parsing field name value."),

                    ElmString fieldName =>
                    continueWithFieldName(fieldName.Value),

                    _ =>
                    "Unexpected type in field name value."
                };
            }
            else
                return "Unexpected number of list items: " + fieldListItems.Elements.Count;
        }
        else
            return "Not a list.";
    }

    public static (string expressionString, bool needsParens) ElmTagAsExpression(ElmTag elmTag)
    {
        static string applyNeedsParens((string expressionString, bool needsParens) tuple) =>
            tuple.needsParens ? "(" + tuple.expressionString + ")" : tuple.expressionString;

        if (elmTag.TagName is "Set_elm_builtin")
        {
            if (elmTag.Arguments.Count is 1)
            {
                var singleArgument = elmTag.Arguments[0];

                var singleArgumentDictToList = ElmValueDictToList(singleArgument);

                if (singleArgumentDictToList.Count is 0)
                    return ("Set.empty", needsParens: false);

                var setElements = singleArgumentDictToList.Select(field => field.key).ToList();

                return
                    ("Set.fromList [" + string.Join(",", setElements.Select(ElmValueAsExpression).Select(applyNeedsParens)) + "]",
                    needsParens: true);
            }
        }

        if (elmTag.TagName is "RBEmpty_elm_builtin")
            return ("Dict.empty", needsParens: false);

        var dictToList = ElmValueDictToList(elmTag);

        if (dictToList.Count is 0)
        {
            var (needsParens, argumentsString) =
                elmTag.Arguments.Count switch
                {
                    0 =>
                    (false, ""),

                    _ =>
                    (true, " " + string.Join(" ", elmTag.Arguments.Select(ElmValueAsExpression).Select(applyNeedsParens)))
                };

            return (elmTag.TagName + argumentsString, needsParens);
        }

        return
            ("Dict.fromList [" +
            string.Join(",",
            dictToList
            .Select(field => "(" + ElmValueAsExpression(field.key).expressionString + "," + ElmValueAsExpression(field.value).expressionString + ")")) + "]",
            needsParens: true);
    }

    public static IReadOnlyList<(ElmValue key, ElmValue value)> ElmValueDictToList(ElmValue dict) =>
        ElmValueDictFoldr(
            (key, value, acc) => acc.Insert(0, (key, value)),
            ImmutableList<(ElmValue key, ElmValue value)>.Empty,
            dict);

    public static T ElmValueDictFoldr<T>(Func<ElmValue, ElmValue, T, T> func, T aggregate, ElmValue elmValue)
    {
        if (elmValue is ElmTag elmTag && elmTag.TagName is "RBNode_elm_builtin" && elmTag.Arguments.Count is 5)
        {
            var key = elmTag.Arguments[1];
            var value = elmTag.Arguments[2];
            var left = elmTag.Arguments[3];
            var right = elmTag.Arguments[4];

            return ElmValueDictFoldr(func, func(key, value, ElmValueDictFoldr(func, aggregate, right)), left);
        }

        return aggregate;
    }

    public static Maybe<bool> ElmListItemsLookLikeTupleItems(IReadOnlyList<ElmValue> list)
    {
        if (3 < list.Count)
        {
            return false;
        }
        else
        {
            var areAllItemsEqual = AreElmValueListItemTypesEqual(list);

            if (areAllItemsEqual is Maybe<bool>.Just areAllItemsEqualJust)
                return !areAllItemsEqualJust.Value;
            else
                return Maybe<bool>.nothing();
        }
    }

    public static Maybe<bool> AreElmValueListItemTypesEqual(IReadOnlyList<ElmValue> list)
    {
        var pairsTypesEqual =
            list
            .SelectMany((left, leftIndex) =>
            list
            .Skip(leftIndex + 1)
            .Select(right => AreElmValueTypesEqual(left, right)))
            .ToList();

        if (pairsTypesEqual.All(item => item is Maybe<bool>.Just itemJust && itemJust.Value))
            return true;

        if (pairsTypesEqual.Any(item => item is Maybe<bool>.Just itemJust && !itemJust.Value))
            return false;

        return Maybe<bool>.nothing();
    }

    public static Maybe<bool> AreElmValueTypesEqual(ElmValue valueA, ElmValue valueB)
    {
        if (valueA is ElmInteger && valueB is ElmInteger)
            return true;

        if (valueA is Char && valueB is Char)
            return true;

        if (valueA is ElmString && valueB is ElmString)
            return true;

        if (valueA is ElmList && valueB is ElmList)
            return Maybe<bool>.nothing();

        if (valueA is ElmRecord recordA && valueB is ElmRecord recordB)
        {
            var recordAFieldNames = recordA.Fields.Select(field => field.FieldName).ToList();
            var recordBFieldNames = recordB.Fields.Select(field => field.FieldName).ToList();

            if (!recordAFieldNames.OrderBy(name => name).SequenceEqual(recordBFieldNames))
                return false;

            return Maybe<bool>.nothing();
        }

        if (valueA is ElmTag && valueB is ElmTag)
            return Maybe<bool>.nothing();

        if (valueA is ElmInternal && valueB is ElmInternal)
            return Maybe<bool>.nothing();

        return false;
    }
}
