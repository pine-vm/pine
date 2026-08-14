using System.Linq;

namespace Pine.Core.Elm.LanguageServer.MonacoEditor;


/*

{-| <https://microsoft.github.io/monaco-editor/api/interfaces/monaco.languages.completionitem.html>
-}
type alias MonacoCompletionItem =
    { label : String
    , kind : CompletionItemKind
    , documentation : String
    , insertText : String
    }

 * */
/// <summary>
/// Represents a completion item for the Monaco editor.
/// </summary>
public record MonacoCompletionItem(
    string Label,
    CompletionItemKind Kind,
    string Documentation,
    string InsertText);

/*

{-| <https://microsoft.github.io/monaco-editor/api/enums/monaco.languages.completionitemkind.html>
-}
type CompletionItemKind
    = ConstructorCompletionItemKind
    | EnumCompletionItemKind
    | EnumMemberCompletionItemKind
    | FunctionCompletionItemKind
    | ModuleCompletionItemKind
    | StructCompletionItemKind

 * */

/// <summary>
/// Identifies a Monaco completion item kind.
/// </summary>
public abstract record CompletionItemKind
{
    /// <summary>
    /// Identifies a constructor completion item.
    /// </summary>
    public record ConstructorCompletionItemKind : CompletionItemKind;

    /// <summary>
    /// Identifies an enum completion item.
    /// </summary>
    public record EnumCompletionItemKind : CompletionItemKind;

    /// <summary>
    /// Identifies an enum member completion item.
    /// </summary>
    public record EnumMemberCompletionItemKind : CompletionItemKind;

    /// <summary>
    /// Identifies a function completion item.
    /// </summary>
    public record FunctionCompletionItemKind : CompletionItemKind;

    /// <summary>
    /// Identifies a module completion item.
    /// </summary>
    public record ModuleCompletionItemKind : CompletionItemKind;

    /// <summary>
    /// Identifies a struct completion item.
    /// </summary>
    public record StructCompletionItemKind : CompletionItemKind;
}

/*

type alias MonacoRange =
    {-
       <https://microsoft.github.io/monaco-editor/typedoc/interfaces/IRange.html>
    -}
    { startLineNumber : Int
    , startColumn : Int
    , endLineNumber : Int
    , endColumn : Int
    }

 * */
/// <summary>
/// Represents a range in a Monaco editor document.
/// </summary>
public record MonacoRange(
    int StartLineNumber,
    int StartColumn,
    int EndLineNumber,
    int EndColumn);

/// <summary>
/// Decodes Monaco completion items from Elm values.
/// </summary>
public static class CompletionItemEncoding
{
    /// <summary>
    /// Decodes a Monaco completion item from a Pine value.
    /// </summary>
    public static Result<string, MonacoCompletionItem> Decode(PineValue pineValue)
    {

        var decodeElmValueResult =
            ElmValueEncoding.PineValueAsElmValue(pineValue, null, null);

        {
            if (decodeElmValueResult.IsErrOrNull() is { } err)
            {
                return "Failed decoding as Elm value: " + err;
            }
        }

        if (decodeElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new System.NotImplementedException("Unexpected result type: " + decodeElmValueResult.GetType());
        }

        return Decode(elmValue);
    }

    /// <summary>
    /// Decodes a Monaco completion item from an Elm value.
    /// </summary>
    public static Result<string, MonacoCompletionItem> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord record)
        {
            return "Expected Elm record, got: " + elmValue.GetType();
        }

        if (record.Fields.Count is not 4)
        {
            return "Expected 4 fields, got: " + record.Fields.Count;
        }

        if (record["label"] is not { } labelElmValue)
        {
            return
                "Expected field 'label' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (labelElmValue is not ElmValue.ElmString labelString)
        {
            return "Expected field 'label' to be a string, got: " + labelElmValue.GetType();
        }

        if (record["kind"] is not { } kindValue)
        {
            return
                "Expected field 'kind' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        var decodeKindResult = DecodeCompletionItemKind(kindValue);

        {
            if (decodeKindResult.IsErrOrNull() is { } err)
            {
                return "Failed decoding kind: " + err;
            }
        }

        if (decodeKindResult.IsOkOrNull() is not { } kind)
        {
            throw new System.NotImplementedException("Unexpected result type: " + decodeKindResult.GetType());
        }

        if (record["documentation"] is not { } documentationValue)
        {
            return
                "Expected field 'documentation' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (documentationValue is not ElmValue.ElmString documentationString)
        {
            return "Expected field 'documentation' to be a string, got: " + documentationValue.GetType();
        }

        if (record["insertText"] is not { } insertTextValue)
        {
            return
                "Expected field 'insertText' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (insertTextValue is not ElmValue.ElmString insertTextString)
        {
            return "Expected field 'insertText' to be a string, got: " + insertTextValue.GetType();
        }

        return
            new MonacoCompletionItem(
                labelString.Value,
                kind,
                documentationString.Value,
                insertTextString.Value);
    }

    /// <summary>
    /// Decodes a completion item kind from an Elm value.
    /// </summary>
    public static Result<string, CompletionItemKind> DecodeCompletionItemKind(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmTag tag)
        {
            return "Expected Elm tag, got: " + elmValue.GetType();
        }

        if (tag.TagName is "ConstructorCompletionItemKind")
        {
            return new CompletionItemKind.ConstructorCompletionItemKind();
        }

        if (tag.TagName is "EnumCompletionItemKind")
        {
            return new CompletionItemKind.EnumCompletionItemKind();
        }

        if (tag.TagName is "EnumMemberCompletionItemKind")
        {
            return new CompletionItemKind.EnumMemberCompletionItemKind();
        }

        if (tag.TagName is "FunctionCompletionItemKind")
        {
            return new CompletionItemKind.FunctionCompletionItemKind();
        }

        if (tag.TagName is "ModuleCompletionItemKind")
        {
            return new CompletionItemKind.ModuleCompletionItemKind();
        }

        if (tag.TagName is "StructCompletionItemKind")
        {
            return new CompletionItemKind.StructCompletionItemKind();
        }

        return "Unexpected tag name: " + tag.TagName;
    }
}

/// <summary>
/// Decodes Monaco ranges from Elm values.
/// </summary>
public static class MonacoRangeEncoding
{
    /// <summary>
    /// Decodes a Monaco range from a Pine value.
    /// </summary>
    public static Result<string, MonacoRange> Decode(PineValue pineValue)
    {
        var decodeElmValueResult = ElmValueEncoding.PineValueAsElmValue(pineValue, null, null);

        if (decodeElmValueResult.IsErrOrNull() is { } err)
        {
            return "Failed decoding as Elm value: " + err;
        }

        if (decodeElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new System.NotImplementedException("Unexpected result type: " + decodeElmValueResult.GetType());
        }

        return Decode(elmValue);
    }

    /// <summary>
    /// Decodes a Monaco range from an Elm value.
    /// </summary>
    public static Result<string, MonacoRange> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord record)
        {
            return "Expected Elm record, got: " + elmValue.GetType();
        }

        // We expect exactly 4 fields: startLineNumber, startColumn, endLineNumber, endColumn
        if (record.Fields.Count is not 4)
        {
            return "Expected 4 fields, got: " + record.Fields.Count;
        }

        if (record["startLineNumber"] is not { } startLineNumberValue)
        {
            return
                "Expected field 'startLineNumber' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (startLineNumberValue is not ElmValue.ElmInteger startLineNumberInteger)
        {
            return "Expected field 'startLineNumber' to be an integer, got: " + startLineNumberValue.GetType();
        }

        if (record["startColumn"] is not { } startColumnValue)
        {
            return
                "Expected field 'startColumn' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (startColumnValue is not ElmValue.ElmInteger startColumnInteger)
        {
            return "Expected field 'startColumn' to be an integer, got: " + startColumnValue.GetType();
        }

        if (record["endLineNumber"] is not { } endLineNumberValue)
        {
            return
                "Expected field 'endLineNumber' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (endLineNumberValue is not ElmValue.ElmInteger endLineNumberInteger)
        {
            return "Expected field 'endLineNumber' to be an integer, got: " + endLineNumberValue.GetType();
        }

        if (record["endColumn"] is not { } endColumnValue)
        {
            return
                "Expected field 'endColumn' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (endColumnValue is not ElmValue.ElmInteger endColumnInteger)
        {
            return "Expected field 'endColumn' to be an integer, got: " + endColumnValue.GetType();
        }

        return
            new MonacoRange(
                (int)startLineNumberInteger.Value,
                (int)startColumnInteger.Value,
                (int)endLineNumberInteger.Value,
                (int)endColumnInteger.Value);
    }
}
