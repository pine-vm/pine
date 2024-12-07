using Pine.Core;
using Pine.ElmInteractive;
using System.Linq;

namespace Pine.Elm.MonacoEditor;


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

public abstract record CompletionItemKind
{
    public record ConstructorCompletionItemKind : CompletionItemKind;
    public record EnumCompletionItemKind : CompletionItemKind;
    public record EnumMemberCompletionItemKind : CompletionItemKind;
    public record FunctionCompletionItemKind : CompletionItemKind;
    public record ModuleCompletionItemKind : CompletionItemKind;
    public record StructCompletionItemKind : CompletionItemKind;
}

public static class CompletionItemEncoding
{
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
            throw new System.NotImplementedException
                ("Unexpected result type: " + decodeElmValueResult.GetType());
        }

        return Decode(elmValue);
    }

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
            throw new System.NotImplementedException
                ("Unexpected result type: " + decodeKindResult.GetType());
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

        return new MonacoCompletionItem(
            labelString.Value,
            kind,
            documentationString.Value,
            insertTextString.Value);
    }

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
