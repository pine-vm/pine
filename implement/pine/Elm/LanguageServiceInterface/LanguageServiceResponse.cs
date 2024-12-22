using Pine.Core;
using Pine.ElmInteractive;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Elm.LanguageServiceInterface;


/*

type Response
    = WorkspaceSummaryResponse
    | ProvideHoverResponse (List String)
    | ProvideCompletionItemsResponse (List Frontend.MonacoEditor.MonacoCompletionItem)
    | ProvideDefinitionResponse (List LocationUnderFilePath)
    | TextDocumentSymbolResponse (List DocumentSymbol)
    | TextDocumentReferencesResponse (List LocationUnderFilePath)
    | TextDocumentRenameResponse WorkspaceEdit

 * */

public abstract record Response
{
    public record WorkspaceSummaryResponse
        : Response;

    public record ProvideHoverResponse(
        IReadOnlyList<string> Strings)
        : Response;

    public record ProvideCompletionItemsResponse(
        IReadOnlyList<MonacoEditor.MonacoCompletionItem> CompletionItems)
        : Response;

    public record ProvideDefinitionResponse(
        IReadOnlyList<LocationUnderFilePath> Locations)
        : Response;

    public record TextDocumentSymbolResponse(
        IReadOnlyList<DocumentSymbol> Symbols)
        : Response;

    public record TextDocumentReferencesResponse(
        IReadOnlyList<LocationUnderFilePath> Locations)
        : Response;

    public record TextDocumentRenameResponse(
        WorkspaceEdit WorkspaceEdit)
        : Response;
}

/*

type alias LocationUnderFilePath =
    { filePath : String
    , range : Frontend.MonacoEditor.MonacoRange
    }
 * */

public record LocationUnderFilePath(
    string FilePath,
    MonacoEditor.MonacoRange Range);


/*

type DocumentSymbol
    = DocumentSymbol DocumentSymbolStruct


type alias DocumentSymbolStruct =
    { name : String
    , kind : SymbolKind
    , range : Frontend.MonacoEditor.MonacoRange
    , selectionRange : Frontend.MonacoEditor.MonacoRange
    , children : List DocumentSymbol
    }


type SymbolKind
    = SymbolKind_File
    | SymbolKind_Module
    | SymbolKind_Namespace
    | SymbolKind_Package
    | SymbolKind_Class
    | SymbolKind_Enum
    | SymbolKind_Interface
    | SymbolKind_Function
    | SymbolKind_Constant
    | SymbolKind_String
    | SymbolKind_Number
    | SymbolKind_Boolean
    | SymbolKind_Array
    | SymbolKind_EnumMember
    | SymbolKind_Struct

 * */

public record DocumentSymbol(
    DocumentSymbolStruct Struct);

public record DocumentSymbolStruct(
    string Name,
    SymbolKind Kind,
    MonacoEditor.MonacoRange Range,
    MonacoEditor.MonacoRange SelectionRange,
    IReadOnlyList<DocumentSymbol> Children);

public enum SymbolKind
{
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    EnumMember = 22,
    Struct = 23
}

/*

type alias WorkspaceEdit =
    List TextDocumentEdit


type alias TextDocumentEdit =
    { filePath : String
    , edits : List TextEdit
    }


type alias TextEdit =
    { range : Frontend.MonacoEditor.MonacoRange
    , newText : String
    }

 * */

public record WorkspaceEdit(
    IReadOnlyList<TextDocumentEdit> Edits);

public record TextDocumentEdit(
    string FilePath,
    IReadOnlyList<TextEdit> Edits);

public record TextEdit(
    MonacoEditor.MonacoRange Range,
    string NewText);


public static class ResponseEncoding
{
    public static Result<string, Response> Decode(PineValue pineValue)
    {
        var elmValueResult =
            ElmValueEncoding.PineValueAsElmValue(pineValue, null, null);

        {
            if (elmValueResult.IsErrOrNull() is { } err)
            {
                return "Failed decoding as Elm value: " + err;
            }
        }

        if (elmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + elmValueResult.GetType());
        }

        if (elmValue is not ElmValue.ElmTag responseTag)
        {
            return "Expected Elm tag, got: " + elmValue.GetType();
        }

        if (responseTag.TagName is "WorkspaceSummaryResponse")
        {
            if (responseTag.Arguments.Count is not 0)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            return new Response.WorkspaceSummaryResponse();
        }

        if (responseTag.TagName is "ProvideHoverResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            if (responseTag.Arguments[0] is not ElmValue.ElmList responseList)
            {
                return
                    "Unexpected response tag argument type: " +
                    responseTag.Arguments[0].GetType();
            }

            var plainStrings = new string[responseList.Elements.Count];

            for (var i = 0; i < responseList.Elements.Count; i++)
            {
                if (responseList.Elements[i] is not ElmValue.ElmString plainString)
                {
                    return
                        "Unexpected response tag element type: " +
                        responseList.Elements[i].GetType();
                }

                plainStrings[i] = plainString.Value;
            }

            return new Response.ProvideHoverResponse(plainStrings);
        }

        if (responseTag.TagName is "ProvideCompletionItemsResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            if (responseTag.Arguments[0] is not ElmValue.ElmList responseList)
            {
                return
                    "Unexpected response tag argument type: " +
                    responseTag.Arguments[0].GetType();
            }

            var completionItems = new MonacoEditor.MonacoCompletionItem[responseList.Elements.Count];

            for (var i = 0; i < responseList.Elements.Count; i++)
            {
                var completionItemResult =
                    MonacoEditor.CompletionItemEncoding.Decode(responseList.Elements[i]);

                {
                    if (completionItemResult.IsErrOrNull() is { } err)
                    {
                        return "Failed decoding completion item: " + err;
                    }
                }

                if (completionItemResult.IsOkOrNull() is not { } completionItem)
                {
                    throw new System.NotImplementedException
                        ("Unexpected result type: " + completionItemResult.GetType());
                }

                completionItems[i] = completionItem;
            }

            return new Response.ProvideCompletionItemsResponse(completionItems);
        }

        if (responseTag.TagName is "ProvideDefinitionResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            var locationsResult =
                DecodeLocationUnderFilePathList(responseTag.Arguments[0]);
            {
                if (locationsResult.IsErrOrNull() is { } err)
                {
                    return "Failed decoding locations: " + err;
                }
            }

            if (locationsResult.IsOkOrNull() is not { } locations)
            {
                throw new System.NotImplementedException
                    ("Unexpected result type: " + locationsResult.GetType());
            }

            return new Response.ProvideDefinitionResponse(locations);
        }

        if (responseTag.TagName is "TextDocumentSymbolResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            if (responseTag.Arguments[0] is not ElmValue.ElmList responseList)
            {
                return
                    "Unexpected response tag argument type: " +
                    responseTag.Arguments[0].GetType();
            }

            var symbols = new DocumentSymbol[responseList.Elements.Count];

            for (var i = 0; i < responseList.Elements.Count; i++)
            {
                var symbolResult =
                    DocumentSymbolEncoding.Decode(responseList.Elements[i]);

                {
                    if (symbolResult.IsErrOrNull() is { } err)
                    {
                        return "Failed decoding symbol at index [" + i + "]: " + err;
                    }
                }

                if (symbolResult.IsOkOrNull() is not { } symbol)
                {
                    throw new System.NotImplementedException
                        ("Unexpected result type: " + symbolResult.GetType());
                }

                symbols[i] = symbol;
            }

            return new Response.TextDocumentSymbolResponse(symbols);
        }

        if (responseTag.TagName is "TextDocumentReferencesResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            var locationsResult =
                DecodeLocationUnderFilePathList(responseTag.Arguments[0]);
            {
                if (locationsResult.IsErrOrNull() is { } err)
                {
                    return "Failed decoding locations: " + err;
                }
            }

            if (locationsResult.IsOkOrNull() is not { } locations)
            {
                throw new System.NotImplementedException
                    ("Unexpected result type: " + locationsResult.GetType());
            }

            return new Response.TextDocumentReferencesResponse(locations);
        }

        if (responseTag.TagName is "TextDocumentRenameResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            var workspaceEditResult =
                WorkspaceEditEncoding.Decode(responseTag.Arguments[0]);
            {
                if (workspaceEditResult.IsErrOrNull() is { } err)
                {
                    return "Failed decoding workspace edit: " + err;
                }
            }

            if (workspaceEditResult.IsOkOrNull() is not { } workspaceEdit)
            {
                throw new System.NotImplementedException
                    ("Unexpected result type: " + workspaceEditResult.GetType());
            }

            return new Response.TextDocumentRenameResponse(workspaceEdit);
        }

        return
            "Unexpected response tag: " +
            responseTag.TagName;
    }

    public static Result<string, IReadOnlyList<LocationUnderFilePath>> DecodeLocationUnderFilePathList(
        ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmList list)
        {
            return "Expected Elm list, got: " + elmValue.GetType();
        }

        var locations = new LocationUnderFilePath[list.Elements.Count];

        for (var i = 0; i < list.Elements.Count; i++)
        {
            var locationResult =
                LocationUnderFilePathEncoding.Decode(list.Elements[i]);
            {
                if (locationResult.IsErrOrNull() is { } err)
                {
                    return "Failed decoding location at index [" + i + "]: " + err;
                }
            }

            if (locationResult.IsOkOrNull() is not { } location)
            {
                throw new System.NotImplementedException
                    ("Unexpected result type: " + locationResult.GetType());
            }

            locations[i] = location;
        }

        return locations;
    }
}

public static class LocationUnderFilePathEncoding
{
    public static Result<string, LocationUnderFilePath> Decode(PineValue pineValue)
    {
        var decodeElmValueResult = ElmValueEncoding.PineValueAsElmValue(pineValue, null, null);

        if (decodeElmValueResult.IsErrOrNull() is { } err)
        {
            return "Failed decoding as Elm value: " + err;
        }

        if (decodeElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + decodeElmValueResult.GetType());
        }

        return Decode(elmValue);
    }

    public static Result<string, LocationUnderFilePath> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord record)
        {
            return "Expected Elm record, got: " + elmValue.GetType();
        }

        // We expect exactly 2 fields: filePath, range
        if (record.Fields.Count is not 2)
        {
            return "Expected 2 fields, got: " + record.Fields.Count;
        }

        if (record["filePath"] is not { } filePathValue)
        {
            return
                "Expected field 'filePath' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (filePathValue is not ElmValue.ElmString filePathString)
        {
            return "Expected field 'filePath' to be a string, got: " + filePathValue.GetType();
        }

        if (record["range"] is not { } rangeValue)
        {
            return
                "Expected field 'range' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        var rangeDecodeResult =
            MonacoEditor.MonacoRangeEncoding.Decode(rangeValue);

        if (rangeDecodeResult.IsErrOrNull() is { } rangeDecodeErr)
        {
            return "Failed decoding range: " + rangeDecodeErr;
        }

        if (rangeDecodeResult.IsOkOrNull() is not { } range)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + rangeDecodeResult.GetType());
        }

        return new LocationUnderFilePath(filePathString.Value, range);
    }
}

public static class DocumentSymbolEncoding
{
    public static Result<string, DocumentSymbol> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmTag tag)
        {
            return "Expected Elm tag, got: " + elmValue.GetType();
        }
        if (tag.TagName is "DocumentSymbol")
        {
            if (tag.Arguments.Count is not 1)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            if (tag.Arguments[0] is not ElmValue.ElmRecord record)
            {
                return
                    "Unexpected tag argument type: " +
                    tag.Arguments[0].GetType();
            }
            return DecodeDocumentSymbolStruct(record);
        }
        return "Unexpected tag name: " + tag.TagName;
    }

    public static Result<string, DocumentSymbol> DecodeDocumentSymbolStruct(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord record)
        {
            return "Expected Elm record, got: " + elmValue.GetType();
        }

        if (record.Fields.Count is not 5)
        {
            return "Expected 5 fields, got: " + record.Fields.Count;
        }

        if (record["name"] is not { } nameValue)
        {
            return
                "Expected field 'name' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (nameValue is not ElmValue.ElmString nameString)
        {
            return "Expected field 'name' to be a string, got: " + nameValue.GetType();
        }

        if (record["kind"] is not { } kindValue)
        {
            return
                "Expected field 'kind' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        var decodeKindResult = SymbolKindEncoding.Decode(kindValue);
        {
            if (decodeKindResult.IsErrOrNull() is { } err)
            {
                return "Failed decoding kind: " + err;
            }
        }

        if (decodeKindResult.IsOkOrNullable() is not { } kind)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + decodeKindResult.GetType());
        }

        if (record["range"] is not { } rangeValue)
        {
            return
                "Expected field 'range' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        var rangeDecodeResult =
            MonacoEditor.MonacoRangeEncoding.Decode(rangeValue);
        {
            if (rangeDecodeResult.IsErrOrNull() is { } rangeDecodeErr)
            {
                return "Failed decoding range: " + rangeDecodeErr;
            }
        }

        if (rangeDecodeResult.IsOkOrNull() is not { } range)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + rangeDecodeResult.GetType());
        }

        if (record["selectionRange"] is not { } selectionRangeValue)
        {
            return
                "Expected field 'selectionRange' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        var selectionRangeDecodeResult =
            MonacoEditor.MonacoRangeEncoding.Decode(selectionRangeValue);
        {
            if (selectionRangeDecodeResult.IsErrOrNull() is { } selectionRangeDecodeErr)
            {
                return "Failed decoding selectionRange: " + selectionRangeDecodeErr;
            }
        }

        if (selectionRangeDecodeResult.IsOkOrNull() is not { } selectionRange)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + selectionRangeDecodeResult.GetType());
        }

        if (record["children"] is not { } childrenValue)
        {
            return
                "Expected field 'children' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (childrenValue is not ElmValue.ElmList childrenList)
        {
            return "Expected field 'children' to be a list, got: " + childrenValue.GetType();
        }

        var children = new DocumentSymbol[childrenList.Elements.Count];

        for (var i = 0; i < childrenList.Elements.Count; i++)
        {
            var childResult = Decode(childrenList.Elements[i]);
            {
                if (childResult.IsErrOrNull() is { } err)
                {
                    return "Failed decoding child at index [" + i + "]: " + err;
                }
            }

            if (childResult.IsOkOrNull() is not { } child)
            {
                throw new System.NotImplementedException
                    ("Unexpected result type: " + childResult.GetType());
            }
            children[i] = child;
        }

        return new DocumentSymbol(new DocumentSymbolStruct(
            nameString.Value,
            kind,
            range,
            selectionRange,
            children));
    }
}

public static class SymbolKindEncoding
{
    public static Result<string, SymbolKind> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmTag tag)
        {
            return "Expected Elm tag, got: " + elmValue.GetType();
        }

        if (tag.TagName is "SymbolKind_File")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }

            return SymbolKind.File;
        }

        if (tag.TagName is "SymbolKind_Module")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Module;
        }

        if (tag.TagName is "SymbolKind_Namespace")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Namespace;
        }

        if (tag.TagName is "SymbolKind_Package")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Package;
        }

        if (tag.TagName is "SymbolKind_Class")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Class;
        }

        if (tag.TagName is "SymbolKind_Enum")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Enum;
        }

        if (tag.TagName is "SymbolKind_Interface")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Interface;
        }

        if (tag.TagName is "SymbolKind_Function")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Function;
        }

        if (tag.TagName is "SymbolKind_Constant")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Constant;
        }

        if (tag.TagName is "SymbolKind_String")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.String;
        }

        if (tag.TagName is "SymbolKind_Number")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Number;
        }

        if (tag.TagName is "SymbolKind_Boolean")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Boolean;
        }

        if (tag.TagName is "SymbolKind_Array")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Array;
        }

        if (tag.TagName is "SymbolKind_EnumMember")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.EnumMember;
        }

        if (tag.TagName is "SymbolKind_Struct")
        {
            if (tag.Arguments.Count is not 0)
            {
                return
                    "Unexpected tag arguments count: " +
                    tag.Arguments.Count;
            }
            return SymbolKind.Struct;
        }

        return "Unexpected tag name: " + tag.TagName;
    }
}

public static class WorkspaceEditEncoding
{
    public static Result<string, WorkspaceEdit> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmList list)
        {
            return "Expected Elm list, got: " + elmValue.GetType();
        }
        var textDocumentEdits = new TextDocumentEdit[list.Elements.Count];
        for (var i = 0; i < list.Elements.Count; i++)
        {
            var textDocumentEditResult =
                TextDocumentEditEncoding.Decode(list.Elements[i]);
            {
                if (textDocumentEditResult.IsErrOrNull() is { } err)
                {
                    return "Failed decoding text document edit at index [" + i + "]: " + err;
                }
            }

            if (textDocumentEditResult.IsOkOrNull() is not { } textDocumentEdit)
            {
                throw new System.NotImplementedException
                    ("Unexpected result type: " + textDocumentEditResult.GetType());
            }

            textDocumentEdits[i] = textDocumentEdit;
        }
        return new WorkspaceEdit(textDocumentEdits);
    }
}

public static class TextDocumentEditEncoding
{
    public static Result<string, TextDocumentEdit> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord record)
        {
            return "Expected Elm record, got: " + elmValue.GetType();
        }

        if (record.Fields.Count is not 2)
        {
            return "Expected 2 fields, got: " + record.Fields.Count;
        }

        if (record["filePath"] is not { } filePathValue)
        {
            return
                "Expected field 'filePath' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (filePathValue is not ElmValue.ElmString filePathString)
        {
            return "Expected field 'filePath' to be a string, got: " + filePathValue.GetType();
        }

        if (record["edits"] is not { } editsValue)
        {
            return
                "Expected field 'edits' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (editsValue is not ElmValue.ElmList editsListValue)
        {
            return "Expected field 'edits' to be a list, got: " + editsValue.GetType();
        }

        var textEdits = new TextEdit[editsListValue.Elements.Count];

        for (var i = 0; i < editsListValue.Elements.Count; i++)
        {
            var textEditResult = TextEditEncoding.Decode(editsListValue.Elements[i]);
            {
                if (textEditResult.IsErrOrNull() is { } err)
                {
                    return "Failed decoding text edit at index [" + i + "]: " + err;
                }
            }
            if (textEditResult.IsOkOrNull() is not { } textEdit)
            {
                throw new System.NotImplementedException
                    ("Unexpected result type: " + textEditResult.GetType());
            }

            textEdits[i] = textEdit;
        }

        return new TextDocumentEdit(filePathString.Value, textEdits);
    }
}

public static class TextEditEncoding
{
    public static Result<string, TextEdit> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord record)
        {
            return "Expected Elm record, got: " + elmValue.GetType();
        }

        if (record.Fields.Count is not 2)
        {
            return "Expected 2 fields, got: " + record.Fields.Count;
        }

        if (record["range"] is not { } rangeValue)
        {
            return
                "Expected field 'range' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        var rangeDecodeResult =
            MonacoEditor.MonacoRangeEncoding.Decode(rangeValue);

        {
            if (rangeDecodeResult.IsErrOrNull() is { } rangeDecodeErr)
            {
                return "Failed decoding range: " + rangeDecodeErr;
            }
        }

        if (rangeDecodeResult.IsOkOrNull() is not { } range)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + rangeDecodeResult.GetType());
        }

        if (record["newText"] is not { } newTextValue)
        {
            return
                "Expected field 'newText' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (newTextValue is not ElmValue.ElmString newTextString)
        {
            return "Expected field 'newText' to be a string, got: " + newTextValue.GetType();
        }

        return new TextEdit(range, newTextString.Value);
    }
}

