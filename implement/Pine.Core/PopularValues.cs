using Pine.Core.Elm;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core;

public static class PopularValues
{
    public static readonly IReadOnlyList<string> PopularStrings =
        [.. PopularStringsSource
        .Concat(SingleCharStrings)
        .Concat(TwoLetterStrings)
        .Distinct()];

    private static IEnumerable<string> SingleCharStrings =>
        from c in Enumerable.Range(0, 128)
        select new string([(char)c]);

    private static IEnumerable<string> TwoLetterStrings =>
        from c1 in Enumerable.Range(0, 128)
        from c2 in Enumerable.Range(0, 128)
        select new string([(char)c1, (char)c2]);

    private static IEnumerable<string> PopularStringsSource =>
    [
        "Pine",
        "pine",
        "Pine_kernel",

        "Elm",
        "elm",

        "Elm_Record",
        "Elm_Bytes",
        "Elm_Float",

        "Basics",
        "List",
        "Maybe",
        "Result",
        "Tuple",
        "Char",
        "String",
        "Platform",
        "Array",
        "Dict",
        "Set",
        "Bytes",
        "Encode",
        "Decode",
        "Bytes.Encode",
        "Bytes.Decode",
        "Json",
        "Regex",
        "Time",
        "Debug",
        "Process",

        // From the Elm core Basics module
        "Basics",
        "Bool",
        "True",
        "False",
        "Int",
        "Float",
        "infix",
        "left",
        "right",
        "Never",
        "never",
        "(+)",
        "(-)",
        "(*)",
        "(/)",
        "(//)",
        "(^)",
        "toFloat",
        "round",
        "floor",
        "ceiling",
        "truncate",
        "max",
        "min",
        "compare",
        "not",
        "xor",
        "modBy",
        "remainderBy",
        "negate",
        "abs",
        "clamp",
        "sqrt",
        "logBase",
        "e",
        "pi",
        "cos",
        "sin",
        "tan",
        "acos",
        "asin",
        "atan",
        "atan2",
        "degrees",
        "radians",
        "turns",
        "toPolar",
        "fromPolar",
        "isNaN",
        "isInfinite",
        "identity",
        "always",
        "Never",
        "never",

        "Order",
        "EQ",
        "LT",
        "GT",

        "isPineBlob",
        "isPineList",
        "setToList",
        "dictToList",
        "dictKeys",
        "dictFoldr",

        "(==)",
        "(/=)",
        "(<)",
        "(>)",
        "(<=)",
        "(>=)",
        "(<|)",
        "(|>)",
        "(<<)",
        "(>>)",
        "(&&)",
        "(||)",
        "(++)",

        "==",
        "/=",
        "<",
        ">",
        "<=",
        ">=",
        "<|",
        "|>",
        "<<",
        ">>",
        "&&",
        "||",
        "++",

        // From the Elm core Maybe module
        "Maybe",
        "Nothing",
        "Just",
        "map",
        "map2",
        "map3",
        "map4",
        "map5",
        "withDefault",
        "andThen",


        "Err",
        "Ok",

        // From the Elm core Dict module
        "Dict",
        "NColor",
        "Red",
        "Black",
        "RBNode_elm_builtin",
        "RBEmpty_elm_builtin",
        "empty",
        "singleton",
        "insert",
        "update",
        "remove",
        "isEmpty",
        "member",
        "get",
        "size",
        "keys",
        "values",
        "toList",
        "fromList",
        "map",
        "foldl",
        "foldr",
        "filter",
        "partition",
        "union",
        "intersect",
        "diff",
        "merge",
        "key",
        "value",

        // From the Elm core Set module
        "Set_elm_builtin",

        // From the Elm core Bitwise module
        "Bitwise",
        "and",
        "or",
        "xor",
        "complement",
        "shiftLeftBy",
        "shiftRightBy",
        "shiftRightZfBy",

        // From the Elm kernel Parser module
        "Parser",
        "DeadEnd",
        "row",
        "col",
        "Expecting",
        "ExpectingInt",
        "ExpectingHex",
        "ExpectingOctal",
        "ExpectingBinary",
        "ExpectingFloat",
        "ExpectingNumber",
        "ExpectingVariable",
        "ExpectingSymbol",
        "ExpectingKeyword",
        "ExpectingEnd",
        "UnexpectedChar",
        "Problem",
        "BadRepeat",

        "int",
        "hex",
        "octal",
        "binary",
        "float",

        "sequence",
        "start",
        "separator",
        "end",
        "spaces",
        "item",
        "trailing",

        "Trailing",
        "Forbidden",
        "Optional",
        "Mandatory",

        "Nestable",
        "NotNestable",

        "State",
        "src",
        "srcChars",
        "offset",
        "indent",
        "context",
        "row",
        "col",

        "direction",
        "precedence",
        "operator",


        // From the Pine module
        "Pine",
        "Value",
        "ListValue",
        "BlobValue",

        "Expression",
        "Literal",
        "LiteralExpression",
        "List",
        "ListExpression",
        "ParseAndEval",
        "ParseAndEvalExpression",
        "Conditional",
        "ConditionalExpression",
        "Environment",
        "EnvironmentExpression",
        "Function",
        "FunctionExpression",
        "KernelApplication",
        "KernelApplicationExpression",
        "StringTag",
        "StringTagExpression",

        "PathDescription",
        "DescribePathNode",
        "DescribePathEnd",

        "Literal",
        "List",
        "ParseAndEval",
        "Conditional",
        "Environment",
        "Function",
        "KernelApplication",
        "StringTag",

        "equal",
        "length",
        "head",
        "skip",
        "take",
        "reverse",
        "negate",
        "concat",
        "add_int",
        "mul_int",
        "int_add",
        "int_mul",
        "bit_and",
        "bit_or",
        "bit_xor",
        "list_head",
        "int_is_sorted_asc",

        "functionName",
        "argument",
        "input",
        "condition",
        "falseBranch",
        "trueBranch",
        "environment",
        "function",
        "expression",
        "encoded",

        // From the FirCompiler module
        "FirCompiler",
        "FunctionApplicationExpression",
        "ReferenceExpression",
        "DeclarationBlockExpression",
        "PineFunctionApplicationExpression",

        "Deconstruction",
        "ListItemDeconstruction",
        "SkipItemsDeconstruction",
        "PineFunctionApplicationDeconstruction",

        "FunctionEnvironment",
        "LocalEnvironment",
        "ImportedEnvironment",
        "IndependentEnvironment",


        // From the Elm syntax library
        "Module",
        "NormalModule",
        "PortModule",
        "File",
        "Import",

        "exposingList",
        "moduleName",
        "moduleAlias",

        "Declaration",
        "FunctionDeclaration",
        "AliasDeclaration",
        "CustomTypeDeclaration",
        "PortDeclaration",
        "InfixDeclaration",
        "Destructuring",


        "UnitExpr",
        "Application",
        "OperatorApplication",
        "FunctionOrValue",
        "IfBlock",
        "PrefixOperator",
        "Operator",
        "Integer",
        "Hex",
        "Floatable",
        "Negation",
        "Literal",
        "CharLiteral",
        "TupledExpression",
        "ParenthesizedExpression",
        "LetExpression",
        "CaseExpression",
        "LambdaExpression",
        "RecordExpr",
        "ListExpr",
        "RecordAccess",
        "RecordAccessFunction",
        "RecordUpdateExpression",
        "GLSLExpression",

        "Function",
        "Typed",

        "LetDeclaration",
        "LetFunction",
        "LetDestructuring",


        "Pattern",
        "AllPattern",
        "UnitPattern",
        "CharPattern",
        "StringPattern",
        "IntPattern",
        "HexPattern",
        "FloatPattern",
        "TuplePattern",
        "RecordPattern",
        "UnConsPattern",
        "ListPattern",
        "VarPattern",
        "NamedPattern",
        "AsPattern",
        "ParenthesizedPattern",


        "TypeAnnotation",
        "GenericType",
        "Typed",
        "Unit",
        "Tupled",
        "Record",
        "GenericRecord",
        "FunctionTypeAnnotation",


        "row",
        "column",
        "start",
        "end",

        "Node",
        "Range",

        "name",

        "InfixDirection",
        "Left",
        "Right",
        "Non",

        "documentation",
        "signature",
        "typeAnnotation",

        "(|=)",
        "(|.)",
        "|=",
        "|.",


        "PStep",
        "Good",
        "Bad",
        "PState",
        "WithComments",
        "Rope",
        "Leaf",
        "Branch2",

        "module",
        "exposing",
        "import",
        "as",
        "if",
        "then",
        "else",
        "let",
        "in",
        "case",
        "of",
        "type",
        "alias",
        "port",
        "infix",
        "infixl",
        "infixr",
        "where",

        "Problem",
        "ExpectingNumber",
        "ExpectingSymbol",
        "ExpectingAnyChar",
        "ExpectingKeyword",
        "ExpectingCharSatisfyingPredicate",
        "ExpectingStringSatisfyingPredicate",
        "ExpectingCustom",
        "ExpectingOneOf",



        // module LanguageService

        "LanguageService",
        "LanguageServiceState",

        "filePath",
        "text",
        "parsedFile",
        "syntax",

        "DeclarationScope",
        "TopLevelScope",
        "LocalScope",

        "Declaration",
        "FunctionOrValueDeclaration",
        "TypeAliasDeclaration",
        "ChoiceTypeDeclaration",

        "LocationUnderFilePath",
        "CookedDocumentation",
        "Range",
        "DeclarationRange",

        // module Frontend.MonacoEditor

        "EditorMarkerSeverity",
        "ErrorSeverity",
        "WarningSeverity",
        "InfoSeverity",
        "HintSeverity",

        "MonacoCompletionItem",
        "label",
        "kind",
        "detail",
        "documentation",
        "sortText",
        "filterText",
        "insertText",
        "range",
        "commitCharacters",
        "additionalTextEdits",
        "command",
        "data",

        "MonacoCompletionItemKind",
        "ConstructorCompletionItemKind",
        "EnumCompletionItemKind",
        "EnumMemberCompletionItemKind",
        "FunctionCompletionItemKind",
        "ModuleCompletionItemKind",
        "StructCompletionItemKind",

        "MonacoLocation",
        "uri",
        "range",

        "MonacoPosition",
        "lineNumber",
        "column",

        "MonacoRange",
        "startLineNumber",
        "startColumn",
        "endLineNumber",
        "endColumn",


        // kernel modules Json.Encode and Json.Decode

        "Json",
        "Encode",
        "Decode",
        "Json.Encode",
        "Json.Decode",

        "Value",
        "NullValue",
        "BoolValue",
        "IntValue",
        "StringValue",
        "ArrayValue",
        "ObjectValue",
        "FloatValue",

        "null",
        "bool",
        "true",
        "false",
        "int",
        "string",
        "array",
        "object",
        "float",
        "encode",

        "Error",
        "Field",
        "Index",
        "OneOf",
        "Failure",

        "Decoder",

        "succeed",
        "fail",
        "list",


        // module Platform.WebService

        "WebService",
        "Platform.WebService",

        "WebServiceConfig",
        "init",
        "subscriptions",

        "Subscriptions",
        "httpRequest",
        "posixTimeIsPast",

        "HttpRequestProperties",
        "method",
        "uri",
        "headers",
        "bodyAsBase64",
        "body",

        "Commands",
        "Command",
        "RespondToHttpRequest",
        "CreateVolatileProcess",
        "RequestToVolatileProcess",
        "TerminateVolatileProcess",
        "ReadRuntimeInformationCommand",
        "WriteToVolatileProcessNativeStdInCommand",
        "ReadAllFromVolatileProcessNativeCommand",

        "RespondToHttpRequestStruct",
        "httpRequestId",
        "response",

        "HttpResponse",
        "statusCode",
        "headers",
        "body",
        "bodyAsBase64",

        "HttpHeader",
        "name",
        "values",

        "RequestToVolatileProcessResult",
        "RequestToVolatileProcessError",
        "ProcessNotFound",
        "RequestToVolatileProcessComplete",
        "exceptionToString",
        "returnValueToString",
        "durationInMilliseconds",

        "RuntimeInformationRecord",
        "runtimeIdentifier",
        "osPlatform",

    ];

    public static IEnumerable<ElmValue> PopularElmValuesSource()
    {
        static IReadOnlyList<ElmValue> ListOfCharsFromString(string s) =>
            [.. s.Select(c => ElmValue.CharInstance(c))];

        static IEnumerable<(string tagName, IReadOnlyList<ElmValue> tagArgs)> PopularTagUsages()
        {
            yield return ("Function", ListOfCharsFromString("Function"));

            yield return ("BlobValue", ListOfCharsFromString("BlobValue"));

            yield return ("ListValue", ListOfCharsFromString("ListValue"));

            yield return ("Nothing", ListOfCharsFromString("Nothing"));

            yield return ("Just", ListOfCharsFromString("Just"));

            yield return ("Err", ListOfCharsFromString("Err"));

            yield return ("Err", ListOfCharsFromString("Ok"));

            yield return ("Ok", ListOfCharsFromString("Ok"));

            yield return ("RBEmpty_elm_builtin", ListOfCharsFromString("RBEmpty_elm_builtin"));

            yield return ("RBEmpty_elm_builtin", [new ElmValue.ElmList([])]);

            yield return ("RBEmpty_elm_builtin", []);

            yield return ("RBEmpty_elm_builtin", ListOfCharsFromString("RBNode_elm_builtin"));

            yield return ("RBNode_elm_builtin", ListOfCharsFromString("RBNode_elm_builtin"));

            yield return ("LT", ListOfCharsFromString("LT"));

            yield return ("LT", ListOfCharsFromString("EQ"));

            yield return ("LT", ListOfCharsFromString("GT"));

            yield return ("EQ", ListOfCharsFromString("EQ"));

            yield return ("EQ", ListOfCharsFromString("GT"));

            yield return ("GT", ListOfCharsFromString("GT"));

            yield return ("Red", ListOfCharsFromString("Red"));

            yield return ("Red", ListOfCharsFromString("Black"));
        }

        foreach (var (tagName, tagArgs) in PopularTagUsages())
        {
            yield return ElmValue.TagInstance(tagName, tagArgs);
        }

        for (int i = 1; i < 4; i++)
        {
            for (int j = 1; j < 4; j++)
            {
                yield return
                    new ElmValue.ElmList(
                        [ElmValue.Integer(i),
                        ElmValue.Integer(j)
                        ]);
            }
        }
    }
}
