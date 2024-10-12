using Pine.ElmInteractive;
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
        "Elm_Record",
        "Elm_Bytes",

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
        "(==)",
        "(/=)",
        "(<)",
        "(>)",
        "(<=)",
        "(>=)",
        "max",
        "min",
        "compare",
        "not",
        "(&&)",
        "(||)",
        "xor",
        "(++)",
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
        "(<|)",
        "(|>)",
        "(<<)",
        "(>>)",
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
        "File",
        "Import",

        "Declaration",
        "FunctionDeclaration",
        "AliasDeclaration",
        "CustomTypeDeclaration",
        "PortDeclaration",
        "InfixDeclaration",
        "Destructuring",

        "FunctionOrValue",
        "Function",
        "Application",
        "Typed",

        "row",
        "column",
        "start",
        "end",

        "Node",
        "Range",

        "name"
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
