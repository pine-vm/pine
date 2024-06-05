using System.Collections.Generic;
using System.Linq;

namespace Pine;

public static class PopularValues
{
    public static readonly IReadOnlyList<string> PopularStrings = [.. PopularStringsSource.Distinct()];

    private static IEnumerable<string> PopularStringsSource =>
    [
        "Bool",
        "True",
        "False",

        "EQ",
        "LT",
        "GT",

        "Nothing",
        "Just",
        "Err",
        "Ok",

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


        // From the Elm core Dict module
        "Red",
        "Black",
        "RBNode_elm_builtin",
        "RBEmpty_elm_builtin",

        // From the Pine module
        "Pine",
        "Value",
        "ListValue",
        "BlobValue",

        "Expression",
        "LiteralExpression",
        "ListExpression",
        "ParseAndEvalExpression",
        "ConditionalExpression",
        "FunctionExpression",
        "KernelApplicationExpression",
        "StringTagExpression",

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
        "list_head",
        "skip",
        "take",
        "reverse",
        "negate",
        "concat",
        "add_int",
        "mul_int",
        "is_sorted_ascending_int",

        "Literal",
        "List",
        "ParseAndEval",
        "Conditional",
        "Environment",
        "Function",
        "KernelApplication",
        "StringTag",

        "functionName",
        "argument",
        "condition",
        "ifTrue",
        "ifFalse",
        "environment",
        "function",
        "expression",

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

    ];
}
