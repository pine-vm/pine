using Pine.Core.CodeAnalysis;
using Pine.Core.Files;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxTreeClassic;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Methods for compiling Elm source code and environments into Pine values, using the standard packaging for
/// Elm functions and modules as found at
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/FirCompiler.elm#L2179-L2242"></see>
/// and
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/ElmCompiler.elm"></see>
/// </summary>
public class ElmCompiler
{
    public static Result<string, PineValue> CompileInteractiveEnvironment(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths)
    {
        /*
         * WIP TODO:
         * Implement filtering, ordering.
         */

        var elmModuleFiles =
            appCodeTree.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        var compiledModuleEntries = new List<PineValue>();

        foreach (var moduleFile in elmModuleFiles)
        {
            var moduleText =
                Encoding.UTF8.GetString(moduleFile.fileContent.Span);

            var parseResult =
                ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText);

            {
                if (parseResult.IsErrOrNull() is { } err)
                {
                    return err;
                }
            }

            if (parseResult.IsOkOrNull() is not { } parseModuleOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().Name);
            }

            var moduleName =
                SyntaxTypes.Module.GetModuleName(parseModuleOk.ModuleDefinition.Value).Value;

            var moduleNameFlattened =
                string.Join(".", moduleName);

            var moduleValue = CompileModule(parseModuleOk);

            var namedModuleEntry =
                PineValue.List(
                    [
                    StringEncoding.ValueFromString(moduleNameFlattened),
                    moduleValue
                    ]);

            compiledModuleEntries.Add(namedModuleEntry);
        }

        var compiledEnvValue =
            PineValue.List(
                [
                ..compiledModuleEntries
                ]);

        return compiledEnvValue;
    }

    private static PineValue CompileModule(SyntaxTypes.File parsedModule)
    {
        var declarations =
            parsedModule.Declarations
            .Select(declNode => declNode.Value)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>();

        IReadOnlyList<(string, PineValue)> compiledFunctions =
            [..
            declarations
            .Select(declaration =>
            (declaration.Function.Declaration.Value.Name.Value,
            CompileFunctionDeclaration(declaration)))
            ];

        var compiledFunctionDeclaration =
            new ElmModuleInCompilation(
                FunctionDeclarations: compiledFunctions);

        var moduleValue =
            EmitModuleValue(compiledFunctionDeclaration);

        return moduleValue;
    }

    /*
    type alias ElmModuleInCompilation =
        { functionDeclarations : List ( String, Pine.Value )
        , typeDeclarations : List ( String, ElmModuleTypeDeclaration )
        }
     * */

    private record ElmModuleInCompilation(
        IReadOnlyList<(string declName, PineValue)> FunctionDeclarations);

    private static PineValue CompileFunctionDeclaration(
        SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
    {
        if (functionDeclaration.Function.Declaration.Value.Arguments.Count is not 0)
        {
            throw new NotImplementedException(
                "Only zero-argument functions are supported in this temporary implementation.");
        }

        var functionBody =
            functionDeclaration.Function.Declaration.Value.Expression.Value;

        if (functionBody is SyntaxTypes.Expression.Integer integerLiteral)
        {
            return EmitPlainValueDeclaration(
                EmitIntegerLiteral(integerLiteral.Value));
        }

        if (functionBody is SyntaxTypes.Expression.Literal stringLiteral)
        {
            return EmitPlainValueDeclaration(
                EmitStringLiteral(stringLiteral.Value));
        }

        if (functionBody is SyntaxTypes.Expression.CharLiteral charLiteral)
        {
            return EmitPlainValueDeclaration(
                EmitCharLiteral(charLiteral.Value));
        }

        if (functionBody is SyntaxTypes.Expression.FunctionOrValue functionOrValue)
        {
            if (functionOrValue.ModuleName.Count is 0)
            {
                if (functionOrValue.Name is "True")
                {
                    return EmitPlainValueDeclaration(
                        EmitBooleanLiteral(true));
                }

                if (functionOrValue.Name is "False")
                {
                    return EmitPlainValueDeclaration(
                        EmitBooleanLiteral(false));
                }
            }
        }

        throw new NotImplementedException(
            "Expression type not supported in this temporary implementation: " +
            functionBody.GetType().Name);
    }

    private static PineValue EmitPlainValueDeclaration(PineValue value)
    {
        return
            FunctionRecord.EncodeFunctionRecordInValueTagged(
                new FunctionRecord(
                    InnerFunction: Expression.LiteralInstance(value),
                    ParameterCount: 0,
                    EnvFunctions: ReadOnlyMemory<PineValue>.Empty,
                    ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty
                ));
    }

    private static PineValue EmitModuleValue(
        ElmModuleInCompilation compiledModule)
    {
        /*
        emitModuleValue : ElmModuleInCompilation -> Pine.Value
        emitModuleValue parsedModule =
            let
                typeDescriptions : List ( String, Pine.Value )
                typeDescriptions =
                    List.foldr
                        (\( typeName, typeDeclaration ) aggregate ->
                            ( typeName, emitTypeDeclarationValue typeDeclaration ) :: aggregate
                        )
                        []
                        parsedModule.typeDeclarations
            in
            Pine.ListValue
                (List.map Pine.valueFromContextExpansionWithName
                    (List.concat [ parsedModule.functionDeclarations, typeDescriptions ])
                )
         * */

        IReadOnlyList<(string declName, PineValue declValue)> typeDescriptions = [];

        PineValue[] entries =
            [.. compiledModule.FunctionDeclarations
            .Concat(typeDescriptions)
            .Select(tuple => ValueFromContextExpansionWithName(tuple.declName, tuple.Item2))
            ];

        return PineValue.List(entries);
    }

    private static PineValue EmitStringLiteral(string str)
    {
        /*
        valueFromString : String -> Pine.Value
        valueFromString string =
            Pine.ListValue
                [ elmStringTypeTagNameAsValue
                , Pine.ListValue [ Pine.computeValueFromString string ]
                ]
         * */

        return ElmValueEncoding.StringAsPineValue(str);
    }

    private static PineValue EmitIntegerLiteral(BigInteger value)
    {
        return IntegerEncoding.EncodeSignedInteger(value);
    }

    private static PineValue EmitCharLiteral(int value)
    {
        return ElmValueEncoding.ElmCharAsPineValue(value);
    }

    private static PineValue EmitBooleanLiteral(bool value)
    {
        return KernelFunction.ValueFromBool(value);
    }

    private static PineValue ValueFromContextExpansionWithName(
        string name,
        PineValue pineValue)
    {
        /*
        valueFromContextExpansionWithName : ( String, Value ) -> Value
        valueFromContextExpansionWithName ( declName, declValue ) =
            ListValue [ computeValueFromString declName, declValue ]
         * */

        return PineValue.List([StringEncoding.ValueFromString(name), pineValue]);
    }

    public static PineValue Compile(
        AppCompilationUnits compilationUnits)
    {
        throw new NotImplementedException();
    }
}
