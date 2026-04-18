using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

internal static class InterpreterTestHelper
{
    /// <summary>
    /// Parses an Elm module text and returns a dictionary of its top-level declarations
    /// keyed by <see cref="DeclQualifiedName"/> with empty namespaces (so unqualified
    /// references from within the same module resolve directly by name).
    /// </summary>
    public static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        ParseDeclarations(string elmModuleText)
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new Exception("Failed to parse module: " + err));

        var declarations = new Dictionary<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var declNode in parsedFile.Declarations)
        {
            var name = DeclarationName(declNode.Value);

            if (name is null)
                continue;

            declarations[new DeclQualifiedName([], name)] = declNode.Value;
        }

        return declarations;
    }

    /// <summary>
    /// Returns the simple name of the top-level declaration, or null if the declaration kind
    /// does not carry a name relevant for lookup.
    /// </summary>
    public static string? DeclarationName(SyntaxTypes.Declaration declaration) =>
        declaration switch
        {
            SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration =>
            functionDeclaration.Function.Declaration.Value.Name.Value,

            SyntaxTypes.Declaration.AliasDeclaration aliasDeclaration =>
            aliasDeclaration.TypeAlias.Name.Value,

            SyntaxTypes.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            choiceTypeDeclaration.TypeDeclaration.Name.Value,

            _ =>
            null,
        };

    /// <summary>
    /// Retrieves the body expression of the function declaration with the given name from
    /// the declarations dictionary.
    /// </summary>
    public static SyntaxTypes.Expression GetFunctionBody(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        string functionName)
    {
        var declaration = declarations[new DeclQualifiedName([], functionName)];

        if (declaration is SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
        {
            return functionDeclaration.Function.Declaration.Value.Expression.Value;
        }

        throw new InvalidOperationException(
            "Declaration '" + functionName + "' is not a function declaration.");
    }
}
