using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Linq;

using Stil4mFile = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.File;
using Stil4mFromFull = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel;
using Stil4mToFull = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Multi-module evaluation overloads. These accept a collection of Elm module source
/// texts (or pre-parsed <see cref="SyntaxModel.File"/> values), apply the existing
/// <see cref="Canonicalization"/> implementation to qualify all references in
/// expressions, and then dispatch evaluation against the canonicalized declarations
/// keyed by their full module-qualified name.
/// </summary>
public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Parses the supplied <paramref name="moduleSourceTexts"/> to qualify every reference in every module,
    /// and then interprets the canonicalized root expression against a declaration
    /// dictionary keyed by full module-qualified name.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpretAsElmValue(
        string rootExpressionText,
        IReadOnlyList<string> moduleSourceTexts)
    {
        // Parse every module text into the full SyntaxModel form.
        var parsedFiles = new List<SyntaxModel.File>(moduleSourceTexts.Count);

        for (var i = 0; i < moduleSourceTexts.Count; i++)
        {
            var parseResult = ElmSyntaxParser.ParseModuleText(moduleSourceTexts[i]);

            if (parseResult.IsErrOrNull() is { } parseErr)
            {
                return
                    new ElmInterpretationError(
                        "Failed to parse module #" + i + ": " + parseErr,
                        []);
            }

            if (parseResult.IsOkOrNull() is not { } parsedFile)
            {
                throw new System.NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().FullName);
            }

            parsedFiles.Add(parsedFile);
        }

        return ParseAndInterpretAsElmValue(rootExpressionText, parsedFiles);
    }

    /// <summary>
    /// As <see cref="ParseAndInterpretAsElmValue(string, IReadOnlyList{string})"/>, but accepts
    /// pre-parsed modules.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpretAsElmValue(
        string rootExpressionText,
        IReadOnlyList<SyntaxModel.File> modules)
    {
        if (modules.Count is 0)
        {
            return
                new ElmInterpretationError(
                    "ParseAndInterpret with modules requires at least one module "
                    + "(the synthetic root module containing 'pine_root_expression').",
                    []);
        }

        // Convert to the Stil4mElmSyntax7 form expected by Canonicalization.
        var stil4mFiles = new List<Stil4mFile>(modules.Count);

        for (var i = 0; i < modules.Count; i++)
        {
            stil4mFiles.Add(Stil4mFromFull.Convert(modules[i]));
        }

        var canonicalizeResult = Canonicalization.CanonicalizeAllowingErrors(stil4mFiles);

        if (canonicalizeResult.IsErrOrNull() is { } canonErr)
        {
            return
                new ElmInterpretationError(
                    "Canonicalization failed: " + canonErr,
                    []);
        }

        if (canonicalizeResult.IsOkOrNull() is not { } canonicalized)
        {
            throw new System.NotImplementedException(
                "Unexpected canonicalization result type: " + canonicalizeResult.GetType().FullName);
        }

        // Build a declarations dictionary keyed by full module-qualified name. For
        // InfixDeclaration entries, the key uses the operator symbol as DeclName (so
        // BuildInfixOperatorMap finds them).
        var declarations = new Dictionary<DeclQualifiedName, SyntaxModel.Declaration>();

        foreach (var (moduleNameKey, (canonicalizedFile, errors, shadowings)) in canonicalized)
        {
            var fullModuleFile = Stil4mToFull.Convert(canonicalizedFile);

            var moduleNameParts = moduleNameKey.ToList();

            foreach (var declNode in fullModuleFile.Declarations)
            {
                if (declNode.Value is SyntaxModel.Declaration.InfixDeclaration infixDecl)
                {
                    declarations[DeclQualifiedName.Create(moduleNameParts, infixDecl.Infix.Operator.Value)] = declNode.Value;

                    continue;
                }

                var declName = DeclarationSimpleName(declNode.Value);

                if (declName is null)
                    continue;

                declarations[DeclQualifiedName.Create(moduleNameParts, declName)] = declNode.Value;
            }
        }

        var parseRootResult =
            ElmSyntaxParser.ParseExpression(rootExpressionText);

        if (parseRootResult.IsErrOrNull() is { } parseRootErr)
        {
            return
                new ElmInterpretationError(
                    "Failed to parse root expression: " + parseRootErr,
                    []);
        }

        if (parseRootResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseRootResult.GetType().FullName);
        }

        var canonicalizeRootResult = CanonicalizeExpression(rootExpression);

        if (canonicalizeRootResult.Errors.Count is not 0)
        {
            return
                new ElmInterpretationError(
                    "Canonicalization of root expression failed: " + string.Join("; ", canonicalizeRootResult.Errors),
                    []);
        }

        return InterpretAsElmValue(canonicalizeRootResult.Value, declarations);
    }

    private static CanonicalizationResult<SyntaxModel.Expression> CanonicalizeExpression(
        SyntaxModel.Expression expr)
    {
        var stil4mExpr = Stil4mFromFull.Convert(expr);

        return
            Canonicalization.CanonicalizeExpression(stil4mExpr, ImplicitImportConfig.Default)
            .MapValue(stil4mCanonicalExpr => Stil4mToFull.Convert(stil4mCanonicalExpr));
    }


    /// <summary>
    /// Returns the simple top-level name of <paramref name="declaration"/>, or null when
    /// the declaration kind does not carry such a name (e.g. infix or destructuring).
    /// </summary>
    private static string? DeclarationSimpleName(SyntaxModel.Declaration declaration) =>
        declaration switch
        {
            SyntaxModel.Declaration.FunctionDeclaration functionDeclaration =>
            functionDeclaration.Function.Declaration.Value.Name.Value,

            SyntaxModel.Declaration.AliasDeclaration aliasDeclaration =>
            aliasDeclaration.TypeAlias.Name.Value,

            SyntaxModel.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            choiceTypeDeclaration.TypeDeclaration.Name.Value,

            _ =>
            null,
        };
}
