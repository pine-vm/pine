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
    /// Program code of an app prepared to run functions in the interpreter.
    /// </summary>
    public record Prepared(
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> Declarations);

    /// <summary>
    /// Parses the supplied <paramref name="moduleSourceTexts"/> to qualify every reference in every module,
    /// and then interprets the canonicalized root expression against a declaration
    /// dictionary keyed by full module-qualified name.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpretAsElmValue(
        string rootExpressionText,
        IReadOnlyList<string> moduleSourceTexts)
    {
        var prepareModulesResult = PrepareModules(moduleSourceTexts);

        if (prepareModulesResult.IsErrOrNull() is { } prepareErr)
        {
            return prepareErr;
        }

        if (prepareModulesResult.IsOkOrNull() is not { } preprocessed)
        {
            throw new System.NotImplementedException(
                "Unexpected result type from PrepareModules: " + prepareModulesResult.GetType().FullName);
        }

        return InterpretAsElmValue(rootExpressionText, preprocessed);
    }

    /// <summary>
    /// Parses the supplied <paramref name="expressionText"/> and qualifies every reference.
    /// </summary>
    public static Result<string, SyntaxModel.Expression> ParseAndCanonicalizeExpressionWithDefaultImports(
        string expressionText)
    {
        var parseResult = ElmSyntaxParser.ParseExpression(expressionText);

        if (parseResult.IsErrOrNull() is { } parseRootErr)
        {
            return "Failed to parse root expression: " + parseRootErr;
        }

        if (parseResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        var canonicalizeRootResult = CanonicalizeExpression(rootExpression);

        if (canonicalizeRootResult.Errors.Count is not 0)
        {
            return "Canonicalization of root expression failed: " + string.Join("; ", canonicalizeRootResult.Errors);
        }

        return canonicalizeRootResult.Value;
    }

    /// <summary>
    /// Parses the supplied <paramref name="moduleSourceTexts"/> to qualify every reference in every module.
    /// </summary>
    public static Result<ElmInterpretationError, Prepared> PrepareModules(
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

        return PrepareModules(parsedFiles);
    }

    /// <summary>
    /// As <see cref="PrepareModules(IReadOnlyList{string})"/>, but accepts
    /// pre-parsed modules.
    /// </summary>
    public static Result<ElmInterpretationError, Prepared> PrepareModules(
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
        var declarations = new Dictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>();

        foreach (var (moduleNameKey, (canonicalizedFile, errors, shadowings)) in canonicalized)
        {
            var fullModuleFile = Stil4mToFull.Convert(canonicalizedFile);

            var abstractFile = ElmSyntaxAbstract.ConvertFromConcrete.FromFile(fullModuleFile);

            var moduleNameParts = moduleNameKey.ToList();

            foreach (var declNode in abstractFile.Declarations)
            {
                if (declNode is ElmSyntaxAbstract.Declaration.InfixDeclaration infixDecl)
                {
                    declarations[DeclQualifiedName.Create(moduleNameParts, infixDecl.Infix.Operator)] = declNode;

                    continue;
                }

                var declName = DeclarationSimpleName(declNode);

                if (declName is null)
                    continue;

                declarations[DeclQualifiedName.Create(moduleNameParts, declName)] = declNode;
            }
        }

        return new Prepared(declarations);
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
    private static string? DeclarationSimpleName(ElmSyntaxAbstract.Declaration declaration) =>
        declaration switch
        {
            ElmSyntaxAbstract.Declaration.FunctionDeclaration functionDeclaration =>
            functionDeclaration.Function.Declaration.Name,

            ElmSyntaxAbstract.Declaration.AliasDeclaration aliasDeclaration =>
            aliasDeclaration.TypeAlias.Name,

            ElmSyntaxAbstract.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            choiceTypeDeclaration.TypeDeclaration.Name,

            _ =>
            null,
        };
}
