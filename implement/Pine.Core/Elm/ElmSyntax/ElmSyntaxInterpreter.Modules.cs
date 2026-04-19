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
    /// Name of the synthetic root function that wraps the supplied root expression.
    /// </summary>
    private const string SyntheticRootFunctionName = "pine_root_expression";

    /// <summary>
    /// Module name used for the dedicated synthetic module that hosts the root
    /// expression. The module has no explicit <c>import</c> statements, so the
    /// only names available to the root expression are those provided by
    /// <see cref="ImplicitImportConfig.Default"/> (everything from <c>Basics</c>;
    /// the canonical names of <c>List</c>, <c>Char</c>, <c>String</c>, <c>Maybe</c>,
    /// <c>Result</c>, <c>Debug</c>, <c>Tuple</c>, <c>Platform</c>; the aliases
    /// <c>Cmd</c>/<c>Sub</c>; and the names exposed from those modules as
    /// documented in <c>guide/elm-programming-language-semantics.md</c>) plus
    /// fully-qualified references to declarations in the user-supplied dependency
    /// modules.
    /// </summary>
    private const string SyntheticRootModuleName = "PineRootExpression";

    /// <summary>
    /// Parses the supplied <paramref name="moduleSourceTexts"/> together with a
    /// dedicated synthetic root module containing
    /// <c>pine_root_expression = <paramref name="rootExpressionText"/></c>, runs
    /// <see cref="Canonicalization.CanonicalizeAllowingErrors(IReadOnlyList{Stil4mFile})"/>
    /// to qualify every reference in every module (including the root expression),
    /// and then interprets the canonicalized root expression against a declaration
    /// dictionary keyed by full module-qualified name.
    /// </summary>
    /// <remarks>
    /// The root expression is canonicalized inside its own fresh module that has
    /// no explicit <c>import</c> statements. This gives REPL-like semantics: no
    /// user-supplied module's <c>import ... as Alias</c> or <c>exposing (..)</c>
    /// clause leaks into the root expression's lexical scope, so the root
    /// expression must reference declarations from <paramref name="moduleSourceTexts"/>
    /// by their fully-qualified canonical names (e.g. <c>Foo.bar</c>, not
    /// <c>F.bar</c> or a bare <c>bar</c>).
    ///
    /// However, <see cref="ImplicitImportConfig.Default"/> is still applied to the
    /// synthetic root module by canonicalization, exactly as the Elm compiler
    /// applies it to every regular module. This means the root expression has
    /// direct access to the names listed in the "Implicit Imports" section of
    /// <c>guide/elm-programming-language-semantics.md</c> — for example,
    /// <c>1 + 2</c>, <c>Just 7</c>, <c>True</c>, <c>List.map</c>,
    /// <c>String.fromInt</c>, <c>Char.toCode</c>, etc.
    /// </remarks>
    /// <param name="rootExpressionText">
    /// The Elm expression text to evaluate. References are resolved as if the
    /// expression appeared at the top level of a fresh module with no
    /// user-supplied imports.
    /// </param>
    /// <param name="moduleSourceTexts">
    /// Source texts of the participating Elm dependency modules. The root
    /// expression may call into any declaration in any of these modules using
    /// the declaration's fully-qualified canonical name. The order is not
    /// significant.
    /// </param>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> with the evaluated <see cref="ElmValue"/> on
    /// success, or an <see cref="ElmInterpretationError"/> describing a parse,
    /// canonicalization or runtime failure on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpret(
        string rootExpressionText,
        IReadOnlyList<string> moduleSourceTexts)
    {
        // Build the dedicated synthetic root module. It has no `import`
        // statements; only the implicit imports applied by canonicalization are
        // in scope for the root expression. References to user-supplied modules
        // must therefore use the modules' fully-qualified canonical names.
        var rootModuleText = BuildSyntheticRootModuleText(rootExpressionText);

        var allModuleTexts = new List<string>(moduleSourceTexts.Count + 1)
        {
            rootModuleText,
        };

        for (var i = 0; i < moduleSourceTexts.Count; i++)
        {
            allModuleTexts.Add(moduleSourceTexts[i]);
        }

        // Parse every module text into the full SyntaxModel form.
        var parsedFiles = new List<SyntaxModel.File>(allModuleTexts.Count);

        for (var i = 0; i < allModuleTexts.Count; i++)
        {
            var parseResult = ElmSyntaxParser.ParseModuleText(allModuleTexts[i]);

            if (parseResult.IsErrOrNull() is { } parseErr)
            {
                // i == 0 corresponds to the synthetic root module; emit a more
                // helpful diagnostic for that case so callers can distinguish a
                // problem in the supplied root expression from a problem in one
                // of the dependency modules.
                if (i is 0)
                {
                    return new ElmInterpretationError(
                        "Failed to parse synthetic root module wrapping the root expression: "
                        + parseErr,
                        []);
                }

                return new ElmInterpretationError(
                    "Failed to parse module #" + (i - 1) + ": " + parseErr,
                    []);
            }

            if (parseResult.IsOkOrNull() is not { } parsedFile)
            {
                throw new System.NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().FullName);
            }

            parsedFiles.Add(parsedFile);
        }

        return ParseAndInterpret(parsedFiles);
    }

    /// <summary>
    /// As <see cref="ParseAndInterpret(string, IReadOnlyList{string})"/>, but accepts
    /// pre-parsed modules. The first module must be the synthetic root module: it
    /// must declare a top-level value named <c>pine_root_expression</c> whose body
    /// is the expression to evaluate, and it should have no explicit imports so
    /// that the root expression's lexical scope contains only the implicit
    /// imports.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpret(
        IReadOnlyList<SyntaxModel.File> modules)
    {
        if (modules.Count is 0)
        {
            return new ElmInterpretationError(
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
            return new ElmInterpretationError(
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

        // Identify the canonicalized form of the synthetic root module by matching
        // its module name against the first input module.
        var rootContextModuleName =
            SyntaxModel.Module.GetModuleName(modules[0].ModuleDefinition.Value).Value;

        SyntaxModel.File? canonicalizedRootContextFile = null;

        foreach (var (moduleNameKey, (canonicalizedFile, _errors, _shadowings)) in canonicalized)
        {
            var fullModuleFile = Stil4mToFull.Convert(canonicalizedFile);

            var moduleNameParts = moduleNameKey.ToList();

            if (NameSequenceEquals(moduleNameParts, rootContextModuleName))
            {
                canonicalizedRootContextFile = fullModuleFile;
            }

            foreach (var declNode in fullModuleFile.Declarations)
            {
                if (declNode.Value is SyntaxModel.Declaration.InfixDeclaration infixDecl)
                {
                    declarations[new DeclQualifiedName(moduleNameParts, infixDecl.Infix.Operator.Value)] =
                        declNode.Value;
                    continue;
                }

                var declName = DeclarationSimpleName(declNode.Value);

                if (declName is null)
                    continue;

                declarations[new DeclQualifiedName(moduleNameParts, declName)] = declNode.Value;
            }
        }

        if (canonicalizedRootContextFile is null)
        {
            return new ElmInterpretationError(
                "Canonicalization did not return the synthetic root module ("
                + string.Join(".", rootContextModuleName) + ").",
                []);
        }

        // Locate the synthetic root expression's body in the canonicalized root module.
        SyntaxModel.Expression? rootExpression = null;

        foreach (var declNode in canonicalizedRootContextFile.Declarations)
        {
            if (declNode.Value is SyntaxModel.Declaration.FunctionDeclaration functionDecl
                && functionDecl.Function.Declaration.Value.Name.Value == SyntheticRootFunctionName)
            {
                rootExpression = functionDecl.Function.Declaration.Value.Expression.Value;
                break;
            }
        }

        if (rootExpression is null)
        {
            return new ElmInterpretationError(
                "Synthetic root module does not contain the expected '" + SyntheticRootFunctionName
                + "' declaration. When passing pre-parsed modules, the first module must declare "
                + "this function with the expression to evaluate as its body.",
                []);
        }

        return Interpret(rootExpression, declarations);
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

    /// <summary>
    /// Builds the source text of the dedicated synthetic root module that wraps
    /// <paramref name="expressionText"/> as the body of the
    /// <see cref="SyntheticRootFunctionName"/> declaration. The module has no
    /// <c>import</c> statements, so canonicalization will only apply the
    /// implicit imports defined by <see cref="ImplicitImportConfig.Default"/> to
    /// the root expression — matching the "Implicit Imports" section of
    /// <c>guide/elm-programming-language-semantics.md</c> and giving the root
    /// expression REPL-like semantics with respect to the user-supplied
    /// dependency modules.
    /// </summary>
    private static string BuildSyntheticRootModuleText(string expressionText)
    {
        var indented =
            string.Join(
                "\n",
                expressionText
                    .Replace("\r\n", "\n")
                    .Split('\n')
                    .Select(line => "    " + line));

        return
            "module " + SyntheticRootModuleName + " exposing (..)\n\n\n"
            + SyntheticRootFunctionName + " =\n"
            + indented
            + "\n";
    }

    private static bool NameSequenceEquals(IReadOnlyList<string> a, IReadOnlyList<string> b)
    {
        if (a.Count != b.Count)
            return false;

        for (var i = 0; i < a.Count; i++)
        {
            if (!string.Equals(a[i], b[i], System.StringComparison.Ordinal))
                return false;
        }

        return true;
    }
}
