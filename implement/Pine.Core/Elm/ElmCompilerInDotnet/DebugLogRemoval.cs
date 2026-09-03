using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Removes fully applied <c>Debug.log</c> calls while preserving their value argument.
/// </summary>
public static class DebugLogRemoval
{
    /// <summary>
    /// Rewrites all function bodies in a module.
    /// </summary>
    public static SyntaxTypes.File RewriteFile(SyntaxTypes.File file) =>
        file with
        {
            Declarations = [.. file.Declarations.Select(RewriteDeclaration)]
        };

    /// <summary>
    /// Rewrites fully applied <c>Debug.log message value</c> expressions to <c>value</c>.
    /// </summary>
    public static SyntaxTypes.Expression RewriteExpression(SyntaxTypes.Expression expression) =>
        RewriteNormalizedExpression(
            ElmSyntaxAbstractTransformations.FlattenAllNestedApplicationHeads(expression));

    private static SyntaxTypes.Expression RewriteNormalizedExpression(
        SyntaxTypes.Expression expression)
    {
        var withRewrittenChildren =
            ElmSyntaxAbstractTransformations.MapChildExpressions(
                expression,
                RewriteNormalizedExpression);

        if (withRewrittenChildren is SyntaxTypes.Expression.Application application &&
            application.Function is SyntaxTypes.Expression.Identifier identifier &&
            application.Arguments.Count >= 2 &&
            identifier.QualifiedName is
            {
                Namespaces: ["Debug"],
                DeclName: "log"
            })
        {
            var valueArgument = application.Arguments[1];

            if (application.Arguments.Count is 2)
                return valueArgument;

            return
                new SyntaxTypes.Expression.Application(
                    valueArgument,
                    [.. application.Arguments.Skip(2)]);
        }

        return withRewrittenChildren;
    }

    private static SyntaxTypes.Declaration RewriteDeclaration(
        SyntaxTypes.Declaration declaration)
    {
        if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
            return declaration;

        return
            new SyntaxTypes.Declaration.FunctionDeclaration(
                functionDeclaration.Function with
                {
                    Declaration =
                    functionDeclaration.Function.Declaration with
                    {
                        Expression =
                        RewriteExpression(
                            functionDeclaration.Function.Declaration.Expression)
                    }
                });
    }
}
