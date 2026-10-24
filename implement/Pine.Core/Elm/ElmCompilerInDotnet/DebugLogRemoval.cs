using Pine.Core.Elm.Syntax.Abstract;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Removes fully applied <c>Debug.log</c> calls while preserving their value argument.
/// </summary>
public static class DebugLogRemoval
{
    /// <summary>
    /// Rewrites all function bodies in a module.
    /// </summary>
    public static File RewriteFile(File file) =>
        file with
        {
            Declarations = [.. file.Declarations.Select(RewriteDeclaration)]
        };

    /// <summary>
    /// Rewrites fully applied <c>Debug.log message value</c> expressions to <c>value</c>.
    /// </summary>
    public static Expression RewriteExpression(Expression expression) =>
        RewriteNormalizedExpression(
            ElmSyntaxAbstractTransformations.FlattenAllNestedApplicationHeads(expression));

    private static Expression RewriteNormalizedExpression(
        Expression expression)
    {
        var withRewrittenChildren =
            ElmSyntaxAbstractTransformations.MapChildExpressions(
                expression,
                RewriteNormalizedExpression);

        if (withRewrittenChildren is Expression.Application application &&
            application.Function is Expression.Identifier identifier &&
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
                new Expression.Application(
                    valueArgument,
                    [.. application.Arguments.Skip(2)]);
        }

        return withRewrittenChildren;
    }

    /// <summary>Rewrites one declaration without a module container.</summary>
    public static Declaration RewriteDeclaration(
        Declaration declaration)
    {
        if (declaration is not Declaration.FunctionDeclaration functionDeclaration)
            return declaration;

        return
            new Declaration.FunctionDeclaration(
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
