using System;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Functions to report Elm syntax nodes contents.
/// </summary>
public static class ReportSyntaxNodes
{
    /// <summary>
    /// Scan all nodes in a declaration and report contained nodes by type via the provided callbacks.
    /// </summary>
    public static void ReportViaCallback(
        Declaration declaration,
        Action<Expression.LambdaExpression> reportLambda,
        Action<Expression.LetBlock> reportLetBlock,
        Action<Expression.LetDeclaration.LetFunction> reportLetFunction)
    {
        var fullDeclaration = ToFullSyntaxModel.Convert(declaration);

        SyntaxModel.ReportSyntaxNodes.ReportViaCallback(
            fullDeclaration,
            reportLambda:
            lambdaFound =>
            reportLambda(
                new Expression.LambdaExpression(FromFullSyntaxModel.Convert(lambdaFound.Lambda))),
            reportLetBlock:
            letBlockFound =>
            reportLetBlock(FromFullSyntaxModel.Convert(letBlockFound)),
            reportLetFunction:
            letFunctionFound =>
            reportLetFunction(
                new Expression.LetDeclaration.LetFunction(
                    FromFullSyntaxModel.Convert(letFunctionFound.Function))));
    }
}
