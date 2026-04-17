using System;

namespace Pine.Core.Elm.ElmSyntax.SyntaxModel;

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
        switch (declaration)
        {
            case Declaration.FunctionDeclaration funcDecl:
                ReportFunctionDeclaration(funcDecl, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Declaration.ChoiceTypeDeclaration:
            case Declaration.AliasDeclaration:
            case Declaration.PortDeclaration:
            case Declaration.InfixDeclaration:

                // These declarations do not contain expressions.
                break;

            default:
                throw new NotImplementedException(
                    "Unexpected declaration type: " + declaration.GetType().FullName);
        }
    }

    /// <summary>
    /// Scan all nodes in a function declaration and report contained nodes by type via the provided callbacks.
    /// </summary>
    public static void ReportFunctionDeclaration(
        Declaration.FunctionDeclaration funcDecl,
        Action<Expression.LambdaExpression> reportLamba,
        Action<Expression.LetBlock> reportLetBlock,
        Action<Expression.LetDeclaration.LetFunction> reportLetFunction)
    {
        ReportExpression(
            funcDecl.Function.Declaration.Value.Expression.Value,
            reportLamba,
            reportLetBlock,
            reportLetFunction);
    }

    /// <summary>
    /// Recursively scan an expression and report contained nodes by type via the provided callbacks.
    /// </summary>
    public static void ReportExpression(
        Expression expression,
        Action<Expression.LambdaExpression> reportLambda,
        Action<Expression.LetBlock> reportLetBlock,
        Action<Expression.LetDeclaration.LetFunction> reportLetFunction)
    {
        switch (expression)
        {
            case Expression.UnitExpr:
            case Expression.Literal:
            case Expression.CharLiteral:
            case Expression.Integer:
            case Expression.Floatable:
            case Expression.FunctionOrValue:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:

                // Leaf expressions contain no sub-expressions.
                break;

            case Expression.Negation negation:
                ReportExpression(negation.Expression.Value, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements.Nodes)
                    ReportExpression(element.Value, reportLambda, reportLetBlock, reportLetFunction);

                break;

            case Expression.IfBlock ifBlock:
                ReportExpression(ifBlock.Condition.Value, reportLambda, reportLetBlock, reportLetFunction);
                ReportExpression(ifBlock.ThenBlock.Value, reportLambda, reportLetBlock, reportLetFunction);
                ReportExpression(ifBlock.ElseBlock.Value, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Expression.ParenthesizedExpression paren:
                ReportExpression(paren.Expression.Value, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Expression.Application app:
                ReportExpression(app.Function.Value, reportLambda, reportLetBlock, reportLetFunction);

                foreach (var arg in app.Arguments)
                    ReportExpression(arg.Value, reportLambda, reportLetBlock, reportLetFunction);

                break;

            case Expression.OperatorApplication opApp:
                ReportExpression(opApp.Left.Value, reportLambda, reportLetBlock, reportLetFunction);
                ReportExpression(opApp.Right.Value, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Expression.TupledExpression tuple:
                foreach (var element in tuple.Elements.Nodes)
                    ReportExpression(element.Value, reportLambda, reportLetBlock, reportLetFunction);

                break;

            case Expression.LambdaExpression lambda:
                reportLambda(lambda);
                ReportExpression(lambda.Lambda.Expression.Value, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Expression.CaseExpression caseExpr:
                ReportExpression(caseExpr.CaseBlock.Expression.Value, reportLambda, reportLetBlock, reportLetFunction);

                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                    ReportExpression(caseItem.Expression.Value, reportLambda, reportLetBlock, reportLetFunction);

                break;

            case Expression.LetExpression letExpr:
                reportLetBlock(letExpr.Value);

                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case Expression.LetDeclaration.LetFunction letFunc:
                            reportLetFunction(letFunc);

                            ReportExpression(
                                letFunc.Function.Declaration.Value.Expression.Value,
                                reportLambda,
                                reportLetBlock,
                                reportLetFunction);

                            break;

                        case Expression.LetDeclaration.LetDestructuring letDestructuring:
                            ReportExpression(
                                letDestructuring.Expression.Value,
                                reportLambda,
                                reportLetBlock,
                                reportLetFunction);

                            break;

                        default:
                            throw new NotImplementedException(
                                "Unexpected let declaration type: " + decl.Value.GetType().FullName);
                    }
                }

                ReportExpression(letExpr.Value.Expression.Value, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields.Nodes)
                    ReportExpression(field.ValueExpr.Value, reportLambda, reportLetBlock, reportLetFunction);

                break;

            case Expression.RecordAccess recordAccess:
                ReportExpression(recordAccess.Record.Value, reportLambda, reportLetBlock, reportLetFunction);
                break;

            case Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields.Nodes)
                    ReportExpression(field.ValueExpr.Value, reportLambda, reportLetBlock, reportLetFunction);

                break;

            default:
                throw new NotImplementedException(
                    "Unexpected expression type: " + expression.GetType().FullName);
        }
    }
}
