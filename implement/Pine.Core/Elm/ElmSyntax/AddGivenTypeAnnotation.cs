using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Provides functionality to set or update type annotations in Elm syntax trees.
/// Supports updating type annotations for top-level declarations and recursively
/// for local declarations within let-blocks, if-blocks, case-blocks, and other expressions.
/// </summary>
public class SetTypeAnnotation
{
    private static readonly Location s_locationZero = new(0, 0);

    private static readonly Range s_rangeZero = new(s_locationZero, s_locationZero);

    /// <summary>
    /// Creates a new syntax node with the specified value at the zero range.
    /// </summary>
    /// <typeparam name="T">The type of value contained in the node.</typeparam>
    /// <param name="value">The value to wrap in a node.</param>
    /// <returns>A new node containing the specified value.</returns>
    private static Node<T> MakeNode<T>(T value) =>
        new(s_rangeZero, value);

    /// <summary>
    /// Sets type annotations for declarations in a file based on the provided dictionary.
    /// </summary>
    /// <param name="fileBefore">The file to update.</param>
    /// <param name="declarationsTypeAnnotations">
    /// A dictionary mapping declaration paths to their type annotations.
    /// Declaration paths are lists of strings representing the nesting of declarations
    /// (e.g., ["foo", "helper"] for a helper function inside foo).
    /// </param>
    /// <param name="declarationWithoutEntry">
    /// Optional callback invoked when a declaration is found that has no corresponding entry
    /// in the declarationsTypeAnnotations dictionary.
    /// </param>
    /// <param name="entryWithoutDeclaration">
    /// Optional callback invoked when an entry in the dictionary has no corresponding declaration
    /// in the file.
    /// </param>
    /// <returns>A new file with updated type annotations.</returns>
    public static File SetTypeAnnotations(
        File fileBefore,
        ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation> declarationsTypeAnnotations,
        System.Action<IReadOnlyList<string>>? declarationWithoutEntry,
        System.Action<IReadOnlyList<string>>? entryWithoutDeclaration)
    {
        var queriedDeclarations =
            new HashSet<IReadOnlyList<string>>(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        var file =
            UpdateTypeAnnotations(
                fileBefore,
                (declarationPath, annotationBefore) =>
                {
                    queriedDeclarations.Add(declarationPath);

                    if (declarationsTypeAnnotations.TryGetValue(declarationPath, out var typeAnnotation))
                    {
                        return typeAnnotation;
                    }

                    declarationWithoutEntry?.Invoke(declarationPath);

                    return null;
                });

        foreach (var entry in queriedDeclarations)
        {
            if (!declarationsTypeAnnotations.ContainsKey(entry))
            {
                entryWithoutDeclaration?.Invoke(entry);
            }
        }

        return file;
    }

    /// <summary>
    /// Updates type annotations for all declarations in a file using a callback function.
    /// </summary>
    /// <param name="fileBefore">The file to update.</param>
    /// <param name="declarationsTypeAnnotations">
    /// A callback function that receives the declaration path and existing type annotation (if any),
    /// and returns the new type annotation to use (or null to keep the existing one).
    /// </param>
    /// <returns>A new file with updated type annotations.</returns>
    public static File UpdateTypeAnnotations(
        File fileBefore,
        System.Func<IReadOnlyList<string>, TypeAnnotation?, TypeAnnotation?> declarationsTypeAnnotations)
    {
        var updatedDeclarations =
            fileBefore.Declarations
            .Select(
                declaration =>
                UpdateDeclarationTypeAnnotations(
                    declaration,
                    declarationPath: [],
                    declarationsTypeAnnotations))
            .ToArray();

        return
            fileBefore with
            {
                Declarations = updatedDeclarations
            };
    }

    /// <summary>
    /// Updates a FunctionStruct's type annotation and recursively updates nested expressions.
    /// </summary>
    private static FunctionStruct UpdateFunctionStructTypeAnnotation(
        FunctionStruct functionStruct,
        IReadOnlyList<string> fullPath,
        System.Func<IReadOnlyList<string>, TypeAnnotation?, TypeAnnotation?> declarationsTypeAnnotations)
    {
        var funcName = functionStruct.Declaration.Value.Name.Value;

        var existingTypeAnnotation = functionStruct.Signature?.Value.TypeAnnotation.Value;
        var newTypeAnnotation = declarationsTypeAnnotations(fullPath, existingTypeAnnotation);

        // Update the expression for nested let-blocks
        var updatedExpression =
            UpdateExpressionTypeAnnotations(
                functionStruct.Declaration.Value.Expression,
                fullPath,
                declarationsTypeAnnotations);

        var updatedImpl =
            functionStruct.Declaration.Value with
            {
                Expression = updatedExpression
            };

        var updatedDeclNode = functionStruct.Declaration with { Value = updatedImpl };

        Node<Signature>? updatedSignature;

        if (newTypeAnnotation is not null)
        {
            var newTypeAnnotationNode = MakeNode(newTypeAnnotation);

            var newSignature =
                new Signature(
                    Name: MakeNode(funcName),
                    ColonLocation: s_locationZero,
                    TypeAnnotation: newTypeAnnotationNode);

            updatedSignature = MakeNode(newSignature);
        }
        else
        {
            updatedSignature = functionStruct.Signature;
        }

        return
            functionStruct with
            {
                Signature = updatedSignature,
                Declaration = updatedDeclNode
            };
    }

    /// <summary>
    /// Updates type annotations for a top-level declaration.
    /// </summary>
    /// <param name="declaration">The declaration node to update.</param>
    /// <param name="declarationPath">The path to the declaration (empty for top-level).</param>
    /// <param name="declarationsTypeAnnotations">The callback to determine new type annotations.</param>
    /// <returns>An updated declaration node.</returns>
    private static Node<Declaration> UpdateDeclarationTypeAnnotations(
        Node<Declaration> declaration,
        IReadOnlyList<string> declarationPath,
        System.Func<IReadOnlyList<string>, TypeAnnotation?, TypeAnnotation?> declarationsTypeAnnotations)
    {
        if (declaration.Value is Declaration.FunctionDeclaration funcDecl)
        {
            var funcName = funcDecl.Function.Declaration.Value.Name.Value;
            var fullPath = declarationPath.Concat([funcName]).ToList();

            var updatedFunc =
                UpdateFunctionStructTypeAnnotation(
                    funcDecl.Function,
                    fullPath,
                    declarationsTypeAnnotations);

            var updatedFuncDecl = new Declaration.FunctionDeclaration(updatedFunc);
            return declaration with { Value = updatedFuncDecl };
        }

        return declaration;
    }

    /// <summary>
    /// Recursively updates type annotations within an expression.
    /// Traverses into let-blocks, if-blocks, case-blocks, lambda expressions, and other
    /// compound expressions to find and update nested function declarations.
    /// </summary>
    /// <param name="expression">The expression to traverse.</param>
    /// <param name="parentPath">The path to the parent declaration containing this expression.</param>
    /// <param name="declarationsTypeAnnotations">The callback to determine new type annotations.</param>
    /// <returns>An updated expression with any nested type annotations updated.</returns>
    private static Node<ExpressionSyntax> UpdateExpressionTypeAnnotations(
        Node<ExpressionSyntax> expression,
        IReadOnlyList<string> parentPath,
        System.Func<IReadOnlyList<string>, TypeAnnotation?, TypeAnnotation?> declarationsTypeAnnotations)
    {
        switch (expression.Value)
        {
            case ExpressionSyntax.LetExpression letExpr:
                var updatedDeclarations = new List<Node<ExpressionSyntax.LetDeclaration>>();

                foreach (var letDecl in letExpr.Value.Declarations)
                {
                    var updatedLetDecl =
                        UpdateLetDeclarationTypeAnnotations(
                            letDecl,
                            parentPath,
                            declarationsTypeAnnotations);

                    updatedDeclarations.Add(updatedLetDecl);
                }

                var updatedInExpr =
                    UpdateExpressionTypeAnnotations(
                        letExpr.Value.Expression,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedLetBlock =
                    letExpr.Value with
                    {
                        Declarations = updatedDeclarations,
                        Expression = updatedInExpr
                    };

                var updatedLetExpression = new ExpressionSyntax.LetExpression(updatedLetBlock);
                return expression with { Value = updatedLetExpression };

            case ExpressionSyntax.IfBlock ifBlock:
                var updatedCondition =
                    UpdateExpressionTypeAnnotations(
                        ifBlock.Condition,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedThenBlock =
                    UpdateExpressionTypeAnnotations(
                        ifBlock.ThenBlock,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedElseBlock =
                    UpdateExpressionTypeAnnotations(
                        ifBlock.ElseBlock,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedIfBlock =
                    new ExpressionSyntax.IfBlock(
                        ifBlock.IfTokenLocation,
                        updatedCondition,
                        ifBlock.ThenTokenLocation,
                        updatedThenBlock,
                        ifBlock.ElseTokenLocation,
                        updatedElseBlock);

                return expression with { Value = updatedIfBlock };

            case ExpressionSyntax.CaseExpression caseExpr:
                var updatedScrutinee =
                    UpdateExpressionTypeAnnotations(
                        caseExpr.CaseBlock.Expression,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedCases = new List<Case>();

                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                {
                    var updatedCaseExpr =
                        UpdateExpressionTypeAnnotations(
                            caseItem.Expression,
                            parentPath,
                            declarationsTypeAnnotations);

                    updatedCases.Add(caseItem with { Expression = updatedCaseExpr });
                }

                var updatedCaseBlock =
                    caseExpr.CaseBlock with
                    {
                        Expression = updatedScrutinee,
                        Cases = updatedCases
                    };

                return expression with { Value = new ExpressionSyntax.CaseExpression(updatedCaseBlock) };

            case ExpressionSyntax.LambdaExpression lambdaExpr:
                var updatedLambdaBody =
                    UpdateExpressionTypeAnnotations(
                        lambdaExpr.Lambda.Expression,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedLambda =
                    lambdaExpr.Lambda with { Expression = updatedLambdaBody };

                return expression with { Value = new ExpressionSyntax.LambdaExpression(updatedLambda) };

            case ExpressionSyntax.Application appExpr:
                var updatedArgs =
                    appExpr.Arguments
                    .Select(arg => UpdateExpressionTypeAnnotations(arg, parentPath, declarationsTypeAnnotations))
                    .ToList();

                return expression with { Value = new ExpressionSyntax.Application(updatedArgs) };

            case ExpressionSyntax.OperatorApplication opApp:
                var updatedLeft =
                    UpdateExpressionTypeAnnotations(
                        opApp.Left,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedRight =
                    UpdateExpressionTypeAnnotations(
                        opApp.Right,
                        parentPath,
                        declarationsTypeAnnotations);

                var updatedOpApp = opApp with { Left = updatedLeft, Right = updatedRight };
                return expression with { Value = updatedOpApp };

            case ExpressionSyntax.ParenthesizedExpression parenExpr:
                var updatedInner =
                    UpdateExpressionTypeAnnotations(
                        parenExpr.Expression,
                        parentPath,
                        declarationsTypeAnnotations);

                return expression with { Value = new ExpressionSyntax.ParenthesizedExpression(updatedInner) };

            case ExpressionSyntax.TupledExpression tupledExpr:
                var updatedElements =
                    UpdateSeparatedSyntaxList(
                        tupledExpr.Elements,
                        elem => UpdateExpressionTypeAnnotations(elem, parentPath, declarationsTypeAnnotations));

                return expression with { Value = new ExpressionSyntax.TupledExpression(updatedElements) };

            case ExpressionSyntax.ListExpr listExpr:
                var updatedListElements =
                    UpdateSeparatedSyntaxList(
                        listExpr.Elements,
                        elem => UpdateExpressionTypeAnnotations(elem, parentPath, declarationsTypeAnnotations));

                return expression with { Value = new ExpressionSyntax.ListExpr(updatedListElements) };

            case ExpressionSyntax.RecordExpr recordExpr:
                var updatedRecordFields =
                    UpdateSeparatedSyntaxList(
                        recordExpr.Fields,
                        field => field with
                        {
                            ValueExpr =
                            UpdateExpressionTypeAnnotations(field.ValueExpr, parentPath, declarationsTypeAnnotations)
                        });

                return expression with { Value = new ExpressionSyntax.RecordExpr(updatedRecordFields) };

            case ExpressionSyntax.RecordUpdateExpression recordUpdate:
                var updatedUpdateFields =
                    UpdateSeparatedSyntaxList(
                        recordUpdate.Fields,
                        field => field with
                        {
                            ValueExpr =
                            UpdateExpressionTypeAnnotations(field.ValueExpr, parentPath, declarationsTypeAnnotations)
                        });

                return expression with { Value = recordUpdate with { Fields = updatedUpdateFields } };

            case ExpressionSyntax.RecordAccess recordAccess:
                var updatedRecord =
                    UpdateExpressionTypeAnnotations(
                        recordAccess.Record,
                        parentPath,
                        declarationsTypeAnnotations);

                return expression with { Value = recordAccess with { Record = updatedRecord } };

            case ExpressionSyntax.Negation negation:
                var updatedNegationExpr =
                    UpdateExpressionTypeAnnotations(
                        negation.Expression,
                        parentPath,
                        declarationsTypeAnnotations);

                return expression with { Value = new ExpressionSyntax.Negation(updatedNegationExpr) };

            // Leaf expressions that don't contain nested expressions
            case ExpressionSyntax.FunctionOrValue:
            case ExpressionSyntax.Integer:
            case ExpressionSyntax.Floatable:
            case ExpressionSyntax.Literal:
            case ExpressionSyntax.CharLiteral:
            case ExpressionSyntax.UnitExpr:
            case ExpressionSyntax.PrefixOperator:
            case ExpressionSyntax.RecordAccessFunction:
                return expression;

            default:
                throw new System.NotImplementedException(
                    $"Unhandled expression type in UpdateExpressionTypeAnnotations: {expression.Value.GetType().FullName}");
        }
    }

    /// <summary>
    /// Updates type annotations for declarations within a let-block.
    /// Handles both let-function declarations (which can have type annotations) and
    /// let-destructuring declarations (which only need their expressions traversed).
    /// </summary>
    /// <param name="letDecl">The let declaration to update.</param>
    /// <param name="parentPath">The path to the parent declaration.</param>
    /// <param name="declarationsTypeAnnotations">The callback to determine new type annotations.</param>
    /// <returns>An updated let declaration.</returns>
    private static Node<ExpressionSyntax.LetDeclaration> UpdateLetDeclarationTypeAnnotations(
        Node<ExpressionSyntax.LetDeclaration> letDecl,
        IReadOnlyList<string> parentPath,
        System.Func<IReadOnlyList<string>, TypeAnnotation?, TypeAnnotation?> declarationsTypeAnnotations)
    {
        switch (letDecl.Value)
        {
            case ExpressionSyntax.LetDeclaration.LetFunction letFunc:
                var funcName = letFunc.Function.Declaration.Value.Name.Value;
                var fullPath = parentPath.Concat([funcName]).ToList();

                var updatedFunc =
                    UpdateFunctionStructTypeAnnotation(
                        letFunc.Function,
                        fullPath,
                        declarationsTypeAnnotations);

                return
                    letDecl with
                    {
                        Value = new ExpressionSyntax.LetDeclaration.LetFunction(updatedFunc)
                    };

            case ExpressionSyntax.LetDeclaration.LetDestructuring letDestructuring:

                var updatedDestrExpr =
                    UpdateExpressionTypeAnnotations(
                        letDestructuring.Expression,
                        parentPath,
                        declarationsTypeAnnotations);

                return
                    letDecl with
                    {
                        Value = letDestructuring with { Expression = updatedDestrExpr }
                    };

            default:
                return letDecl;
        }
    }

    /// <summary>
    /// Updates all elements in a separated syntax list using the provided update function.
    /// </summary>
    /// <typeparam name="T">The type of elements in the list.</typeparam>
    /// <param name="list">The list to update.</param>
    /// <param name="updateFunc">The function to apply to each element.</param>
    /// <returns>A new list with updated elements.</returns>
    private static SeparatedSyntaxList<T> UpdateSeparatedSyntaxList<T>(
        SeparatedSyntaxList<T> list,
        System.Func<T, T> updateFunc)
    {
        switch (list)
        {
            case SeparatedSyntaxList<T>.Empty:
                return list;

            case SeparatedSyntaxList<T>.NonEmpty nonEmpty:
                var updatedFirst = updateFunc(nonEmpty.First);

                var updatedRest =
                    nonEmpty.Rest
                    .Select(item => (item.SeparatorLocation, Node: updateFunc(item.Node)))
                    .ToList();

                return new SeparatedSyntaxList<T>.NonEmpty(updatedFirst, updatedRest);

            default:
                return list;
        }
    }
}
