using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Format for Elm syntax for use in snapshot tests.
/// Some of the tested implementations generate new code in form of syntax nodes,
/// and we want format it a bit to make it easier to read and compare in snapshot tests.
/// The format applied here is mainly adding line breaks in function application expressions and
/// some list expressions depending on the syntactical complexity of contained items.
/// </summary>
public class SnapshotTestFormat
{
    public static File Format(File file)
    {
        var formatedLessAVH4 = FormatLessAVH4(file);

        // Avh4Format covers proper indentation and spacing

        var formatted = Avh4Format.Format(formatedLessAVH4);

        return formatted;
    }

    /// <summary>
    /// This part is only for adding line breaks in some cases.
    /// Transforms function applications to multiline format for snapshot testing.
    /// </summary>
    private static File FormatLessAVH4(File file)
    {
        return new File(
            ModuleDefinition: file.ModuleDefinition,
            Imports: file.Imports,
            Declarations: [.. file.Declarations.Select(FormatDeclaration)],
            Comments: file.Comments
        );
    }

    private static Node<Declaration> FormatDeclaration(Node<Declaration> node)
    {
        return node with
        {
            Value = node.Value switch
            {
                Declaration.FunctionDeclaration funcDecl =>
                new Declaration.FunctionDeclaration(
                    Function: new FunctionStruct(
                        Documentation: funcDecl.Function.Documentation,
                        Signature: funcDecl.Function.Signature,
                        Declaration: FormatFunctionImplementation(funcDecl.Function.Declaration)
                    )
                ),

                Declaration.CustomTypeDeclaration typeDecl =>
                typeDecl,

                Declaration.AliasDeclaration aliasDecl =>
                aliasDecl,

                Declaration.InfixDeclaration infixDecl =>
                infixDecl,

                Declaration.PortDeclaration portDecl =>
                portDecl,

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting not implemented for declaration type: {node.Value.GetType().Name}")
            }
        };
    }

    private static Node<FunctionImplementation> FormatFunctionImplementation(Node<FunctionImplementation> implNode)
    {
        return implNode with
        {
            Value = implNode.Value with
            {
                Expression = FormatExpression(implNode.Value.Expression)
            }
        };
    }

    private static Node<Expression> FormatExpression(Node<Expression> node)
    {
        var formattedValue = node.Value switch
        {
            Expression.Application app => FormatApplication(node.Range, app),

            Expression.OperatorApplication opApp when ShouldFormatOperatorApplicationAsMultiline(opApp) =>
                FormatOperatorApplication(node.Range, opApp),

            Expression.OperatorApplication opApp => new Expression.OperatorApplication(
                Operator: opApp.Operator,
                Direction: opApp.Direction,
                Left: FormatExpression(opApp.Left),
                Right: FormatExpression(opApp.Right)
            ),

            Expression.ListExpr listExpr when ShouldFormatListAsMultiline(listExpr) =>
                FormatListExpr(node.Range, listExpr),

            Expression.ListExpr listExpr => new Expression.ListExpr(
                Elements: [.. listExpr.Elements.Select(FormatExpression)]
            ),

            Expression.TupledExpression tuple => new Expression.TupledExpression(
                Elements: [.. tuple.Elements.Select(FormatExpression)]
            ),

            Expression.ParenthesizedExpression paren => new Expression.ParenthesizedExpression(
                Expression: FormatExpression(paren.Expression)
            ),

            Expression.LambdaExpression lambda =>
            FormatLambdaExpression(node.Range, lambda),

            Expression.RecordExpr record =>
            FormatRecordExpr(node.Range, record),

            Expression.RecordUpdateExpression recUpdate =>
            new Expression.RecordUpdateExpression(
                RecordName: recUpdate.RecordName,
                Fields: [.. recUpdate.Fields.Select(f => f with
                {
                    Value = (f.Value.fieldName, FormatExpression(f.Value.valueExpr))
                })]
            ),

            Expression.LetExpression letExpr => new Expression.LetExpression(
                Value: new Expression.LetBlock(
                    Declarations: [.. letExpr.Value.Declarations.Select(decl => decl with
                    {
                        Value = decl.Value switch
                        {
                            Expression.LetDeclaration.LetFunction letFunc => letFunc,

                            Expression.LetDeclaration.LetDestructuring letDestr =>
                            new Expression.LetDeclaration.LetDestructuring(
                                Pattern: letDestr.Pattern,
                                Expression: FormatExpression(letDestr.Expression)
                            ),

                            _ =>
                            throw new System.NotImplementedException(
                                $"Formatting not implemented for let declaration type: {decl.Value.GetType().Name}")
                        }
                    })],
                    Expression: FormatExpression(letExpr.Value.Expression)
                )
            ),

            Expression.IfBlock ifBlock =>
            new Expression.IfBlock(
                Condition: FormatExpression(ifBlock.Condition),
                ThenBlock: FormatExpression(ifBlock.ThenBlock),
                ElseBlock: FormatExpression(ifBlock.ElseBlock)
            ),

            Expression.CaseExpression caseExpr =>
            new Expression.CaseExpression(
                new CaseBlock(
                    Expression: caseExpr.CaseBlock.Expression,
                    Cases: [.. caseExpr.CaseBlock.Cases.Select(c => c with
                    {
                        Expression = FormatExpression(c.Expression)
                    })])),

            Expression.FunctionOrValue funcOrVal =>
            funcOrVal,

            Expression.Literal literal =>
            literal,

            Expression.CharLiteral charLiteral =>
            charLiteral,

            Expression.Integer intLiteral =>
            intLiteral,

            Expression.Hex hexLiteral =>
            hexLiteral,

            Expression.UnitExpr unitExpr =>
            unitExpr,

            Expression.RecordAccessFunction recordAccess =>
            recordAccess,

            Expression.RecordAccess recordAccess =>
            recordAccess,

            Expression.Floatable floatable =>
            floatable,

            _ =>
            throw new System.NotImplementedException(
                $"Formatting not implemented for expression type: {node.Value.GetType().Name}")
        };

        // For multiline lists, create a new node with a multiline range
        if (node.Value is Expression.ListExpr originalList && formattedValue is Expression.ListExpr formattedList
            && ShouldFormatListAsMultiline(originalList))
        {
            // Calculate the multiline range based on the formatted elements
            if (formattedList.Elements.Count > 0)
            {
                var firstElem = formattedList.Elements[0];

                var lastElem = formattedList.Elements[formattedList.Elements.Count - 1];

                var newRange = new Range(
                    Start: new Location(Row: firstElem.Range.Start.Row, Column: 1),
                    End: new Location(Row: lastElem.Range.End.Row, Column: 15)
                );

                return new Node<Expression>(newRange, formattedValue);
            }
        }

        // For lambda expressions, create a new node with a multiline range
        if (formattedValue is Expression.LambdaExpression lambdaExpr)
        {
            // Lambda should span multiple rows (from start to end of body)
            var bodyRange = lambdaExpr.Lambda.Expression.Range;
            var multilineRange = new Range(
                Start: node.Range.Start,
                End: bodyRange.End
            );
            return new Node<Expression>(multilineRange, formattedValue);
        }

        // For applications, create a new node with a multiline range
        if (formattedValue is Expression.Application application && application.Arguments.Count > 1)
        {
            // Application should span multiple rows (from start to end of last argument)
            var lastArg = application.Arguments[application.Arguments.Count - 1];

            var multilineRange = new Range(
                Start: node.Range.Start,
                End: lastArg.Range.End
            );

            return new Node<Expression>(multilineRange, formattedValue);
        }

        // For parenthesized expressions containing multiline content, update the range
        if (formattedValue is Expression.ParenthesizedExpression parenExpr)
        {
            var innerRange = parenExpr.Expression.Range;
            // Check if inner expression spans multiple rows OR is an application (which should always be multiline)
            if (innerRange.Start.Row != innerRange.End.Row || parenExpr.Expression.Value is Expression.Application)
            {
                // Inner expression is multiline, so parenthesized expression should span those rows
                // For applications, ensure they span at least a few rows even if Range suggests otherwise
                var endRow =
                    parenExpr.Expression.Value is Expression.Application
                    ?
                    (innerRange.End.Row > innerRange.Start.Row + 20 ? innerRange.End.Row : innerRange.Start.Row + 20)
                    :
                    innerRange.End.Row;

                var multilineRange = new Range(
                    Start: node.Range.Start,
                    End: new Location(Row: endRow, Column: innerRange.End.Column)
                );

                return new Node<Expression>(multilineRange, formattedValue);
            }
        }

        return node with { Value = formattedValue };
    }

    private static bool ShouldFormatListAsMultiline(Expression.ListExpr list)
    {
        if (list.Elements.Count is 0)
            return false;

        // Make list multiline if it contains records, nested multiline lists, or operator applications
        return list.Elements.Any(elem =>
            elem.Value is Expression.RecordExpr ||
            elem.Value is Expression.OperatorApplication ||
            elem.Value is Expression.ListExpr innerList && ShouldFormatListAsMultiline(innerList)
        );
    }

    private static bool ShouldFormatOperatorApplicationAsMultiline(Expression.OperatorApplication opApp)
    {
        // Format operator application as multiline if the left side is a multiline list or record
        return opApp.Left.Value is Expression.ListExpr leftList && ShouldFormatListAsMultiline(leftList)
            || opApp.Left.Value is Expression.RecordExpr;
    }

    private static Expression FormatOperatorApplication(Range originalRange, Expression.OperatorApplication opApp)
    {
        // Transform operator application to multiline by marking operands with separate row locations
        // This way Avh4Format will recognize it needs special handling

        var formattedLeft = FormatExpression(opApp.Left);
        var formattedRight = FormatExpression(opApp.Right);

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        // Left operand on one row
        var leftWithLocation = formattedLeft with
        {
            Range = new Range(
                Start: new Location(Row: fakeRow, Column: 1),
                End: new Location(Row: fakeRow + 1, Column: 15)
            )
        };

        // Right operand on next row
        var rightWithLocation = formattedRight with
        {
            Range = new Range(
                Start: new Location(Row: fakeRow + 2, Column: 5),
                End: new Location(Row: fakeRow + 2, Column: 15)
            )
        };

        return new Expression.OperatorApplication(
            Operator: opApp.Operator,
            Direction: opApp.Direction,
            Left: leftWithLocation,
            Right: rightWithLocation
        );
    }

    private static Expression FormatListExpr(Range originalRange, Expression.ListExpr list)
    {
        // Format list to multiline if it contains multiline elements

        if (list.Elements.Count is 0)
            return list;

        var formattedElements = list.Elements.Select(FormatExpression).ToList();

        // Check if any element is multiline
        var hasMultilineElement = formattedElements.Any(elem =>
            elem.Range.End.Row > elem.Range.Start.Row ||
            IsExpressionMultiline(elem));

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        if (hasMultilineElement)
        {
            // Multiline: put each element on a different row
            var elementsWithLocation = new List<Node<Expression>>();
            for (var i = 0; i < formattedElements.Count; i++)
            {
                var elem = formattedElements[i];
                var elemRow = fakeRow + i + 1; // +1 to account for opening bracket

                elementsWithLocation.Add(elem with
                {
                    Range = new Range(
                        Start: new Location(Row: elemRow, Column: 5),
                        End: new Location(Row: elemRow + 1, Column: 15)
                    )
                });
            }

            return new Expression.ListExpr(Elements: elementsWithLocation);
        }

        return new Expression.ListExpr(Elements: formattedElements);
    }

    private static Expression FormatRecordExpr(Range originalRange, Expression.RecordExpr record)
    {
        // Transform record to multiline by marking fields with separate row locations
        // This way Avh4Format will format it as multiline

        if (record.Fields.Count is 0)
            return record;

        var formattedFields = record.Fields.Select(f => f with
        {
            Value = (f.Value.fieldName, FormatExpression(f.Value.valueExpr))
        }).ToList();

        // Always format records as multiline by placing fields on different rows
        // Avh4Format will decide the actual formatting based on complexity
        var shouldBeMultiline = true;

        if (!shouldBeMultiline && record.Fields.Count is 1)
        {
            // Check if the single field's value expression is multiline
            var fieldValueExpr = formattedFields[0].Value.valueExpr;
            shouldBeMultiline = IsExpressionMultiline(fieldValueExpr);
        }

        // Use original row as base to maintain relative positioning
        var fakeRow = originalRange.Start.Row;

        var fieldsWithLocation = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

        // Multiline: put each field on a different row
        for (var i = 0; i < formattedFields.Count; i++)
        {
            var field = formattedFields[i];
            var fieldRow = fakeRow + i + 1; // +1 to account for opening brace

            // For single-field records, make the field span multiple rows to signal multiline
            var fieldEndRow = fieldRow;
            if (record.Fields.Count is 1)
            {
                fieldEndRow = fieldRow + 1;
            }

            fieldsWithLocation.Add(field with
            {
                Range = new Range(
                    Start: new Location(Row: fieldRow, Column: 5),
                    End: new Location(Row: fieldEndRow, Column: 15)
                )
            });
        }

        return new Expression.RecordExpr(
            Fields: fieldsWithLocation
        );
    }

    private static Expression FormatLambdaExpression(Range originalRange, Expression.LambdaExpression lambda)
    {
        // Always format lambda expressions as multiline by placing the body on a new row
        // This ensures there's always a line break between the arguments and body

        var bodyExpression = lambda.Lambda.Expression.Value;
        Expression formattedBodyValue;

        // Format different body types appropriately for lambda context
        if (bodyExpression is Expression.ListExpr listExpr && listExpr.Elements.Count > 0)
        {
            // Always format lists inside lambda bodies as multiline (forced)
            formattedBodyValue = FormatListExprAsMultiline(lambda.Lambda.Expression.Range, listExpr);
        }
        else if (bodyExpression is Expression.Application app && app.Arguments.Count > 0)
        {
            // Always format applications inside lambda bodies as multiline (forced)
            // And ensure any list arguments are also multiline
            formattedBodyValue = FormatApplicationForLambdaBody(lambda.Lambda.Expression.Range, app);
        }
        else
        {
            formattedBodyValue = FormatExpression(lambda.Lambda.Expression).Value;
        }

        // Use original row as base - ensure the body starts on a different row than the lambda
        // to trigger multiline detection in Avh4Format
        var fakeRow = originalRange.Start.Row;

        // Place the body on a row significantly later to signal multiline formatting
        // The body should start at row+10 to be clearly detected as multiline by Avh4Format
        // (Avh4Format checks if originalExprStartRow > currentContext.CurrentRow at line 2290)
        var bodyWithLocation = new Node<Expression>(
            Range: new Range(
                Start: new Location(Row: fakeRow + 10, Column: 1),
                End: new Location(Row: fakeRow + 20, Column: 1)
            ),
            Value: formattedBodyValue
        );

        var formattedLambda = new LambdaStruct(
            Arguments: lambda.Lambda.Arguments,
            Expression: bodyWithLocation
        );

        return new Expression.LambdaExpression(Lambda: formattedLambda);
    }

    private static Expression FormatApplicationForLambdaBody(Range originalRange, Expression.Application app)
    {
        // Format application for lambda body context - ensure list arguments are multiline

        if (app.Arguments.Count is 0)
            return app;

        // Format each argument, forcing lists to be multiline
        var formattedArgs = app.Arguments.Select(arg =>
        {
            if (arg.Value is Expression.ListExpr listExpr && listExpr.Elements.Count > 0)
            {
                // Force list to be multiline
                var formattedList = FormatListExprAsMultiline(arg.Range, listExpr);
                return arg with { Value = formattedList };
            }
            else
            {
                return FormatExpression(arg);
            }
        }).ToList();

        // Mark as multiline by giving different rows to each argument
        var fakeRow = originalRange.Start.Row;

        var argsWithLocation = formattedArgs.Select((arg, i) =>
        {
            var argRow = fakeRow + i;
            return arg with
            {
                Range = new Range(
                    Start: new Location(Row: argRow, Column: i is 0 ? 1 : 5),
                    End: new Location(Row: argRow, Column: 15)
                )
            };
        }).ToList();

        return new Expression.Application(
            Arguments: argsWithLocation
        );
    }

    private static Expression FormatListExprAsMultiline(Range originalRange, Expression.ListExpr list)
    {
        // Format list as multiline unconditionally (for lambda bodies)

        if (list.Elements.Count is 0)
            return list;

        var formattedElements = list.Elements.Select(FormatExpression).ToList();

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        // Always multiline: put each element on a different row
        var elementsWithLocation = new List<Node<Expression>>();
        for (var i = 0; i < formattedElements.Count; i++)
        {
            var elem = formattedElements[i];
            var elemRow = fakeRow + i + 1; // +1 to account for opening bracket

            elementsWithLocation.Add(elem with
            {
                Range = new Range(
                    Start: new Location(Row: elemRow, Column: 5),
                    End: new Location(Row: elemRow + 1, Column: 15)
                )
            });
        }

        return new Expression.ListExpr(Elements: elementsWithLocation);
    }

    private static bool IsExpressionMultiline(Node<Expression> exprNode)
    {
        // Check if expression spans multiple rows or contains multiline structures
        if (exprNode.Range.End.Row > exprNode.Range.Start.Row)
            return true;

        return exprNode.Value switch
        {
            Expression.RecordExpr record =>
            record.Fields.Count > 0, // Records are considered multiline if they have fields

            Expression.ListExpr list =>
            list.Elements.Count > 1 ||
                (list.Elements.Count is 1 && IsExpressionMultiline(list.Elements[0])),

            Expression.Application app => app.Arguments.Count > 1,
            _ => false
        };
    }

    private static Expression FormatApplication(Range originalRange, Expression.Application app)
    {
        // Transform application to multiline by marking it with separate row locations
        // This way Avh4Format will format it as multiline

        if (app.Arguments.Count is 0)
            return app;

        // First argument is the function, rest are arguments
        var formattedArgs = app.Arguments.Select(FormatExpression).ToList();

        // Mark as multiline by giving different rows to each argument
        // Use original row as base to maintain relative positioning
        var fakeRow = originalRange.Start.Row;

        var argsWithLocation = formattedArgs.Select((arg, i) =>
        {
            var argRow = fakeRow + (i * 10); // Use larger spacing to ensure distinct rows
            // Set columns to create proper indentation signal for Avh4Format
            // First arg at a base position, subsequent args indented
            return arg with
            {
                Range = new Range(
                    Start: new Location(Row: argRow, Column: i is 0 ? 1 : 5),
                    End: new Location(Row: argRow, Column: 15)
                )
            };
        }).ToList();

        return new Expression.Application(
            Arguments: argsWithLocation
        );
    }
}
