using System.Collections.Generic;
using System.Linq;

using Location = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location;
using Range = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Range;

namespace Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

/// <summary>
/// Format for Elm syntax for use in snapshot tests.
/// Some of the tested implementations generate new code in form of syntax nodes,
/// and we want format it a bit to make it easier to read and compare in snapshot tests.
/// The format applied here is mainly adding line breaks in function application expressions and
/// some list expressions depending on the syntactical complexity of contained items.
/// </summary>
public class SnapshotTestFormat
{
    /// <summary>
    /// Format a concretized File for snapshot testing.
    /// </summary>
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
            Comments: file.Comments,
            IncompleteDeclarations: file.IncompleteDeclarations
        );
    }

    private static Stil4mElmSyntax7.Node<Declaration> FormatDeclaration(Stil4mElmSyntax7.Node<Declaration> node)
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

    private static Stil4mElmSyntax7.Node<FunctionImplementation> FormatFunctionImplementation(
        Stil4mElmSyntax7.Node<FunctionImplementation> implNode)
    {
        return implNode with
        {
            Value = implNode.Value with
            {
                Expression = FormatExpression(implNode.Value.Expression)
            }
        };
    }

    private static Stil4mElmSyntax7.Node<Expression> FormatExpression(Stil4mElmSyntax7.Node<Expression> node)
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
                Elements: MapSeparatedList(listExpr.Elements, FormatExpression)
            ),

            Expression.TupledExpression tuple => new Expression.TupledExpression(
                Elements: MapSeparatedList(tuple.Elements, FormatExpression)
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
                PipeLocation: recUpdate.PipeLocation,
                Fields: MapSeparatedList(recUpdate.Fields, f =>
                    new RecordExprField(f.FieldName, f.EqualsLocation, FormatExpression(f.ValueExpr)))
            ),

            Expression.LetExpression letExpr => new Expression.LetExpression(
                Value: new Expression.LetBlock(
                    LetTokenLocation: letExpr.Value.LetTokenLocation,
                    Declarations: [.. letExpr.Value.Declarations.Select(decl => decl with
                    {
                        Value = decl.Value switch
                        {
                            Expression.LetDeclaration.LetFunction letFunc =>
                            new Expression.LetDeclaration.LetFunction(
                                Function: letFunc.Function with
                                {
                                    Declaration = letFunc.Function.Declaration with
                                    {
                                        Value = letFunc.Function.Declaration.Value with
                                        {
                                            Expression = FormatExpression(letFunc.Function.Declaration.Value.Expression)
                                        }
                                    }
                                }
                            ),

                            Expression.LetDeclaration.LetDestructuring letDestr =>
                            new Expression.LetDeclaration.LetDestructuring(
                                Pattern: letDestr.Pattern,
                                EqualsTokenLocation: letDestr.EqualsTokenLocation,
                                Expression: FormatExpression(letDestr.Expression)
                            ),

                            _ =>
                            throw new System.NotImplementedException(
                                $"Formatting not implemented for let declaration type: {decl.Value.GetType().Name}")
                        }
                    })],
                    InTokenLocation: letExpr.Value.InTokenLocation,
                    Expression: FormatExpression(letExpr.Value.Expression)
                )
            ),

            Expression.IfBlock ifBlock =>
            new Expression.IfBlock(
                IfTokenLocation: ifBlock.IfTokenLocation,
                Condition: FormatExpression(ifBlock.Condition),
                ThenTokenLocation: ifBlock.ThenTokenLocation,
                ThenBlock: FormatExpression(ifBlock.ThenBlock),
                ElseTokenLocation: ifBlock.ElseTokenLocation,
                ElseBlock: FormatExpression(ifBlock.ElseBlock)
            ),

            Expression.CaseExpression caseExpr =>
            new Expression.CaseExpression(
                new CaseBlock(
                    CaseTokenLocation: caseExpr.CaseBlock.CaseTokenLocation,
                    Expression: caseExpr.CaseBlock.Expression,
                    OfTokenLocation: caseExpr.CaseBlock.OfTokenLocation,
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

            Expression.Negation negation =>
            new Expression.Negation(FormatExpression(negation.Expression)),

            Expression.PrefixOperator prefixOp =>
            prefixOp,

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
                var lastElem = formattedList.Elements[^1];

                var newRange = new Range(
                    Start: new Location(Row: firstElem.Range.Start.Row, Column: 1),
                    End: new Location(Row: lastElem.Range.End.Row, Column: 15)
                );

                return new Stil4mElmSyntax7.Node<Expression>(newRange, formattedValue);
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
            return new Stil4mElmSyntax7.Node<Expression>(multilineRange, formattedValue);
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

            return new Stil4mElmSyntax7.Node<Expression>(multilineRange, formattedValue);
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

                return new Stil4mElmSyntax7.Node<Expression>(multilineRange, formattedValue);
            }
        }

        // For record expressions with fields, create a multiline range to signal that the record 
        // should be formatted with closing brace on a new line. This is needed since we removed
        // the CloseBraceLocation property and now rely on the containing node's range.
        // NOTE: The fields' value expressions already have their own ranges set in FormatRecordExpr,
        // so this only affects the detection of whether the RECORD itself is multiline.
        if (formattedValue is Expression.RecordExpr recordExpr && recordExpr.Fields.Count > 0)
        {
            var fields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(recordExpr.Fields);
            var lastField = fields[^1];
            // Use the last field's row + 1 for closing brace on next line
            var lastFieldRow = lastField.FieldName.Range.Start.Row;
            var endRow = lastFieldRow + 1;

            var multilineRange = new Range(
                Start: node.Range.Start,
                End: new Location(Row: endRow, Column: 5)
            );

            return new Stil4mElmSyntax7.Node<Expression>(multilineRange, formattedValue);
        }

        return node with { Value = formattedValue };
    }

    private static bool ShouldFormatListAsMultiline(Expression.ListExpr list)
    {
        if (list.Elements.Count is 0)
            return false;

        // Make list multiline if it contains records, nested multiline lists, operator applications,
        // or any applications (to make function calls readable in snapshots)
        return list.Elements.Nodes.Any(elem =>
            elem.Value is Expression.RecordExpr ||
            elem.Value is Expression.OperatorApplication ||
            elem.Value is Expression.Application ||
            elem.Value is Expression.ListExpr innerList && ShouldFormatListAsMultiline(innerList)
        );
    }

    private static bool ShouldFormatOperatorApplicationAsMultiline(Expression.OperatorApplication opApp)
    {
        // Format operator application as multiline if the left side is a multiline list or record
        return opApp.Left.Value is Expression.ListExpr leftList && ShouldFormatListAsMultiline(leftList)
            || opApp.Left.Value is Expression.RecordExpr;
    }

    private static bool ShouldFormatApplicationAsMultiline(Expression.Application app)
    {
        // Application should be multiline if any argument is a complex (multiline) list or record
        // or if any argument is itself an application that should be multiline
        return app.Arguments.Skip(1).Any(arg =>
            arg.Value is Expression.RecordExpr ||
            arg.Value is Expression.ListExpr listExpr && ShouldFormatListAsMultiline(listExpr) ||
            arg.Value is Expression.Application innerApp && ShouldFormatApplicationAsMultiline(innerApp)
        );
    }

    private static Expression FormatOperatorApplication(Range originalRange, Expression.OperatorApplication opApp)
    {
        // Transform operator application to multiline by marking operands with separate row locations

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

        // Operator on the next row (multiline)
        var operatorWithLocation = opApp.Operator with
        {
            Range = new Range(
                Start: new Location(Row: fakeRow + 2, Column: 1),
                End: new Location(Row: fakeRow + 2, Column: 1 + opApp.Operator.Value.Length)
            )
        };

        // Right operand on next row after operator
        var rightWithLocation = formattedRight with
        {
            Range = new Range(
                Start: new Location(Row: fakeRow + 2, Column: 5),
                End: new Location(Row: fakeRow + 2, Column: 15)
            )
        };

        return new Expression.OperatorApplication(
            Operator: operatorWithLocation,
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

        var formattedElements = list.Elements.Nodes.Select(FormatExpression).ToList();

        // Check if any element is multiline
        var hasMultilineElement = formattedElements.Any(elem =>
            elem.Range.End.Row > elem.Range.Start.Row ||
            IsExpressionMultiline(elem));

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        if (hasMultilineElement)
        {
            // Multiline: put each element on a different row
            var elementsWithLocation = new List<Stil4mElmSyntax7.Node<Expression>>();
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

            return new Expression.ListExpr(
                Elements: ToSeparatedList(elementsWithLocation, list.Elements)
            );
        }

        return new Expression.ListExpr(
            Elements: ToSeparatedList(formattedElements, list.Elements)
        );
    }

    private static Expression FormatRecordExpr(Range originalRange, Expression.RecordExpr record)
    {
        // Transform record to multiline by marking fields with separate row locations

        if (record.Fields.Count is 0)
            return record;

        var formattedFields = record.Fields.Nodes.Select(f =>
            (fieldName: f.FieldName, valueExpr: FormatExpression(f.ValueExpr), equalsLocation: f.EqualsLocation)).ToList();

        // Use original row as base to maintain relative positioning
        var fakeRow = originalRange.Start.Row;

        var fieldsWithLocation = new List<RecordExprField>();

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

            // Update the field name location to reflect the new row
            var updatedFieldName = field.fieldName with
            {
                Range = new Range(
                    Start: new Location(Row: fieldRow, Column: 5),
                    End: new Location(Row: fieldRow, Column: 5 + field.fieldName.Value.Length)
                )
            };

            // Check if value is a complex expression that should be on a new line
            // (nested records, multiline lists, applications)
            var valueIsComplex = field.valueExpr.Value is Expression.RecordExpr nestedRecord && nestedRecord.Fields.Count > 0
                || field.valueExpr.Value is Expression.ListExpr nestedList && ShouldFormatListAsMultiline(nestedList)
                || field.valueExpr.Value is Expression.Application;

            // Update the value expression location
            // If value is complex, put it on a new row to signal multiline formatting
            // If the formatted value already has a multiline range (e.g., nested records get
            // multiline ranges in FormatExpression), preserve that multiline indication
            var valueStartRow = valueIsComplex ? fieldRow + 1 : fieldRow;

            // Check if the formatted value already has a multiline range
            var formattedValueIsMultiline = field.valueExpr.Range.End.Row > field.valueExpr.Range.Start.Row;

            // For values that are already multiline (e.g., nested records), preserve the row span
            // For non-multiline values, end row equals start row
            var valueEndRow = formattedValueIsMultiline
                ? valueStartRow + (field.valueExpr.Range.End.Row - field.valueExpr.Range.Start.Row)
                : valueStartRow;

            var updatedValueExpr = field.valueExpr with
            {
                Range = new Range(
                    Start: new Location(Row: valueStartRow, Column: field.valueExpr.Range.Start.Column),
                    End: new Location(Row: valueEndRow, Column: 15)
                )
            };

            fieldsWithLocation.Add(new RecordExprField(updatedFieldName, field.equalsLocation, updatedValueExpr));
        }

        return new Expression.RecordExpr(
            Fields: ToSeparatedListFromRecordExprFields(fieldsWithLocation, record.Fields)
        );
    }

    private static Expression FormatLambdaExpression(Range originalRange, Expression.LambdaExpression lambda)
    {
        // Always format lambda expressions as multiline by placing the body on a new row

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
            formattedBodyValue = FormatApplicationForLambdaBody(lambda.Lambda.Expression.Range, app);
        }
        else
        {
            formattedBodyValue = FormatExpression(lambda.Lambda.Expression).Value;
        }

        // Use a very large row number to ensure the body is always treated as being on a new line
        // during formatting. The Avh4Format checks if body row > current row to decide multiline.
        // Using 10000 ensures this is always true regardless of actual file position.
        var futureRow = 10000;

        // Place the body on a row significantly later to signal multiline formatting
        var bodyWithLocation = new Stil4mElmSyntax7.Node<Expression>(
            Range: new Range(
                Start: new Location(Row: futureRow, Column: 1),
                End: new Location(Row: futureRow + 10, Column: 1)
            ),
            Value: formattedBodyValue
        );

        var formattedLambda = new LambdaStruct(
            BackslashLocation: lambda.Lambda.BackslashLocation,
            Arguments: lambda.Lambda.Arguments,
            ArrowLocation: lambda.Lambda.ArrowLocation,
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
                var formattedList = (Expression.ListExpr)FormatListExprAsMultiline(arg.Range, listExpr);

                // Create a multiline range based on the formatted elements
                var firstElem = formattedList.Elements[0];
                var lastElem = formattedList.Elements[^1];
                var multilineRange = new Range(
                    Start: new Location(Row: firstElem.Range.Start.Row, Column: 1),
                    End: new Location(Row: lastElem.Range.End.Row, Column: 15)
                );

                return new Stil4mElmSyntax7.Node<Expression>(multilineRange, formattedList);
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

            // Preserve multiline ranges for arguments that are already multiline
            var argEndRow = arg.Range.End.Row > arg.Range.Start.Row
                ? argRow + (arg.Range.End.Row - arg.Range.Start.Row)
                : argRow;

            return arg with
            {
                Range = new Range(
                    Start: new Location(Row: argRow, Column: i is 0 ? 1 : 5),
                    End: new Location(Row: argEndRow, Column: 15)
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

        var formattedElements = list.Elements.Nodes.Select(FormatExpression).ToList();

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        // Always multiline: put each element on a different row
        var elementsWithLocation = new List<Stil4mElmSyntax7.Node<Expression>>();
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

        return new Expression.ListExpr(
            Elements: ToSeparatedList(elementsWithLocation, list.Elements)
        );
    }

    private static bool IsExpressionMultiline(Stil4mElmSyntax7.Node<Expression> exprNode)
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

        if (app.Arguments.Count is 0)
            return app;

        // First argument is the function, rest are arguments
        var formattedArgs = app.Arguments.Select(FormatExpression).ToList();

        // Mark as multiline by giving different rows to each argument
        var fakeRow = originalRange.Start.Row;

        var argsWithLocation = formattedArgs.Select((arg, i) =>
        {
            var argRow = fakeRow + (i * 10); // Use larger spacing to ensure distinct rows

            // Preserve multiline ranges for arguments that are already multiline
            // (e.g., lists that should be formatted as multiline)
            var argEndRow = arg.Range.End.Row > arg.Range.Start.Row
                ? argRow + (arg.Range.End.Row - arg.Range.Start.Row)
                : argRow;

            return arg with
            {
                Range = new Range(
                    Start: new Location(Row: argRow, Column: i is 0 ? 1 : 5),
                    End: new Location(Row: argEndRow, Column: 15)
                )
            };
        }).ToList();

        return new Expression.Application(
            Arguments: argsWithLocation
        );
    }

    /// <summary>
    /// Helper to map over a SeparatedSyntaxList.
    /// </summary>
    private static SeparatedSyntaxList<TNode> MapSeparatedList<TNode>(
        SeparatedSyntaxList<TNode> list,
        System.Func<TNode, TNode> mapper)
    {
        return list switch
        {
            SeparatedSyntaxList<TNode>.Empty =>
                list,

            SeparatedSyntaxList<TNode>.NonEmpty nonEmpty =>
                new SeparatedSyntaxList<TNode>.NonEmpty(
                    First: mapper(nonEmpty.First),
                    Rest: [.. nonEmpty.Rest.Select(r => (r.SeparatorLocation, mapper(r.Node)))]),

            _ =>
                throw new System.NotImplementedException(
                    $"Unexpected SeparatedSyntaxList type: {list.GetType().Name}")
        };
    }

    /// <summary>
    /// Convert a list back to SeparatedSyntaxList, preserving separator locations from the original.
    /// </summary>
    private static SeparatedSyntaxList<TNode> ToSeparatedList<TNode>(
        IReadOnlyList<TNode> items,
        SeparatedSyntaxList<TNode> original)
    {
        if (items.Count == 0)
            return new SeparatedSyntaxList<TNode>.Empty();

        if (original is SeparatedSyntaxList<TNode>.NonEmpty originalNonEmpty)
        {
            var rest = new List<(Location SeparatorLocation, TNode Node)>();
            for (var i = 1; i < items.Count; i++)
            {
                var separatorLoc = i - 1 < originalNonEmpty.Rest.Count
                    ? originalNonEmpty.Rest[i - 1].SeparatorLocation
                    : new Location(1, 1);
                rest.Add((separatorLoc, items[i]));
            }
            return new SeparatedSyntaxList<TNode>.NonEmpty(items[0], rest);
        }

        // Fallback - create with default separator locations
        var restDefault = new List<(Location SeparatorLocation, TNode Node)>();
        for (var i = 1; i < items.Count; i++)
        {
            restDefault.Add((new Location(1, 1), items[i]));
        }
        return new SeparatedSyntaxList<TNode>.NonEmpty(items[0], restDefault);
    }

    /// <summary>
    /// Convert a list of tuples back to SeparatedSyntaxList, preserving separator locations from the original.
    /// </summary>
    private static SeparatedSyntaxList<RecordExprField> ToSeparatedListFromRecordExprFields(
        IReadOnlyList<RecordExprField> items,
        SeparatedSyntaxList<RecordExprField> original)
    {
        if (items.Count == 0)
            return new SeparatedSyntaxList<RecordExprField>.Empty();

        if (original is SeparatedSyntaxList<RecordExprField>.NonEmpty originalNonEmpty)
        {
            var rest = new List<(Location SeparatorLocation, RecordExprField Node)>();
            for (var i = 1; i < items.Count; i++)
            {
                var separatorLoc = i - 1 < originalNonEmpty.Rest.Count
                    ? originalNonEmpty.Rest[i - 1].SeparatorLocation
                    : new Location(1, 1);
                rest.Add((separatorLoc, items[i]));
            }
            return new SeparatedSyntaxList<RecordExprField>.NonEmpty(items[0], rest);
        }

        // Fallback - create with default separator locations
        var restDefault = new List<(Location SeparatorLocation, RecordExprField Node)>();
        for (var i = 1; i < items.Count; i++)
        {
            restDefault.Add((new Location(1, 1), items[i]));
        }
        return new SeparatedSyntaxList<RecordExprField>.NonEmpty(items[0], restDefault);
    }
}
