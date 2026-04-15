using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;
using AbstractSyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmSyntax;

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
        return
            new File(
                ModuleDefinition: file.ModuleDefinition,
                Imports: file.Imports,
                Declarations: [.. file.Declarations.Select(FormatDeclaration)],
                Comments: file.Comments,
                IncompleteDeclarations: file.IncompleteDeclarations);
    }

    private static Node<Declaration> FormatDeclaration(Node<Declaration> node)
    {
        return
            node with
            {
                Value =
                node.Value switch
                {
                    Declaration.FunctionDeclaration funcDecl =>
                    new Declaration.FunctionDeclaration(
                        Function: new FunctionStruct(
                            Documentation: funcDecl.Function.Documentation,
                            Signature: funcDecl.Function.Signature,
                            Declaration: FormatFunctionImplementation(funcDecl.Function.Declaration))),

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

    private static Node<FunctionImplementation> FormatFunctionImplementation(
        Node<FunctionImplementation> implNode)
    {
        return
            implNode with
            {
                Value =
                implNode.Value with
                {
                    Expression = FormatExpression(implNode.Value.Expression)
                }
            };
    }

    private static Node<ExpressionSyntax> FormatExpression(Node<ExpressionSyntax> node)
    {
        var formattedValue =
            node.Value switch
            {
                ExpressionSyntax.Application app =>
                FormatApplication(node.Range, app),

                ExpressionSyntax.OperatorApplication opApp when ShouldFormatOperatorApplicationAsMultiline(opApp) =>
                FormatOperatorApplication(node.Range, opApp),

                ExpressionSyntax.OperatorApplication opApp =>
                new ExpressionSyntax.OperatorApplication(
                    Operator: opApp.Operator,
                    Direction: opApp.Direction,
                    Left: FormatExpression(opApp.Left),
                    Right: FormatExpression(opApp.Right)),

                ExpressionSyntax.ListExpr listExpr when ShouldFormatListAsMultiline(listExpr) =>
                FormatListExpr(node.Range, listExpr),

                ExpressionSyntax.ListExpr listExpr =>
                new ExpressionSyntax.ListExpr(
                    Elements: MapSeparatedList(listExpr.Elements, FormatExpression)),

                ExpressionSyntax.TupledExpression tuple when ShouldFormatTupleAsMultiline(tuple) =>
                FormatTupledExpression(node.Range, tuple),

                ExpressionSyntax.TupledExpression tuple =>
                new ExpressionSyntax.TupledExpression(
                    Elements: MapSeparatedList(tuple.Elements, FormatExpression)),

                ExpressionSyntax.ParenthesizedExpression paren =>
                new ExpressionSyntax.ParenthesizedExpression(
                    Expression: FormatExpression(paren.Expression)),

                ExpressionSyntax.LambdaExpression lambda =>
                FormatLambdaExpression(node.Range, lambda),

                ExpressionSyntax.RecordExpr record =>
                FormatRecordExpr(node.Range, record),

                ExpressionSyntax.RecordUpdateExpression recUpdate =>
                new ExpressionSyntax.RecordUpdateExpression(
                    RecordName: recUpdate.RecordName,
                    PipeLocation: recUpdate.PipeLocation,
                    Fields: MapSeparatedList(
                        recUpdate.Fields,
                        f =>
                        new RecordExprField(f.FieldName, f.EqualsLocation, FormatExpression(f.ValueExpr)))),

                ExpressionSyntax.LetExpression letExpr =>
                new ExpressionSyntax.LetExpression(
                    Value: new ExpressionSyntax.LetBlock(
                        LetTokenLocation: letExpr.Value.LetTokenLocation,
                        Declarations:
                        [
                        .. letExpr.Value.Declarations.Select(
                            decl => decl with
                            {
                                Value =
                                decl.Value switch
                                {
                                    ExpressionSyntax.LetDeclaration.LetFunction letFunc =>
                                    new ExpressionSyntax.LetDeclaration.LetFunction(
                                        Function: letFunc.Function with
                                        {
                                            Declaration =
                                            letFunc.Function.Declaration with
                                            {
                                                Value =
                                                letFunc.Function.Declaration.Value with
                                                {
                                                    Expression =
                                                    FormatExpression(letFunc.Function.Declaration.Value.Expression)
                                                }
                                            }
                                        }),

                                    ExpressionSyntax.LetDeclaration.LetDestructuring letDestr =>
                                    new ExpressionSyntax.LetDeclaration.LetDestructuring(
                                        Pattern: letDestr.Pattern,
                                        EqualsTokenLocation: letDestr.EqualsTokenLocation,
                                        Expression: FormatExpression(letDestr.Expression)),

                                    _ =>
                                    throw new System.NotImplementedException(
                                        $"Formatting not implemented for let declaration type: {decl.Value.GetType().Name}")
                                }
                            })
                        ],
                        InTokenLocation: letExpr.Value.InTokenLocation,
                        Expression: FormatExpression(letExpr.Value.Expression))),

                ExpressionSyntax.IfBlock ifBlock =>
                new ExpressionSyntax.IfBlock(
                    IfTokenLocation: ifBlock.IfTokenLocation,
                    Condition: FormatExpression(ifBlock.Condition),
                    ThenTokenLocation: ifBlock.ThenTokenLocation,
                    ThenBlock: FormatExpression(ifBlock.ThenBlock),
                    ElseTokenLocation: ifBlock.ElseTokenLocation,
                    ElseBlock: FormatExpression(ifBlock.ElseBlock)),

                ExpressionSyntax.CaseExpression caseExpr =>
                new ExpressionSyntax.CaseExpression(
                    new CaseBlock(
                        CaseTokenLocation: caseExpr.CaseBlock.CaseTokenLocation,
                        Expression: caseExpr.CaseBlock.Expression,
                        OfTokenLocation: caseExpr.CaseBlock.OfTokenLocation,
                        Cases:
                        [
                        .. caseExpr.CaseBlock.Cases.Select(
                            c => c with
                            {
                                Expression = FormatExpression(c.Expression)
                            })
                        ])),

                ExpressionSyntax.FunctionOrValue funcOrVal =>
                funcOrVal,

                ExpressionSyntax.Literal literal =>
                literal,

                ExpressionSyntax.CharLiteral charLiteral =>
                charLiteral,

                ExpressionSyntax.Integer intLiteral =>
                intLiteral,

                ExpressionSyntax.UnitExpr unitExpr =>
                unitExpr,

                ExpressionSyntax.RecordAccessFunction recordAccess =>
                recordAccess,

                ExpressionSyntax.RecordAccess recordAccess =>
                recordAccess,

                ExpressionSyntax.Floatable floatable =>
                floatable,

                ExpressionSyntax.Negation negation =>
                new ExpressionSyntax.Negation(FormatExpression(negation.Expression)),

                ExpressionSyntax.PrefixOperator prefixOp =>
                prefixOp,

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting not implemented for expression type: {node.Value.GetType().Name}")
            };

        // For multiline lists, create a new node with a multiline range
        if (node.Value is ExpressionSyntax.ListExpr originalList &&
            formattedValue is ExpressionSyntax.ListExpr formattedList
            && ShouldFormatListAsMultiline(originalList))
        {
            // Calculate the multiline range based on the formatted elements
            if (formattedList.Elements.Count > 0)
            {
                var firstElem = formattedList.Elements[0];
                var lastElem = formattedList.Elements[^1];

                var newRange =
                    new Range(
                        Start: new Location(Row: firstElem.Range.Start.Row, Column: 1),
                        End: new Location(Row: lastElem.Range.End.Row, Column: 15));

                return new Node<ExpressionSyntax>(newRange, formattedValue);
            }
        }

        // For multiline tuples, create a new node with a multiline range
        if (node.Value is ExpressionSyntax.TupledExpression originalTuple &&
            formattedValue is ExpressionSyntax.TupledExpression formattedTuple
            && ShouldFormatTupleAsMultiline(originalTuple))
        {
            if (formattedTuple.Elements.Count > 0)
            {
                var firstElem = formattedTuple.Elements[0];
                var lastElem = formattedTuple.Elements[^1];

                var newRange =
                    new Range(
                        Start: new Location(Row: firstElem.Range.Start.Row, Column: 1),
                        End: new Location(Row: lastElem.Range.End.Row + 1, Column: 15));

                return new Node<ExpressionSyntax>(newRange, formattedValue);
            }
        }

        // For lambda expressions, create a new node with a multiline range
        if (formattedValue is ExpressionSyntax.LambdaExpression lambdaExpr)
        {
            // Lambda should span multiple rows (from start to end of body)
            var bodyRange = lambdaExpr.Lambda.Expression.Range;

            var multilineRange =
                new Range(
                    Start: node.Range.Start,
                    End: bodyRange.End);

            return new Node<ExpressionSyntax>(multilineRange, formattedValue);
        }

        // For applications, create a new node with a multiline range
        if (formattedValue is ExpressionSyntax.Application application && application.Arguments.Count > 0)
        {
            // Application should span multiple rows (from start to end of last argument)
            var lastArg = application.Arguments[application.Arguments.Count - 1];

            var multilineRange =
                new Range(
                    Start: node.Range.Start,
                    End: lastArg.Range.End);

            return new Node<ExpressionSyntax>(multilineRange, formattedValue);
        }

        // For parenthesized expressions containing multiline content, update the range
        if (formattedValue is ExpressionSyntax.ParenthesizedExpression parenExpr)
        {
            var innerRange = parenExpr.Expression.Range;
            // Check if inner expression spans multiple rows OR is an application (which should always be multiline)
            if (innerRange.Start.Row != innerRange.End.Row ||
                parenExpr.Expression.Value is ExpressionSyntax.Application)
            {
                // Inner expression is multiline, so parenthesized expression should span those rows
                // For applications, ensure they span at least a few rows even if Range suggests otherwise
                var endRow =
                    parenExpr.Expression.Value is ExpressionSyntax.Application
                    ?
                    (innerRange.End.Row > innerRange.Start.Row + 20 ? innerRange.End.Row : innerRange.Start.Row + 20)
                    :
                    innerRange.End.Row;

                var multilineRange =
                    new Range(
                        Start: node.Range.Start,
                        End: new Location(Row: endRow, Column: innerRange.End.Column));

                return new Node<ExpressionSyntax>(multilineRange, formattedValue);
            }
        }

        // For record expressions with fields, create a multiline range to signal that the record 
        // should be formatted with closing brace on a new line. This is needed since we removed
        // the CloseBraceLocation property and now rely on the containing node's range.
        // NOTE: The fields' value expressions already have their own ranges set in FormatRecordExpr,
        // so this only affects the detection of whether the RECORD itself is multiline.
        if (formattedValue is ExpressionSyntax.RecordExpr recordExpr && recordExpr.Fields.Count > 0)
        {
            var fields = AbstractSyntaxTypes.FromFullSyntaxModel.ToList(recordExpr.Fields);
            var lastField = fields[^1];
            // Use the last field's row + 1 for closing brace on next line
            var lastFieldRow = lastField.FieldName.Range.Start.Row;
            var endRow = lastFieldRow + 1;

            var multilineRange =
                new Range(
                    Start: node.Range.Start,
                    End: new Location(Row: endRow, Column: 5));

            return new Node<ExpressionSyntax>(multilineRange, formattedValue);
        }

        return node with { Value = formattedValue };
    }

    private static bool ShouldFormatListAsMultiline(ExpressionSyntax.ListExpr list)
    {
        if (list.Elements.Count is 0)
            return false;

        // Make list multiline if it contains records, nested multiline lists, operator applications,
        // or any applications (to make function calls readable in snapshots)

        for (var i = 0; i < list.Elements.Count; i++)
        {
            var elem = list.Elements[i];

            if (elem.Value is ExpressionSyntax.RecordExpr)
                return true;

            if (elem.Value is ExpressionSyntax.ListExpr innerList && ShouldFormatListAsMultiline(innerList))
                return true;

            if (elem.Value is ExpressionSyntax.OperatorApplication)
                return true;

            if (elem.Value is ExpressionSyntax.Application)
                return true;
        }

        return false;
    }

    private static bool ShouldFormatOperatorApplicationAsMultiline(ExpressionSyntax.OperatorApplication opApp)
    {
        // Format operator application as multiline if the left side is a multiline list or record
        return
            opApp.Left.Value is ExpressionSyntax.ListExpr leftList && ShouldFormatListAsMultiline(leftList)
            || opApp.Left.Value is ExpressionSyntax.RecordExpr;
    }

    private static bool ShouldFormatTupleAsMultiline(ExpressionSyntax.TupledExpression tuple)
    {
        // Format tuple as multiline if any element is an application, record,
        // or a multiline list - same criteria used for lists.

        for (var i = 0; i < tuple.Elements.Count; i++)
        {
            var elem = tuple.Elements[i];

            if (elem.Value is ExpressionSyntax.Application)
                return true;

            if (elem.Value is ExpressionSyntax.RecordExpr)
                return true;

            if (elem.Value is ExpressionSyntax.ListExpr innerList && ShouldFormatListAsMultiline(innerList))
                return true;

            if (elem.Value is ExpressionSyntax.OperatorApplication)
                return true;
        }

        return false;
    }

    private static bool ShouldFormatApplicationAsMultiline(ExpressionSyntax.Application app)
    {
        // Application should be multiline if any argument is a complex (multiline) list or record
        // or if any argument is itself an application that should be multiline
        return
            app.Arguments.Any(
                arg =>
                arg.Value is ExpressionSyntax.RecordExpr ||
                arg.Value is ExpressionSyntax.ListExpr listExpr && ShouldFormatListAsMultiline(listExpr) ||
                arg.Value is ExpressionSyntax.Application innerApp && ShouldFormatApplicationAsMultiline(innerApp));
    }

    private static ExpressionSyntax FormatOperatorApplication(
        Range originalRange,
        ExpressionSyntax.OperatorApplication opApp)
    {
        // Transform operator application to multiline by marking operands with separate row locations

        var formattedLeft = FormatExpression(opApp.Left);
        var formattedRight = FormatExpression(opApp.Right);

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        // Left operand on one row
        var leftWithLocation =
            formattedLeft with
            {
                Range =
                new Range(
                    Start: new Location(Row: fakeRow, Column: 1),
                    End: new Location(Row: fakeRow + 1, Column: 15))
            };

        // Operator on the next row (multiline)
        var operatorWithLocation =
            opApp.Operator with
            {
                Range =
                new Range(
                    Start: new Location(Row: fakeRow + 2, Column: 1),
                    End: new Location(Row: fakeRow + 2, Column: 1 + opApp.Operator.Value.Length))
            };

        // Right operand on next row after operator
        var rightWithLocation =
            formattedRight with
            {
                Range =
                new Range(
                    Start: new Location(Row: fakeRow + 2, Column: 5),
                    End: new Location(Row: fakeRow + 2, Column: 15))
            };

        return
            new ExpressionSyntax.OperatorApplication(
                Operator: operatorWithLocation,
                Direction: opApp.Direction,
                Left: leftWithLocation,
                Right: rightWithLocation);
    }

    private static ExpressionSyntax FormatListExpr(Range originalRange, ExpressionSyntax.ListExpr list)
    {
        // Format list to multiline if it contains multiline elements

        if (list.Elements.Count is 0)
            return list;

        var formattedElements = list.Elements.Nodes.Select(FormatExpression).ToList();

        // Check if any element is multiline
        var hasMultilineElement =
            formattedElements.Any(
                elem =>
                elem.Range.End.Row > elem.Range.Start.Row ||
                IsExpressionMultiline(elem));

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        if (hasMultilineElement)
        {
            // Multiline: put each element on a different row
            var itemsWithLocation = new List<Node<ExpressionSyntax>>();

            for (var i = 0; i < formattedElements.Count; i++)
            {
                var elem = formattedElements[i];
                var elemRow = fakeRow + i + 1; // +1 to account for opening bracket

                itemsWithLocation.Add(
                    elem with
                    {
                        Range =
                        new Range(
                            Start: new Location(Row: elemRow, Column: 5),
                            End: new Location(Row: elemRow + 1, Column: 15))
                    });
            }

            return
                new ExpressionSyntax.ListExpr(
                    Elements: ToSeparatedList(itemsWithLocation, list.Elements));
        }

        return
            new ExpressionSyntax.ListExpr(
                Elements: ToSeparatedList(formattedElements, list.Elements));
    }

    private static ExpressionSyntax FormatTupledExpression(
        Range originalRange,
        ExpressionSyntax.TupledExpression tuple)
    {
        // Format tuple to multiline when it contains multiline elements (applications, records, etc.)

        if (tuple.Elements.Count is 0)
            return tuple;

        var formattedElements = tuple.Elements.Nodes.Select(FormatExpression).ToList();

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        // Put each element on a different row
        var itemsWithLocation = new List<Node<ExpressionSyntax>>();

        for (var i = 0; i < formattedElements.Count; i++)
        {
            var elem = formattedElements[i];
            var elemRow = fakeRow + i + 1; // +1 to account for opening paren

            // Preserve multiline ranges for elements that are already multiline
            var elemEndRow =
                elem.Range.End.Row > elem.Range.Start.Row
                ?
                elemRow + (elem.Range.End.Row - elem.Range.Start.Row)
                :
                elemRow;

            itemsWithLocation.Add(
                elem with
                {
                    Range =
                    new Range(
                        Start: new Location(Row: elemRow, Column: 5),
                        End: new Location(Row: elemEndRow + 1, Column: 15))
                });
        }

        return
            new ExpressionSyntax.TupledExpression(
                Elements: ToSeparatedList(itemsWithLocation, tuple.Elements));
    }

    private static ExpressionSyntax FormatRecordExpr(Range originalRange, ExpressionSyntax.RecordExpr record)
    {
        // Transform record to multiline by marking fields with separate row locations

        if (record.Fields.Count is 0)
            return record;

        var formattedFields =
            record.Fields.Nodes.Select(
                f =>
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
            var updatedFieldName =
                field.fieldName with
                {
                    Range =
                    new Range(
                        Start: new Location(Row: fieldRow, Column: 5),
                        End: new Location(Row: fieldRow, Column: 5 + field.fieldName.Value.Length))
                };

            // Check if value is a complex expression that should be on a new line
            // (nested records, multiline lists, applications)
            var valueIsComplex =
                field.valueExpr.Value is ExpressionSyntax.RecordExpr nestedRecord && nestedRecord.Fields.Count > 0
                || field.valueExpr.Value is ExpressionSyntax.ListExpr nestedList &&
                ShouldFormatListAsMultiline(nestedList)
                || field.valueExpr.Value is ExpressionSyntax.Application;

            // Update the value expression location
            // If value is complex, put it on a new row to signal multiline formatting
            // If the formatted value already has a multiline range (e.g., nested records get
            // multiline ranges in FormatExpression), preserve that multiline indication
            var valueStartRow = valueIsComplex ? fieldRow + 1 : fieldRow;

            // Check if the formatted value already has a multiline range
            var formattedValueIsMultiline = field.valueExpr.Range.End.Row > field.valueExpr.Range.Start.Row;

            // For values that are already multiline (e.g., nested records), preserve the row span
            // For non-multiline values, end row equals start row
            var valueEndRow =
                formattedValueIsMultiline
                ?
                valueStartRow + (field.valueExpr.Range.End.Row - field.valueExpr.Range.Start.Row)
                :
                valueStartRow;

            var updatedValueExpr =
                field.valueExpr with
                {
                    Range =
                    new Range(
                        Start: new Location(Row: valueStartRow, Column: field.valueExpr.Range.Start.Column),
                        End: new Location(Row: valueEndRow, Column: 15))
                };

            fieldsWithLocation.Add(new RecordExprField(updatedFieldName, field.equalsLocation, updatedValueExpr));
        }

        return
            new ExpressionSyntax.RecordExpr(
                Fields: ToSeparatedListFromRecordExprFields(fieldsWithLocation, record.Fields));
    }

    private static ExpressionSyntax FormatLambdaExpression(
        Range originalRange,
        ExpressionSyntax.LambdaExpression lambda)
    {
        // Always format lambda expressions as multiline by placing the body on a new row

        var bodyExpression = lambda.Lambda.Expression.Value;
        ExpressionSyntax formattedBodyValue;

        // Format different body types appropriately for lambda context
        if (bodyExpression is ExpressionSyntax.ListExpr listExpr && listExpr.Elements.Count > 0)
        {
            // Always format lists inside lambda bodies as multiline (forced)
            formattedBodyValue = FormatListExprAsMultiline(lambda.Lambda.Expression.Range, listExpr);
        }
        else if (bodyExpression is ExpressionSyntax.Application app && app.Arguments.Count > 0)
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
        var bodyWithLocation =
            new Node<ExpressionSyntax>(
                Range: new Range(
                    Start: new Location(Row: futureRow, Column: 1),
                    End: new Location(Row: futureRow + 10, Column: 1)),
                Value: formattedBodyValue);

        var formattedLambda =
            new LambdaStruct(
                BackslashLocation: lambda.Lambda.BackslashLocation,
                Arguments: lambda.Lambda.Arguments,
                ArrowLocation: lambda.Lambda.ArrowLocation,
                Expression: bodyWithLocation);

        return new ExpressionSyntax.LambdaExpression(Lambda: formattedLambda);
    }

    private static ExpressionSyntax FormatApplicationForLambdaBody(
        Range originalRange,
        ExpressionSyntax.Application app)
    {
        // Format application for lambda body context - ensure list arguments are multiline

        var formattedFunction = FormatExpression(app.Function);

        // Format each argument, forcing lists to be multiline
        var formattedArgs =
            app.Arguments.Select(
                arg =>
                {
                    if (arg.Value is ExpressionSyntax.ListExpr listExpr && listExpr.Elements.Count > 0)
                    {
                        // Force list to be multiline
                        var formattedList = (ExpressionSyntax.ListExpr)FormatListExprAsMultiline(arg.Range, listExpr);

                        // Create a multiline range based on the formatted elements
                        var firstElem = formattedList.Elements[0];
                        var lastElem = formattedList.Elements[^1];

                        var multilineRange =
                            new Range(
                                Start: new Location(Row: firstElem.Range.Start.Row, Column: 1),
                                End: new Location(Row: lastElem.Range.End.Row, Column: 15));

                        return new Node<ExpressionSyntax>(multilineRange, formattedList);
                    }
                    else
                    {
                        return FormatExpression(arg);
                    }
                }).ToList();

        // Mark as multiline by giving different rows to each argument
        var fakeRow = originalRange.Start.Row;

        var functionWithLocation =
            formattedFunction with
            {
                Range =
                new Range(
                    Start: new Location(Row: fakeRow, Column: 1),
                    End: new Location(Row: fakeRow, Column: 15))
            };

        var argsWithLocation =
            formattedArgs.Select(
                (arg, i) =>
                {
                    var argRow = fakeRow + i + 1;

                    // Preserve multiline ranges for arguments that are already multiline
                    var argEndRow =
                        arg.Range.End.Row > arg.Range.Start.Row
                        ?
                        argRow + (arg.Range.End.Row - arg.Range.Start.Row)
                        :
                        argRow;

                    return
                        arg with
                        {
                            Range =
                            new Range(
                                Start: new Location(Row: argRow, Column: 5),
                                End: new Location(Row: argEndRow, Column: 15))
                        };
                }).ToList();

        return
            new ExpressionSyntax.Application(
                Function: functionWithLocation,
                Arguments: argsWithLocation);
    }

    private static ExpressionSyntax FormatListExprAsMultiline(Range originalRange, ExpressionSyntax.ListExpr list)
    {
        // Format list as multiline unconditionally (for lambda bodies)

        if (list.Elements.Count is 0)
            return list;

        var formattedElements = list.Elements.Nodes.Select(FormatExpression).ToList();

        // Use original row as base
        var fakeRow = originalRange.Start.Row;

        // Always multiline: put each element on a different row
        var itemsWithLocation = new List<Node<ExpressionSyntax>>();

        for (var i = 0; i < formattedElements.Count; i++)
        {
            var elem = formattedElements[i];
            var elemRow = fakeRow + i + 1; // +1 to account for opening bracket

            itemsWithLocation.Add(
                elem with
                {
                    Range =
                    new Range(
                        Start: new Location(Row: elemRow, Column: 5),
                        End: new Location(Row: elemRow + 1, Column: 15))
                });
        }

        return
            new ExpressionSyntax.ListExpr(
                Elements: ToSeparatedList(itemsWithLocation, list.Elements));
    }

    private static bool IsExpressionMultiline(Node<ExpressionSyntax> exprNode)
    {
        // Check if expression spans multiple rows or contains multiline structures
        if (exprNode.Range.End.Row > exprNode.Range.Start.Row)
            return true;

        return exprNode.Value switch
        {
            ExpressionSyntax.RecordExpr record =>
            record.Fields.Count > 0, // Records are considered multiline if they have fields

            ExpressionSyntax.ListExpr list =>
            list.Elements.Count > 1 ||
            (list.Elements.Count is 1 && IsExpressionMultiline(list.Elements[0])),

            ExpressionSyntax.Application app => app.Arguments.Count > 0,

            _ =>
            false
        };
    }

    private static ExpressionSyntax FormatApplication(Range originalRange, ExpressionSyntax.Application app)
    {
        // Transform application to multiline by marking it with separate row locations

        var formattedFunction = FormatExpression(app.Function);
        var formattedArgs = app.Arguments.Select(FormatExpression).ToList();

        // Mark as multiline by giving different rows to each argument
        var fakeRow = originalRange.Start.Row;

        var functionWithLocation =
            formattedFunction with
            {
                Range =
                new Range(
                    Start: new Location(Row: fakeRow, Column: 1),
                    End: new Location(Row: fakeRow, Column: 15))
            };

        var argsWithLocation =
            formattedArgs.Select(
                (arg, i) =>
                {
                    var argRow = fakeRow + ((i + 1) * 10); // Use larger spacing to ensure distinct rows

                    // Preserve multiline ranges for arguments that are already multiline
                    // (e.g., lists that should be formatted as multiline)
                    var argEndRow =
                        arg.Range.End.Row > arg.Range.Start.Row
                        ?
                        argRow + (arg.Range.End.Row - arg.Range.Start.Row)
                        :
                        argRow;

                    return
                        arg with
                        {
                            Range =
                            new Range(
                                Start: new Location(Row: argRow, Column: 5),
                                End: new Location(Row: argEndRow, Column: 15))
                        };
                }).ToList();

        return
            new ExpressionSyntax.Application(
                Function: functionWithLocation,
                Arguments: argsWithLocation);
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
                var separatorLoc =
                    i - 1 < originalNonEmpty.Rest.Count
                    ?
                    originalNonEmpty.Rest[i - 1].SeparatorLocation
                    :
                    new Location(1, 1);

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
                var separatorLoc =
                    i - 1 < originalNonEmpty.Rest.Count
                    ?
                    originalNonEmpty.Rest[i - 1].SeparatorLocation
                    :
                    new Location(1, 1);

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

    /// <summary>
    /// Renders a single declaration with a fully qualified name.
    /// The declaration name (and signature name if present) is overridden
    /// with the full qualified name from the dictionary entry key.
    /// The result has no leading or trailing whitespace.
    /// Relative row differences within the declaration are preserved,
    /// but any absolute offset is ignored (the rendering starts at row 1).
    /// </summary>
    /// <param name="qualifiedName">
    /// The fully qualified name to use for the declaration (e.g. "MyApp.declName").
    /// </param>
    /// <param name="declaration">
    /// The Elm declaration syntax model to render.
    /// </param>
    /// <returns>
    /// The rendered declaration text without leading or trailing whitespace.
    /// </returns>
    public static string RenderQualifiedDeclaration(
        DeclQualifiedName qualifiedName,
        Declaration declaration,
        IReadOnlyDictionary<QualifiedNameRef, QualifiedNameRef>? nameMap = null)
    {
        var fullName = qualifiedName.FullName;

        // Override the declaration name with the qualified name.
        var renamed = RenameDeclaration(declaration, fullName);

        // Wrap in a minimal File so we can reuse the existing formatting pipeline.
        var dummyModuleDef =
            new Node<Module>(
                s_dummyRange,
                new Module.NormalModule(
                    ModuleTokenLocation: s_dummyLocation,
                    ModuleData: new DefaultModuleData(
                        ModuleName: new Node<IReadOnlyList<string>>(s_dummyRange, ["_Dummy"]),
                        ExposingTokenLocation: s_dummyLocation,
                        ExposingList: new Node<Exposing>(
                            s_dummyRange,
                            new Exposing.All(s_dummyRange)))));

        var file =
            new File(
                ModuleDefinition: dummyModuleDef,
                Imports: [],
                Declarations:
                [
                new Node<Declaration>(
                    new Range(
                        Start: new Location(Row: 4, Column: 1),
                        End: new Location(Row: 100, Column: 1)),
                    renamed)
                ],
                Comments: [],
                IncompleteDeclarations: []);

        // Apply name mapping if provided.
        if (nameMap is { Count: > 0 })
        {
            file = NameMapper.MapNames(file, nameMap);
        }

        // Run through SnapshotTestFormat.Format + Rendering (same pipeline as module rendering).
        var formatted = Format(file);
        var rendered = Rendering.ToString(formatted);

        // Extract only the declaration part (skip the module header line).
        // The module header is "module _Dummy exposing (..)\n" followed by blank lines and the declaration.
        var lines = rendered.Split('\n');

        // Find the first non-empty line after the module header
        var declStartIndex = -1;

        for (var i = 0; i < lines.Length; i++)
        {
            if (lines[i].StartsWith("module "))
                continue;

            if (string.IsNullOrWhiteSpace(lines[i]))
                continue;

            declStartIndex = i;
            break;
        }

        if (declStartIndex < 0)
            return string.Empty;

        // Find the last non-empty line
        var declEndIndex = lines.Length - 1;

        while (declEndIndex >= declStartIndex && string.IsNullOrWhiteSpace(lines[declEndIndex]))
        {
            declEndIndex--;
        }

        // Join the declaration lines
        return string.Join("\n", lines[declStartIndex..(declEndIndex + 1)]);
    }

    /// <summary>
    /// Converts a list of modules (in the Stil4m abstract syntax) to a flat dictionary
    /// of declarations keyed by their fully qualified name.
    /// Each declaration in a module is prefixed with the module name to form the qualified key.
    /// </summary>
    public static IReadOnlyDictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration>
        ModulesToFlatDeclarationDictionary(
        IReadOnlyList<AbstractSyntaxTypes.File> modules)
    {
        var dict = new Dictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration>();

        for (var moduleIndex = 0; moduleIndex < modules.Count; moduleIndex++)
        {
            var module = modules[moduleIndex];
            var moduleName = AbstractSyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            for (var declIndex = 0; declIndex < module.Declarations.Count; declIndex++)
            {
                var decl = module.Declarations[declIndex];
                var declName = GetDeclarationName(decl.Value);

                if (declName is null)
                    continue;

                var qualifiedName =
                    new DeclQualifiedName(
                        Namespaces: moduleName,
                        DeclName: declName);

                dict[qualifiedName] = decl.Value;
            }
        }

        return dict;
    }

    /// <summary>
    /// Filters a declaration dictionary to only include declarations whose qualified name
    /// has the given module name as its namespace prefix.
    /// </summary>
    /// <param name="declarations">The full declaration dictionary to filter.</param>
    /// <param name="moduleName">
    /// The module name to filter by (e.g. ["MyApp"] or ["Some", "Nested", "Module"]).
    /// </param>
    /// <returns>
    /// A dictionary containing only declarations whose namespace matches the given module name.
    /// </returns>
    public static IReadOnlyDictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration>
        FilterDeclarationsByModuleName(
        IReadOnlyDictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration> declarations,
        IReadOnlyList<string> moduleName)
    {
        var result = new Dictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration>();

        foreach (var (key, value) in declarations)
        {
            if (key.Namespaces.Count != moduleName.Count)
                continue;

            var match = true;

            for (var i = 0; i < moduleName.Count; i++)
            {
                if (key.Namespaces[i] != moduleName[i])
                {
                    match = false;
                    break;
                }
            }

            if (match)
            {
                result[key] = value;
            }
        }

        return result;
    }

    /// <summary>
    /// Sort order for rendering declarations in snapshot tests.
    /// </summary>
    public enum DeclarationSortOrder
    {
        /// <summary>
        /// Sort declarations by fully qualified name in ascending alphabetical order.
        /// </summary>
        NameAsc,

        /// <summary>
        /// Sort declarations by number of transitive dependencies in descending order.
        /// Declarations with the most dependencies appear first.
        /// Within groups with the same dependency count, declarations are sorted by name ascending.
        /// This ensures that every declaration appears before its own dependencies in the output.
        /// </summary>
        DependenciesDesc,
    }

    /// <summary>
    /// Renders all declarations in a dictionary using <see cref="RenderQualifiedDeclaration"/>,
    /// joining them with double newlines.
    /// This is a convenience method for rendering all declarations from a filtered dictionary.
    /// Each declaration is first converted from the Stil4m abstract syntax to the full syntax model,
    /// then formatted and rendered with the qualified name.
    /// </summary>
    public static string RenderQualifiedDeclarations(
        IReadOnlyDictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration> declarations,
        DeclarationSortOrder sortOrder,
        IReadOnlyDictionary<QualifiedNameRef, QualifiedNameRef>? nameMap = null)
    {
        var parts = new List<string>();

        var ordered =
            sortOrder switch
            {
                DeclarationSortOrder.NameAsc =>
                declarations.OrderBy(kv => kv.Key),

                DeclarationSortOrder.DependenciesDesc =>
                OrderByDependenciesDesc(declarations),

                _ =>
                throw new System.ArgumentOutOfRangeException(
                    nameof(sortOrder),
                    sortOrder,
                    "Unknown sort order")
            };

        foreach (var (qualifiedName, abstractDecl) in ordered)
        {
            ValidateFullyQualifiedReferences(qualifiedName, abstractDecl);
            var fullDecl = ConvertAbstractDeclaration(abstractDecl);
            parts.Add(RenderQualifiedDeclaration(qualifiedName, fullDecl, nameMap));
        }

        // Use two blank lines between declarations (same spacing as Avh4 elm-format).
        return string.Join("\n\n\n", parts);
    }

    private static readonly Location s_dummyLocation = new(Row: 1, Column: 1);

    private static readonly Range s_dummyRange = new(Start: s_dummyLocation, End: s_dummyLocation);

    /// <summary>
    /// Extracts the declaration name from a Stil4m abstract syntax declaration.
    /// </summary>
    private static string? GetDeclarationName(AbstractSyntaxTypes.Declaration declaration) =>
        declaration switch
        {
            AbstractSyntaxTypes.Declaration.FunctionDeclaration funcDecl =>
            funcDecl.Function.Declaration.Value.Name.Value,

            AbstractSyntaxTypes.Declaration.CustomTypeDeclaration typeDecl =>
            typeDecl.TypeDeclaration.Name.Value,

            AbstractSyntaxTypes.Declaration.AliasDeclaration aliasDecl =>
            aliasDecl.TypeAlias.Name.Value,

            AbstractSyntaxTypes.Declaration.PortDeclaration portDecl =>
            portDecl.Signature.Name.Value,

            AbstractSyntaxTypes.Declaration.InfixDeclaration infixDecl =>
            infixDecl.Infix.Operator.Value,

            _ =>
            null
        };

    /// <summary>
    /// Converts a Stil4m abstract syntax declaration to a full syntax model declaration.
    /// </summary>
    private static Declaration ConvertAbstractDeclaration(
        AbstractSyntaxTypes.Declaration abstractDecl) =>
        abstractDecl switch
        {
            AbstractSyntaxTypes.Declaration.FunctionDeclaration funcDecl =>
            new Declaration.FunctionDeclaration(
                AbstractSyntaxTypes.ToFullSyntaxModel.Convert(funcDecl.Function)),

            AbstractSyntaxTypes.Declaration.CustomTypeDeclaration typeDecl =>
            new Declaration.CustomTypeDeclaration(
                AbstractSyntaxTypes.ToFullSyntaxModel.Convert(typeDecl.TypeDeclaration)),

            AbstractSyntaxTypes.Declaration.AliasDeclaration aliasDecl =>
            new Declaration.AliasDeclaration(
                AbstractSyntaxTypes.ToFullSyntaxModel.Convert(aliasDecl.TypeAlias)),

            AbstractSyntaxTypes.Declaration.PortDeclaration portDecl =>
            new Declaration.PortDeclaration(
                PortTokenLocation: s_dummyLocation,
                Signature: AbstractSyntaxTypes.ToFullSyntaxModel.Convert(portDecl.Signature)),

            AbstractSyntaxTypes.Declaration.InfixDeclaration infixDecl =>
            new Declaration.InfixDeclaration(
                AbstractSyntaxTypes.ToFullSyntaxModel.Convert(infixDecl.Infix)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected abstract declaration type: " + abstractDecl.GetType().Name)
        };

    /// <summary>
    /// Returns a new Declaration with the name replaced by the specified qualified name.
    /// For function declarations, both the signature name and the implementation name are replaced.
    /// </summary>
    private static Declaration RenameDeclaration(Declaration declaration, string qualifiedName) =>
        declaration switch
        {
            Declaration.FunctionDeclaration funcDecl =>
            new Declaration.FunctionDeclaration(
                Function: new FunctionStruct(
                    Documentation: funcDecl.Function.Documentation,
                    Signature: funcDecl.Function.Signature is { } sig
                    ?
                    sig with
                    {
                        Value =
                        sig.Value with
                        {
                            Name = sig.Value.Name with { Value = qualifiedName }
                        }
                    }
                    :
                    null,
                    Declaration: funcDecl.Function.Declaration with
                    {
                        Value =
                        funcDecl.Function.Declaration.Value with
                        {
                            Name = funcDecl.Function.Declaration.Value.Name with { Value = qualifiedName }
                        }
                    })),

            Declaration.CustomTypeDeclaration typeDecl =>
            new Declaration.CustomTypeDeclaration(
                TypeDeclaration: typeDecl.TypeDeclaration with
                {
                    Name = typeDecl.TypeDeclaration.Name with { Value = qualifiedName }
                }),

            Declaration.AliasDeclaration aliasDecl =>
            new Declaration.AliasDeclaration(
                TypeAlias: aliasDecl.TypeAlias with
                {
                    Name = aliasDecl.TypeAlias.Name with { Value = qualifiedName }
                }),

            Declaration.PortDeclaration portDecl =>
            new Declaration.PortDeclaration(
                PortTokenLocation: portDecl.PortTokenLocation,
                Signature: portDecl.Signature with
                {
                    Name = portDecl.Signature.Name with { Value = qualifiedName }
                }),

            Declaration.InfixDeclaration infixDecl =>
            new Declaration.InfixDeclaration(
                Infix: infixDecl.Infix with
                {
                    Operator = infixDecl.Infix.Operator with { Value = qualifiedName }
                }),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected declaration type for renaming: " + declaration.GetType().Name)
        };

    /// <summary>
    /// Orders declarations by number of transitive dependencies descending, then by name ascending.
    /// This ensures that any declaration appears before its own dependencies in the output.
    /// </summary>
    private static IOrderedEnumerable<KeyValuePair<DeclQualifiedName, AbstractSyntaxTypes.Declaration>>
        OrderByDependenciesDesc(
        IReadOnlyDictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration> declarations)
    {
        var transitiveCounts = DeriveTransitiveDependencyCounts(declarations);

        return
            declarations
            .OrderByDescending(
                kv =>
                transitiveCounts.TryGetValue(kv.Key, out var count) ? count : 0)
            .ThenBy(kv => kv.Key);
    }

    /// <summary>
    /// Derives the number of transitive dependencies for each declaration in the dictionary.
    /// First computes the set of direct dependencies for each declaration by scanning all
    /// <see cref="QualifiedNameRef"/> references in expressions, patterns, and type annotations.
    /// Then computes transitive closures to get the full count.
    /// </summary>
    public static IReadOnlyDictionary<DeclQualifiedName, int> DeriveTransitiveDependencyCounts(
        IReadOnlyDictionary<DeclQualifiedName, AbstractSyntaxTypes.Declaration> declarations)
    {
        // Step 1: Build a set of all declaration names in the dictionary for filtering references.
        var allDeclNames = new HashSet<DeclQualifiedName>(declarations.Keys);

        // Step 2: Derive direct dependencies for each declaration.
        var directDeps = new Dictionary<DeclQualifiedName, ImmutableHashSet<DeclQualifiedName>>();

        foreach (var (declName, decl) in declarations)
        {
            var refs = CollectReferencesFromDeclaration(decl);

            // Filter to only references that are in our dictionary (known declarations).
            directDeps[declName] = refs.Intersect(allDeclNames).Remove(declName);
        }

        // Step 3: Compute transitive closure for each declaration.
        var transitiveCounts = new Dictionary<DeclQualifiedName, int>();

        foreach (var declName in declarations.Keys)
        {
            var transitive = new HashSet<DeclQualifiedName>();
            var stack = new Stack<DeclQualifiedName>(directDeps[declName]);

            while (stack.Count > 0)
            {
                var dep = stack.Pop();

                if (!transitive.Add(dep))
                    continue;

                if (directDeps.TryGetValue(dep, out var depDeps))
                {
                    foreach (var dd in depDeps)
                    {
                        stack.Push(dd);
                    }
                }
            }

            transitiveCounts[declName] = transitive.Count;
        }

        return transitiveCounts;
    }

    /// <summary>
    /// Collects all qualified name references from an abstract syntax declaration.
    /// </summary>
    private static ImmutableHashSet<DeclQualifiedName> CollectReferencesFromDeclaration(
        AbstractSyntaxTypes.Declaration declaration)
    {
        switch (declaration)
        {
            case AbstractSyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                var funcRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                if (funcDecl.Function.Signature is { } sig)
                    funcRefs = funcRefs.Union(CollectReferencesFromTypeAnnotation(sig.Value.TypeAnnotation.Value));

                funcRefs =
                    funcRefs.Union(
                        CollectReferencesFromExpression(funcDecl.Function.Declaration.Value.Expression.Value));

                foreach (var arg in funcDecl.Function.Declaration.Value.Arguments)
                    funcRefs = funcRefs.Union(CollectReferencesFromPattern(arg.Value));

                return funcRefs;

            case AbstractSyntaxTypes.Declaration.AliasDeclaration aliasDecl:
                return CollectReferencesFromTypeAnnotation(aliasDecl.TypeAlias.TypeAnnotation.Value);

            case AbstractSyntaxTypes.Declaration.CustomTypeDeclaration typeDecl:
                var typeRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var constructor in typeDecl.TypeDeclaration.Constructors)
                    foreach (var arg in constructor.Value.Arguments)
                        typeRefs = typeRefs.Union(CollectReferencesFromTypeAnnotation(arg.Value));

                return typeRefs;

            case AbstractSyntaxTypes.Declaration.PortDeclaration portDecl:
                return CollectReferencesFromTypeAnnotation(portDecl.Signature.TypeAnnotation.Value);

            default:
                throw new System.NotImplementedException(
                    "Unexpected declaration type for reference collection: " + declaration.GetType().Name);
        }
    }

    private static ImmutableHashSet<DeclQualifiedName> CollectReferencesFromExpression(
        AbstractSyntaxTypes.Expression expression)
    {
        switch (expression)
        {
            case AbstractSyntaxTypes.Expression.FunctionOrValue fov:
                if (fov.ModuleName is { Count: > 0 })
                {
                    return
                        [
                        new DeclQualifiedName(
                            Namespaces: fov.ModuleName,
                            DeclName: fov.Name)
                        ];
                }

                return [];

            case AbstractSyntaxTypes.Expression.Application app:
                var appRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var arg in app.Arguments)
                    appRefs = appRefs.Union(CollectReferencesFromExpression(arg.Value));

                return appRefs;

            case AbstractSyntaxTypes.Expression.OperatorApplication opApp:
                return
                    CollectReferencesFromExpression(opApp.Left.Value)
                    .Union(CollectReferencesFromExpression(opApp.Right.Value));

            case AbstractSyntaxTypes.Expression.IfBlock ifBlock:
                return
                    CollectReferencesFromExpression(ifBlock.Condition.Value)
                    .Union(CollectReferencesFromExpression(ifBlock.ThenBlock.Value))
                    .Union(CollectReferencesFromExpression(ifBlock.ElseBlock.Value));

            case AbstractSyntaxTypes.Expression.Negation neg:
                return CollectReferencesFromExpression(neg.Expression.Value);

            case AbstractSyntaxTypes.Expression.ListExpr list:
                var listRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var elem in list.Elements)
                    listRefs = listRefs.Union(CollectReferencesFromExpression(elem.Value));

                return listRefs;

            case AbstractSyntaxTypes.Expression.TupledExpression tuple:
                var tupleRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var elem in tuple.Elements)
                    tupleRefs = tupleRefs.Union(CollectReferencesFromExpression(elem.Value));

                return tupleRefs;

            case AbstractSyntaxTypes.Expression.ParenthesizedExpression paren:
                return CollectReferencesFromExpression(paren.Expression.Value);

            case AbstractSyntaxTypes.Expression.LambdaExpression lambda:
                var lambdaRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var arg in lambda.Lambda.Arguments)
                    lambdaRefs = lambdaRefs.Union(CollectReferencesFromPattern(arg.Value));

                return lambdaRefs.Union(CollectReferencesFromExpression(lambda.Lambda.Expression.Value));

            case AbstractSyntaxTypes.Expression.CaseExpression caseExpr:
                var caseRefs = CollectReferencesFromExpression(caseExpr.CaseBlock.Expression.Value);

                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                {
                    caseRefs = caseRefs.Union(CollectReferencesFromPattern(caseItem.Pattern.Value));
                    caseRefs = caseRefs.Union(CollectReferencesFromExpression(caseItem.Expression.Value));
                }

                return caseRefs;

            case AbstractSyntaxTypes.Expression.LetExpression letExpr:
                var letRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var letDecl in letExpr.Value.Declarations)
                {
                    switch (letDecl.Value)
                    {
                        case AbstractSyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            if (letFunc.Function.Signature is { } letSig)
                            {
                                letRefs =
                                    letRefs.Union(
                                        CollectReferencesFromTypeAnnotation(letSig.Value.TypeAnnotation.Value));
                            }

                            letRefs =
                                letRefs.Union(
                                    CollectReferencesFromExpression(letFunc.Function.Declaration.Value.Expression.Value));

                            foreach (var arg in letFunc.Function.Declaration.Value.Arguments)
                                letRefs = letRefs.Union(CollectReferencesFromPattern(arg.Value));

                            break;

                        case AbstractSyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            letRefs = letRefs.Union(CollectReferencesFromPattern(letDestr.Pattern.Value));
                            letRefs = letRefs.Union(CollectReferencesFromExpression(letDestr.Expression.Value));
                            break;
                    }
                }

                return letRefs.Union(CollectReferencesFromExpression(letExpr.Value.Expression.Value));

            case AbstractSyntaxTypes.Expression.RecordExpr recordExpr:
                var recRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var field in recordExpr.Fields)
                    recRefs = recRefs.Union(CollectReferencesFromExpression(field.Value.valueExpr.Value));

                return recRefs;

            case AbstractSyntaxTypes.Expression.RecordAccess recordAccess:
                return CollectReferencesFromExpression(recordAccess.Record.Value);

            case AbstractSyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                var updateRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var field in recordUpdate.Fields)
                    updateRefs = updateRefs.Union(CollectReferencesFromExpression(field.Value.valueExpr.Value));

                return updateRefs;

            case AbstractSyntaxTypes.Expression.UnitExpr:
            case AbstractSyntaxTypes.Expression.Integer:
            case AbstractSyntaxTypes.Expression.Floatable:
            case AbstractSyntaxTypes.Expression.CharLiteral:
            case AbstractSyntaxTypes.Expression.Literal:
            case AbstractSyntaxTypes.Expression.GLSLExpression:
            case AbstractSyntaxTypes.Expression.RecordAccessFunction:
            case AbstractSyntaxTypes.Expression.PrefixOperator:
                return [];

            default:
                throw new System.NotImplementedException(
                    "Unexpected expression type for reference collection: " + expression.GetType().Name);
        }
    }

    private static ImmutableHashSet<DeclQualifiedName> CollectReferencesFromPattern(
        AbstractSyntaxTypes.Pattern pattern)
    {
        switch (pattern)
        {
            case AbstractSyntaxTypes.Pattern.NamedPattern named:
                var namedRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                if (named.Name.ModuleName is { Count: > 0 })
                {
                    namedRefs =
                        namedRefs.Add(
                            new DeclQualifiedName(
                                Namespaces: named.Name.ModuleName,
                                DeclName: named.Name.Name));
                }

                foreach (var arg in named.Arguments)
                    namedRefs = namedRefs.Union(CollectReferencesFromPattern(arg.Value));

                return namedRefs;

            case AbstractSyntaxTypes.Pattern.UnConsPattern unCons:
                return
                    CollectReferencesFromPattern(unCons.Head.Value)
                    .Union(CollectReferencesFromPattern(unCons.Tail.Value));

            case AbstractSyntaxTypes.Pattern.TuplePattern tuple:
                var tupleRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var elem in tuple.Elements)
                    tupleRefs = tupleRefs.Union(CollectReferencesFromPattern(elem.Value));

                return tupleRefs;

            case AbstractSyntaxTypes.Pattern.ListPattern list:
                var listRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var elem in list.Elements)
                    listRefs = listRefs.Union(CollectReferencesFromPattern(elem.Value));

                return listRefs;

            case AbstractSyntaxTypes.Pattern.ParenthesizedPattern paren:
                return CollectReferencesFromPattern(paren.Pattern.Value);

            case AbstractSyntaxTypes.Pattern.AsPattern asPattern:
                return CollectReferencesFromPattern(asPattern.Pattern.Value);

            case AbstractSyntaxTypes.Pattern.AllPattern:
            case AbstractSyntaxTypes.Pattern.VarPattern:
            case AbstractSyntaxTypes.Pattern.UnitPattern:
            case AbstractSyntaxTypes.Pattern.CharPattern:
            case AbstractSyntaxTypes.Pattern.StringPattern:
            case AbstractSyntaxTypes.Pattern.IntPattern:
            case AbstractSyntaxTypes.Pattern.HexPattern:
            case AbstractSyntaxTypes.Pattern.FloatPattern:
            case AbstractSyntaxTypes.Pattern.RecordPattern:
                return [];

            default:
                throw new System.NotImplementedException(
                    "Unexpected pattern type for reference collection: " + pattern.GetType().Name);
        }
    }

    private static ImmutableHashSet<DeclQualifiedName> CollectReferencesFromTypeAnnotation(
        AbstractSyntaxTypes.TypeAnnotation typeAnnotation)
    {
        switch (typeAnnotation)
        {
            case AbstractSyntaxTypes.TypeAnnotation.Typed typed:
                var typedRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                if (typed.TypeName.Value.ModuleName is { Count: > 0 })
                {
                    typedRefs =
                        typedRefs.Add(
                            new DeclQualifiedName(
                                Namespaces: typed.TypeName.Value.ModuleName,
                                DeclName: typed.TypeName.Value.Name));
                }

                foreach (var arg in typed.TypeArguments)
                    typedRefs = typedRefs.Union(CollectReferencesFromTypeAnnotation(arg.Value));

                return typedRefs;

            case AbstractSyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType:
                return
                    CollectReferencesFromTypeAnnotation(funcType.ArgumentType.Value)
                    .Union(CollectReferencesFromTypeAnnotation(funcType.ReturnType.Value));

            case AbstractSyntaxTypes.TypeAnnotation.Tupled tupled:
                var tupledRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var elem in tupled.TypeAnnotations)
                    tupledRefs = tupledRefs.Union(CollectReferencesFromTypeAnnotation(elem.Value));

                return tupledRefs;

            case AbstractSyntaxTypes.TypeAnnotation.Record record:
                var recordRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var field in record.RecordDefinition.Fields)
                    recordRefs = recordRefs.Union(CollectReferencesFromTypeAnnotation(field.Value.FieldType.Value));

                return recordRefs;

            case AbstractSyntaxTypes.TypeAnnotation.GenericRecord genericRecord:
                var genRecRefs = ImmutableHashSet<DeclQualifiedName>.Empty;

                foreach (var field in genericRecord.RecordDefinition.Value.Fields)
                    genRecRefs = genRecRefs.Union(CollectReferencesFromTypeAnnotation(field.Value.FieldType.Value));

                return genRecRefs;

            default:
                throw new System.NotImplementedException(
                    "Unexpected type annotation type for reference collection: " + typeAnnotation.GetType().Name);
        }
    }

    /// <summary>
    /// Validates that all <see cref="AbstractSyntaxTypes.Expression.FunctionOrValue"/> references
    /// in a declaration are either fully qualified (have a module name) or refer to a local binding
    /// (parameter name, let binding, case pattern, or lambda parameter).
    /// Throws an exception if an unqualified non-local reference is found.
    /// </summary>
    public static void ValidateFullyQualifiedReferences(
        DeclQualifiedName declName,
        AbstractSyntaxTypes.Declaration declaration)
    {
        if (declaration is not AbstractSyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            return;

        var funcImpl = funcDecl.Function.Declaration.Value;

        var localBindings = new HashSet<string>();

        foreach (var arg in funcImpl.Arguments)
            CollectPatternBindings(arg.Value, localBindings);

        ValidateExpressionReferences(declName, funcImpl.Expression.Value, localBindings);
    }

    private static void CollectPatternBindings(
        AbstractSyntaxTypes.Pattern pattern,
        HashSet<string> bindings)
    {
        switch (pattern)
        {
            case AbstractSyntaxTypes.Pattern.VarPattern var:
                bindings.Add(var.Name);
                break;

            case AbstractSyntaxTypes.Pattern.AsPattern asPattern:
                bindings.Add(asPattern.Name.Value);
                CollectPatternBindings(asPattern.Pattern.Value, bindings);
                break;

            case AbstractSyntaxTypes.Pattern.NamedPattern named:
                foreach (var arg in named.Arguments)
                    CollectPatternBindings(arg.Value, bindings);

                break;

            case AbstractSyntaxTypes.Pattern.TuplePattern tuple:
                foreach (var elem in tuple.Elements)
                    CollectPatternBindings(elem.Value, bindings);

                break;

            case AbstractSyntaxTypes.Pattern.ListPattern list:
                foreach (var elem in list.Elements)
                    CollectPatternBindings(elem.Value, bindings);

                break;

            case AbstractSyntaxTypes.Pattern.UnConsPattern unCons:
                CollectPatternBindings(unCons.Head.Value, bindings);
                CollectPatternBindings(unCons.Tail.Value, bindings);
                break;

            case AbstractSyntaxTypes.Pattern.ParenthesizedPattern paren:
                CollectPatternBindings(paren.Pattern.Value, bindings);
                break;

            case AbstractSyntaxTypes.Pattern.RecordPattern record:
                foreach (var field in record.Fields)
                    bindings.Add(field.Value);

                break;
        }
    }

    private static void ValidateExpressionReferences(
        DeclQualifiedName declName,
        AbstractSyntaxTypes.Expression expression,
        HashSet<string> localBindings)
    {
        switch (expression)
        {
            case AbstractSyntaxTypes.Expression.FunctionOrValue fov:
                if (fov.ModuleName.Count is 0 && !localBindings.Contains(fov.Name))
                {
                    throw new System.InvalidOperationException(
                        $"Declaration '{declName.FullName}' contains unqualified reference '{fov.Name}' " +
                        $"that is not a local binding. All non-local references must be fully qualified.");
                }

                break;

            case AbstractSyntaxTypes.Expression.Application app:
                foreach (var arg in app.Arguments)
                    ValidateExpressionReferences(declName, arg.Value, localBindings);

                break;

            case AbstractSyntaxTypes.Expression.OperatorApplication opApp:
                ValidateExpressionReferences(declName, opApp.Left.Value, localBindings);
                ValidateExpressionReferences(declName, opApp.Right.Value, localBindings);
                break;

            case AbstractSyntaxTypes.Expression.IfBlock ifBlock:
                ValidateExpressionReferences(declName, ifBlock.Condition.Value, localBindings);
                ValidateExpressionReferences(declName, ifBlock.ThenBlock.Value, localBindings);
                ValidateExpressionReferences(declName, ifBlock.ElseBlock.Value, localBindings);
                break;

            case AbstractSyntaxTypes.Expression.Negation neg:
                ValidateExpressionReferences(declName, neg.Expression.Value, localBindings);
                break;

            case AbstractSyntaxTypes.Expression.ListExpr list:
                foreach (var elem in list.Elements)
                    ValidateExpressionReferences(declName, elem.Value, localBindings);

                break;

            case AbstractSyntaxTypes.Expression.TupledExpression tuple:
                foreach (var elem in tuple.Elements)
                    ValidateExpressionReferences(declName, elem.Value, localBindings);

                break;

            case AbstractSyntaxTypes.Expression.ParenthesizedExpression paren:
                ValidateExpressionReferences(declName, paren.Expression.Value, localBindings);
                break;

            case AbstractSyntaxTypes.Expression.LambdaExpression lambda:
                var lambdaBindings = new HashSet<string>(localBindings);

                foreach (var arg in lambda.Lambda.Arguments)
                    CollectPatternBindings(arg.Value, lambdaBindings);

                ValidateExpressionReferences(declName, lambda.Lambda.Expression.Value, lambdaBindings);
                break;

            case AbstractSyntaxTypes.Expression.CaseExpression caseExpr:
                ValidateExpressionReferences(declName, caseExpr.CaseBlock.Expression.Value, localBindings);

                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                {
                    var caseBindings = new HashSet<string>(localBindings);
                    CollectPatternBindings(caseItem.Pattern.Value, caseBindings);
                    ValidateExpressionReferences(declName, caseItem.Expression.Value, caseBindings);
                }

                break;

            case AbstractSyntaxTypes.Expression.LetExpression letExpr:
                var letBindings = new HashSet<string>(localBindings);

                foreach (var letDecl in letExpr.Value.Declarations)
                {
                    switch (letDecl.Value)
                    {
                        case AbstractSyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            letBindings.Add(letFunc.Function.Declaration.Value.Name.Value);
                            break;

                        case AbstractSyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            CollectPatternBindings(letDestr.Pattern.Value, letBindings);
                            break;
                    }
                }

                foreach (var letDecl in letExpr.Value.Declarations)
                {
                    switch (letDecl.Value)
                    {
                        case AbstractSyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            var letFuncBindings = new HashSet<string>(letBindings);

                            foreach (var arg in letFunc.Function.Declaration.Value.Arguments)
                                CollectPatternBindings(arg.Value, letFuncBindings);

                            ValidateExpressionReferences(declName, letFunc.Function.Declaration.Value.Expression.Value, letFuncBindings);
                            break;

                        case AbstractSyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            ValidateExpressionReferences(declName, letDestr.Expression.Value, letBindings);
                            break;
                    }
                }

                ValidateExpressionReferences(declName, letExpr.Value.Expression.Value, letBindings);
                break;

            case AbstractSyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    ValidateExpressionReferences(declName, field.Value.valueExpr.Value, localBindings);

                break;

            case AbstractSyntaxTypes.Expression.RecordAccess recordAccess:
                ValidateExpressionReferences(declName, recordAccess.Record.Value, localBindings);
                break;

            case AbstractSyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    ValidateExpressionReferences(declName, field.Value.valueExpr.Value, localBindings);

                break;
        }
    }
}
