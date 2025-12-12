using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Functionality for rendering Elm source files while preserving original source locations and formatting.
/// </summary>
/// <remarks>
/// Note: The particular syntax model here (https://github.com/stil4m/elm-syntax/tree/7.3.9/src/Elm/Syntax) does not retain
/// locations for all syntax elements.
/// Examples of tokens without location data include item separators in list expressions and record expressions.
/// For these tokens without location data, the renderer typically inserts a single space between the token and the following node.
/// </remarks>
public class Rendering
{
    /// <summary>
    /// Context for tracking position while rendering with location preservation.
    /// </summary>
    private class RenderContext
    {
        public StringBuilder Output { get; } = new();

        public int CurrentRow { get; set; } = 1;

        public int CurrentColumn { get; set; } = 1;

        public IReadOnlyList<Node<string>> Comments { get; set; } = [];

        private int _nextCommentIndex = 0;

        /// <summary>
        /// Advances to the given location by adding spaces or newlines as needed.
        /// If the target location is before the current position, uses the minimum spacing instead.
        /// Renders any comments that fall between the current position and the target position.
        /// </summary>
        public void AdvanceToLocation(Location targetLocation, int minSpaces = 1)
        {
            // Render any comments that should appear before reaching the target location
            RenderCommentsUpTo(targetLocation);

            // Handle row changes
            while (CurrentRow < targetLocation.Row)
            {
                Output.Append('\n');
                CurrentRow++;
                CurrentColumn = 1;
            }

            // Handle column changes on the same row
            if (CurrentRow == targetLocation.Row)
            {
                var spacesToAdd = targetLocation.Column - CurrentColumn;
                if (spacesToAdd > 0)
                {
                    Output.Append(' ', spacesToAdd);
                    CurrentColumn = targetLocation.Column;
                }
                else if (spacesToAdd < 0 && minSpaces > 0)
                {
                    // Location is in the past, use minimum spacing
                    Output.Append(' ', minSpaces);
                    CurrentColumn += minSpaces;
                }
                // If spacesToAdd is 0, we're already at the target, don't add spaces
            }
        }

        /// <summary>
        /// Renders all comments that appear before the given location.
        /// </summary>
        private void RenderCommentsUpTo(Location targetLocation)
        {
            while (_nextCommentIndex < Comments.Count)
            {
                var comment = Comments[_nextCommentIndex];

                // Check if this comment should be rendered before the target location
                // A comment should be rendered if:
                // 1. It starts after the current position (or at a future row)
                // 2. It starts before the target location
                var commentIsAfterCurrent = comment.Range.Start.Row > CurrentRow ||
                    (comment.Range.Start.Row == CurrentRow && comment.Range.Start.Column >= CurrentColumn);

                var commentIsBeforeTarget = comment.Range.Start.Row < targetLocation.Row ||
                    (comment.Range.Start.Row == targetLocation.Row && comment.Range.Start.Column < targetLocation.Column);

                if (commentIsAfterCurrent && commentIsBeforeTarget)
                {
                    // Advance to comment location (but don't recurse - use simple advance)
                    AdvanceToLocationSimple(comment.Range.Start);

                    // Render the comment
                    Output.Append(comment.Value);

                    // Update position based on comment length
                    foreach (var ch in comment.Value)
                    {
                        if (ch is '\n')
                        {
                            CurrentRow++;
                            CurrentColumn = 1;
                        }
                        else
                        {
                            CurrentColumn++;
                        }
                    }

                    _nextCommentIndex++;
                }
                else if (commentIsBeforeTarget)
                {
                    // Comment is before current position, skip it (already rendered or missed)
                    _nextCommentIndex++;
                }
                else
                {
                    // This comment is after the target location, so stop
                    break;
                }
            }
        }

        /// <summary>
        /// Simple advance without comment rendering (to avoid recursion).
        /// </summary>
        private void AdvanceToLocationSimple(Location targetLocation)
        {
            // Handle row changes
            while (CurrentRow < targetLocation.Row)
            {
                Output.Append('\n');
                CurrentRow++;
                CurrentColumn = 1;
            }

            // Handle column changes on the same row
            if (CurrentRow == targetLocation.Row)
            {
                var spacesToAdd = targetLocation.Column - CurrentColumn;
                if (spacesToAdd > 0)
                {
                    Output.Append(' ', spacesToAdd);
                    CurrentColumn = targetLocation.Column;
                }
            }
        }

        /// <summary>
        /// Advances by at least the minimum number of spaces (or newlines if needed).
        /// </summary>
        public void AdvanceByMinimum(int minSpaces)
        {
            Output.Append(' ', minSpaces);
            CurrentColumn += minSpaces;
        }

        /// <summary>
        /// Appends text and updates the current position.
        /// </summary>
        public void Append(string text)
        {
            Output.Append(text);

            // Update position based on the text content
            foreach (var ch in text)
            {
                if (ch is '\n')
                {
                    CurrentRow++;
                    CurrentColumn = 1;
                }
                else
                {
                    CurrentColumn++;
                }
            }
        }
    }

    /// <summary>
    /// Renders a file while preserving the original source locations.
    /// </summary>
    public static string ToString(File file)
    {
        var context = new RenderContext
        {
            Comments = [.. file.Comments.OrderBy(c => c.Range.Start.Row).ThenBy(c => c.Range.Start.Column)]
        };

        // Render module definition
        RenderModule(file.ModuleDefinition, context);

        // Render imports
        foreach (var import in file.Imports)
        {
            context.AdvanceToLocation(import.Range.Start);
            RenderImport(import, context);
        }

        // Render declarations
        foreach (var declaration in file.Declarations)
        {
            context.AdvanceToLocation(declaration.Range.Start);
            RenderDeclaration(declaration, context);
        }

        // Ensure file ends with a trailing newline (AVH4 elm-format style)
        var result = context.Output.ToString();
        if (!result.EndsWith("\n"))
        {
            result += "\n";
        }

        return result;
    }

    private static void RenderImport(
        Node<Import> importNode,
        RenderContext context)
    {
        context.Append("import");

        // Advance to module name location
        context.AdvanceToLocation(importNode.Value.ModuleName.Range.Start);
        context.Append(RenderModuleName(importNode.Value.ModuleName.Value));

        if (importNode.Value.ModuleAlias is { } alias)
        {
            // For now, use minimum spacing (can be enhanced with location data if available)
            context.AdvanceByMinimum(1);
            context.Append("as");
            context.AdvanceByMinimum(1);
            context.Append(RenderModuleName(alias.Value));
        }

        if (importNode.Value.ExposingList is { } exposingList)
        {
            // Advance to exposing keyword location
            context.AdvanceToLocation(exposingList.Range.Start);
            context.Append("exposing");

            // Render the exposing part
            RenderExposing(exposingList, context);
        }
    }

    private static void RenderModule(
        Node<Module> moduleNode,
        RenderContext context)
    {
        string moduleKeyword;
        Node<IReadOnlyList<string>> moduleName;
        Node<Exposing> exposingList;

        switch (moduleNode.Value)
        {
            case Module.NormalModule normalModule:
                moduleKeyword = "module";
                moduleName = normalModule.ModuleData.ModuleName;
                exposingList = normalModule.ModuleData.ExposingList;
                break;
            case Module.PortModule portModule:
                moduleKeyword = "port module";
                moduleName = portModule.ModuleData.ModuleName;
                exposingList = portModule.ModuleData.ExposingList;
                break;
            case Module.EffectModule effectModule:
                moduleKeyword = "effect module";
                moduleName = effectModule.ModuleData.ModuleName;
                exposingList = effectModule.ModuleData.ExposingList;
                break;
            default:
                throw new NotImplementedException($"Unknown module type: {moduleNode.Value.GetType().Name}");
        }

        // Start at the module definition's location
        context.AdvanceToLocation(moduleNode.Range.Start);
        context.Append(moduleKeyword);

        // Advance to module name location
        context.AdvanceToLocation(moduleName.Range.Start);
        context.Append(RenderModuleName(moduleName.Value));

        // Advance to exposing list location and append "exposing"
        context.AdvanceToLocation(exposingList.Range.Start);
        context.Append("exposing");

        // Render the exposing part
        RenderExposing(exposingList, context);
    }

    private static void RenderExposing(
        Node<Exposing> exposingNode,
        RenderContext context)
    {
        switch (exposingNode.Value)
        {
            case Exposing.All all:
                {
                    // For Exposing.All, advance to the opening paren location (before the dots)
                    // The all.Range is for the ".." only, so we need to back up 1 for the "("
                    var openParenLocation = new Location(all.Range.Start.Row, all.Range.Start.Column - 1);
                    context.AdvanceToLocation(openParenLocation, minSpaces: 1);
                    context.Append("(");

                    // Now advance to the dots location (no minimum spacing needed)
                    context.AdvanceToLocation(all.Range.Start, minSpaces: 0);
                    context.Append("..)");
                    break;
                }

            case Exposing.Explicit explicit_:
                {
                    // For multi-line exposing lists, the opening paren should be placed on the same line
                    // as the first item, before it. For single-line, it should be right after "exposing" with a space.
                    // We can detect this by checking if the first item is on a different row than current position.
                    if (explicit_.Nodes.Count > 0)
                    {
                        var firstItemLocation = explicit_.Nodes[0].Range.Start;

                        // Check if the first item is on a different row (multi-line list)
                        if (firstItemLocation.Row > context.CurrentRow)
                        {
                            // Multi-line: place opening paren on the SAME ROW as the first item,
                            // 2 columns before it (for "( ")
                            var openParenLocation = new Location(
                                firstItemLocation.Row,
                                firstItemLocation.Column - 2);
                            context.AdvanceToLocation(openParenLocation, minSpaces: 1);
                        }
                        else
                        {
                            // Single-line: just add a space before the opening paren
                            context.AdvanceByMinimum(1);
                        }
                    }
                    else
                    {
                        // Empty list: just add a space
                        context.AdvanceByMinimum(1);
                    }
                    context.Append("(");

                    // Detect if this is a multi-line list by checking if any item is on a different row
                    var isMultiLine = false;
                    if (explicit_.Nodes.Count > 1)
                    {
                        var firstRow = explicit_.Nodes[0].Range.Start.Row;
                        for (var i = 1; i < explicit_.Nodes.Count; i++)
                        {
                            if (explicit_.Nodes[i].Range.Start.Row != firstRow)
                            {
                                isMultiLine = true;
                                break;
                            }
                        }
                    }

                    for (var i = 0; i < explicit_.Nodes.Count; i++)
                    {
                        var node = explicit_.Nodes[i];

                        if (i > 0)
                        {
                            if (isMultiLine)
                            {
                                // Multi-line: comma at start of new line (2 columns before the item)
                                var commaLocation = new Location(
                                    node.Range.Start.Row,
                                    node.Range.Start.Column - 2);
                                context.AdvanceToLocation(commaLocation, minSpaces: 1);
                                context.Append(", ");
                            }
                            else
                            {
                                // Single-line: comma immediately after previous item
                                context.Append(", ");
                            }
                        }

                        // Advance to the item's start location
                        context.AdvanceToLocation(node.Range.Start, minSpaces: i > 0 && !isMultiLine ? 0 : 1);

                        // Handle TypeExpose with Open specially to preserve spacing before (..)
                        if (node.Value is TopLevelExpose.TypeExpose typeExpose && typeExpose.ExposedType.Open is not null)
                        {
                            context.Append(typeExpose.ExposedType.Name);

                            // The Open range corresponds to the ".." only  
                            // Use the Open.Start column directly as the target for the opening paren
                            // (This appears to point to where we want the paren, not the dots)
                            var typeOpenParenLoc = new Location(
                                typeExpose.ExposedType.Open.Start.Row,
                                typeExpose.ExposedType.Open.Start.Column);
                            context.AdvanceToLocation(typeOpenParenLoc, minSpaces: 0);
                            context.Append("(");

                            // Advance 1 more column for the dots
                            context.AdvanceByMinimum(0);
                            context.Append("..");

                            // Append closing paren immediately after (not tracked in syntax model)
                            context.Append(")");
                        }
                        else
                        {
                            context.Append(RenderTopLevelExpose(node.Value));
                        }
                    }

                    // Handle closing paren
                    if (isMultiLine)
                    {
                        // Multi-line: closing paren should be on the next line
                        // We can use the exposingNode.Range.End to find where it should be
                        // The End points to the character after the closing paren, so back up 1 column
                        var closingParenLocation = new Location(
                            exposingNode.Range.End.Row,
                            exposingNode.Range.End.Column - 1);
                        context.AdvanceToLocation(closingParenLocation, minSpaces: 1);
                    }
                    else
                    {
                        // Single-line: closing paren immediately after last item (1 space before)
                        context.AdvanceByMinimum(0);
                    }
                    context.Append(")");
                    break;
                }

            default:
                throw new NotImplementedException(
                    $"Unknown exposing type: {exposingNode.Value.GetType().Name}");
        }
    }

    private static void RenderDeclaration(
        Node<Declaration> declarationNode,
        RenderContext context)
    {
        switch (declarationNode.Value)
        {
            case Declaration.FunctionDeclaration funcDecl:
                RenderFunction(funcDecl.Function, context);
                break;

            case Declaration.CustomTypeDeclaration customType:
                RenderCustomType(customType.TypeDeclaration, context);
                break;

            case Declaration.AliasDeclaration aliasDecl:
                RenderTypeAlias(aliasDecl.TypeAlias, context);
                break;

            case Declaration.PortDeclaration portDecl:
                context.Append("port ");
                RenderSignature(portDecl.Signature, context);
                break;

            case Declaration.InfixDeclaration infixDecl:
                context.Append(RenderInfix(infixDecl.Infix));
                break;

            default:
                throw new NotImplementedException(
                    $"Unknown declaration type: {declarationNode.Value.GetType().Name}");
        }
    }

    private static void RenderTypeAlias(
        TypeAlias typeAlias,
        RenderContext context)
    {
        // Render "type alias"
        context.Append("type");
        context.AdvanceByMinimum(1);
        context.Append("alias");

        // Advance to name location
        context.AdvanceToLocation(typeAlias.Name.Range.Start);
        var nameEndColumn = typeAlias.Name.Range.End.Column;
        context.Append(typeAlias.Name.Value);

        // Render generics if present
        foreach (var generic in typeAlias.Generics)
        {
            context.AdvanceToLocation(generic.Range.Start);
            context.Append(generic.Value);
            nameEndColumn = generic.Range.End.Column;
        }

        // Position the equals sign based on type annotation location
        var typeLocation = typeAlias.TypeAnnotation.Range.Start;

        if (typeLocation.Row == context.CurrentRow)
        {
            // Same row: Add minimal space before =, then advance to type location after =
            context.AdvanceByMinimum(1);
            context.Append("=");
            // Advance to type location (this will add the appropriate spacing after =)
            context.AdvanceToLocation(typeLocation, minSpaces: 1);
        }
        else
        {
            // Different row: put equals on current row with spacing that matches source formatting
            // Try to maintain consistent spacing (typically 1-2 spaces before =)
            context.AdvanceByMinimum(1);
            context.Append("=");
        }

        // Render type annotation with location preservation
        RenderTypeAnnotation(typeAlias.TypeAnnotation, context);
    }

    private static void RenderCustomType(
        TypeStruct typeStruct,
        RenderContext context)
    {
        // Render documentation comment if present
        if (typeStruct.Documentation is { } docComment)
        {
            // Append doc comment (this updates position automatically)
            context.Append(docComment.Value);
            // Move to next line after doc comment
            context.Output.Append('\n');
            context.CurrentRow++;
            context.CurrentColumn = 1;
        }

        // Render "type"
        context.Append("type");

        // Advance to name location
        context.AdvanceToLocation(typeStruct.Name.Range.Start);
        var nameEndColumn = typeStruct.Name.Range.End.Column;
        context.Append(typeStruct.Name.Value);

        // Render generics if present
        foreach (var generic in typeStruct.Generics)
        {
            context.AdvanceToLocation(generic.Range.Start);
            context.Append(generic.Value);
            nameEndColumn = generic.Range.End.Column;
        }

        // Position the equals sign based on first constructor location
        if (typeStruct.Constructors.Count > 0)
        {
            var firstConstructor = typeStruct.Constructors[0];
            var firstConstructorLocation = firstConstructor.Value.Name.Range.Start;

            if (firstConstructorLocation.Row == context.CurrentRow)
            {
                // Same row as type name: put "= " on current line before constructor
                context.AdvanceByMinimum(1);
                context.Append("=");
                // Loop will advance to constructor
            }
            else
            {
                // Different row: "=" should be on same row as constructor, 2 columns before it
                var equalsLocation = new Location(firstConstructorLocation.Row, firstConstructorLocation.Column - 2);
                context.AdvanceToLocation(equalsLocation);
                context.Append("=");
                // Loop will advance to constructor
            }
        }
        else
        {
            // No constructors (shouldn't happen)
            context.AdvanceByMinimum(1);
            context.Append("=");
        }

        // Render constructors
        for (var i = 0; i < typeStruct.Constructors.Count; i++)
        {
            var constructor = typeStruct.Constructors[i];

            if (i is 0)
            {
                // First constructor - advance to its location
                context.AdvanceToLocation(constructor.Value.Name.Range.Start, minSpaces: 1);
            }
            else if (i > 0)
            {
                var prevConstructor = typeStruct.Constructors[i - 1];

                // Check if on new line
                if (constructor.Value.Name.Range.Start.Row > prevConstructor.Value.Name.Range.End.Row)
                {
                    // New line - need to position the pipe
                    // Advance to new row and position pipe before constructor name
                    // The pipe should be aligned properly with spacing before the constructor name
                    var constructorRow = constructor.Value.Name.Range.Start.Row;
                    var constructorNameColumn = constructor.Value.Name.Range.Start.Column;

                    // Calculate pipe column: the pattern is typically "    | Name"
                    // So pipe is 2 columns before the constructor name (for "| ")
                    var pipeColumn = constructorNameColumn - 2;

                    context.AdvanceToLocation(new Location(constructorRow, pipeColumn), minSpaces: 0);
                    context.Append("|");

                    // Now advance to constructor name location
                    context.AdvanceToLocation(constructor.Value.Name.Range.Start, minSpaces: 1);
                }
                else
                {
                    // Same line - pipe with spacing
                    context.AdvanceByMinimum(1);
                    context.Append("|");
                    context.AdvanceByMinimum(1);
                }
            }

            // Render constructor name
            context.Append(constructor.Value.Name.Value);

            // Render constructor arguments
            foreach (var arg in constructor.Value.Arguments)
            {
                // RenderTypeAnnotation will handle advancing to arg.Range.Start
                RenderTypeAnnotation(arg, context);
            }
        }
    }

    private static void RenderTypeAnnotation(
        Node<TypeAnnotation> typeAnnotationNode,
        RenderContext context,
        bool skipAdvanceIfDifferentRow = false)
    {
        var typeAnnotation = typeAnnotationNode.Value;

        // Optionally skip advancing if the target is on a different row (handles stale ranges from reformatting)
        if (skipAdvanceIfDifferentRow && typeAnnotationNode.Range.Start.Row != context.CurrentRow)
        {
            // Don't advance - the caller has already positioned us correctly
        }
        else
        {
            context.AdvanceToLocation(typeAnnotationNode.Range.Start);
        }

        switch (typeAnnotation)
        {
            case TypeAnnotation.GenericType generic:
                context.Append(generic.Name);
                break;

            case TypeAnnotation.Typed typed:
                RenderTypedAnnotation(typed, context);
                break;

            case TypeAnnotation.Unit:
                context.Append("()");
                break;

            case TypeAnnotation.Record record:
                RenderRecordDefinition(record.RecordDefinition, context);
                break;

            case TypeAnnotation.FunctionTypeAnnotation funcType:
                // Render the argument type (parens are handled via single-element Tupled in AST)
                RenderTypeAnnotation(funcType.ArgumentType, context);

                // Check if return type is on a new line
                var returnTypeOnNewLine = funcType.ReturnType.Range.Start.Row > context.CurrentRow;

                if (returnTypeOnNewLine)
                {
                    // Place arrow at the beginning of the new line, before the return type
                    // Pattern: "    -> ReturnType"
                    // Arrow is 3 columns before the return type (for "-> ")
                    var arrowColumn = Math.Max(1, funcType.ReturnType.Range.Start.Column - 3);
                    context.AdvanceToLocation(new Location(funcType.ReturnType.Range.Start.Row, arrowColumn), minSpaces: 0);
                    context.Append("->");
                    // Now advance to return type location
                    context.AdvanceToLocation(funcType.ReturnType.Range.Start, minSpaces: 1);
                }
                else
                {
                    // Same line: add spacing before arrow
                    context.AdvanceByMinimum(1);
                    context.Append("->");
                    // Advance to the return type's location
                    context.AdvanceToLocation(funcType.ReturnType.Range.Start, minSpaces: 1);
                }

                // Render the return type (parens are handled via single-element Tupled in AST)
                RenderTypeAnnotation(funcType.ReturnType, context);
                break;

            case TypeAnnotation.Tupled tupled:
                RenderTupledTypeAnnotation(tupled, context);
                break;

            case TypeAnnotation.GenericRecord genericRecord:
                RenderGenericRecordTypeAnnotation(genericRecord, context);
                break;

            default:
                throw new NotImplementedException(
                    $"Unknown type annotation: {typeAnnotation.GetType().Name}");
        }
    }

    private static void RenderTupledTypeAnnotation(
        TypeAnnotation.Tupled tupled,
        RenderContext context)
    {
        // Single-element tuple is just parentheses (no spaces)
        if (tupled.TypeAnnotations.Count is 1)
        {
            context.Append("(");
            RenderTypeAnnotation(tupled.TypeAnnotations[0], context);
            context.Append(")");
            return;
        }

        // Multi-element tuple: ( element1, element2, ... )
        context.Append("(");

        for (var i = 0; i < tupled.TypeAnnotations.Count; i++)
        {
            var typeNode = tupled.TypeAnnotations[i];

            // Advance to this type's location
            context.AdvanceToLocation(typeNode.Range.Start, minSpaces: i is 0 ? 0 : 1);
            RenderTypeAnnotation(typeNode, context);

            if (i < tupled.TypeAnnotations.Count - 1)
            {
                // Add comma immediately after this type (not the last one)
                context.Append(",");
            }
        }

        // Closing paren location is not tracked, so use 1 space
        context.AdvanceByMinimum(1);
        context.Append(")");
    }

    private static void RenderGenericRecordTypeAnnotation(
        TypeAnnotation.GenericRecord genericRecord,
        RenderContext context)
    {
        // Format: { genericName | field1 : type1, field2 : type2 }
        context.Append("{");

        // Advance to generic name location
        context.AdvanceToLocation(genericRecord.GenericName.Range.Start, minSpaces: 1);
        context.Append(genericRecord.GenericName.Value);

        // " | "
        context.AdvanceByMinimum(1);
        context.Append("|");

        // Render fields
        var fields = genericRecord.RecordDefinition.Value.Fields;
        for (var i = 0; i < fields.Count; i++)
        {
            var field = fields[i];

            if (i > 0)
            {
                // Add comma between fields
                context.Append(",");
            }

            // Advance to field name location
            context.AdvanceToLocation(field.Value.FieldName.Range.Start, minSpaces: 1);
            context.Append(field.Value.FieldName.Value);

            // " : "
            context.AdvanceByMinimum(1);
            context.Append(":");

            // Render field type
            RenderTypeAnnotation(field.Value.FieldType, context);
        }

        // Closing brace
        context.AdvanceByMinimum(1);
        context.Append("}");
    }

    private static void RenderTypedAnnotation(
        TypeAnnotation.Typed typed,
        RenderContext context)
    {
        var typeName =
            typed.TypeName.Value.ModuleName.Count > 0
            ?
            RenderModuleName(typed.TypeName.Value.ModuleName) + "." + typed.TypeName.Value.Name
            :
            typed.TypeName.Value.Name;

        context.Append(typeName);

        // Render type arguments
        foreach (var arg in typed.TypeArguments)
        {
            // Check if we should advance to the argument's range location.
            // If the argument is on a different row than current (likely due to Avh4Format
            // reformatting without updating nested ranges), just add minimum spacing.
            var isSameRow = arg.Range.Start.Row == context.CurrentRow;
            var isAheadOnSameRow = isSameRow && arg.Range.Start.Column > context.CurrentColumn;

            if (isAheadOnSameRow)
            {
                // Argument is ahead on the same row - advance to it
                context.AdvanceToLocation(arg.Range.Start);
            }
            else
            {
                // Argument is on a different row or at/behind current position - just add space
                context.AdvanceByMinimum(1);
            }
            // Pass skipAdvanceIfDifferentRow=true for type arguments to handle nested stale ranges
            RenderTypeAnnotation(arg, context, skipAdvanceIfDifferentRow: true);
        }
    }

    private static void RenderRecordDefinition(
        RecordDefinition recordDefinition,
        RenderContext context)
    {
        var openingBraceColumn = context.CurrentColumn;
        var openingBraceRow = context.CurrentRow;
        context.Append("{");

        var isMultiLine = false;
        var firstFieldOnSameLineAsOpening = false;

        // Pre-check the record structure
        if (recordDefinition.Fields.Count > 0)
        {
            var firstField = recordDefinition.Fields[0];

            // Check if first field is on same line as opening brace
            if (firstField.Range.Start.Row == openingBraceRow)
            {
                // Check if opening brace is on its own indented line (pure multi-line)
                // or on the same line as the equals sign (mixed mode)
                // Heuristic: if openingBraceColumn is small (<=10), it's on its own line
                if (openingBraceColumn > 10)
                {
                    firstFieldOnSameLineAsOpening = true;
                }
            }

            // Check if this is a multi-line record
            foreach (var field in recordDefinition.Fields)
            {
                if (field.Range.Start.Row > openingBraceRow)
                {
                    isMultiLine = true;
                    break;
                }
            }
        }

        for (var i = 0; i < recordDefinition.Fields.Count; i++)
        {
            var field = recordDefinition.Fields[i];

            if (i > 0)
            {
                var prevField = recordDefinition.Fields[i - 1];

                // Check if this field is on a different row than the previous one
                if (field.Range.Start.Row > prevField.Range.End.Row)
                {
                    // Multi-line field transition
                    if (firstFieldOnSameLineAsOpening)
                    {
                        // Mixed mode: first field on same line as {, subsequent fields on new lines
                        // Comma goes at end of previous line immediately after the type
                        context.Append(",");
                        // Advance to new line with base indentation (4 spaces)
                        context.AdvanceToLocation(new Location(field.Range.Start.Row, 5), minSpaces: 0);
                    }
                    else
                    {
                        // Pure multi-line: all fields on separate lines
                        // Comma goes at start of new line
                        context.AdvanceToLocation(new Location(field.Range.Start.Row, openingBraceColumn), minSpaces: 0);
                        context.Append(",");
                        // Now advance to the field's actual location
                        context.AdvanceToLocation(field.Value.FieldName.Range.Start, minSpaces: 0);
                    }
                }
                else
                {
                    // Same line: comma goes right after the previous field
                    context.Append(",");
                    // Advance to the field's location
                    context.AdvanceToLocation(field.Value.FieldName.Range.Start, minSpaces: 0);
                }
            }
            else
            {
                // First field: just advance to its location
                context.AdvanceToLocation(field.Value.FieldName.Range.Start, minSpaces: 0);
            }

            // Render field name
            var fieldNameEnd = field.Value.FieldName.Range.End.Column;
            context.Append(field.Value.FieldName.Value);

            // Position the colon based on field type location
            var typeLocation = field.Value.FieldType.Range.Start;

            // Add 1 space before colon, then advance to type location after colon
            context.AdvanceByMinimum(1);
            context.Append(":");
            // Advance to type location (this preserves the original spacing after colon)
            context.AdvanceToLocation(typeLocation, minSpaces: 1);

            // Render field type
            RenderTypeAnnotation(field.Value.FieldType, context);
        }

        // Render closing brace
        if (isMultiLine)
        {
            // For multi-line records, put closing brace on new line with proper indentation
            var indentColumn = firstFieldOnSameLineAsOpening ? 5 : openingBraceColumn;
            context.Append("\n");  // Move to next line
            // Add indentation
            if (indentColumn > 1)
            {
                context.Append(new string(' ', indentColumn - 1));
            }
            context.Append("}");
        }
        else
        {
            // For single-line records, add space before closing brace
            context.AdvanceByMinimum(1);
            context.Append("}");
        }
    }

    private static void RenderFunction(
        FunctionStruct function,
        RenderContext context)
    {
        if (function.Signature is { } signature)
        {
            context.AdvanceToLocation(signature.Range.Start);
            RenderSignature(signature.Value, context);
        }

        var impl = function.Declaration;
        context.AdvanceToLocation(impl.Range.Start);
        context.Append(impl.Value.Name.Value);

        // Render arguments
        foreach (var arg in impl.Value.Arguments)
        {
            // Check if we should advance to the argument's range location.
            // If the argument is on a different row than current (likely due to Avh4Format
            // reformatting without updating nested ranges), just add minimum spacing.
            var isSameRow = arg.Range.Start.Row == context.CurrentRow;
            var isAheadOnSameRow = isSameRow && arg.Range.Start.Column > context.CurrentColumn;

            if (isAheadOnSameRow)
            {
                // Argument is ahead on the same row - advance to it
                context.AdvanceToLocation(arg.Range.Start);
            }
            else
            {
                // Argument is on a different row or at/behind current position - just add space
                context.AdvanceByMinimum(1);
            }
            context.Append(RenderPattern(arg.Value));
        }

        // Render equals sign and expression
        // The equals sign location is not tracked in the syntax model, so use 1 space
        context.AdvanceByMinimum(1);
        context.Append("=");

        // Render the expression with location preservation
        RenderExpression(impl.Value.Expression, context);
    }

    private static void RenderSignature(
        Signature signature,
        RenderContext context)
    {
        // Render function name
        context.Append(signature.Name.Value);

        // Render colon
        context.AdvanceByMinimum(1);
        context.Append(":");

        // Render type annotation with location preservation
        RenderTypeAnnotation(signature.TypeAnnotation, context);
    }

    private static void RenderExpression(
        Node<Expression> expressionNode,
        RenderContext context)
    {
        var expr = expressionNode.Value;

        context.AdvanceToLocation(expressionNode.Range.Start);

        switch (expr)
        {
            case Expression.UnitExpr:
                context.Append("()");
                break;

            case Expression.Literal literal:
                context.Append(RenderStringLiteral(literal.Value));
                break;

            case Expression.CharLiteral charLit:
                context.Append(RenderCharLiteral(charLit.Value));
                break;

            case Expression.Integer integer:
                context.Append(integer.Value.ToString());
                break;

            case Expression.Hex hex:
                context.Append(RenderHexPattern(hex.Value));
                break;

            case Expression.Floatable floatable:
                context.Append(FormatFloatForElm(floatable.Value));
                break;

            case Expression.Negation negation:
                context.Append("-");
                RenderExpression(negation.Expression, context);
                break;

            case Expression.TupledExpression tupled:
                RenderTupledExpression(expressionNode, tupled, context);
                break;

            case Expression.ListExpr listExpr:
                RenderListExpr(expressionNode, listExpr, context);
                break;

            case Expression.Application app:
                RenderApplication(app, context);
                break;

            case Expression.RecordExpr recordExpr:
                RenderRecordExpr(expressionNode, recordExpr, context);
                break;

            case Expression.RecordUpdateExpression recordUpdate:
                RenderRecordUpdateExpr(expressionNode, recordUpdate, context);
                break;

            case Expression.OperatorApplication opApp:
                RenderOperatorApplication(opApp, context);
                break;

            case Expression.CaseExpression caseExpr:
                RenderCaseExpression(expressionNode, caseExpr.CaseBlock, context);
                break;

            case Expression.LetExpression letExpr:
                RenderLetExpression(letExpr.Value, context);
                break;

            case Expression.IfBlock ifBlock:
                RenderIfExpression(ifBlock, context);
                break;

            case Expression.LambdaExpression lambdaExpr:
                RenderLambdaExpression(lambdaExpr.Lambda, context);
                break;

            case Expression.FunctionOrValue funcOrVal:
                context.Append(funcOrVal.ModuleName.Count > 0
                    ? RenderModuleName(funcOrVal.ModuleName) + "." + funcOrVal.Name
                    : funcOrVal.Name);
                break;

            case Expression.RecordAccessFunction recordAccess:
                context.Append(recordAccess.FunctionName);
                break;

            case Expression.ParenthesizedExpression parenExpr:
                context.Append("(");
                RenderExpression(parenExpr.Expression, context);
                // Closing paren location is at the end of the parenthesized expression's range
                var closingParenLocation = new Location(expressionNode.Range.End.Row, expressionNode.Range.End.Column - 1);
                context.AdvanceToLocation(closingParenLocation, minSpaces: 1);
                context.Append(")");
                break;

            case Expression.PrefixOperator prefixOp:
                context.Append("(");
                context.Append(prefixOp.Operator);
                context.Append(")");
                break;

            case Expression.RecordAccess recordAccess:
                RenderExpression(recordAccess.Record, context);
                context.Append(".");
                context.Append(recordAccess.FieldName.Value);
                break;

            default:
                throw new NotImplementedException(
                    $"Unknown expression type: {expr.GetType().Name}");
        }
    }

    private static void RenderApplication(
        Expression.Application app,
        RenderContext context)
    {
        // Render each argument (including the function as the first argument)
        for (var i = 0; i < app.Arguments.Count; i++)
        {
            var arg = app.Arguments[i];

            if (i > 0)
            {
                // Advance to the argument's location
                context.AdvanceToLocation(arg.Range.Start, minSpaces: 1);
            }

            RenderExpression(arg, context);
        }
    }

    private static void RenderOperatorApplication(
        Expression.OperatorApplication opApp,
        RenderContext context)
    {
        // Render left operand
        RenderExpression(opApp.Left, context);

        // Check if right operand starts on a different row than where we currently are
        // Also ensure right operand is actually after current position (clamping handles backwards locations)
        if (opApp.Right.Range.Start.Row > context.CurrentRow)
        {
            // Multi-line case: operator should be on the same line as right operand
            // Calculate position: operator goes before the right operand
            // Position it at (right operand column - operator length - 1 space)
            // Ensure column doesn't go negative by using Math.Max
            var operatorColumn = Math.Max(1, opApp.Right.Range.Start.Column - opApp.Operator.Length - 1);
            var operatorLocation = new Location(opApp.Right.Range.Start.Row, operatorColumn);

            context.AdvanceToLocation(operatorLocation, minSpaces: 0);
            context.Append(opApp.Operator);

            // Advance to right operand (should be 1 space after operator)
            context.AdvanceToLocation(opApp.Right.Range.Start, minSpaces: 1);
        }
        else
        {
            // Same line case: operator goes 1 space after left operand
            context.AdvanceByMinimum(1);
            context.Append(opApp.Operator);

            // Advance to right operand
            // AdvanceToLocation handles clamping if right operand precedes current position
            context.AdvanceToLocation(opApp.Right.Range.Start, minSpaces: 1);
        }

        RenderExpression(opApp.Right, context);
    }

    private static void RenderTupledExpression(
        Node<Expression> tupledExprNode,
        Expression.TupledExpression tupled,
        RenderContext context)
    {
        var openingParenColumn = context.CurrentColumn;

        context.Append("(");

        var previousItemRow = tupledExprNode.Range.Start.Row;

        for (var i = 0; i < tupled.Elements.Count; i++)
        {
            var element = tupled.Elements[i];

            if (i > 0)
            {
                var linebreak =
                    element.Range.Start.Row > previousItemRow;

                var commaColumn =
                    linebreak
                    ?
                    openingParenColumn
                    :
                    context.CurrentColumn;

                context.AdvanceToLocation(new Location(element.Range.Start.Row, commaColumn), minSpaces: 0);

                context.Append(",");

                // Now advance to the element's actual location
                context.AdvanceToLocation(element.Range.Start, minSpaces: 1);
            }
            else
            {
                // First element: just advance to its location
                context.AdvanceToLocation(element.Range.Start, minSpaces: 1);
            }

            RenderExpression(element, context);

            // Update previousItemRow to track the last rendered item's ending row
            previousItemRow = element.Range.End.Row;
        }

        {
            // Advance to the closing paren position (one column before the tuple's Range.End)
            var closingParenLocation = new Location(tupledExprNode.Range.End.Row, tupledExprNode.Range.End.Column - 1);

            var linebreak =
                previousItemRow < closingParenLocation.Row;

            context.AdvanceToLocation(closingParenLocation, minSpaces: linebreak ? 0 : 1);
            context.Append(")");
        }
    }

    private static void RenderListExpr(
        Node<Expression> listExprNode,
        Expression.ListExpr listExpr,
        RenderContext context)
    {
        var openingBracketColumn = context.CurrentColumn;

        context.Append("[");

        var previousItemRow = listExprNode.Range.Start.Row;

        for (var i = 0; i < listExpr.Elements.Count; i++)
        {
            var item = listExpr.Elements[i];

            if (i > 0)
            {
                var linebreak =
                    item.Range.Start.Row > previousItemRow;

                var commaColumn =
                    linebreak
                    ?
                    openingBracketColumn
                    :
                    context.CurrentColumn;

                context.AdvanceToLocation(new Location(item.Range.Start.Row, commaColumn), minSpaces: 0);

                context.Append(",");

                // Now advance to the element's actual location
                context.AdvanceToLocation(item.Range.Start, minSpaces: 0);
            }
            else
            {
                // First element: just advance to its location
                context.AdvanceToLocation(item.Range.Start, minSpaces: 1);
            }

            RenderExpression(item, context);

            // Update previousItemRow to track the last rendered item's ending row
            previousItemRow = item.Range.End.Row;
        }

        {
            // Advance to the closing bracket position (one column before the list's Range.End)
            var closingBracketLocation = new Location(listExprNode.Range.End.Row, listExprNode.Range.End.Column - 1);

            var linebreak =
                previousItemRow < closingBracketLocation.Row;

            context.AdvanceToLocation(closingBracketLocation, minSpaces: linebreak ? 0 : 1);
            context.Append("]");
        }
    }

    private static void RenderRecordExpr(
        Node<Expression> recordNode,
        Expression.RecordExpr recordExpr,
        RenderContext context)
    {
        context.Append("{");

        // Check if fields span multiple rows to determine comma placement
        var isMultiLine = false;
        if (recordExpr.Fields.Count > 1)
        {
            var firstFieldRow = recordExpr.Fields[0].Range.Start.Row;
            var lastFieldRow = recordExpr.Fields[recordExpr.Fields.Count - 1].Range.Start.Row;
            isMultiLine = lastFieldRow > firstFieldRow;
        }

        for (var i = 0; i < recordExpr.Fields.Count; i++)
        {
            var fieldNode = recordExpr.Fields[i];
            var (fieldName, valueExpr) = fieldNode.Value;

            // For multi-line records, check if comma should come before this field
            if (isMultiLine && i > 0)
            {
                var currentFieldRow = fieldNode.Range.Start.Row;
                var prevFieldRow = recordExpr.Fields[i - 1].Range.Start.Row;

                if (currentFieldRow > prevFieldRow)
                {
                    // Comma at the beginning of the new line
                    // Check if the field node's range starts before the field name
                    // If so, it likely includes the comma location
                    Location commaLocation;
                    if (fieldNode.Range.Start.Column < fieldName.Range.Start.Column)
                    {
                        // Field range includes leading content (comma), use it
                        commaLocation = fieldNode.Range.Start;
                    }
                    else
                    {
                        // Field range doesn't include comma, calculate it
                        // Standard pattern is ", fieldname" so comma is 2 columns before field name
                        commaLocation = new Location(
                            fieldName.Range.Start.Row,
                            fieldName.Range.Start.Column - 2);
                    }
                    context.AdvanceToLocation(commaLocation, minSpaces: 0);
                    context.Append(",");
                }
            }

            // Advance to field name location
            context.AdvanceToLocation(fieldName.Range.Start, minSpaces: 1);
            context.Append(fieldName.Value);

            // Add space before equals (not tracked in syntax model, so use 1 space)
            context.AdvanceByMinimum(1);
            context.Append("=");

            // Advance to value expression location
            context.AdvanceToLocation(valueExpr.Range.Start, minSpaces: 1);
            RenderExpression(valueExpr, context);

            // For non-multi-line or same-row fields, add comma immediately after
            if (i < recordExpr.Fields.Count - 1 && !isMultiLine)
            {
                context.Append(",");
            }
            else if (i < recordExpr.Fields.Count - 1 && isMultiLine)
            {
                var currentFieldRow = fieldNode.Range.Start.Row;
                var nextFieldRow = recordExpr.Fields[i + 1].Range.Start.Row;

                if (currentFieldRow == nextFieldRow)
                {
                    // Same row: comma immediately after
                    context.Append(",");
                }
                // Otherwise, comma will be placed at the beginning of the next line
            }
        }

        // Closing brace position is calculated from the record's overall range
        // The closing brace is at the last column of the range
        var closingBraceLocation = new Location(recordNode.Range.End.Row, recordNode.Range.End.Column - 1);
        context.AdvanceToLocation(closingBraceLocation, minSpaces: 1);
        context.Append("}");
    }

    private static void RenderRecordUpdateExpr(
        Node<Expression> recordNode,
        Expression.RecordUpdateExpression recordUpdate,
        RenderContext context)
    {
        context.Append("{");

        // Advance to record name location
        context.AdvanceToLocation(recordUpdate.RecordName.Range.Start, minSpaces: 1);
        context.Append(recordUpdate.RecordName.Value);

        // Check if this is a multi-line record update
        var isMultiLine = recordUpdate.Fields.Count > 0 &&
                          recordUpdate.Fields[0].Value.fieldName.Range.Start.Row > recordUpdate.RecordName.Range.End.Row;

        if (isMultiLine)
        {
            // Multi-line: pipe is on a new line
            var firstFieldName = recordUpdate.Fields[0].Value.fieldName;

            // Place pipe 2 columns before the first field name (for "| ")
            var pipeLocation = new Location(
                firstFieldName.Range.Start.Row,
                firstFieldName.Range.Start.Column - 2);
            context.AdvanceToLocation(pipeLocation, minSpaces: 1);
            context.Append("|");
        }
        else
        {
            // Single-line: pipe with space after record name
            context.AdvanceByMinimum(1);
            context.Append("|");
        }

        for (var i = 0; i < recordUpdate.Fields.Count; i++)
        {
            var fieldNode = recordUpdate.Fields[i];
            var (fieldName, valueExpr) = fieldNode.Value;

            if (isMultiLine && i > 0)
            {
                // Multi-line: comma at start of new line (2 columns before the field)
                var commaLocation = new Location(
                    fieldName.Range.Start.Row,
                    fieldName.Range.Start.Column - 2);
                context.AdvanceToLocation(commaLocation, minSpaces: 1);
                context.Append(",");
            }

            // Advance to field name location
            context.AdvanceToLocation(fieldName.Range.Start, minSpaces: 1);
            context.Append(fieldName.Value);

            // Add space before equals (not tracked in syntax model, so use 1 space)
            context.AdvanceByMinimum(1);
            context.Append("=");

            // Advance to value expression location
            context.AdvanceToLocation(valueExpr.Range.Start, minSpaces: 1);
            RenderExpression(valueExpr, context);

            if (!isMultiLine && i < recordUpdate.Fields.Count - 1)
            {
                // Single-line: add comma immediately after this field (not the last one)
                context.Append(",");
            }
        }

        // Closing brace position is calculated from the record's overall range
        // The closing brace is at the last column of the range
        var closingBraceLocation = new Location(recordNode.Range.End.Row, recordNode.Range.End.Column - 1);
        context.AdvanceToLocation(closingBraceLocation, minSpaces: 1);
        context.Append("}");
    }

    private static void RenderCaseExpression(
        Node<Expression> caseExprNode,
        CaseBlock caseBlock,
        RenderContext context)
    {
        // Render "case "
        context.Append("case");
        context.AdvanceByMinimum(1);

        // Render the scrutinee expression
        RenderExpression(caseBlock.Expression, context);

        // Render " of" (not tracked, so use 1 space)
        context.AdvanceByMinimum(1);
        context.Append("of");

        // Render each case branch
        for (var i = 0; i < caseBlock.Cases.Count; i++)
        {
            var caseItem = caseBlock.Cases[i];

            // Advance to pattern location
            context.AdvanceToLocation(caseItem.Pattern.Range.Start, minSpaces: 1);
            context.Append(RenderPattern(caseItem.Pattern.Value));

            // Render " ->" (not tracked, so use 1 space)
            context.AdvanceByMinimum(1);
            context.Append("->");

            // Advance to expression location
            context.AdvanceToLocation(caseItem.Expression.Range.Start, minSpaces: 1);
            RenderExpression(caseItem.Expression, context);
        }
    }

    private static void RenderLetExpression(
        Expression.LetBlock letBlock,
        RenderContext context)
    {
        // Render "let" keyword
        context.Append("let");

        // Render each declaration
        for (var i = 0; i < letBlock.Declarations.Count; i++)
        {
            var declNode = letBlock.Declarations[i];
            var decl = declNode.Value;

            // Advance to declaration location
            context.AdvanceToLocation(declNode.Range.Start, minSpaces: 1);

            switch (decl)
            {
                case Expression.LetDeclaration.LetFunction letFunc:
                    RenderLetFunction(letFunc, context);
                    break;

                case Expression.LetDeclaration.LetDestructuring letDestr:
                    // Render pattern
                    context.Append(RenderPattern(letDestr.Pattern.Value));
                    // Render " =" (not tracked, so use 1 space)
                    context.AdvanceByMinimum(1);
                    context.Append("=");
                    // Advance to expression location
                    context.AdvanceToLocation(letDestr.Expression.Range.Start, minSpaces: 1);
                    RenderExpression(letDestr.Expression, context);
                    break;

                default:
                    throw new NotImplementedException($"Unknown let declaration type: {decl.GetType().Name}");
            }
        }

        // Render "in" keyword
        // The "in" should be on the line before the expression (or same line if expression is indented enough)
        // We'll place it so that advancing to the expression location will put us at the right spot
        // If expression is on a different row, "in" goes on the row before at the same column as expression minus length("in ")
        if (letBlock.Expression.Range.Start.Row > context.CurrentRow)
        {
            // Multi-line case: "in" on its own line
            // Calculate column for "in" based on where expression will be
            // Expression is at column C, so "in" should be at roughly the same indentation
            // We'll put "in" at column (C - 0) since they're on different lines
            var inRow = letBlock.Expression.Range.Start.Row - 1;
            var inCol = letBlock.Expression.Range.Start.Column;
            context.AdvanceToLocation(new Location(inRow, inCol), minSpaces: 1);
            context.Append("in");

            // Now advance to expression location
            context.AdvanceToLocation(letBlock.Expression.Range.Start, minSpaces: 0);
        }
        else
        {
            // Same line case: "in" goes 1 space after last declaration
            context.AdvanceByMinimum(1);
            context.Append("in");
            context.AdvanceToLocation(letBlock.Expression.Range.Start, minSpaces: 1);
        }
        RenderExpression(letBlock.Expression, context);
    }

    private static void RenderLetFunction(
        Expression.LetDeclaration.LetFunction letFunc,
        RenderContext context)
    {
        // Render signature if present
        if (letFunc.Function.Signature is not null)
        {
            var sig = letFunc.Function.Signature.Value;
            context.Append(sig.Name.Value);
            context.AdvanceByMinimum(1);
            context.Append(":");
            context.AdvanceToLocation(sig.TypeAnnotation.Range.Start, minSpaces: 1);
            RenderTypeAnnotation(sig.TypeAnnotation, context);

            // After signature, advance to the implementation location
            context.AdvanceToLocation(letFunc.Function.Declaration.Range.Start, minSpaces: 1);
        }

        var impl = letFunc.Function.Declaration.Value;

        // Render function name
        context.Append(impl.Name.Value);

        // Render arguments
        foreach (var arg in impl.Arguments)
        {
            context.AdvanceByMinimum(1);
            context.Append(RenderPattern(arg.Value));
        }

        // Render " =" (not tracked, so use 1 space)
        context.AdvanceByMinimum(1);
        context.Append("=");

        // Advance to expression location
        context.AdvanceToLocation(impl.Expression.Range.Start, minSpaces: 1);
        RenderExpression(impl.Expression, context);
    }

    private static void RenderIfExpression(
        Expression.IfBlock ifBlock,
        RenderContext context)
    {
        // Render "if" first
        context.Append("if");
        var ifRow = context.CurrentRow;

        // Check if this is multiline format by seeing if condition starts on a different row than "if"
        // This handles cases where:
        // 1. The condition expression itself spans multiple lines
        // 2. There's a comment before the condition, pushing it to a new line
        var isConditionMultiLine = ifBlock.Condition.Range.Start.Row != ifRow ||
                                   ifBlock.Condition.Range.Start.Row != ifBlock.Condition.Range.End.Row;

        // Render condition
        if (isConditionMultiLine)
        {
            // For multiline conditions, advance to the condition's start location
            // This will add the necessary newline and indentation
            context.AdvanceToLocation(ifBlock.Condition.Range.Start, minSpaces: 1);
        }
        else
        {
            // Single-line condition: just add a space
            context.AdvanceByMinimum(1);
        }
        RenderExpression(ifBlock.Condition, context);

        // Render "then"
        if (isConditionMultiLine)
        {
            // For multiline conditions, "then" should be on its own line aligned with "if"
            // "then" column should be 4 less than condition column (dedented to same level as "if")
            var thenCol = ifBlock.Condition.Range.Start.Column > 4
                ? ifBlock.Condition.Range.Start.Column - 4
                : 1;

            // Smart positioning of "then" keyword based on context:
            // We need to determine where "then" goes by examining comments in the gap
            // between condition.End and thenBlock.Start
            //
            // The formatter positions comments differently based on whether they're
            // "before then" or "after then":
            // - "Before then" comments: at row Condition.End.Row + 1 (immediate next row)
            // - "After then" comments: at row >= Condition.End.Row + 2 (leaving room for "then")
            //
            // Strategy:
            // 1. Comments at row Condition.End.Row + 1 are "before then" → then goes AFTER
            // 2. Comments at row > Condition.End.Row + 1 are "after then" → then goes BEFORE
            //
            // We scan comments in row order. While we see comments at consecutive rows
            // starting from Condition.End.Row + 1, they're "before then". As soon as we
            // see a gap (or run out of comments), that's where "then" goes.

            var thenRow = ifBlock.Condition.Range.End.Row + 1;

            // Collect comments in the gap between condition and then-block
            var commentsInGap = context.Comments
                .Where(c => c.Range.Start.Row > ifBlock.Condition.Range.End.Row &&
                           c.Range.Start.Row < ifBlock.ThenBlock.Range.Start.Row)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            // Advance thenRow past any "before then" comments
            // These are comments that form a consecutive block starting at thenRow
            foreach (var comment in commentsInGap)
            {
                if (comment.Range.Start.Row == thenRow)
                {
                    // This comment is right where we'd put "then" - it's a "before then" comment
                    // Advance thenRow past this comment
                    thenRow = comment.Range.End.Row + 1;
                }
                else
                {
                    // There's a gap - this comment is "after then"
                    // Stop, don't advance thenRow further
                    break;
                }
            }

            context.AdvanceToLocation(new Location(thenRow, thenCol), minSpaces: 1);
            context.Append("then");
        }
        else
        {
            // Single-line condition: "then" follows with a space
            context.AdvanceByMinimum(1);
            context.Append("then");
        }

        // Advance to then block location
        context.AdvanceToLocation(ifBlock.ThenBlock.Range.Start, minSpaces: 1);
        RenderExpression(ifBlock.ThenBlock, context);

        // Render "else" keyword
        // Check if the else block is an if expression (for "else if" pattern)
        var isElseIf = ifBlock.ElseBlock.Value is Expression.IfBlock;

        // Strategy for positioning keywords without location info:
        // Find the gap left by the formatter in the collection of comments.
        // The formatter positions comments around keywords, leaving gaps where keywords should go.

        if (ifBlock.ElseBlock.Range.Start.Row > context.CurrentRow)
        {
            // Multi-line case
            // Calculate else column: same as "if" (dedented from else block content)
            var elseCol = ifBlock.ElseBlock.Range.Start.Column > 4
                ? ifBlock.ElseBlock.Range.Start.Column - 4
                : ifBlock.ElseBlock.Range.Start.Column;

            // Find "else" position by looking for the gap in comments
            // "else" goes on the first row after then-block that doesn't have a comment
            // (the formatter leaves a blank line, then puts "else")
            var elseRow = ifBlock.ThenBlock.Range.End.Row + 2; // Default: blank line + else

            // Collect comments in the gap between then-block and else-block
            var commentsInGap = context.Comments
                .Where(c => c.Range.Start.Row > ifBlock.ThenBlock.Range.End.Row &&
                           c.Range.Start.Row < ifBlock.ElseBlock.Range.Start.Row)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            if (isElseIf && commentsInGap.Count is not 0)
            {
                // For "else if" with comments between else and if:
                // Both "else" and "if" are at the same column level
                // The formatter positions "if" at the dedented level (same as "else")
                // So elseCol = ElseBlock.Range.Start.Column (not dedented further)
                var elseColForCommentCase = ifBlock.ElseBlock.Range.Start.Column;

                context.AdvanceToLocation(new Location(elseRow, elseColForCommentCase), minSpaces: 1);
                context.Append("else");

                // Now advance to else block location (the nested if)
                // The renderer will output comments as it advances
                context.AdvanceToLocation(ifBlock.ElseBlock.Range.Start, minSpaces: 0);
            }
            else if (isElseIf)
            {
                // No comments - keep "else if" on the same line
                // The if location points to the "i" in "if", so "else " should be 5 characters before it
                var elseLocation = new Location(
                    ifBlock.ElseBlock.Range.Start.Row,
                    ifBlock.ElseBlock.Range.Start.Column - 5);
                context.AdvanceToLocation(elseLocation, minSpaces: 1);
                context.Append("else ");
            }
            else
            {
                // Regular else: place "else" on the row before the first element after else
                // 
                // The formatter positions content in this sequence:
                // 1. Then block content (e.g., `[ 13 ]`)
                // 2. Blank line (optional)
                // 3. "else" keyword
                // 4. Comments (optional) - these appear between "else" and the else expression
                // 5. Else block content (e.g., `[ 17 ]`)
                //
                // The commentsInGap collection contains comments between then-block and else-block.
                // If there are comments, they appear AFTER "else" but BEFORE the else expression.
                // So we place "else" on the row before the first comment (or the else expression if no comments).

                if (commentsInGap.Count is not 0)
                {
                    // There are comments between then and else blocks
                    // "else" goes on the row before the first comment
                    elseRow = commentsInGap[0].Range.Start.Row - 1;
                }
                else
                {
                    // No comments - "else" goes on the row before else block
                    elseRow = ifBlock.ElseBlock.Range.Start.Row - 1;
                }

                context.AdvanceToLocation(new Location(elseRow, elseCol), minSpaces: 1);
                context.Append("else");

                // Now advance to else block location
                context.AdvanceToLocation(ifBlock.ElseBlock.Range.Start, minSpaces: 0);
            }
        }
        else
        {
            // Same line case: "else" goes 1 space after then block
            context.AdvanceByMinimum(1);
            context.Append("else");
            context.AdvanceToLocation(ifBlock.ElseBlock.Range.Start, minSpaces: 1);
        }
        RenderExpression(ifBlock.ElseBlock, context);
    }

    private static void RenderLambdaExpression(
        LambdaStruct lambda,
        RenderContext context)
    {
        // Render opening backslash (no opening paren - that comes from source location if present)
        context.Append("\\");

        // Render arguments
        for (var i = 0; i < lambda.Arguments.Count; i++)
        {
            var arg = lambda.Arguments[i];

            // Advance to argument location (first argument gets 1 space minimum after backslash)
            context.AdvanceToLocation(arg.Range.Start, minSpaces: 1);
            context.Append(RenderPattern(arg.Value));
        }

        // Render " ->" (not tracked, so use 1 space)
        context.AdvanceByMinimum(1);
        context.Append("->");

        // Render expression (RenderExpression will advance to its location)
        RenderExpression(lambda.Expression, context);

        // No closing parenthesis here - that should come from source location if present
    }

    private static string RenderModuleName(IReadOnlyList<string> moduleName) =>
        string.Join(".", moduleName);

    private static string RenderTopLevelExpose(TopLevelExpose expose)
    {
        return expose switch
        {
            TopLevelExpose.InfixExpose infix =>
            $"({infix.Name})",

            TopLevelExpose.FunctionExpose func =>
            func.Name,

            TopLevelExpose.TypeOrAliasExpose typeOrAlias =>
            typeOrAlias.Name,

            TopLevelExpose.TypeExpose typeExpose =>
            typeExpose.ExposedType.Open is not null
            ?
            $"{typeExpose.ExposedType.Name}(..)"
            :
            typeExpose.ExposedType.Name,

            _ =>
            throw new NotImplementedException(
                $"Unknown expose type: {expose.GetType().Name}")
        };
    }

    private static string RenderInfix(Infix infix)
    {
        var direction = infix.Direction.Value switch
        {
            InfixDirection.Left =>
            "left ",  // Pad with space to align with "right"

            InfixDirection.Right =>
            "right",

            InfixDirection.Non =>
            "non  ",   // Pad with spaces to align with "right"

            _ =>
            throw new NotImplementedException(
                $"Unknown infix direction: {infix.Direction.Value}")
        };

        return $"infix {direction} {infix.Precedence.Value} ({infix.Operator.Value}) = {infix.FunctionName.Value}";
    }

    private static string RenderStringLiteral(string value)
    {
        // Check if multi-line string
        if (value.Contains('\n'))
        {
            return "\"\"\"" + value + "\"\"\"";
        }

        // Escape special characters
        var escaped = value
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t");

        return "\"" + escaped + "\"";
    }

    internal static string RenderCharLiteral(int value)
    {
        var c = char.ConvertFromUtf32(value);

        if (c is "'")
            return "'\\''";

        if (c is "\\")
            return "'\\\\'";

        if (c is "\n")
            return "'\\n'";

        if (c is "\r")
            return "'\\u{000D}'";  // elm-format uses Unicode escape for carriage return

        if (c is "\t")
            return "'\\t'";

        return "'" + c + "'";
    }

    /// <summary>
    /// Formats a hexadecimal pattern with padding to match elm-format behavior.
    /// Pads to lengths of 2, 4, 8, or multiples of 8 (e.g., 8, 16, 32...).
    /// </summary>
    internal static string RenderHexPattern(long value)
    {
        // Convert to hex string (uppercase)
        var hex = value.ToString("X");

        // Determine target length: 2, 4, 8, 16, 32, etc.
        // Matches elm-format padding behavior

        int targetLength;

        if (hex.Length <= 2)
            targetLength = 2;
        else if (hex.Length <= 4)
            targetLength = 4;
        else if (hex.Length <= 8)
            targetLength = 8;
        else
            // Round up to next multiple of 8
            targetLength = (hex.Length + 7) / 8 * 8;

        // Pad with leading zeros
        var paddedHex = hex.PadLeft(targetLength, '0');

        return "0x" + paddedHex;
    }

    internal static string FormatFloatForElm(double value)
    {
        // Use "R" (round-trip) format which produces the shortest representation
        // that parses back to the same value
        var str = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture);

        if (str.Contains('E') || str.Contains('e'))
        {
            // Convert scientific notation to fixed-point decimal
            // The format "F17" uses fixed-point with up to 17 digits
            str = value.ToString("F17", System.Globalization.CultureInfo.InvariantCulture).TrimEnd('0');

            // Make sure we don't end with just a decimal point
            if (str.EndsWith('.'))
            {
                str += "0";
            }
        }

        // Elm requires at least one digit after the decimal point
        if (!str.Contains('.'))
        {
            str += ".0";
        }

        return str;
    }

    private static string RenderPattern(Pattern pattern)
    {
        return pattern switch
        {
            Pattern.AllPattern =>
            "_",

            Pattern.VarPattern varPat =>
            varPat.Name,

            Pattern.UnitPattern =>
            "()",

            Pattern.CharPattern charPat =>
            RenderCharLiteral(charPat.Value),

            Pattern.StringPattern stringPat =>
            RenderStringLiteral(stringPat.Value),

            Pattern.IntPattern intPat =>
            intPat.Value.ToString(),

            Pattern.HexPattern hexPat =>
            RenderHexPattern(hexPat.Value),

            Pattern.FloatPattern floatPat =>
            floatPat.Value.ToString(System.Globalization.CultureInfo.InvariantCulture),

            Pattern.TuplePattern tuplePat =>
            "( " + string.Join(", ", tuplePat.Elements.Select(e => RenderPattern(e.Value))) + " )",

            Pattern.RecordPattern recordPat =>
            "{ " + string.Join(", ", recordPat.Fields.Select(f => f.Value)) + " }",

            Pattern.UnConsPattern unconsPat =>
            RenderPatternParenthesizedIfNeeded(unconsPat.Head.Value) + " :: " + RenderPattern(unconsPat.Tail.Value),

            Pattern.ListPattern listPat =>
            listPat.Elements.Count is 0
            ?
            "[]"
            :
            "[ " + string.Join(", ", listPat.Elements.Select(e => RenderPattern(e.Value))) + " ]",

            Pattern.NamedPattern namedPat =>
            RenderNamedPattern(namedPat),

            Pattern.AsPattern asPat =>
            RenderPatternParenthesizedIfNeeded(asPat.Pattern.Value) + " as " + asPat.Name.Value,

            Pattern.ParenthesizedPattern parenPat =>
            "(" + RenderPattern(parenPat.Pattern.Value) + ")",

            _ =>
            throw new NotImplementedException(
                $"Unknown pattern type: {pattern.GetType().Name}")
        };
    }

    private static string RenderNamedPattern(Pattern.NamedPattern namedPat)
    {
        var name =
            namedPat.Name.ModuleName.Count > 0
            ?
            RenderModuleName(namedPat.Name.ModuleName) + "." + namedPat.Name.Name
            :
            namedPat.Name.Name;

        if (namedPat.Arguments.Count is 0)
            return name;

        return name + " " + string.Join(" ", namedPat.Arguments.Select(a => RenderPatternParenthesizedIfNeeded(a.Value)));
    }

    private static string RenderPatternParenthesizedIfNeeded(Pattern pattern)
    {
        return pattern switch
        {
            Pattern.NamedPattern namedPat when namedPat.Arguments.Count > 0 =>
                "(" + RenderPattern(pattern) + ")",
            Pattern.AsPattern => "(" + RenderPattern(pattern) + ")",
            Pattern.UnConsPattern => "(" + RenderPattern(pattern) + ")",
            _ => RenderPattern(pattern)
        };
    }
}
