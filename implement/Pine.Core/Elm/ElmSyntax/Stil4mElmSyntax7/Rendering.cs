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
    /// Represents configuration options for mapping qualified names using a custom function.
    /// </summary>
    /// <remarks>Use this record to supply a custom mapping strategy for qualified names when processing or
    /// transforming them. If no mapping function is provided, qualified names remain unchanged.</remarks>
    /// <param name="MapQualifiedName">A delegate that defines how to transform a <see cref="QualifiedNameRef"/> instance.
    /// If <see langword="null"/>, no mapping is applied.
    /// </param>
    public record Config(
        Func<QualifiedNameRef, QualifiedNameRef>? MapQualifiedName);

    /// <summary>
    /// Preserves input source locations during rendering.
    /// </summary>
    public static Config ConfigPreserveLocations(
        Func<QualifiedNameRef, QualifiedNameRef>? mapQualifiedName = null) =>
        new(
            MapQualifiedName: mapQualifiedName);

    /// <summary>
    /// Preserves input source locations during rendering.
    /// </summary>
    public static Config ConfigPreserveLocations(
        IReadOnlyDictionary<QualifiedNameRef, QualifiedNameRef> mapQualifiedName) =>
        new(
            MapQualifiedName: originalQualifiedName =>
                mapQualifiedName.TryGetValue(originalQualifiedName, out var mappedQualifiedName)
                ? mappedQualifiedName
                : originalQualifiedName);

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
                // If spacesToAdd == 0, we're already at the target, don't add spaces
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
    public static string ToString(
        File file,
        Config config)
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
            RenderDeclaration(declaration, context, config);
        }

        return context.Output.ToString();
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
        RenderContext context,
        Config config)
    {
        switch (declarationNode.Value)
        {
            case Declaration.FunctionDeclaration funcDecl:
                RenderFunction(funcDecl.Function, context, config);
                break;

            case Declaration.CustomTypeDeclaration customType:
                RenderCustomType(customType.TypeDeclaration, context, config);
                break;

            case Declaration.AliasDeclaration aliasDecl:
                RenderTypeAlias(aliasDecl.TypeAlias, context, config);
                break;

            case Declaration.PortDeclaration portDecl:
                context.Append("port " + RenderSignature(portDecl.Signature, config));
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
        RenderContext context,
        Config config)
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
        RenderTypeAnnotation(typeAlias.TypeAnnotation, context, config);
    }

    private static void RenderCustomType(
        TypeStruct typeStruct,
        RenderContext context,
        Config config)
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
                RenderTypeAnnotation(arg, context, config);
            }
        }
    }

    private static void RenderTypeAnnotation(
        Node<TypeAnnotation> typeAnnotationNode,
        RenderContext context,
        Config config,
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
                RenderTypedAnnotation(typed, context, config);
                break;

            case TypeAnnotation.Unit:
                context.Append("()");
                break;

            case TypeAnnotation.Record record:
                RenderRecordDefinition(record.RecordDefinition, context, config);
                break;

            case TypeAnnotation.FunctionTypeAnnotation funcType:
                // Render the argument type (parens are handled via single-element Tupled in AST)
                RenderTypeAnnotation(funcType.ArgumentType, context, config);

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
                RenderTypeAnnotation(funcType.ReturnType, context, config);
                break;

            case TypeAnnotation.Tupled tupled:
                RenderTupledTypeAnnotation(tupled, context, config);
                break;

            default:
                // For other types, fall back to simple rendering
                context.Append(RenderTypeAnnotation(typeAnnotation, config));
                break;
        }
    }

    private static void RenderTupledTypeAnnotation(
        TypeAnnotation.Tupled tupled,
        RenderContext context,
        Config config)
    {
        // Single-element tuple is just parentheses (no spaces)
        if (tupled.TypeAnnotations.Count == 1)
        {
            context.Append("(");
            RenderTypeAnnotation(tupled.TypeAnnotations[0], context, config);
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
            RenderTypeAnnotation(typeNode, context, config);

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

    private static void RenderTypedAnnotation(
        TypeAnnotation.Typed typed,
        RenderContext context,
        Config config)
    {
        var originalQualifiedName =
            new QualifiedNameRef(
                ModuleName: typed.TypeName.Value.ModuleName,
                Name: typed.TypeName.Value.Name);

        var mappedQualifiedName =
            config.MapQualifiedName is { } mapQualifiedName
            ?
            mapQualifiedName(originalQualifiedName)
            :
            originalQualifiedName;

        var typeName =
            mappedQualifiedName.ModuleName.Count > 0
            ?
            RenderModuleName(mappedQualifiedName.ModuleName) + "." + mappedQualifiedName.Name
            :
            mappedQualifiedName.Name;

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
            RenderTypeAnnotation(arg, context, config, skipAdvanceIfDifferentRow: true);
        }
    }

    private static void RenderRecordDefinition(
        RecordDefinition recordDefinition,
        RenderContext context,
        Config config)
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
            RenderTypeAnnotation(field.Value.FieldType, context, config);
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
        RenderContext context,
        Config config)
    {
        if (function.Signature is { } signature)
        {
            context.AdvanceToLocation(signature.Range.Start);
            RenderSignature(signature.Value, context, config);
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
        RenderExpression(impl.Value.Expression, context, config);
    }

    private static void RenderSignature(
        Signature signature,
        RenderContext context,
        Config config)
    {
        // Render function name
        context.Append(signature.Name.Value);

        // Render colon
        context.AdvanceByMinimum(1);
        context.Append(":");

        // Render type annotation with location preservation
        RenderTypeAnnotation(signature.TypeAnnotation, context, config);
    }

    private static void RenderExpression(
        Node<Expression> expressionNode,
        RenderContext context,
        Config config)
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
                context.Append("0x" + hex.Value.ToString("X"));
                break;

            case Expression.Floatable floatable:
                context.Append(FormatFloatForElm(floatable.Value));
                break;

            case Expression.Negation negation:
                context.Append("-");
                RenderExpression(negation.Expression, context, config);
                break;

            case Expression.TupledExpression tupled:
                RenderTupledExpression(expressionNode, tupled, context, config);
                break;

            case Expression.ListExpr listExpr:
                RenderListExpr(expressionNode, listExpr, context, config);
                break;

            case Expression.Application app:
                RenderApplication(app, context, config);
                break;

            case Expression.RecordExpr recordExpr:
                RenderRecordExpr(expressionNode, recordExpr, context, config);
                break;

            case Expression.RecordUpdateExpression recordUpdate:
                RenderRecordUpdateExpr(expressionNode, recordUpdate, context, config);
                break;

            case Expression.OperatorApplication opApp:
                RenderOperatorApplication(opApp, context, config);
                break;

            case Expression.CaseExpression caseExpr:
                RenderCaseExpression(expressionNode, caseExpr.CaseBlock, context, config);
                break;

            case Expression.LetExpression letExpr:
                RenderLetExpression(letExpr.Value, context, config);
                break;

            case Expression.IfBlock ifBlock:
                RenderIfExpression(ifBlock, context, config);
                break;

            case Expression.LambdaExpression lambdaExpr:
                RenderLambdaExpression(lambdaExpr.Lambda, context, config);
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
                RenderExpression(parenExpr.Expression, context, config);
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
                RenderExpression(recordAccess.Record, context, config);
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
        RenderContext context,
        Config config)
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

            RenderExpression(arg, context, config);
        }
    }

    private static void RenderOperatorApplication(
        Expression.OperatorApplication opApp,
        RenderContext context,
        Config config)
    {
        // Render left operand
        RenderExpression(opApp.Left, context, config);

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

        RenderExpression(opApp.Right, context, config);
    }

    private static void RenderTupledExpression(
        Node<Expression> tupledExprNode,
        Expression.TupledExpression tupled,
        RenderContext context,
        Config config)
    {
        context.Append("(");

        for (var i = 0; i < tupled.Elements.Count; i++)
        {
            var element = tupled.Elements[i];

            if (i > 0)
            {
                // Add comma (no space before it)
                context.Append(",");
            }

            // Advance to element location
            context.AdvanceToLocation(element.Range.Start, minSpaces: 1);
            RenderExpression(element, context, config);
        }

        // Closing paren location is at the end of the tupled expression's range
        var closingParenLocation = new Location(tupledExprNode.Range.End.Row, tupledExprNode.Range.End.Column - 1);
        context.AdvanceToLocation(closingParenLocation, minSpaces: 1);
        context.Append(")");
    }

    private static void RenderListExpr(
        Node<Expression> listExprNode,
        Expression.ListExpr listExpr,
        RenderContext context,
        Config config)
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

            RenderExpression(item, context, config);

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
        RenderContext context,
        Config config)
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
            RenderExpression(valueExpr, context, config);

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
        RenderContext context,
        Config config)
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
            RenderExpression(valueExpr, context, config);

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
        RenderContext context,
        Config config)
    {
        // Render "case "
        context.Append("case");
        context.AdvanceByMinimum(1);

        // Render the scrutinee expression
        RenderExpression(caseBlock.Expression, context, config);

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
            RenderExpression(caseItem.Expression, context, config);
        }
    }

    private static void RenderLetExpression(
        Expression.LetBlock letBlock,
        RenderContext context,
        Config config)
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
                    RenderLetFunction(letFunc, context, config);
                    break;

                case Expression.LetDeclaration.LetDestructuring letDestr:
                    // Render pattern
                    context.Append(RenderPattern(letDestr.Pattern.Value));
                    // Render " =" (not tracked, so use 1 space)
                    context.AdvanceByMinimum(1);
                    context.Append("=");
                    // Advance to expression location
                    context.AdvanceToLocation(letDestr.Expression.Range.Start, minSpaces: 1);
                    RenderExpression(letDestr.Expression, context, config);
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
        RenderExpression(letBlock.Expression, context, config);
    }

    private static void RenderLetFunction(
        Expression.LetDeclaration.LetFunction letFunc,
        RenderContext context,
        Config config)
    {
        // Render signature if present
        if (letFunc.Function.Signature is not null)
        {
            var sig = letFunc.Function.Signature.Value;
            context.Append(sig.Name.Value);
            context.AdvanceByMinimum(1);
            context.Append(":");
            context.AdvanceToLocation(sig.TypeAnnotation.Range.Start, minSpaces: 1);
            context.Append(RenderTypeAnnotation(sig.TypeAnnotation.Value, config));

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
        RenderExpression(impl.Expression, context, config);
    }

    private static void RenderIfExpression(
        Expression.IfBlock ifBlock,
        RenderContext context,
        Config config)
    {
        // Render "if "
        context.Append("if");
        context.AdvanceByMinimum(1);

        // Render condition
        RenderExpression(ifBlock.Condition, context, config);

        // Render " then" (not tracked, so use 1 space)
        context.AdvanceByMinimum(1);
        context.Append("then");

        // Advance to then block location
        context.AdvanceToLocation(ifBlock.ThenBlock.Range.Start, minSpaces: 1);
        RenderExpression(ifBlock.ThenBlock, context, config);

        // Render "else" keyword
        // Check if the else block is an if expression (for "else if" pattern)
        var isElseIf = ifBlock.ElseBlock.Value is Expression.IfBlock;

        // Similar to "in" in let expressions, "else" positioning depends on layout
        // In multi-line if expressions, "else" typically aligns with "if" (not the else block)
        // But we don't track "if" location, so we'll use a heuristic:
        // If else block is indented more than 4 columns, "else" goes 4 columns to the left of else block
        if (ifBlock.ElseBlock.Range.Start.Row > context.CurrentRow)
        {
            // Multi-line case: "else" on its own line or same line as "if" for "else if"
            if (isElseIf)
            {
                // For "else if", keep them on the same line
                // The if location points to the "i" in "if", so "else " should be 5 characters before it
                var elseLocation = new Location(
                    ifBlock.ElseBlock.Range.Start.Row,
                    ifBlock.ElseBlock.Range.Start.Column - 5);
                context.AdvanceToLocation(elseLocation, minSpaces: 1);
                context.Append("else ");
            }
            else
            {
                // Regular else: "else" on its own line
                // Calculate column for "else" - typically less indented than the else block
                var elseRow = ifBlock.ElseBlock.Range.Start.Row - 1;
                // Heuristic: if else block is indented (col > 4), place "else" 4 columns earlier
                var elseCol = ifBlock.ElseBlock.Range.Start.Column > 4
                    ? ifBlock.ElseBlock.Range.Start.Column - 4
                    : ifBlock.ElseBlock.Range.Start.Column;
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
        RenderExpression(ifBlock.ElseBlock, context, config);
    }

    private static void RenderLambdaExpression(
        LambdaStruct lambda,
        RenderContext context,
        Config config)
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
        RenderExpression(lambda.Expression, context, config);

        // No closing parenthesis here - that should come from source location if present
    }

    private static string RenderModuleName(IReadOnlyList<string> moduleName) =>
        string.Join(".", moduleName);

    private static string RenderExposing(Exposing exposing)
    {
        return exposing switch
        {
            Exposing.All => "(..)",

            Exposing.Explicit explicit_ =>
                "(" + string.Join(", ", explicit_.Nodes.Select(n => RenderTopLevelExpose(n.Value))) + ")",

            _ =>
            throw new NotImplementedException(
                $"Unknown exposing type: {exposing.GetType().Name}")
        };
    }

    private static string RenderTopLevelExpose(TopLevelExpose expose)
    {
        return expose switch
        {
            TopLevelExpose.InfixExpose infix => $"({infix.Name})",
            TopLevelExpose.FunctionExpose func => func.Name,
            TopLevelExpose.TypeOrAliasExpose typeOrAlias => typeOrAlias.Name,
            TopLevelExpose.TypeExpose typeExpose =>
                typeExpose.ExposedType.Open is not null
                    ? $"{typeExpose.ExposedType.Name}(..)"
                    : typeExpose.ExposedType.Name,
            _ => throw new NotImplementedException($"Unknown expose type: {expose.GetType().Name}")
        };
    }

    private static string RenderImport(Import import)
    {
        var sb = new StringBuilder();
        sb.Append("import ");
        sb.Append(RenderModuleName(import.ModuleName.Value));

        if (import.ModuleAlias is not null)
        {
            sb.Append(" as ");
            sb.Append(RenderModuleName(import.ModuleAlias.Value));
        }

        if (import.ExposingList is not null)
        {
            sb.Append(" exposing ");
            sb.Append(RenderExposing(import.ExposingList.Value));
        }

        return sb.ToString();
    }

    private static string RenderSignature(
        Signature signature,
        Config config) =>
        $"{signature.Name.Value} : {RenderTypeAnnotation(signature.TypeAnnotation.Value, config)}";

    private static string RenderTypeAnnotation(
        TypeAnnotation typeAnnotation,
        Config config)
    {
        return typeAnnotation switch
        {
            TypeAnnotation.GenericType generic => generic.Name,

            TypeAnnotation.Typed typed =>
            RenderTypedAnnotation(typed, config),

            TypeAnnotation.Unit => "()",

            TypeAnnotation.Tupled tupled when tupled.TypeAnnotations.Count == 1 =>
            // Single-element tuple is just parentheses
            "(" + RenderTypeAnnotation(tupled.TypeAnnotations[0].Value, config) + ")",

            TypeAnnotation.Tupled tupled =>
            "( " + string.Join(", ", tupled.TypeAnnotations.Select(t => RenderTypeAnnotation(t.Value, config))) + " )",

            TypeAnnotation.Record record =>
            RenderRecordDefinition(record.RecordDefinition, config),

            TypeAnnotation.GenericRecord genericRecord =>
            "{ " + genericRecord.GenericName.Value +
            " | " +
            RenderRecordFields(genericRecord.RecordDefinition.Value, config) + " }",

            TypeAnnotation.FunctionTypeAnnotation funcType =>
            RenderFunctionTypeAnnotation(funcType, config),

            _ =>
            throw new NotImplementedException(
                $"Unknown type annotation: {typeAnnotation.GetType().Name}")
        };
    }

    private static string RenderTypedAnnotation(
        TypeAnnotation.Typed typed,
        Config config)
    {
        var originalQualifiedName =
            new QualifiedNameRef(
                ModuleName: typed.TypeName.Value.ModuleName,
                Name: typed.TypeName.Value.Name);

        var mappedQualifiedName =
            config.MapQualifiedName is { } mapQualifiedName
            ?
            mapQualifiedName(originalQualifiedName)
            :
            originalQualifiedName;

        var typeName =
            mappedQualifiedName.ModuleName.Count > 0
            ?
            RenderModuleName(mappedQualifiedName.ModuleName) + "." + mappedQualifiedName.Name
            :
            mappedQualifiedName.Name;

        if (typed.TypeArguments.Count is 0)
            return typeName;

        return
            typeName +
            " " +
            string.Join(" ", typed.TypeArguments.Select(a => RenderTypeAnnotationParenthesized(a.Value, config)));
    }

    private static string RenderTypeAnnotationParenthesized(
        TypeAnnotation typeAnnotation,
        Config config)
    {
        // Wrap complex types in parentheses
        return typeAnnotation switch
        {
            TypeAnnotation.FunctionTypeAnnotation =>
            "(" + RenderTypeAnnotation(typeAnnotation, config) + ")",

            TypeAnnotation.Typed typed when typed.TypeArguments.Count > 0 =>
            "(" + RenderTypeAnnotation(typeAnnotation, config) + ")",

            _ =>
            RenderTypeAnnotation(typeAnnotation, config)
        };
    }

    private static string RenderFunctionTypeAnnotation(
        TypeAnnotation.FunctionTypeAnnotation funcType,
        Config config)
    {
        // Argument needs parens only if it's a function type (for precedence)
        // Typed with args (like Array a) doesn't need parens - type application binds tighter than ->
        var argStr = funcType.ArgumentType.Value switch
        {
            TypeAnnotation.FunctionTypeAnnotation =>
                "(" + RenderTypeAnnotation(funcType.ArgumentType.Value, config) + ")",
            _ =>
                RenderTypeAnnotation(funcType.ArgumentType.Value, config)
        };

        // Return type rendering
        var retStr = RenderTypeAnnotation(funcType.ReturnType.Value, config);

        return argStr + " -> " + retStr;
    }

    private static string RenderRecordDefinition(
        RecordDefinition recordDefinition,
        Config config)
    {
        if (recordDefinition.Fields.Count is 0)
            return "{}";

        return "{ " + RenderRecordFields(recordDefinition, config) + " }";
    }

    private static string RenderRecordFields(
        RecordDefinition recordDefinition,
        Config config) =>
        string.Join(", ", recordDefinition.Fields.Select(f =>
            f.Value.FieldName.Value + " : " + RenderTypeAnnotation(f.Value.FieldType.Value, config)));

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

    private static string RenderExpression(Expression expression)
    {
        return expression switch
        {
            Expression.UnitExpr => "()",

            Expression.Literal literal =>
            RenderStringLiteral(literal.Value),

            Expression.CharLiteral charLiteral =>
            RenderCharLiteral(charLiteral.Value),

            Expression.Integer integer =>
            integer.Value.ToString(),

            Expression.Hex hex =>
            "0x" + hex.Value.ToString("X"),

            Expression.Floatable floatable =>
            FormatFloatForElm(floatable.Value),

            Expression.Negation negation =>
            "-" + RenderExpressionParenthesizedIfNeeded(negation.Expression.Value),

            Expression.ListExpr listExpr =>
            listExpr.Elements.Count is 0
            ?
            "[]"
            :
            "[ " + string.Join(", ", listExpr.Elements.Select(e => RenderExpression(e.Value))) + " ]",

            Expression.FunctionOrValue funcOrVal =>
            funcOrVal.ModuleName.Count > 0
            ?
            RenderModuleName(funcOrVal.ModuleName) + "." + funcOrVal.Name
            :
            funcOrVal.Name,

            Expression.IfBlock ifBlock =>
            "if " + RenderExpression(ifBlock.Condition.Value) +
            " then " + RenderExpression(ifBlock.ThenBlock.Value) +
            " else " + RenderExpression(ifBlock.ElseBlock.Value),

            Expression.PrefixOperator prefixOp => "(" + prefixOp.Operator + ")",

            Expression.ParenthesizedExpression paren =>
                "(" + RenderExpression(paren.Expression.Value) + ")",

            Expression.Application app =>
                string.Join(" ", app.Arguments.Select(a => RenderExpressionParenthesizedIfNeeded(a.Value))),

            Expression.OperatorApplication opApp =>
                RenderExpression(opApp.Left.Value) + " " + opApp.Operator + " " + RenderExpression(opApp.Right.Value),

            Expression.TupledExpression tupled =>
                "( " + string.Join(", ", tupled.Elements.Select(e => RenderExpression(e.Value))) + " )",

            Expression.LambdaExpression lambda =>
                "\\" + string.Join(" ", lambda.Lambda.Arguments.Select(a => RenderPattern(a.Value))) +
                " -> " + RenderExpression(lambda.Lambda.Expression.Value),

            Expression.CaseExpression caseExpr =>
                RenderCaseExpression(caseExpr.CaseBlock),

            Expression.LetExpression letExpr =>
                RenderLetExpression(letExpr.Value),

            Expression.RecordExpr recordExpr =>
                RenderRecordExpr(recordExpr),

            Expression.RecordAccess recordAccess =>
                RenderExpressionParenthesizedIfNeeded(recordAccess.Record.Value) + "." + recordAccess.FieldName.Value,

            Expression.RecordAccessFunction accessFunc => accessFunc.FunctionName,

            Expression.RecordUpdateExpression recordUpdate =>
                "{ " + recordUpdate.RecordName.Value + " | " +
                string.Join(", ", recordUpdate.Fields.Select(f =>
                    f.Value.fieldName.Value + " = " + RenderExpression(f.Value.valueExpr.Value))) + " }",

            _ =>
            throw new NotImplementedException(
                $"Unknown expression type: {expression.GetType().Name}")
        };
    }

    private static string RenderExpressionParenthesizedIfNeeded(Expression expression)
    {
        // Wrap complex expressions in parentheses when they appear as arguments
        return expression switch
        {
            Expression.Application =>
            "(" + RenderExpression(expression) + ")",

            Expression.OperatorApplication =>
            "(" + RenderExpression(expression) + ")",

            Expression.IfBlock =>
            "(" + RenderExpression(expression) + ")",

            Expression.CaseExpression =>
            "(" + RenderExpression(expression) + ")",

            Expression.LetExpression =>
            "(" + RenderExpression(expression) + ")",

            Expression.LambdaExpression =>
            "(" + RenderExpression(expression) + ")",
            Expression.Negation => "(" + RenderExpression(expression) + ")",

            _ =>
            RenderExpression(expression)
        };
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

    internal static string RenderHexPattern(long value)
    {
        // Convert to hex string (uppercase)
        var hex = value.ToString("X");

        // Determine target length: 2, 4, 8, 12, 16, etc.
        // Matches elm-format padding behavior
        int targetLength;
        if (hex.Length <= 2)
            targetLength = 2;
        else if (hex.Length <= 4)
            targetLength = 4;
        else
            // Round up to next multiple of 4
            targetLength = ((hex.Length + 3) / 4) * 4;

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

    private static string RenderCaseExpression(CaseBlock caseBlock)
    {
        var sb = new StringBuilder();
        sb.Append("case ");
        sb.Append(RenderExpression(caseBlock.Expression.Value));
        sb.Append(" of ");

        for (var i = 0; i < caseBlock.Cases.Count; i++)
        {
            var case_ = caseBlock.Cases[i];
            sb.Append(RenderPattern(case_.Pattern.Value));
            sb.Append(" -> ");
            sb.Append(RenderExpression(case_.Expression.Value));

            if (i < caseBlock.Cases.Count - 1)
            {
                sb.Append(' ');
            }
        }

        return sb.ToString();
    }

    private static string RenderLetExpression(Expression.LetBlock letBlock)
    {
        var sb = new StringBuilder();
        sb.Append("let ");

        foreach (var decl in letBlock.Declarations)
        {
            sb.Append(RenderLetDeclaration(decl.Value));
            sb.Append(' ');
        }

        sb.Append("in ");
        sb.Append(RenderExpression(letBlock.Expression.Value));

        return sb.ToString();
    }

    private static string RenderLetDeclaration(Expression.LetDeclaration letDecl)
    {
        return letDecl switch
        {
            Expression.LetDeclaration.LetFunction letFunc =>
                RenderLetFunction(letFunc),

            Expression.LetDeclaration.LetDestructuring letDestr =>
                RenderPattern(letDestr.Pattern.Value) + " = " + RenderExpression(letDestr.Expression.Value),

            _ => throw new NotImplementedException($"Unknown let declaration type: {letDecl.GetType().Name}")
        };
    }

    private static string RenderLetFunction(Expression.LetDeclaration.LetFunction letFunc)
    {
        var impl = letFunc.Function.Declaration.Value;
        var header = impl.Name.Value;
        if (impl.Arguments.Count > 0)
        {
            header += " " + string.Join(" ", impl.Arguments.Select(a => RenderPattern(a.Value)));
        }
        return header + " = " + RenderExpression(impl.Expression.Value);
    }

    private static string RenderRecordExpr(Expression.RecordExpr recordExpr)
    {
        if (recordExpr.Fields.Count is 0)
            return "{}";

        return "{ " + string.Join(", ", recordExpr.Fields.Select(f =>
            f.Value.fieldName.Value + " = " + RenderExpression(f.Value.valueExpr.Value))) + " }";
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
        var name = namedPat.Name.ModuleName.Count > 0
            ? RenderModuleName(namedPat.Name.ModuleName) + "." + namedPat.Name.Name
            : namedPat.Name.Name;

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
