using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

public class Rendering
{
    /// <summary>
    /// Represents a line with an indent level (where each level is 4 spaces) and content.
    /// The indent is stored separately to allow efficient string building at the root.
    /// </summary>
    public readonly record struct IndentedLine(
        int IndentLevel,
        string Content)
    {
        public static readonly IndentedLine Empty =
            new(0, "");

        public IndentedLine IndentFurther(int additionalLevels) =>
            new(IndentLevel + additionalLevels, Content);
    }

    /// <summary>
    /// Centralized enum for expression complexity classification.
    /// Determines whether expressions need multi-line rendering.
    /// </summary>
    public enum ExpressionComplexity
    {
        /// <summary>Expression can be rendered on a single line.</summary>
        Simple,

        /// <summary>Expression requires multi-line rendering.</summary>
        Complex
    }

    public record Config(
        LineBreakingConfig LineBreaking,
        Func<QualifiedNameRef, QualifiedNameRef>? MapQualifiedName);

    public abstract record LineBreakingConfig()
    {
        public static LineBreakingConfig SnapshotTestsDefault =>
            new CanonicalBasedOnComplexity();

        /// <summary>
        /// Insert line breaks based on the complexity of expressions.
        /// </summary>
        public sealed record CanonicalBasedOnComplexity
            : LineBreakingConfig;

        /// <summary>
        /// Preserves the original input line locations.
        /// </summary>
        /// <remarks>Use this type when it is important to maintain the mapping between output lines and
        /// their corresponding positions in the input source. This can be useful for scenarios such as diagnostics,
        /// error reporting, or source mapping, where accurate line location information is required.</remarks>
        public sealed record PreserveInputLocations
            : LineBreakingConfig;
    }

    /// <summary>
    /// Do not use any location information from the given nodes, but normalize
    /// all locations for a consistent layout.
    /// 
    /// <para>
    /// The output should be stable with the use of elm-format.
    /// </para>
    /// </summary>
    public static Config ConfigNormalizeAllLocations(
        LineBreakingConfig lineBreaking,
        Func<QualifiedNameRef, QualifiedNameRef>? mapQualifiedName = null) =>
        new(
            LineBreaking: lineBreaking,
            MapQualifiedName: mapQualifiedName);

    /// <summary>
    /// Preserves input source locations during rendering.
    /// </summary>
    public static Config ConfigPreserveLocations(
        Func<QualifiedNameRef, QualifiedNameRef>? mapQualifiedName = null) =>
        new(
            LineBreaking: new LineBreakingConfig.PreserveInputLocations(),
            MapQualifiedName: mapQualifiedName);

    /// <summary>
    /// Do not use any location information from the given nodes, but normalize
    /// all locations for a consistent layout.
    /// 
    /// <para>
    /// The output should be stable with the use of elm-format.
    /// </para>
    /// </summary>
    public static Config ConfigNormalizeAllLocations(
        LineBreakingConfig lineBreaking,
        IReadOnlyDictionary<QualifiedNameRef, QualifiedNameRef> mapQualifiedName) =>
        new(
            LineBreaking: lineBreaking,
            MapQualifiedName: qn =>
            mapQualifiedName.TryGetValue(qn, out var mapped) ? mapped : qn);

    public static string ToString(
        File file,
        Config config)
    {
        if (config.LineBreaking is LineBreakingConfig.PreserveInputLocations)
        {
            return ToStringPreservingLocations(file, config);
        }

        var indentedLines = ToIndentedLines(file, config);

        var sb = new StringBuilder();
        var isFirst = true;

        foreach (var line in indentedLines)
        {
            if (!isFirst)
            {
                sb.Append('\n');
            }

            isFirst = false;

            // Append indent spaces (4 spaces per level) and content in one go
            var indentSpaces = line.IndentLevel * 4;
            if (indentSpaces > 0)
            {
                sb.Append(' ', indentSpaces);
            }

            sb.Append(line.Content);
        }

        return sb.ToString();
    }

    /// <summary>
    /// Context for tracking position while rendering with location preservation.
    /// </summary>
    private class RenderContext
    {
        public StringBuilder Output { get; } = new();
        public int CurrentRow { get; set; } = 1;
        public int CurrentColumn { get; set; } = 1;

        /// <summary>
        /// Advances to the given location by adding spaces or newlines as needed.
        /// If the target location is before the current position, uses the minimum spacing instead.
        /// </summary>
        public void AdvanceToLocation(Location targetLocation, int minSpaces = 1)
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
                if (ch == '\n')
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
    private static string ToStringPreservingLocations(
        File file,
        Config config)
    {
        var context = new RenderContext();

        // Render module definition
        RenderModulePreservingLocations(file.ModuleDefinition, context);

        // Render imports
        foreach (var import in file.Imports)
        {
            context.AdvanceToLocation(import.Range.Start);
            RenderImportPreservingLocations(import, context);
        }

        // Render declarations
        foreach (var declaration in file.Declarations)
        {
            context.AdvanceToLocation(declaration.Range.Start);
            RenderDeclarationPreservingLocations(declaration, context, config);
        }

        return context.Output.ToString();
    }

    private static void RenderImportPreservingLocations(
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
            RenderExposingPreservingLocations(exposingList, context);
        }
    }

    private static void RenderModulePreservingLocations(
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
        RenderExposingPreservingLocations(exposingList, context);
    }

    private static void RenderExposingPreservingLocations(
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
                    // For multi-line exposing lists, the opening paren should be placed one column before
                    // the first item. For single-line, it should be right after "exposing" with a space.
                    // We can detect this by checking if the first item is on a different row than current position.
                    if (explicit_.Nodes.Count > 0)
                    {
                        var firstItemLocation = explicit_.Nodes[0].Range.Start;

                        // Check if the first item is on a different row (multi-line list)
                        if (firstItemLocation.Row > context.CurrentRow)
                        {
                            // Multi-line: place opening paren one ROW before the first item, 
                            // at column (firstItem.Column - 1) which gives proper indentation
                            var openParenLocation = new Location(
                                firstItemLocation.Row - 1,
                                firstItemLocation.Column - 1);
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
                    bool isMultiLine = false;
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
                throw new NotImplementedException($"Unknown exposing type: {exposingNode.Value.GetType().Name}");
        }
    }

    private static void RenderDeclarationPreservingLocations(
        Node<Declaration> declarationNode,
        RenderContext context,
        Config config)
    {
        switch (declarationNode.Value)
        {
            case Declaration.FunctionDeclaration funcDecl:
                RenderFunctionPreservingLocations(funcDecl.Function, context, config);
                break;

            case Declaration.CustomTypeDeclaration customType:
                RenderCustomTypePreservingLocations(customType.TypeDeclaration, context, config);
                break;

            case Declaration.AliasDeclaration aliasDecl:
                RenderTypeAliasPreservingLocations(aliasDecl.TypeAlias, context, config);
                break;

            case Declaration.PortDeclaration portDecl:
                context.Append("port " + RenderSignature(portDecl.Signature, config));
                break;

            case Declaration.InfixDeclaration infixDecl:
                context.Append(RenderInfix(infixDecl.Infix));
                break;

            default:
                throw new NotImplementedException($"Unknown declaration type: {declarationNode.Value.GetType().Name}");
        }
    }

    private static void RenderTypeAliasPreservingLocations(
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
        RenderTypeAnnotationPreservingLocations(typeAlias.TypeAnnotation, context, config);
    }

    private static void RenderCustomTypePreservingLocations(
        TypeStruct typeStruct,
        RenderContext context,
        Config config)
    {
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
                // Same row: add minimal space before =, then advance to constructor location after =
                context.AdvanceByMinimum(1);
                context.Append("=");
                // Don't advance yet - will be done in the loop
            }
            else
            {
                // Different row: put equals on current row
                context.AdvanceByMinimum(1);
                context.Append("=");
                // Don't advance yet - will be done in the loop
            }
        }
        else
        {
            // No constructors (shouldn't happen in valid Elm)
            context.AdvanceByMinimum(1);
            context.Append("=");
        }

        // Render constructors
        for (var i = 0; i < typeStruct.Constructors.Count; i++)
        {
            var constructor = typeStruct.Constructors[i];

            if (i == 0)
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
                context.AdvanceToLocation(arg.Range.Start);
                RenderTypeAnnotationPreservingLocations(arg, context, config);
            }
        }
    }

    private static void RenderTypeAnnotationPreservingLocations(
        Node<TypeAnnotation> typeAnnotationNode,
        RenderContext context,
        Config config)
    {
        var typeAnnotation = typeAnnotationNode.Value;

        context.AdvanceToLocation(typeAnnotationNode.Range.Start);

        switch (typeAnnotation)
        {
            case TypeAnnotation.GenericType generic:
                context.Append(generic.Name);
                break;

            case TypeAnnotation.Typed typed:
                RenderTypedAnnotationPreservingLocations(typed, context, config);
                break;

            case TypeAnnotation.Unit:
                context.Append("()");
                break;

            case TypeAnnotation.Record record:
                RenderRecordDefinitionPreservingLocations(record.RecordDefinition, context, config);
                break;

            case TypeAnnotation.FunctionTypeAnnotation funcType:
                RenderTypeAnnotationPreservingLocations(funcType.ArgumentType, context, config);
                // Add spacing before ->
                context.AdvanceByMinimum(1);
                context.Append("->");
                // Advance to the return type's location
                context.AdvanceToLocation(funcType.ReturnType.Range.Start, minSpaces: 1);
                RenderTypeAnnotationPreservingLocations(funcType.ReturnType, context, config);
                break;

            case TypeAnnotation.Tupled tupled:
                RenderTupledTypeAnnotationPreservingLocations(tupled, context, config);
                break;

            default:
                // For other types, fall back to simple rendering
                context.Append(RenderTypeAnnotation(typeAnnotation, config));
                break;
        }
    }

    private static void RenderTupledTypeAnnotationPreservingLocations(
        TypeAnnotation.Tupled tupled,
        RenderContext context,
        Config config)
    {
        context.Append("(");

        for (var i = 0; i < tupled.TypeAnnotations.Count; i++)
        {
            var typeNode = tupled.TypeAnnotations[i];

            // Advance to this type's location
            context.AdvanceToLocation(typeNode.Range.Start, minSpaces: i == 0 ? 0 : 1);
            RenderTypeAnnotationPreservingLocations(typeNode, context, config);

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

    private static void RenderTypedAnnotationPreservingLocations(
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
            context.AdvanceToLocation(arg.Range.Start);
            RenderTypeAnnotationPreservingLocations(arg, context, config);
        }
    }

    private static void RenderRecordDefinitionPreservingLocations(
        RecordDefinition recordDefinition,
        RenderContext context,
        Config config)
    {
        var openingBraceColumn = context.CurrentColumn;
        var openingBraceRow = context.CurrentRow;
        context.Append("{");

        bool isMultiLine = false;
        bool firstFieldOnSameLineAsOpening = false;

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
            RenderTypeAnnotationPreservingLocations(field.Value.FieldType, context, config);
        }

        // Render closing brace
        if (isMultiLine)
        {
            // For multi-line records, put closing brace on new line with proper indentation
            int indentColumn = firstFieldOnSameLineAsOpening ? 5 : openingBraceColumn;
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

    private static void RenderFunctionPreservingLocations(
        FunctionStruct function,
        RenderContext context,
        Config config)
    {
        if (function.Signature is { } signature)
        {
            context.AdvanceToLocation(signature.Range.Start);
            RenderSignaturePreservingLocations(signature.Value, context, config);
        }

        var impl = function.Declaration;
        context.AdvanceToLocation(impl.Range.Start);
        context.Append(impl.Value.Name.Value);

        // Render arguments
        foreach (var arg in impl.Value.Arguments)
        {
            context.AdvanceToLocation(arg.Range.Start);
            context.Append(RenderPattern(arg.Value));
        }

        // Render equals sign and expression
        // The equals sign location is not tracked in the syntax model, so use 1 space
        context.AdvanceByMinimum(1);
        context.Append("=");

        // Render the expression with location preservation
        RenderExpressionPreservingLocations(impl.Value.Expression, context, config);
    }

    private static void RenderSignaturePreservingLocations(
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
        RenderTypeAnnotationPreservingLocations(signature.TypeAnnotation, context, config);
    }

    private static void RenderExpressionPreservingLocations(
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
                var floatStr = floatable.Value.ToString("G", System.Globalization.CultureInfo.InvariantCulture);
                // Ensure decimal point is included for whole numbers
                if (!floatStr.Contains('.') && !floatStr.Contains('E') && !floatStr.Contains('e'))
                {
                    floatStr += ".0";
                }
                context.Append(floatStr);
                break;

            case Expression.Negation negation:
                context.Append("-");
                RenderExpressionPreservingLocations(negation.Expression, context, config);
                break;

            case Expression.TupledExpression tupled:
                RenderTupledExpressionPreservingLocations(expressionNode, tupled, context, config);
                break;

            case Expression.ListExpr listExpr:
                RenderListExprPreservingLocations(expressionNode, listExpr, context, config);
                break;

            case Expression.Application app:
                RenderApplicationPreservingLocations(app, context, config);
                break;

            case Expression.RecordExpr recordExpr:
                RenderRecordExprPreservingLocations(expressionNode, recordExpr, context, config);
                break;

            case Expression.RecordUpdateExpression recordUpdate:
                RenderRecordUpdateExprPreservingLocations(expressionNode, recordUpdate, context, config);
                break;

            case Expression.OperatorApplication opApp:
                RenderOperatorApplicationPreservingLocations(opApp, context, config);
                break;

            case Expression.CaseExpression caseExpr:
                RenderCaseExpressionPreservingLocations(expressionNode, caseExpr.CaseBlock, context, config);
                break;

            case Expression.LetExpression letExpr:
                RenderLetExpressionPreservingLocations(letExpr.Value, context, config);
                break;

            case Expression.IfBlock ifBlock:
                RenderIfExpressionPreservingLocations(ifBlock, context, config);
                break;

            case Expression.LambdaExpression lambdaExpr:
                RenderLambdaExpressionPreservingLocations(lambdaExpr.Lambda, context, config);
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
                RenderExpressionPreservingLocations(parenExpr.Expression, context, config);
                // Closing paren location is at the end of the parenthesized expression's range
                var closingParenLocation = new Location(expressionNode.Range.End.Row, expressionNode.Range.End.Column - 1);
                context.AdvanceToLocation(closingParenLocation, minSpaces: 1);
                context.Append(")");
                break;

            default:
                throw new NotImplementedException(
                    $"Unknown expression type: {expr.GetType().Name}");
        }
    }

    private static void RenderApplicationPreservingLocations(
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

            RenderExpressionPreservingLocations(arg, context, config);
        }
    }

    private static void RenderOperatorApplicationPreservingLocations(
        Expression.OperatorApplication opApp,
        RenderContext context,
        Config config)
    {
        // Render left operand
        RenderExpressionPreservingLocations(opApp.Left, context, config);

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

        RenderExpressionPreservingLocations(opApp.Right, context, config);
    }

    private static void RenderTupledExpressionPreservingLocations(
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
            RenderExpressionPreservingLocations(element, context, config);
        }

        // Closing paren location is at the end of the tupled expression's range
        var closingParenLocation = new Location(tupledExprNode.Range.End.Row, tupledExprNode.Range.End.Column - 1);
        context.AdvanceToLocation(closingParenLocation, minSpaces: 1);
        context.Append(")");
    }

    private static void RenderListExprPreservingLocations(
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

            RenderExpressionPreservingLocations(item, context, config);
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

    private static void RenderRecordExprPreservingLocations(
        Node<Expression> recordNode,
        Expression.RecordExpr recordExpr,
        RenderContext context,
        Config config)
    {
        context.Append("{");

        // Check if fields span multiple rows to determine comma placement
        bool isMultiLine = false;
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
                    // Position comma 4 columns before the field name (for ",   " pattern)
                    var commaLocation = new Location(fieldNode.Range.Start.Row, fieldNode.Range.Start.Column - 4);
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
            RenderExpressionPreservingLocations(valueExpr, context, config);

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

    private static void RenderRecordUpdateExprPreservingLocations(
        Node<Expression> recordNode,
        Expression.RecordUpdateExpression recordUpdate,
        RenderContext context,
        Config config)
    {
        context.Append("{");

        // Advance to record name location
        context.AdvanceToLocation(recordUpdate.RecordName.Range.Start, minSpaces: 1);
        context.Append(recordUpdate.RecordName.Value);

        // Add pipe symbol (not tracked, so use 1 space)
        context.AdvanceByMinimum(1);
        context.Append("|");

        for (var i = 0; i < recordUpdate.Fields.Count; i++)
        {
            var fieldNode = recordUpdate.Fields[i];
            var (fieldName, valueExpr) = fieldNode.Value;

            // Advance to field name location
            context.AdvanceToLocation(fieldName.Range.Start, minSpaces: 1);
            context.Append(fieldName.Value);

            // Add space before equals (not tracked in syntax model, so use 1 space)
            context.AdvanceByMinimum(1);
            context.Append("=");

            // Advance to value expression location
            context.AdvanceToLocation(valueExpr.Range.Start, minSpaces: 1);
            RenderExpressionPreservingLocations(valueExpr, context, config);

            if (i < recordUpdate.Fields.Count - 1)
            {
                // Add comma immediately after this field (not the last one)
                context.Append(",");
            }
        }

        // Closing brace position is calculated from the record's overall range
        // The closing brace is at the last column of the range
        var closingBraceLocation = new Location(recordNode.Range.End.Row, recordNode.Range.End.Column - 1);
        context.AdvanceToLocation(closingBraceLocation, minSpaces: 1);
        context.Append("}");
    }

    private static void RenderCaseExpressionPreservingLocations(
        Node<Expression> caseExprNode,
        CaseBlock caseBlock,
        RenderContext context,
        Config config)
    {
        // Render "case "
        context.Append("case");
        context.AdvanceByMinimum(1);

        // Render the scrutinee expression
        RenderExpressionPreservingLocations(caseBlock.Expression, context, config);

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
            RenderExpressionPreservingLocations(caseItem.Expression, context, config);
        }
    }

    private static void RenderLetExpressionPreservingLocations(
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
                    RenderLetFunctionPreservingLocations(letFunc, context, config);
                    break;

                case Expression.LetDeclaration.LetDestructuring letDestr:
                    // Render pattern
                    context.Append(RenderPattern(letDestr.Pattern.Value));
                    // Render " =" (not tracked, so use 1 space)
                    context.AdvanceByMinimum(1);
                    context.Append("=");
                    // Advance to expression location
                    context.AdvanceToLocation(letDestr.Expression.Range.Start, minSpaces: 1);
                    RenderExpressionPreservingLocations(letDestr.Expression, context, config);
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
        RenderExpressionPreservingLocations(letBlock.Expression, context, config);
    }

    private static void RenderLetFunctionPreservingLocations(
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
        RenderExpressionPreservingLocations(impl.Expression, context, config);
    }

    private static void RenderIfExpressionPreservingLocations(
        Expression.IfBlock ifBlock,
        RenderContext context,
        Config config)
    {
        // Render "if "
        context.Append("if");
        context.AdvanceByMinimum(1);

        // Render condition
        RenderExpressionPreservingLocations(ifBlock.Condition, context, config);

        // Render " then" (not tracked, so use 1 space)
        context.AdvanceByMinimum(1);
        context.Append("then");

        // Advance to then block location
        context.AdvanceToLocation(ifBlock.ThenBlock.Range.Start, minSpaces: 1);
        RenderExpressionPreservingLocations(ifBlock.ThenBlock, context, config);

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
        RenderExpressionPreservingLocations(ifBlock.ElseBlock, context, config);
    }

    private static void RenderLambdaExpressionPreservingLocations(
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

        // Render expression (RenderExpressionPreservingLocations will advance to its location)
        RenderExpressionPreservingLocations(lambda.Expression, context, config);

        // No closing parenthesis here - that should come from source location if present
    }

    public static IEnumerable<IndentedLine> ToIndentedLines(
        File file,
        Config config)
    {
        // Render module definition
        foreach (var line in RenderModuleLines(file.ModuleDefinition.Value))
        {
            yield return line;
        }

        // Render imports
        if (file.Imports.Count > 0)
        {
            yield return IndentedLine.Empty;
            foreach (var import in file.Imports)
            {
                yield return new IndentedLine(0, RenderImport(import.Value));
            }
        }

        // Render declarations with comments
        Declaration? previousDeclaration = null;
        foreach (var declaration in file.Declarations)
        {
            // Determine spacing before this declaration
            if (previousDeclaration is Declaration.InfixDeclaration &&
                declaration.Value is Declaration.InfixDeclaration)
            {
                // Consecutive infix declarations get no blank lines between them
            }
            else
            {
                // All other declarations get two blank lines before them
                yield return IndentedLine.Empty;
                yield return IndentedLine.Empty;
            }

            // Get comments that appear between this declaration start and have a row before next declaration
            var declarationComments = GetCommentsForDeclaration(declaration, file.Comments);

            foreach (var line in RenderDeclarationLinesWithComments(declaration.Value, declarationComments, config))
            {
                yield return line;
            }

            previousDeclaration = declaration.Value;
        }
    }

    private static IReadOnlyList<Node<string>> GetCommentsForDeclaration(
        Node<Declaration> declaration,
        IReadOnlyList<Node<string>> allComments)
    {
        var declRange = declaration.Range;

        // Find comments that start after the declaration starts (inline comments)
        return
            [.. allComments
            .Where(c => c.Range.Start.Row >= declRange.Start.Row && c.Range.Start.Row <= declRange.End.Row)
            .OrderBy(c => c.Range.Start.Row)
            .ThenBy(c => c.Range.Start.Column)];
    }

    private static IEnumerable<IndentedLine> RenderModuleLines(Module module)
    {
        string moduleKeyword;
        Node<IReadOnlyList<string>> moduleName;
        Node<Exposing> exposingList;

        switch (module)
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
                throw new NotImplementedException($"Unknown module type: {module.GetType().Name}");
        }

        var moduleNameStr = RenderModuleName(moduleName.Value);

        foreach (var line in RenderModuleWithExposing(moduleKeyword, moduleNameStr, exposingList.Value))
        {
            yield return line;
        }
    }

    private static IEnumerable<IndentedLine> RenderModuleWithExposing(string moduleKeyword, string moduleName, Exposing exposing)
    {
        if (exposing is Exposing.All)
        {
            yield return new IndentedLine(0, $"{moduleKeyword} {moduleName} exposing (..)");
            yield break;
        }

        if (exposing is Exposing.Explicit explicit_)
        {
            // For explicit exposing with multiple items, use multi-line format
            if (explicit_.Nodes.Count > 1)
            {
                yield return new IndentedLine(0, $"{moduleKeyword} {moduleName} exposing");
                for (var i = 0; i < explicit_.Nodes.Count; i++)
                {
                    var prefix =
                        i is 0
                        ?
                        "( " :
                        ", ";

                    yield return new IndentedLine(1, prefix + RenderTopLevelExpose(explicit_.Nodes[i].Value));
                }
                yield return new IndentedLine(1, ")");
            }
            else
            {
                yield return new IndentedLine(0, $"{moduleKeyword} {moduleName} exposing ({string.Join(", ", explicit_.Nodes.Select(n => RenderTopLevelExpose(n.Value)))})");
            }
            yield break;
        }

        throw new NotImplementedException($"Unknown exposing type: {exposing.GetType().Name}");
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

            _ => throw new NotImplementedException($"Unknown exposing type: {exposing.GetType().Name}")
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

    private static IEnumerable<IndentedLine> RenderDeclarationLinesWithComments(
        Declaration declaration,
        IReadOnlyList<Node<string>> comments,
        Config config)
    {
        switch (declaration)
        {
            case Declaration.CustomTypeDeclaration customType:
                foreach (var line in RenderCustomTypeLines(customType.TypeDeclaration, config, comments))
                {
                    yield return line;
                }
                break;

            default:
                foreach (var line in RenderDeclarationLines(declaration, config))
                {
                    yield return line;
                }
                break;
        }
    }

    private static IEnumerable<IndentedLine> RenderDeclarationLines(Declaration declaration, Config config)
    {
        return declaration switch
        {
            Declaration.FunctionDeclaration funcDecl =>
                RenderFunctionLines(funcDecl.Function, config),

            Declaration.CustomTypeDeclaration customType =>
                RenderCustomTypeLines(customType.TypeDeclaration, config),

            Declaration.AliasDeclaration aliasDecl =>
                RenderAliasLines(aliasDecl.TypeAlias, config),

            Declaration.PortDeclaration portDecl =>
                RenderPortLines(portDecl.Signature, config),

            Declaration.InfixDeclaration infixDecl =>
                [new IndentedLine(0, RenderInfix(infixDecl.Infix))],

            _ =>
            throw new NotImplementedException(
                $"Unknown declaration type: {declaration.GetType().Name}")
        };
    }

    private static IEnumerable<IndentedLine> RenderFunctionLines(FunctionStruct function, Config config)
    {
        if (function.Documentation is { } documentation)
        {
            yield return new IndentedLine(0, documentation.Value);
        }

        if (function.Signature is { } signature)
        {
            yield return new IndentedLine(0, RenderSignature(signature.Value, config));
        }

        var impl = function.Declaration.Value;
        var header = impl.Name.Value;

        if (impl.Arguments.Count > 0)
        {
            header += " " + string.Join(" ", impl.Arguments.Select(a => RenderPattern(a.Value)));
        }

        header += " =";

        yield return new IndentedLine(0, header);

        foreach (var line in RenderExpressionLines(impl.Expression.Value, config, indent: 1))
        {
            yield return line;
        }
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

            TypeAnnotation.Tupled tupled =>
            "( " + string.Join(", ", tupled.TypeAnnotations.Select(t => RenderTypeAnnotation(t.Value, config))) + " )",

            TypeAnnotation.Record record =>
            RenderRecordDefinition(record.RecordDefinition, config),

            TypeAnnotation.GenericRecord genericRecord =>
            "{ " + genericRecord.GenericName.Value +
            " | " +
            RenderRecordFields(genericRecord.RecordDefinition.Value, config) + " }",

            TypeAnnotation.FunctionTypeAnnotation funcType =>
            RenderTypeAnnotationParenthesizedForFunction(funcType.ArgumentType.Value, config) +
            " -> " +
            RenderTypeAnnotation(funcType.ReturnType.Value, config),

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

        return typeName + " " + string.Join(" ", typed.TypeArguments.Select(a => RenderTypeAnnotationParenthesized(a.Value, config)));
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

    private static string RenderTypeAnnotationParenthesizedForFunction(
        TypeAnnotation typeAnnotation,
        Config config)
    {
        // Only wrap function types in parentheses when they appear as arguments to a function type
        return typeAnnotation switch
        {
            TypeAnnotation.FunctionTypeAnnotation =>
            "(" + RenderTypeAnnotation(typeAnnotation, config) + ")",

            _ =>
            RenderTypeAnnotation(typeAnnotation, config)
        };
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

    /// <summary>
    /// Renders a custom type declaration with optional comments between constructors.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderCustomTypeLines(
        TypeStruct typeStruct,
        Config config,
        IReadOnlyList<Node<string>>? comments = null)
    {
        if (typeStruct.Documentation is { } documentation)
        {
            yield return new IndentedLine(0, documentation.Value);
        }

        var header = "type " + typeStruct.Name.Value;

        if (typeStruct.Generics.Count > 0)
        {
            header += " " + string.Join(" ", typeStruct.Generics.Select(g => g.Value));
        }

        if (typeStruct.Constructors.Count is 0)
        {
            yield return new IndentedLine(0, header);
            yield break;
        }

        yield return new IndentedLine(0, header);

        for (var i = 0; i < typeStruct.Constructors.Count; i++)
        {
            var constructor = typeStruct.Constructors[i];
            var prefix = i is 0 ? "= " : "| ";
            var constructorStr = constructor.Value.Name.Value;

            if (constructor.Value.Arguments.Count > 0)
            {
                constructorStr += " " + string.Join(" ", constructor.Value.Arguments.Select(a => RenderTypeAnnotationParenthesized(a.Value, config)));
            }

            yield return new IndentedLine(1, prefix + constructorStr);

            // Render comments between constructors if provided
            if (comments is not null)
            {
                var nextConstructorRow = i + 1 < typeStruct.Constructors.Count
                    ? typeStruct.Constructors[i + 1].Range.Start.Row
                    : int.MaxValue;

                var constructorComments = comments
                    .Where(c =>
                        c.Range.Start.Row > constructor.Range.Start.Row &&
                        c.Range.Start.Row < nextConstructorRow)
                    .OrderBy(c => c.Range.Start.Row);

                foreach (var comment in constructorComments)
                {
                    yield return new IndentedLine(2, comment.Value);
                }
            }
        }
    }

    private static IEnumerable<IndentedLine> RenderAliasLines(
        TypeAlias typeAlias,
        Config config)
    {
        if (typeAlias.Documentation is not null)
        {
            yield return new IndentedLine(0, typeAlias.Documentation.Value);
        }

        var header = "type alias " + typeAlias.Name.Value;
        if (typeAlias.Generics.Count > 0)
        {
            header += " " + string.Join(" ", typeAlias.Generics.Select(g => g.Value));
        }

        header += " =";

        yield return new IndentedLine(0, header);
        yield return new IndentedLine(1, RenderTypeAnnotation(typeAlias.TypeAnnotation.Value, config));
    }

    private static IEnumerable<IndentedLine> RenderPortLines(
        Signature signature,
        Config config)
    {
        yield return new IndentedLine(0, "port " + RenderSignature(signature, config));
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

    private static IEnumerable<IndentedLine> RenderExpressionLines(Expression expression, Config config, int indent)
    {
        // Use centralized complexity check
        if (GetExpressionComplexity(expression, config) is ExpressionComplexity.Complex)
        {
            foreach (var line in RenderMultiLine(expression, config, indent))
            {
                yield return line;
            }
        }
        else
        {
            yield return new IndentedLine(indent, RenderExpression(expression));
        }
    }

    /// <summary>
    /// Centralized complexity evaluation for all expression types.
    /// Returns Complex if the expression requires multi-line rendering.
    /// </summary>
    private static ExpressionComplexity GetExpressionComplexity(Expression expression, Config config)
    {
        if (config.LineBreaking is not LineBreakingConfig.CanonicalBasedOnComplexity)
            return ExpressionComplexity.Simple;

        return expression switch
        {
            Expression.Application app when ApplicationNeedsMultiLine(app) =>
                ExpressionComplexity.Complex,

            Expression.ListExpr listExpr when ListNeedsMultiLine(listExpr) =>
                ExpressionComplexity.Complex,

            Expression.RecordExpr recordExpr when recordExpr.Fields.Count > 0 =>
                ExpressionComplexity.Complex,

            Expression.CaseExpression =>
                ExpressionComplexity.Complex,

            Expression.IfBlock =>
                ExpressionComplexity.Complex,

            Expression.LetExpression =>
                ExpressionComplexity.Complex,

            Expression.OperatorApplication opApp when OperatorApplicationNeedsMultiLine(opApp) =>
                ExpressionComplexity.Complex,

            Expression.LambdaExpression lambda when LambdaBodyNeedsMultiLine(lambda.Lambda.Expression.Value) =>
                ExpressionComplexity.Complex,

            Expression.ParenthesizedExpression paren =>
                GetExpressionComplexity(paren.Expression.Value, config),

            _ =>
                ExpressionComplexity.Simple
        };
    }

    /// <summary>
    /// Unwraps any parenthesized expressions to get to the inner expression.
    /// </summary>
    private static Expression UnwrapParentheses(Expression expr)
    {
        while (expr is Expression.ParenthesizedExpression paren)
        {
            expr = paren.Expression.Value;
        }
        return expr;
    }

    /// <summary>
    /// Determines if an operator application needs multi-line rendering.
    /// </summary>
    private static bool OperatorApplicationNeedsMultiLine(Expression.OperatorApplication opApp) =>
        opApp.Left.Value is Expression.ListExpr listExpr && ListNeedsMultiLine(listExpr);

    /// <summary>
    /// Determines if an application expression needs multi-line rendering.
    /// Function applications are always laid out over multiple lines, so that each argument comes with a line break.
    /// </summary>
    private static bool ApplicationNeedsMultiLine(Expression.Application app) =>
        // Multi-line when there are actual arguments (Arguments[0] is the function, Arguments[1..] are actual arguments)
        app.Arguments.Count > 1;

    /// <summary>
    /// Determines if a lambda body needs multi-line rendering.
    /// </summary>
    private static bool LambdaBodyNeedsMultiLine(Expression body) =>
        body switch
        {
            Expression.Application app => ApplicationNeedsMultiLine(app),
            Expression.ListExpr listExpr => ListNeedsMultiLine(listExpr),
            Expression.RecordExpr recordExpr => RecordNeedsMultiLine(recordExpr),
            Expression.CaseExpression => true,
            Expression.LetExpression => true,
            _ => false
        };

    /// <summary>
    /// Determines if an expression is simple (can stay on one line as an argument).
    /// </summary>
    private static bool IsSimpleExpression(Expression expr) =>
        expr switch
        {
            Expression.Integer => true,
            Expression.UnitExpr => true,
            Expression.FunctionOrValue => true,
            _ => false
        };

    /// <summary>
    /// Determines if a list expression needs multi-line rendering.
    /// </summary>
    private static bool ListNeedsMultiLine(Expression.ListExpr listExpr) =>
        listExpr.Elements.Any(e =>
            e.Value is Expression.Application ||
            e.Value is Expression.ListExpr ||
            e.Value is Expression.OperatorApplication ||
            e.Value is Expression.RecordExpr);

    /// <summary>
    /// Determines if a record expression needs multi-line rendering.
    /// </summary>
    private static bool RecordNeedsMultiLine(Expression.RecordExpr recordExpr)
    {
        if (recordExpr.Fields.Count is 0)
            return false;

        // More than one field always means multi-line
        if (recordExpr.Fields.Count > 1)
            return true;

        // Single field: multi-line if the value is complex
        return recordExpr.Fields.Any(f =>
            f.Value.valueExpr.Value is Expression.ListExpr listExpr && listExpr.Elements.Count > 0 ||
            f.Value.valueExpr.Value is Expression.RecordExpr nestedRecord && RecordNeedsMultiLine(nestedRecord) ||
            f.Value.valueExpr.Value is Expression.Application ||
            f.Value.valueExpr.Value is Expression.OperatorApplication);
    }

    /// <summary>
    /// Checks if a list contains any expression that requires the entire list to use complex multi-line format.
    /// </summary>
    private static bool ListContainsComplexExpression(Expression.ListExpr listExpr) =>
        listExpr.Elements.Any(e =>
            e.Value is Expression.Application app && ApplicationHasListArgument(app));

    /// <summary>
    /// Checks if an application has any list argument.
    /// </summary>
    private static bool ApplicationHasListArgument(Expression.Application app)
    {
        for (var i = 1; i < app.Arguments.Count; i++)
        {
            if (app.Arguments[i].Value is Expression.ListExpr)
                return true;
        }
        return false;
    }

    /// <summary>
    /// Unified multi-line rendering method using pattern matching for all expression types.
    /// This is the single entry point for complex expression rendering.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderMultiLine(Expression expression, Config config, int indent)
    {
        return expression switch
        {
            Expression.Application app => RenderMultiLineApplication(app, config, indent),
            Expression.ListExpr listExpr => RenderMultiLineList(listExpr, config, indent),
            Expression.RecordExpr recordExpr => RenderMultiLineRecord(recordExpr, config, indent),
            Expression.CaseExpression caseExpr => RenderMultiLineCaseExpression(caseExpr.CaseBlock, config, indent),
            Expression.OperatorApplication opApp => RenderMultiLineOperatorApplication(opApp, config, indent),
            Expression.IfBlock ifBlock => RenderMultiLineIfBlock(ifBlock, config, indent),
            Expression.LetExpression letExpr => RenderMultiLineLetExpression(letExpr.Value, config, indent),
            Expression.ParenthesizedExpression paren => RenderMultiLineParenthesizedExpression(paren, config, indent),
            _ => [new IndentedLine(indent, RenderExpression(expression))]
        };
    }

    /// <summary>
    /// Renders an application expression in multi-line format.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderMultiLineApplication(Expression.Application app, Config config, int indent)
    {
        // Check if function is a lambda with multi-line body (possibly wrapped in parentheses)
        var funcExpr = UnwrapParentheses(app.Arguments[0].Value);

        if (funcExpr is Expression.LambdaExpression lambda && LambdaBodyNeedsMultiLine(lambda.Lambda.Expression.Value))
        {
            // Render lambda with multi-line body
            var lambdaHeader = "(" + RenderLambdaHeader(lambda.Lambda);
            yield return new IndentedLine(indent, lambdaHeader);

            // Render lambda body multi-line
            foreach (var line in RenderMultiLine(lambda.Lambda.Expression.Value, config, indent + 1))
            {
                yield return line;
            }

            // Closing paren on its own line
            yield return new IndentedLine(indent, ")");

            // Render arguments on subsequent lines
            for (var i = 1; i < app.Arguments.Count; i++)
            {
                yield return new IndentedLine(indent + 1, RenderExpressionParenthesizedIfNeeded(app.Arguments[i].Value));
            }
        }
        else
        {
            // Render function on first line
            yield return new IndentedLine(indent, RenderExpression(app.Arguments[0].Value));
            // Render arguments on subsequent lines with extra indentation
            foreach (var line in RenderApplicationArguments(app.Arguments, 1, config, indent + 1))
            {
                yield return line;
            }
        }
    }

    /// <summary>
    /// Renders a parenthesized expression in multi-line format.
    /// Handles cases like ((\b -> ...) 3) where the inner expression is a lambda application.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderMultiLineParenthesizedExpression(
        Expression.ParenthesizedExpression paren,
        Config config,
        int indent)
    {
        var innerExpr = paren.Expression.Value;

        // Check if the inner expression is an application where the function is a lambda with multi-line body
        if (innerExpr is Expression.Application innerApp && innerApp.Arguments.Count > 0)
        {
            var funcExpr = UnwrapParentheses(innerApp.Arguments[0].Value);

            if (funcExpr is Expression.LambdaExpression lambda && LambdaBodyNeedsMultiLine(lambda.Lambda.Expression.Value))
            {
                // Render: ((\param -> body) arg)
                // Format:
                // ((\param ->
                //     body
                //  )
                //     arg
                // )

                var lambdaHeader = "((" + RenderLambdaHeader(lambda.Lambda);
                yield return new IndentedLine(indent, lambdaHeader);

                // Render lambda body multi-line
                foreach (var line in RenderMultiLine(lambda.Lambda.Expression.Value, config, indent + 1))
                {
                    yield return line;
                }

                // Inner closing paren with 1-space indent (add the extra space to content)
                yield return new IndentedLine(indent, " )");

                // Render arguments on subsequent lines with 4-space indent
                for (var i = 1; i < innerApp.Arguments.Count; i++)
                {
                    yield return new IndentedLine(indent + 1, RenderExpressionParenthesizedIfNeeded(innerApp.Arguments[i].Value));
                }

                // Outer closing paren
                yield return new IndentedLine(indent, ")");
                yield break;
            }
        }

        // Check if the inner expression is a lambda with multi-line body
        if (innerExpr is Expression.LambdaExpression simpleLambda && LambdaBodyNeedsMultiLine(simpleLambda.Lambda.Expression.Value))
        {
            // Render: (\param -> body)
            // Format:
            // (\param ->
            //     body
            // )

            var lambdaHeader = "(" + RenderLambdaHeader(simpleLambda.Lambda);
            yield return new IndentedLine(indent, lambdaHeader);

            // Render lambda body multi-line
            foreach (var line in RenderMultiLine(simpleLambda.Lambda.Expression.Value, config, indent + 1))
            {
                yield return line;
            }

            // Closing paren
            yield return new IndentedLine(indent, ")");
            yield break;
        }

        // Check if the inner expression is a regular application (not a lambda)
        if (innerExpr is Expression.Application regularApp && regularApp.Arguments.Count > 1)
        {
            // Render: (FuncName arg1 arg2)
            // Format:
            // (FuncName
            //     arg1
            //     arg2
            // )

            yield return new IndentedLine(indent, "(" + RenderExpression(regularApp.Arguments[0].Value));

            // Render arguments on subsequent lines with extra indentation
            foreach (var line in RenderApplicationArguments(regularApp.Arguments, 1, config, indent + 1))
            {
                yield return line;
            }

            yield return new IndentedLine(indent, ")");
            yield break;
        }

        // Default: wrap the inner multi-line expression in parentheses
        yield return new IndentedLine(indent, "(");
        foreach (var line in RenderMultiLine(innerExpr, config, indent))
        {
            yield return line;
        }
        yield return new IndentedLine(indent, ")");
    }

    /// <summary>
    /// Renders application arguments on subsequent lines with extra indentation.
    /// Used by both RenderMultiLineApplication and RenderMultiLineParenthesizedExpression.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderApplicationArguments(
        IReadOnlyList<Node<Expression>> arguments,
        int startIndex,
        Config config,
        int indent)
    {
        for (var i = startIndex; i < arguments.Count; i++)
        {
            var arg = arguments[i].Value;
            if (arg is Expression.ListExpr listExpr && listExpr.Elements.Count > 0)
            {
                // Render non-empty lists multi-line
                foreach (var line in RenderMultiLineList(listExpr, config, indent))
                {
                    yield return line;
                }
            }
            else if (GetExpressionComplexity(arg, config) is ExpressionComplexity.Complex)
            {
                // Render complex expressions multi-line
                foreach (var line in RenderMultiLine(arg, config, indent))
                {
                    yield return line;
                }
            }
            else
            {
                yield return new IndentedLine(indent, RenderExpressionParenthesizedIfNeeded(arg));
            }
        }
    }

    /// <summary>
    /// Renders the header portion of a lambda expression (params and arrow).
    /// </summary>
    private static string RenderLambdaHeader(LambdaStruct lambda) =>
        "\\" + string.Join(" ", lambda.Arguments.Select(a => RenderPattern(a.Value))) + " ->";

    /// <summary>
    /// Renders a single list element, handling both simple and complex cases.
    /// This unifies the logic for rendering list items across different contexts.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderListElement(
        Expression element,
        Config config,
        int indent,
        string prefix)
    {
        switch (element)
        {
            case Expression.ListExpr nestedList when ListContainsComplexExpression(nestedList):
                // Nested list with complex expressions - render with nested multi-line format
                foreach (var line in RenderNestedMultiLineList(nestedList, indent, prefix))
                {
                    yield return line;
                }
                break;

            case Expression.ListExpr nestedList:
                // Simple nested list - render inline
                yield return new IndentedLine(indent, prefix + RenderExpression(nestedList));
                break;

            case Expression.Application app:
                // Applications with actual arguments are rendered multi-line with each argument on its own line
                yield return new IndentedLine(indent, prefix + RenderExpression(app.Arguments[0].Value));
                for (var j = 1; j < app.Arguments.Count; j++)
                {
                    var arg = app.Arguments[j].Value;
                    if (arg is Expression.ListExpr argListExpr)
                    {
                        if (ListNeedsMultiLine(argListExpr))
                        {
                            foreach (var line in RenderMultiLineList(argListExpr, config, indent + 1))
                            {
                                yield return line;
                            }
                        }
                        else
                        {
                            yield return new IndentedLine(indent + 1, RenderExpression(argListExpr));
                        }
                    }
                    else
                    {
                        yield return new IndentedLine(indent + 1, RenderExpressionParenthesizedIfNeeded(arg));
                    }
                }
                break;

            case Expression.RecordExpr recordExpr when RecordNeedsMultiLine(recordExpr):
                // Record that needs multi-line rendering
                foreach (var line in RenderRecordInList(recordExpr, config, indent, prefix))
                {
                    yield return line;
                }
                break;

            default:
                // Simple element - render inline
                yield return new IndentedLine(indent, prefix + RenderExpression(element));
                break;
        }
    }

    /// <summary>
    /// Renders a list expression in multi-line format.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderMultiLineList(Expression.ListExpr listExpr, Config config, int indent)
    {
        for (var i = 0; i < listExpr.Elements.Count; i++)
        {
            var item = listExpr.Elements[i].Value;
            var prefix = i is 0 ? "[ " : ", ";

            foreach (var line in RenderListElement(item, config, indent, prefix))
            {
                yield return line;
            }
        }

        yield return new IndentedLine(indent, "]");
    }

    private static IEnumerable<IndentedLine> RenderMultiLineIfBlock(Expression.IfBlock ifBlock, Config config, int indent)
    {
        // Render "if condition then"
        yield return new IndentedLine(indent, "if " + RenderExpression(ifBlock.Condition.Value) + " then");

        // Render then branch with extra indent
        foreach (var line in RenderExpressionLines(ifBlock.ThenBlock.Value, config, indent + 1))
        {
            yield return line;
        }

        // Check if else branch is another if block (else if chain)
        if (ifBlock.ElseBlock.Value is Expression.IfBlock elseIfBlock)
        {
            // Blank line before else if
            yield return IndentedLine.Empty;

            // Render "else if condition then"
            yield return new IndentedLine(indent, "else if " + RenderExpression(elseIfBlock.Condition.Value) + " then");

            // Render then branch with extra indent
            foreach (var line in RenderExpressionLines(elseIfBlock.ThenBlock.Value, config, indent + 1))
            {
                yield return line;
            }

            // Continue with the else block from this else-if
            var currentElse = elseIfBlock.ElseBlock.Value;
            while (currentElse is Expression.IfBlock nextElseIf)
            {
                // Blank line before else if
                yield return IndentedLine.Empty;

                yield return new IndentedLine(indent, "else if " + RenderExpression(nextElseIf.Condition.Value) + " then");

                foreach (var line in RenderExpressionLines(nextElseIf.ThenBlock.Value, config, indent + 1))
                {
                    yield return line;
                }

                currentElse = nextElseIf.ElseBlock.Value;
            }

            // Final else branch
            yield return IndentedLine.Empty;
            yield return new IndentedLine(indent, "else");
            foreach (var line in RenderExpressionLines(currentElse, config, indent + 1))
            {
                yield return line;
            }
        }
        else
        {
            // Simple else branch
            yield return IndentedLine.Empty;
            yield return new IndentedLine(indent, "else");
            foreach (var line in RenderExpressionLines(ifBlock.ElseBlock.Value, config, indent + 1))
            {
                yield return line;
            }
        }
    }

    private static IEnumerable<IndentedLine> RenderMultiLineOperatorApplication(Expression.OperatorApplication opApp, Config config, int indent)
    {
        // Render the left side (which should be multi-line if we got here)
        foreach (var line in RenderExpressionLines(opApp.Left.Value, config, indent))
        {
            yield return line;
        }

        // Render operator and right side with extra indentation
        yield return new IndentedLine(indent + 1, opApp.Operator + " " + RenderExpression(opApp.Right.Value));
    }

    private static IEnumerable<IndentedLine> RenderMultiLineRecord(Expression.RecordExpr recordExpr, Config config, int indent)
    {
        for (var i = 0; i < recordExpr.Fields.Count; i++)
        {
            var field = recordExpr.Fields[i].Value;
            var prefix = i is 0 ? "{ " : ", ";
            var fieldValue = field.valueExpr.Value;

            // Check if the field value needs multi-line rendering
            if (GetExpressionComplexity(fieldValue, config) is ExpressionComplexity.Complex)
            {
                yield return new IndentedLine(indent, prefix + field.fieldName.Value + " =");
                foreach (var line in RenderExpressionLines(fieldValue, config, indent + 1))
                {
                    yield return line;
                }
            }
            else
            {
                yield return new IndentedLine(indent, prefix + field.fieldName.Value + " = " + RenderExpression(fieldValue));
            }
        }

        yield return new IndentedLine(indent, "}");
    }

    private static IEnumerable<IndentedLine> RenderMultiLineCaseExpression(CaseBlock caseBlock, Config config, int indent)
    {
        yield return new IndentedLine(indent, "case " + RenderExpression(caseBlock.Expression.Value) + " of");

        for (var i = 0; i < caseBlock.Cases.Count; i++)
        {
            var caseItem = caseBlock.Cases[i];

            // Add blank line between cases (except before the first one)
            if (i > 0)
            {
                yield return IndentedLine.Empty;
            }

            yield return new IndentedLine(indent + 1, RenderPattern(caseItem.Pattern.Value) + " ->");

            // Render the case expression body
            foreach (var line in RenderExpressionLines(caseItem.Expression.Value, config, indent + 2))
            {
                yield return line;
            }
        }
    }

    private static IEnumerable<IndentedLine> RenderMultiLineLetExpression(Expression.LetBlock letBlock, Config config, int indent)
    {
        // Render "let"
        yield return new IndentedLine(indent, "let");

        // Render declarations
        for (var i = 0; i < letBlock.Declarations.Count; i++)
        {
            var decl = letBlock.Declarations[i].Value;

            // Add blank line between declarations (except before the first one)
            if (i > 0)
            {
                yield return IndentedLine.Empty;
            }

            foreach (var line in RenderLetDeclarationLines(decl, config, indent + 1))
            {
                yield return line;
            }
        }

        // Render "in"
        yield return new IndentedLine(indent, "in");

        // Render the body expression
        foreach (var line in RenderExpressionLines(letBlock.Expression.Value, config, indent))
        {
            yield return line;
        }
    }

    private static IEnumerable<IndentedLine> RenderLetDeclarationLines(Expression.LetDeclaration letDecl, Config config, int indent)
    {
        switch (letDecl)
        {
            case Expression.LetDeclaration.LetFunction letFunc:
                {
                    // Render signature if present
                    if (letFunc.Function.Signature is { } signature)
                    {
                        yield return new IndentedLine(indent, RenderSignature(signature.Value, config));
                    }

                    var impl = letFunc.Function.Declaration.Value;
                    var header = impl.Name.Value;

                    if (impl.Arguments.Count > 0)
                    {
                        header += " " + string.Join(" ", impl.Arguments.Select(a => RenderPattern(a.Value)));
                    }

                    header += " =";

                    yield return new IndentedLine(indent, header);

                    // Render the expression body
                    foreach (var line in RenderExpressionLines(impl.Expression.Value, config, indent + 1))
                    {
                        yield return line;
                    }
                }
                break;

            case Expression.LetDeclaration.LetDestructuring letDestr:
                {
                    var header = RenderPattern(letDestr.Pattern.Value) + " =";

                    yield return new IndentedLine(indent, header);

                    // Render the expression body
                    foreach (var line in RenderExpressionLines(letDestr.Expression.Value, config, indent + 1))
                    {
                        yield return line;
                    }
                }
                break;

            default:
                throw new NotImplementedException($"Unknown let declaration type: {letDecl.GetType().Name}");
        }
    }

    /// <summary>
    /// Renders a record expression inside a list with proper prefix handling.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderRecordInList(Expression.RecordExpr recordExpr, Config config, int indent, string prefix)
    {
        for (var i = 0; i < recordExpr.Fields.Count; i++)
        {
            var field = recordExpr.Fields[i].Value;
            var fieldPrefix = i is 0 ? prefix + "{ " : "  , ";
            var fieldValue = field.valueExpr.Value;

            // Check if the field value needs multi-line rendering
            if (GetExpressionComplexity(fieldValue, config) is ExpressionComplexity.Complex)
            {
                yield return new IndentedLine(indent, fieldPrefix + field.fieldName.Value + " =");
                foreach (var line in RenderExpressionLines(fieldValue, config, indent + 2))
                {
                    yield return line;
                }
            }
            else
            {
                yield return new IndentedLine(indent, fieldPrefix + field.fieldName.Value + " = " + RenderExpression(fieldValue));
            }
        }

        yield return new IndentedLine(indent, "  }");
    }

    /// <summary>
    /// Renders a nested list with complex expressions in multi-line format.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderNestedMultiLineList(Expression.ListExpr nestedList, int outerIndent, string outerPrefix)
    {
        for (var i = 0; i < nestedList.Elements.Count; i++)
        {
            var item = nestedList.Elements[i].Value;
            var innerPrefix = i is 0 ? "[ " : ", ";

            if (i is 0)
            {
                // First item of nested list
                if (item is Expression.Application app && ApplicationHasListArgument(app))
                {
                    yield return new IndentedLine(outerIndent, outerPrefix + innerPrefix + RenderExpression(app.Arguments[0].Value));
                    // Application arguments get extra indent
                    for (var j = 1; j < app.Arguments.Count; j++)
                    {
                        // Extra spaces from prefix lengths are added to content
                        var extraSpaces = new string(' ', outerPrefix.Length + innerPrefix.Length);
                        yield return new IndentedLine(outerIndent + 1, extraSpaces + RenderExpressionParenthesizedIfNeeded(app.Arguments[j].Value));
                    }
                }
                else
                {
                    yield return new IndentedLine(outerIndent, outerPrefix + innerPrefix + RenderExpression(item));
                }
            }
            else
            {
                // Subsequent items of nested list (indented 2 more spaces from base indent)
                if (item is Expression.Application app && ApplicationHasListArgument(app))
                {
                    yield return new IndentedLine(outerIndent, "  " + innerPrefix + RenderExpression(app.Arguments[0].Value));
                    for (var j = 1; j < app.Arguments.Count; j++)
                    {
                        // Extra spaces from prefix length are added to content
                        var extraSpaces = new string(' ', 2 + innerPrefix.Length);
                        yield return new IndentedLine(outerIndent + 1, extraSpaces + RenderExpressionParenthesizedIfNeeded(app.Arguments[j].Value));
                    }
                }
                else
                {
                    yield return new IndentedLine(outerIndent, "  " + innerPrefix + RenderExpression(item));
                }
            }
        }

        // Close nested list (indented 2 more spaces from outer)
        yield return new IndentedLine(outerIndent, "  ]");
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

    private static string RenderCharLiteral(int value)
    {
        var c = char.ConvertFromUtf32(value);

        if (c is "'")
            return "'\\''";

        if (c is "\\")
            return "'\\\\'";

        if (c is "\n")
            return "'\\n'";

        if (c is "\r")
            return "'\\r'";

        if (c is "\t")
            return "'\\t'";

        return "'" + c + "'";
    }

    private static string FormatFloatForElm(double value)
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
            Pattern.AllPattern => "_",

            Pattern.VarPattern varPat => varPat.Name,

            Pattern.UnitPattern => "()",

            Pattern.CharPattern charPat => RenderCharLiteral(charPat.Value),

            Pattern.StringPattern stringPat => RenderStringLiteral(stringPat.Value),

            Pattern.IntPattern intPat => intPat.Value.ToString(),

            Pattern.HexPattern hexPat => "0x" + hexPat.Value.ToString("X"),

            Pattern.FloatPattern floatPat => floatPat.Value.ToString(System.Globalization.CultureInfo.InvariantCulture),

            Pattern.TuplePattern tuplePat =>
                "( " + string.Join(", ", tuplePat.Elements.Select(e => RenderPattern(e.Value))) + " )",

            Pattern.RecordPattern recordPat =>
                "{ " + string.Join(", ", recordPat.Fields.Select(f => f.Value)) + " }",

            Pattern.UnConsPattern unconsPat =>
                RenderPatternParenthesizedIfNeeded(unconsPat.Head.Value) + " :: " + RenderPattern(unconsPat.Tail.Value),

            Pattern.ListPattern listPat =>
                listPat.Elements.Count is 0
                    ? "[]"
                    : "[ " + string.Join(", ", listPat.Elements.Select(e => RenderPattern(e.Value))) + " ]",

            Pattern.NamedPattern namedPat =>
                RenderNamedPattern(namedPat),

            Pattern.AsPattern asPat =>
                RenderPatternParenthesizedIfNeeded(asPat.Pattern.Value) + " as " + asPat.Name.Value,

            Pattern.ParenthesizedPattern parenPat =>
                "(" + RenderPattern(parenPat.Pattern.Value) + ")",

            _ => throw new NotImplementedException($"Unknown pattern type: {pattern.GetType().Name}")
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
