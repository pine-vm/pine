using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Linq;

using FullTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Provides conversion methods from Full syntax model types to Stil4mElmSyntax7 types.
/// These conversions strip the additional token location information present in the concretized types.
/// <para>
/// To learn about the syntax model, see 'guide\elm-syntax-model-and-parser.md'
/// </para>
/// </summary>
public static class FromFullSyntaxModel
{
    /// <summary>
    /// Converts a <see cref="File"/>
    /// </summary>
    public static File Convert(
        FullTypes.File file)
    {
        if (file.IncompleteDeclarations.FirstOrDefault() is { } incompleteDeclarationNode)
        {
            /*
             * If the file should be converted despite syntax errors,
             * set the 'IncompleteDeclarations' to empty before invoking this conversion.
             * */
            throw new ParserException(
                incompleteDeclarationNode.Value.ErrorMessage,
                lineNumber: incompleteDeclarationNode.Value.ErrorLocation.Row,
                columnNumber: incompleteDeclarationNode.Value.ErrorLocation.Column);
        }

        return
            new File(
                ModuleDefinition: ConvertNode(file.ModuleDefinition, Convert),
                Imports: ConvertNodes(file.Imports, Convert),
                Declarations: ConvertNodes(file.Declarations, Convert),
                Comments: file.Comments);
    }

    /// <summary>
    /// Converts a <see cref="Module"/>
    /// </summary>
    public static Module Convert(
        FullTypes.Module module) =>
        module switch
        {
            FullTypes.Module.NormalModule normalModule =>
            new Module.NormalModule(
                Convert(normalModule.ModuleData)),

            FullTypes.Module.PortModule portModule =>
            new Module.PortModule(
                Convert(portModule.ModuleData)),

            FullTypes.Module.EffectModule effectModule =>
            new Module.EffectModule(
                Convert(effectModule.ModuleData)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().Name),
        };

    /// <summary>
    /// Converts a <see cref="DefaultModuleData"/>
    /// </summary>
    public static DefaultModuleData Convert(
        FullTypes.DefaultModuleData moduleData) =>
        new(
            ModuleName: moduleData.ModuleName,
            ExposingList: ConvertNode(moduleData.ExposingList, Convert));

    /// <summary>
    /// Converts a <see cref="EffectModuleData"/>
    /// </summary>
    public static EffectModuleData Convert(
        FullTypes.EffectModuleData moduleData) =>
        new(
            ModuleName: moduleData.ModuleName,
            ExposingList: ConvertNode(moduleData.ExposingList, Convert),
            Command: moduleData.Command,
            Subscription: moduleData.Subscription);

    /// <summary>
    /// Converts an <see cref="Exposing"/>
    /// </summary>
    public static Exposing Convert(
        FullTypes.Exposing exposing) =>
        exposing switch
        {
            FullTypes.Exposing.All all =>
            new Exposing.All(all.Range),

            FullTypes.Exposing.Explicit @explicit =>
            new Exposing.Explicit(
                ConvertSeparatedNodes(@explicit.Nodes, Convert)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().Name),
        };

    /// <summary>
    /// Converts a <see cref="TopLevelExpose"/>
    /// </summary>
    public static TopLevelExpose Convert(
        FullTypes.TopLevelExpose expose) =>
        expose switch
        {
            FullTypes.TopLevelExpose.InfixExpose infixExpose =>
            new TopLevelExpose.InfixExpose(infixExpose.Name),

            FullTypes.TopLevelExpose.FunctionExpose functionExpose =>
            new TopLevelExpose.FunctionExpose(functionExpose.Name),

            FullTypes.TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
            new TopLevelExpose.TypeOrAliasExpose(typeOrAliasExpose.Name),

            FullTypes.TopLevelExpose.TypeExpose typeExpose =>
            new TopLevelExpose.TypeExpose(
                Convert(typeExpose.ExposedType)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected top level expose type: " + expose.GetType().Name),
        };

    /// <summary>
    /// Converts an <see cref="ExposedType"/>
    /// </summary>
    public static ExposedType Convert(
        FullTypes.ExposedType exposedType) =>
        new(
            Name: exposedType.Name,
            Open: exposedType.Open);

    /// <summary>
    /// Converts an <see cref="Import"/>
    /// </summary>
    public static Import Convert(
        FullTypes.Import import) =>
        new(
            ModuleName: import.ModuleName,
            ModuleAlias: import.ModuleAlias?.Alias,
            ExposingList: import.ExposingList is { } exposingList
            ?
            ConvertNode(exposingList.ExposingList, Convert)
            :
            null);

    /// <summary>
    /// Converts a <see cref="Declaration"/>
    /// </summary>
    public static Declaration Convert(
        FullTypes.Declaration declaration) =>
        declaration switch
        {
            FullTypes.Declaration.FunctionDeclaration functionDeclaration =>
            new Declaration.FunctionDeclaration(
                Convert(functionDeclaration.Function)),

            FullTypes.Declaration.CustomTypeDeclaration customTypeDeclaration =>
            new Declaration.CustomTypeDeclaration(
                Convert(customTypeDeclaration.TypeDeclaration)),

            FullTypes.Declaration.AliasDeclaration aliasDeclaration =>
            new Declaration.AliasDeclaration(
                Convert(aliasDeclaration.TypeAlias)),

            FullTypes.Declaration.PortDeclaration portDeclaration =>
            new Declaration.PortDeclaration(
                Convert(portDeclaration.Signature)),

            FullTypes.Declaration.InfixDeclaration infixDeclaration =>
            new Declaration.InfixDeclaration(
                Convert(infixDeclaration.Infix)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().Name),
        };

    /// <summary>
    /// Converts an <see cref="Infix"/>
    /// </summary>
    public static Infix Convert(
        FullTypes.Infix infix) =>
        new(
            Direction: infix.Direction,
            Precedence: infix.Precedence,
            Operator: infix.Operator,
            FunctionName: infix.FunctionName);

    /// <summary>
    /// Converts a <see cref="TypeAlias"/>
    /// </summary>
    public static TypeAlias Convert(
        FullTypes.TypeAlias typeAlias) =>
        new(
            Documentation: typeAlias.Documentation,
            Name: typeAlias.Name,
            Generics: typeAlias.Generics,
            TypeAnnotation: ConvertNode(typeAlias.TypeAnnotation, Convert));

    /// <summary>
    /// Converts a <see cref="TypeStruct"/>
    /// </summary>
    public static TypeStruct Convert(
        FullTypes.TypeStruct typeStruct) =>
        new(
            Documentation: typeStruct.Documentation,
            Name: typeStruct.Name,
            Generics: typeStruct.Generics,
            Constructors: [.. typeStruct.Constructors.Select(c => ConvertNode(c.Constructor, Convert))]);

    /// <summary>
    /// Converts a <see cref="ValueConstructor"/>
    /// </summary>
    public static ValueConstructor Convert(
        FullTypes.ValueConstructor constructor) =>
        new(
            Name: constructor.Name,
            Arguments: ConvertNodes(constructor.Arguments, Convert));

    /// <summary>
    /// Converts a <see cref="TypeAnnotation"/>
    /// Single-element tuples (parenthesized types) are unwrapped to match the
    /// original stil4m/elm-syntax behavior that doesn't emit parenthesized nodes for type arguments.
    /// </summary>
    public static TypeAnnotation Convert(
        FullTypes.TypeAnnotation typeAnnotation) =>
        typeAnnotation switch
        {
            FullTypes.TypeAnnotation.GenericType genericType =>
            new TypeAnnotation.GenericType(genericType.Name),

            FullTypes.TypeAnnotation.Typed typed =>
            new TypeAnnotation.Typed(
                typed.TypeName,
                ConvertTypeAnnotationNodes(typed.TypeArguments)),

            FullTypes.TypeAnnotation.Unit =>
            new TypeAnnotation.Unit(),

            FullTypes.TypeAnnotation.Tupled tupled =>
            ConvertTupledTypeAnnotation(tupled),

            FullTypes.TypeAnnotation.Record record =>
            new TypeAnnotation.Record(
                Convert(record.RecordDefinition)),

            FullTypes.TypeAnnotation.GenericRecord genericRecord =>
            new TypeAnnotation.GenericRecord(
                genericRecord.GenericName,
                ConvertNode(genericRecord.RecordDefinition, Convert)),

            FullTypes.TypeAnnotation.FunctionTypeAnnotation functionType =>
            new TypeAnnotation.FunctionTypeAnnotation(
                ConvertTypeAnnotationNode(functionType.ArgumentType),
                ConvertTypeAnnotationNode(functionType.ReturnType)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type annotation type: " + typeAnnotation.GetType().Name),
        };

    /// <summary>
    /// Converts a <see cref="TypeAnnotation.Tupled"/>
    /// Single-element tuples are unwrapped
    /// to match the original stil4m/elm-syntax behavior.
    /// </summary>
    private static TypeAnnotation ConvertTupledTypeAnnotation(
        FullTypes.TypeAnnotation.Tupled tupled)
    {
        // Unwrap single-element tuples (parenthesized types)
        if (tupled.TypeAnnotations is SeparatedSyntaxList<Node<FullTypes.TypeAnnotation>>.NonEmpty { Rest.Count: 0 } nonEmpty)
        {
            // Single element - unwrap it
            return Convert(nonEmpty.First.Value);
        }

        // Multiple elements - keep as tuple
        return
            new TypeAnnotation.Tupled(
                ConvertSeparatedNodes(tupled.TypeAnnotations, Convert));
    }

    /// <summary>
    /// Converts a Node of TypeAnnotation, unwrapping single-element tuples.
    /// This preserves the outer range but unwraps the inner value.
    /// </summary>
    private static Node<TypeAnnotation> ConvertTypeAnnotationNode(
        Node<FullTypes.TypeAnnotation> node) =>
        new(node.Range, Convert(node.Value));

    /// <summary>
    /// Converts a list of TypeAnnotation nodes (for type arguments).
    /// Single-element tuples in type arguments are unwrapped.
    /// </summary>
    private static IReadOnlyList<Node<TypeAnnotation>> ConvertTypeAnnotationNodes(
        IReadOnlyList<Node<FullTypes.TypeAnnotation>> nodes) =>
        [.. nodes.Select(ConvertTypeAnnotationNode)];

    /// <summary>
    /// Converts a RecordDefinition
    /// </summary>
    public static RecordDefinition Convert(
        FullTypes.RecordDefinition recordDefinition) =>
        new(
            Fields: ConvertSeparatedNodes(recordDefinition.Fields, Convert));

    /// <summary>
    /// Converts a <see cref="RecordField"/>
    /// </summary>
    public static RecordField Convert(
        FullTypes.RecordField recordField) =>
        new(
            FieldName: recordField.FieldName,
            FieldType: ConvertNode(recordField.FieldType, Convert));

    /// <summary>
    /// Converts a <see cref="FunctionStruct"/>
    /// </summary>
    public static FunctionStruct Convert(
        FullTypes.FunctionStruct functionStruct) =>
        new(
            Documentation: functionStruct.Documentation,
            Signature: functionStruct.Signature is { } sig
            ?
            ConvertNode(sig, Convert)
            :
            null,
            Declaration: ConvertNode(functionStruct.Declaration, Convert));

    /// <summary>
    /// Converts a <see cref="FunctionImplementation"/>
    /// </summary>
    public static FunctionImplementation Convert(
        FullTypes.FunctionImplementation functionImplementation) =>
        new(
            Name: functionImplementation.Name,
            Arguments: ConvertNodes(functionImplementation.Arguments, Convert),
            Expression: ConvertExpressionNode(functionImplementation.Expression));

    /// <summary>
    /// Converts a <see cref="Signature"/>
    /// </summary>
    public static Signature Convert(
        FullTypes.Signature signature) =>
        new(
            Name: signature.Name,
            TypeAnnotation: ConvertNode(signature.TypeAnnotation, Convert));

    /// <summary>
    /// Converts a <see cref="Pattern"/>
    /// </summary>
    public static Pattern Convert(
        FullTypes.Pattern pattern) =>
        pattern switch
        {
            FullTypes.Pattern.AllPattern =>
            new Pattern.AllPattern(),

            FullTypes.Pattern.VarPattern varPattern =>
            new Pattern.VarPattern(varPattern.Name),

            FullTypes.Pattern.UnitPattern =>
            new Pattern.UnitPattern(),

            FullTypes.Pattern.CharPattern charPattern =>
            new Pattern.CharPattern(charPattern.Value),

            FullTypes.Pattern.StringPattern stringPattern =>
            new Pattern.StringPattern(stringPattern.Value),

            FullTypes.Pattern.IntPattern intPattern =>
            new Pattern.IntPattern(intPattern.Value),

            FullTypes.Pattern.HexPattern hexPattern =>
            new Pattern.HexPattern(hexPattern.Value),

            FullTypes.Pattern.FloatPattern floatPattern =>
            new Pattern.FloatPattern(floatPattern.Value),

            FullTypes.Pattern.TuplePattern tuplePattern =>
            new Pattern.TuplePattern(
                ConvertSeparatedNodes(tuplePattern.Elements, Convert)),

            FullTypes.Pattern.RecordPattern recordPattern =>
            new Pattern.RecordPattern(ToList(recordPattern.Fields)),

            FullTypes.Pattern.UnConsPattern unConsPattern =>
            new Pattern.UnConsPattern(
                ConvertNode(unConsPattern.Head, Convert),
                ConvertNode(unConsPattern.Tail, Convert)),

            FullTypes.Pattern.ListPattern listPattern =>
            new Pattern.ListPattern(
                ConvertSeparatedNodes(listPattern.Elements, Convert)),

            FullTypes.Pattern.NamedPattern namedPattern =>
            new Pattern.NamedPattern(
                Convert(namedPattern.Name),
                ConvertNodes(namedPattern.Arguments, Convert)),

            FullTypes.Pattern.AsPattern asPattern =>
            new Pattern.AsPattern(
                ConvertNode(asPattern.Pattern, Convert),
                asPattern.Name),

            FullTypes.Pattern.ParenthesizedPattern parenthesizedPattern =>
            new Pattern.ParenthesizedPattern(
                ConvertNode(parenthesizedPattern.Pattern, Convert)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected pattern type: " + pattern.GetType().Name),
        };

    /// <summary>
    /// Converts a <see cref="QualifiedNameRef"/>
    /// </summary>
    public static QualifiedNameRef Convert(
        FullTypes.QualifiedNameRef qualifiedNameRef) =>
        new(
            ModuleName: qualifiedNameRef.ModuleName,
            Name: qualifiedNameRef.Name);

    /// <summary>
    /// Converts an <see cref="Expression"/>
    /// Note: For RecordExpr and RecordUpdateExpression, use ConvertExpressionNode instead
    /// as they need the containing node's range to compute the closing brace location.
    /// </summary>
    public static Expression Convert(
        FullTypes.Expression expression) =>
        expression switch
        {
            FullTypes.Expression.UnitExpr =>
            new Expression.UnitExpr(),

            FullTypes.Expression.Literal literal =>
            new Expression.Literal(literal.Value, literal.IsTripleQuoted),

            FullTypes.Expression.CharLiteral charLiteral =>
            new Expression.CharLiteral(charLiteral.Value),

            FullTypes.Expression.Integer integer =>
            ConvertIntegerExpression(integer),

            FullTypes.Expression.Floatable floatable =>
            new Expression.Floatable(floatable.LiteralText),

            FullTypes.Expression.Negation negation =>
            new Expression.Negation(
                ConvertExpressionNode(negation.Expression)),

            FullTypes.Expression.ListExpr listExpr =>
            new Expression.ListExpr(
                ConvertExpressionNodes(listExpr.Elements)),

            FullTypes.Expression.FunctionOrValue functionOrValue =>
            new Expression.FunctionOrValue(
                functionOrValue.ModuleName,
                functionOrValue.Name),

            FullTypes.Expression.IfBlock ifBlock =>
            new Expression.IfBlock(
                ConvertExpressionNode(ifBlock.Condition),
                ConvertExpressionNode(ifBlock.ThenBlock),
                ConvertExpressionNode(ifBlock.ElseBlock)),

            FullTypes.Expression.PrefixOperator prefixOperator =>
            new Expression.PrefixOperator(prefixOperator.Operator),

            FullTypes.Expression.ParenthesizedExpression parenthesizedExpression =>
            new Expression.ParenthesizedExpression(
                ConvertExpressionNode(parenthesizedExpression.Expression)),

            FullTypes.Expression.Application application =>
            new Expression.Application(
                ConvertExpressionNodes(application.Arguments)),

            FullTypes.Expression.OperatorApplication operatorApplication =>
            new Expression.OperatorApplication(
                operatorApplication.Operator.Value,
                operatorApplication.Direction,
                ConvertExpressionNode(operatorApplication.Left),
                ConvertExpressionNode(operatorApplication.Right)),

            FullTypes.Expression.TupledExpression tupledExpression =>
            new Expression.TupledExpression(
                ConvertExpressionNodes(tupledExpression.Elements)),

            FullTypes.Expression.LambdaExpression lambdaExpression =>
            new Expression.LambdaExpression(
                Convert(lambdaExpression.Lambda)),

            FullTypes.Expression.CaseExpression caseExpression =>
            new Expression.CaseExpression(
                Convert(caseExpression.CaseBlock)),

            FullTypes.Expression.LetExpression letExpression =>
            new Expression.LetExpression(
                Convert(letExpression.Value)),

            // RecordExpr and RecordUpdateExpression need the containing node's range,
            // so they should be converted via ConvertExpressionNode
            FullTypes.Expression.RecordExpr =>
            throw new System.InvalidOperationException(
                "RecordExpr must be converted via ConvertExpressionNode to access the containing node's range"),

            FullTypes.Expression.RecordAccess recordAccess =>
            new Expression.RecordAccess(
                ConvertExpressionNode(recordAccess.Record),
                recordAccess.FieldName),

            FullTypes.Expression.RecordAccessFunction recordAccessFunction =>
            new Expression.RecordAccessFunction(recordAccessFunction.FunctionName),

            FullTypes.Expression.RecordUpdateExpression =>
            throw new System.InvalidOperationException(
                "RecordUpdateExpression must be converted via ConvertExpressionNode to access the containing node's range"),

            FullTypes.Expression.GLSLExpression glslExpression =>
            new Expression.GLSLExpression(glslExpression.ShaderCode),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected expression type: " + expression.GetType().Name),
        };

    /// <summary>
    /// Converts an <see cref="Expression"/> node.
    /// This method handles RecordExpr and RecordUpdateExpression specially by using
    /// the node's range to compute the closing brace location.
    /// </summary>
    public static Node<Expression> ConvertExpressionNode(
        Node<FullTypes.Expression> node)
    {
        // For RecordExpr and RecordUpdateExpression, we need the node's range to compute closing brace location
        // The closing brace is at Range.End with column - 1 (since Range.End is after the brace)
        var closeBraceLocation = new Location(node.Range.End.Row, node.Range.End.Column - 1);

        var convertedValue =
            node.Value switch
            {
                FullTypes.Expression.RecordExpr recordExpr =>
                new Expression.RecordExpr(
                    ConvertRecordFields(recordExpr.Fields, closeBraceLocation, isRecordUpdateExpression: false)),

                FullTypes.Expression.RecordUpdateExpression recordUpdateExpression =>
                new Expression.RecordUpdateExpression(
                    recordUpdateExpression.RecordName,
                    ConvertRecordFields(
                        recordUpdateExpression.Fields,
                        closeBraceLocation,
                        isRecordUpdateExpression: true)),

                // For all other expression types, use the standard conversion
                var expr => Convert(expr)
            };

        return new Node<Expression>(node.Range, convertedValue);
    }

    /// <summary>
    /// Converts a list of expression nodes.
    /// </summary>
    private static IReadOnlyList<Node<Expression>> ConvertExpressionNodes(
        IReadOnlyList<Node<FullTypes.Expression>> nodes) =>
        [.. nodes.Select(ConvertExpressionNode)];

    /// <summary>
    /// Converts a SeparatedSyntaxList of expression nodes to an IReadOnlyList.
    /// </summary>
    private static IReadOnlyList<Node<Expression>> ConvertExpressionNodes(
        SeparatedSyntaxList<Node<FullTypes.Expression>> separatedList) =>
        [.. ToList(separatedList).Select(ConvertExpressionNode)];

    /// <summary>
    /// Converts a <see cref="LambdaStruct"/>
    /// </summary>
    public static LambdaStruct Convert(
        FullTypes.LambdaStruct lambdaStruct) =>
        new(
            Arguments: ConvertNodes(lambdaStruct.Arguments, Convert),
            Expression: ConvertExpressionNode(lambdaStruct.Expression));

    /// <summary>
    /// Converts a <see cref="CaseBlock"/>
    /// </summary>
    public static CaseBlock Convert(
        FullTypes.CaseBlock caseBlock) =>
        new(
            Expression: ConvertExpressionNode(caseBlock.Expression),
            Cases: [.. caseBlock.Cases.Select(Convert)]);

    /// <summary>
    /// Converts a <see cref="Case"/>
    /// </summary>
    public static Case Convert(
        FullTypes.Case @case) =>
        new(
            Pattern: ConvertNode(@case.Pattern, Convert),
            Expression: ConvertExpressionNode(@case.Expression));

    /// <summary>
    /// Converts a <see cref="Expression.LetBlock"/>   
    /// </summary>
    public static Expression.LetBlock Convert(
        FullTypes.Expression.LetBlock letBlock) =>
        new(
            Declarations: ConvertNodes(letBlock.Declarations, Convert),
            Expression: ConvertExpressionNode(letBlock.Expression));

    /// <summary>
    /// Converts a <see cref="Expression.LetDeclaration"/>
    /// </summary>
    public static Expression.LetDeclaration Convert(
        FullTypes.Expression.LetDeclaration letDeclaration) =>
        letDeclaration switch
        {
            FullTypes.Expression.LetDeclaration.LetFunction letFunction =>
            new Expression.LetDeclaration.LetFunction(
                Convert(letFunction.Function)),

            FullTypes.Expression.LetDeclaration.LetDestructuring letDestructuring =>
            new Expression.LetDeclaration.LetDestructuring(
                ConvertNode(letDestructuring.Pattern, Convert),
                ConvertExpressionNode(letDestructuring.Expression)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected let declaration type: " + letDeclaration.GetType().Name),
        };

    // Helper methods for converting nodes and lists

    private static Node<TResult> ConvertNode<TSource, TResult>(
        Node<TSource> node,
        System.Func<TSource, TResult> converter) =>
        new(node.Range, converter(node.Value));

    private static IReadOnlyList<Node<TResult>> ConvertNodes<TSource, TResult>(
        IReadOnlyList<Node<TSource>> nodes,
        System.Func<TSource, TResult> converter) =>
        [.. nodes.Select(node => ConvertNode(node, converter))];

    /// <summary>
    /// Converts a SeparatedSyntaxList to an IReadOnlyList, discarding separator locations.
    /// </summary>
    public static IReadOnlyList<TNode> ToList<TNode>(
        SeparatedSyntaxList<TNode> separatedList) =>
        separatedList switch
        {
            SeparatedSyntaxList<TNode>.Empty => [],

            SeparatedSyntaxList<TNode>.NonEmpty nonEmpty =>
            [nonEmpty.First, .. nonEmpty.Rest.Select(r => r.Node)],

            _ =>
            throw new System.NotImplementedException(
                "Unexpected SeparatedSyntaxList type: " + separatedList.GetType().Name),
        };

    /// <summary>
    /// Converts a SeparatedSyntaxList of nodes to an IReadOnlyList, applying a converter to each node's value.
    /// </summary>
    public static IReadOnlyList<Node<TResult>> ConvertSeparatedNodes<TSource, TResult>(
        SeparatedSyntaxList<Node<TSource>> separatedList,
        System.Func<TSource, TResult> converter) =>
        [.. ToList(separatedList).Select(node => ConvertNode(node, converter))];

    /// <summary>
    /// Converts a full syntax model integer expression (which holds the original literal text)
    /// to the appropriate Stil4mElmSyntax7 expression (Integer or Hex).
    /// <para>
    /// v8: don't split Integer and Hex: <see href="https://github.com/stil4m/elm-syntax/issues/255"/>
    /// </para>
    /// <para>
    /// Store raw string for numbers in the AST: <see href="https://github.com/stil4m/elm-syntax/issues/108"/>
    /// </para>
    /// </summary>
    private static Expression ConvertIntegerExpression(FullTypes.Expression.Integer integer)
    {
        var literalText = integer.LiteralText;

        // Check for hexadecimal format: "0x..." or "-0x..."
        if (literalText.StartsWith("0x") || literalText.StartsWith("-0x"))
        {
            // Parse hex value
            var isNegative = literalText.StartsWith("-");
            var hexPart = isNegative ? literalText[3..] : literalText[2..];
            var value = long.Parse(hexPart, System.Globalization.NumberStyles.HexNumber);
            return new Expression.Hex(isNegative ? -value : value);
        }

        // Decimal integer
        var decValue = long.Parse(literalText);
        return new Expression.Integer(decValue);
    }

    private static IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>
        ConvertRecordFields(
        SeparatedSyntaxList<RecordExprField> fields,
        Location closeBraceLocation,
        bool isRecordUpdateExpression = false)
    {
        if (fields is not SeparatedSyntaxList<RecordExprField>.NonEmpty nonEmpty)
        {
            return [];
        }

        var result = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

        var isSingleField = nonEmpty.Rest.Count is 0;

        // Elm parser quirk: The behavior differs between single-line and multi-line records.
        // Detect multi-line by comparing the first field's row with the closing brace row.
        var isMultiLine =
            closeBraceLocation.Row > nonEmpty.First.FieldName.Range.Start.Row;

        // Process first field
        {
            var convertedValueExpr = ConvertExpressionNode(nonEmpty.First.ValueExpr);
            Location fieldRangeEnd;

            if (isSingleField)
            {
                // Single-field records:
                // - RecordUpdateExpression: use closing brace position
                // - RecordExpr: use expression end
                fieldRangeEnd =
                    isRecordUpdateExpression
                    ?
                    closeBraceLocation
                    :
                    convertedValueExpr.Range.End;
            }
            else if (isMultiLine)
            {
                // Multi-line records with multiple fields
                var nextSeparatorLocation = nonEmpty.Rest[0].SeparatorLocation;

                // Elm parser quirk: RecordUpdateExpression always uses separator positions,
                // but RecordExpr with comma-prefix formatting (comma on next line) uses expression end for first field
                if (!isRecordUpdateExpression && nextSeparatorLocation.Row > convertedValueExpr.Range.End.Row)
                {
                    // RecordExpr with comma-prefix formatting: first field uses expression end
                    fieldRangeEnd = convertedValueExpr.Range.End;
                }
                else
                {
                    // RecordUpdateExpression or comma-suffix RecordExpr: use next separator
                    fieldRangeEnd = nextSeparatorLocation;
                }
            }
            else
            {
                // Single-line, first of multiple fields: use expression end
                fieldRangeEnd = convertedValueExpr.Range.End;
            }

            result.Add(
                new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                    new Range(nonEmpty.First.FieldName.Range.Start, fieldRangeEnd),
                    (nonEmpty.First.FieldName, convertedValueExpr)));
        }

        // Process remaining fields
        for (var i = 0; i < nonEmpty.Rest.Count; i++)
        {
            var (_, field) = nonEmpty.Rest[i];
            var convertedValueExpr = ConvertExpressionNode(field.ValueExpr);

            var isLastField = i == nonEmpty.Rest.Count - 1;
            Location fieldRangeEnd;

            if (isLastField)
            {
                // Last field always uses closing brace position
                fieldRangeEnd = closeBraceLocation;
            }
            else if (isMultiLine)
            {
                // Multi-line, non-last field: use the comma position (separator of next field)
                fieldRangeEnd = nonEmpty.Rest[i + 1].SeparatorLocation;
            }
            else
            {
                // Single-line, non-last field: use expression end
                fieldRangeEnd = convertedValueExpr.Range.End;
            }

            result.Add(
                new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                    new Range(field.FieldName.Range.Start, fieldRangeEnd),
                    (field.FieldName, convertedValueExpr)));
        }

        return result;
    }
}
