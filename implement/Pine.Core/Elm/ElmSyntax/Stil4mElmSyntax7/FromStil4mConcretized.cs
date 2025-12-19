using System.Collections.Generic;
using System.Linq;

using ConcretizedTypes = Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Provides conversion methods from Stil4mConcretized types to Stil4mElmSyntax7 types.
/// These conversions strip the additional token location information present in the concretized types.
/// </summary>
public static class FromStil4mConcretized
{
    /// <summary>
    /// Converts a Stil4mConcretized.File to a Stil4mElmSyntax7.File.
    /// </summary>
    public static File fromStil4mConcretized(
        ConcretizedTypes.File file)
    {
        return
            new File(
                ModuleDefinition: ConvertNode(file.ModuleDefinition, fromStil4mConcretized),
                Imports: ConvertNodes(file.Imports, fromStil4mConcretized),
                Declarations: ConvertNodes(file.Declarations, fromStil4mConcretized),
                Comments: file.Comments);
    }

    /// <summary>
    /// Converts a Stil4mConcretized.Module to a Stil4mElmSyntax7.Module.
    /// </summary>
    public static Module fromStil4mConcretized(
        ConcretizedTypes.Module module) =>
        module switch
        {
            ConcretizedTypes.Module.NormalModule normalModule =>
                new Module.NormalModule(
                    fromStil4mConcretized(normalModule.ModuleData)),

            ConcretizedTypes.Module.PortModule portModule =>
                new Module.PortModule(
                    fromStil4mConcretized(portModule.ModuleData)),

            ConcretizedTypes.Module.EffectModule effectModule =>
                new Module.EffectModule(
                    fromStil4mConcretized(effectModule.ModuleData)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected module type: " + module.GetType().Name),
        };

    /// <summary>
    /// Converts a Stil4mConcretized.DefaultModuleData to a Stil4mElmSyntax7.DefaultModuleData.
    /// </summary>
    public static DefaultModuleData fromStil4mConcretized(
        ConcretizedTypes.DefaultModuleData moduleData) =>
        new(
            ModuleName: moduleData.ModuleName,
            ExposingList: ConvertNode(moduleData.ExposingList, fromStil4mConcretized));

    /// <summary>
    /// Converts a Stil4mConcretized.EffectModuleData to a Stil4mElmSyntax7.EffectModuleData.
    /// </summary>
    public static EffectModuleData fromStil4mConcretized(
        ConcretizedTypes.EffectModuleData moduleData) =>
        new(
            ModuleName: moduleData.ModuleName,
            ExposingList: ConvertNode(moduleData.ExposingList, fromStil4mConcretized),
            Command: moduleData.Command,
            Subscription: moduleData.Subscription);

    /// <summary>
    /// Converts a Stil4mConcretized.Exposing to a Stil4mElmSyntax7.Exposing.
    /// </summary>
    public static Exposing fromStil4mConcretized(
        ConcretizedTypes.Exposing exposing) =>
        exposing switch
        {
            ConcretizedTypes.Exposing.All all =>
                new Exposing.All(all.Range),

            ConcretizedTypes.Exposing.Explicit @explicit =>
                new Exposing.Explicit(
                    ConvertNodes(@explicit.Nodes, fromStil4mConcretized)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected exposing type: " + exposing.GetType().Name),
        };

    /// <summary>
    /// Converts a Stil4mConcretized.TopLevelExpose to a Stil4mElmSyntax7.TopLevelExpose.
    /// </summary>
    public static TopLevelExpose fromStil4mConcretized(
        ConcretizedTypes.TopLevelExpose expose) =>
        expose switch
        {
            ConcretizedTypes.TopLevelExpose.InfixExpose infixExpose =>
                new TopLevelExpose.InfixExpose(infixExpose.Name),

            ConcretizedTypes.TopLevelExpose.FunctionExpose functionExpose =>
                new TopLevelExpose.FunctionExpose(functionExpose.Name),

            ConcretizedTypes.TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
                new TopLevelExpose.TypeOrAliasExpose(typeOrAliasExpose.Name),

            ConcretizedTypes.TopLevelExpose.TypeExpose typeExpose =>
                new TopLevelExpose.TypeExpose(
                    fromStil4mConcretized(typeExpose.ExposedType)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected top level expose type: " + expose.GetType().Name),
        };

    /// <summary>
    /// Converts a Stil4mConcretized.ExposedType to a Stil4mElmSyntax7.ExposedType.
    /// </summary>
    public static ExposedType fromStil4mConcretized(
        ConcretizedTypes.ExposedType exposedType) =>
        new(
            Name: exposedType.Name,
            Open: exposedType.Open);

    /// <summary>
    /// Converts a Stil4mConcretized.Import to a Stil4mElmSyntax7.Import.
    /// </summary>
    public static Import fromStil4mConcretized(
        ConcretizedTypes.Import import) =>
        new(
            ModuleName: import.ModuleName,
            ModuleAlias: import.ModuleAlias?.Alias,
            ExposingList: import.ExposingList is { } exposingList
                ? ConvertNode(exposingList.ExposingList, fromStil4mConcretized)
                : null);

    /// <summary>
    /// Converts a Stil4mConcretized.Declaration to a Stil4mElmSyntax7.Declaration.
    /// </summary>
    public static Declaration fromStil4mConcretized(
        ConcretizedTypes.Declaration declaration) =>
        declaration switch
        {
            ConcretizedTypes.Declaration.FunctionDeclaration functionDeclaration =>
                new Declaration.FunctionDeclaration(
                    fromStil4mConcretized(functionDeclaration.Function)),

            ConcretizedTypes.Declaration.CustomTypeDeclaration customTypeDeclaration =>
                new Declaration.CustomTypeDeclaration(
                    fromStil4mConcretized(customTypeDeclaration.TypeDeclaration)),

            ConcretizedTypes.Declaration.AliasDeclaration aliasDeclaration =>
                new Declaration.AliasDeclaration(
                    fromStil4mConcretized(aliasDeclaration.TypeAlias)),

            ConcretizedTypes.Declaration.PortDeclaration portDeclaration =>
                new Declaration.PortDeclaration(
                    fromStil4mConcretized(portDeclaration.Signature)),

            ConcretizedTypes.Declaration.InfixDeclaration infixDeclaration =>
                new Declaration.InfixDeclaration(
                    fromStil4mConcretized(infixDeclaration.Infix)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected declaration type: " + declaration.GetType().Name),
        };

    /// <summary>
    /// Converts a Stil4mConcretized.Infix to a Stil4mElmSyntax7.Infix.
    /// </summary>
    public static Infix fromStil4mConcretized(
        ConcretizedTypes.Infix infix) =>
        new(
            Direction: infix.Direction,
            Precedence: infix.Precedence,
            Operator: infix.Operator,
            FunctionName: infix.FunctionName);

    /// <summary>
    /// Converts a Stil4mConcretized.TypeAlias to a Stil4mElmSyntax7.TypeAlias.
    /// </summary>
    public static TypeAlias fromStil4mConcretized(
        ConcretizedTypes.TypeAlias typeAlias) =>
        new(
            Documentation: typeAlias.Documentation,
            Name: typeAlias.Name,
            Generics: typeAlias.Generics,
            TypeAnnotation: ConvertNode(typeAlias.TypeAnnotation, fromStil4mConcretized));

    /// <summary>
    /// Converts a Stil4mConcretized.TypeStruct to a Stil4mElmSyntax7.TypeStruct.
    /// </summary>
    public static TypeStruct fromStil4mConcretized(
        ConcretizedTypes.TypeStruct typeStruct) =>
        new(
            Documentation: typeStruct.Documentation,
            Name: typeStruct.Name,
            Generics: typeStruct.Generics,
            Constructors: [.. typeStruct.Constructors.Select(c => ConvertNode(c.Constructor, fromStil4mConcretized))]);

    /// <summary>
    /// Converts a Stil4mConcretized.ValueConstructor to a Stil4mElmSyntax7.ValueConstructor.
    /// </summary>
    public static ValueConstructor fromStil4mConcretized(
        ConcretizedTypes.ValueConstructor constructor) =>
        new(
            Name: constructor.Name,
            Arguments: ConvertNodes(constructor.Arguments, fromStil4mConcretized));

    /// <summary>
    /// Converts a Stil4mConcretized.TypeAnnotation to a Stil4mElmSyntax7.TypeAnnotation.
    /// </summary>
    public static TypeAnnotation fromStil4mConcretized(
        ConcretizedTypes.TypeAnnotation typeAnnotation) =>
        typeAnnotation switch
        {
            ConcretizedTypes.TypeAnnotation.GenericType genericType =>
                new TypeAnnotation.GenericType(genericType.Name),

            ConcretizedTypes.TypeAnnotation.Typed typed =>
                new TypeAnnotation.Typed(
                    typed.TypeName,
                    ConvertNodes(typed.TypeArguments, fromStil4mConcretized)),

            ConcretizedTypes.TypeAnnotation.Unit =>
                new TypeAnnotation.Unit(),

            ConcretizedTypes.TypeAnnotation.Tupled tupled =>
                new TypeAnnotation.Tupled(
                    ConvertSeparatedNodes(tupled.TypeAnnotations, fromStil4mConcretized)),

            ConcretizedTypes.TypeAnnotation.Record record =>
                new TypeAnnotation.Record(
                    fromStil4mConcretized(record.RecordDefinition)),

            ConcretizedTypes.TypeAnnotation.GenericRecord genericRecord =>
                new TypeAnnotation.GenericRecord(
                    genericRecord.GenericName,
                    ConvertNode(genericRecord.RecordDefinition, fromStil4mConcretized)),

            ConcretizedTypes.TypeAnnotation.FunctionTypeAnnotation functionType =>
                new TypeAnnotation.FunctionTypeAnnotation(
                    ConvertNode(functionType.ArgumentType, fromStil4mConcretized),
                    ConvertNode(functionType.ReturnType, fromStil4mConcretized)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected type annotation type: " + typeAnnotation.GetType().Name),
        };

    /// <summary>
    /// Converts a Stil4mConcretized.RecordDefinition to a Stil4mElmSyntax7.RecordDefinition.
    /// </summary>
    public static RecordDefinition fromStil4mConcretized(
        ConcretizedTypes.RecordDefinition recordDefinition) =>
        new(
            Fields: ConvertSeparatedNodes(recordDefinition.Fields, fromStil4mConcretized));

    /// <summary>
    /// Converts a Stil4mConcretized.RecordField to a Stil4mElmSyntax7.RecordField.
    /// </summary>
    public static RecordField fromStil4mConcretized(
        ConcretizedTypes.RecordField recordField) =>
        new(
            FieldName: recordField.FieldName,
            FieldType: ConvertNode(recordField.FieldType, fromStil4mConcretized));

    /// <summary>
    /// Converts a Stil4mConcretized.FunctionStruct to a Stil4mElmSyntax7.FunctionStruct.
    /// </summary>
    public static FunctionStruct fromStil4mConcretized(
        ConcretizedTypes.FunctionStruct functionStruct) =>
        new(
            Documentation: functionStruct.Documentation,
            Signature: functionStruct.Signature is { } sig
                ? ConvertNode(sig, fromStil4mConcretized)
                : null,
            Declaration: ConvertNode(functionStruct.Declaration, fromStil4mConcretized));

    /// <summary>
    /// Converts a Stil4mConcretized.FunctionImplementation to a Stil4mElmSyntax7.FunctionImplementation.
    /// </summary>
    public static FunctionImplementation fromStil4mConcretized(
        ConcretizedTypes.FunctionImplementation functionImplementation) =>
        new(
            Name: functionImplementation.Name,
            Arguments: ConvertNodes(functionImplementation.Arguments, fromStil4mConcretized),
            Expression: ConvertExpressionNode(functionImplementation.Expression));

    /// <summary>
    /// Converts a Stil4mConcretized.Signature to a Stil4mElmSyntax7.Signature.
    /// </summary>
    public static Signature fromStil4mConcretized(
        ConcretizedTypes.Signature signature) =>
        new(
            Name: signature.Name,
            TypeAnnotation: ConvertNode(signature.TypeAnnotation, fromStil4mConcretized));

    /// <summary>
    /// Converts a Stil4mConcretized.Pattern to a Stil4mElmSyntax7.Pattern.
    /// </summary>
    public static Pattern fromStil4mConcretized(
        ConcretizedTypes.Pattern pattern) =>
        pattern switch
        {
            ConcretizedTypes.Pattern.AllPattern =>
                new Pattern.AllPattern(),

            ConcretizedTypes.Pattern.VarPattern varPattern =>
                new Pattern.VarPattern(varPattern.Name),

            ConcretizedTypes.Pattern.UnitPattern =>
                new Pattern.UnitPattern(),

            ConcretizedTypes.Pattern.CharPattern charPattern =>
                new Pattern.CharPattern(charPattern.Value),

            ConcretizedTypes.Pattern.StringPattern stringPattern =>
                new Pattern.StringPattern(stringPattern.Value),

            ConcretizedTypes.Pattern.IntPattern intPattern =>
                new Pattern.IntPattern(intPattern.Value),

            ConcretizedTypes.Pattern.HexPattern hexPattern =>
                new Pattern.HexPattern(hexPattern.Value),

            ConcretizedTypes.Pattern.FloatPattern floatPattern =>
                new Pattern.FloatPattern(floatPattern.Value),

            ConcretizedTypes.Pattern.TuplePattern tuplePattern =>
                new Pattern.TuplePattern(
                    ConvertNodes(tuplePattern.Elements, fromStil4mConcretized)),

            ConcretizedTypes.Pattern.RecordPattern recordPattern =>
                new Pattern.RecordPattern(recordPattern.Fields),

            ConcretizedTypes.Pattern.UnConsPattern unConsPattern =>
                new Pattern.UnConsPattern(
                    ConvertNode(unConsPattern.Head, fromStil4mConcretized),
                    ConvertNode(unConsPattern.Tail, fromStil4mConcretized)),

            ConcretizedTypes.Pattern.ListPattern listPattern =>
                new Pattern.ListPattern(
                    ConvertNodes(listPattern.Elements, fromStil4mConcretized)),

            ConcretizedTypes.Pattern.NamedPattern namedPattern =>
                new Pattern.NamedPattern(
                    fromStil4mConcretized(namedPattern.Name),
                    ConvertNodes(namedPattern.Arguments, fromStil4mConcretized)),

            ConcretizedTypes.Pattern.AsPattern asPattern =>
                new Pattern.AsPattern(
                    ConvertNode(asPattern.Pattern, fromStil4mConcretized),
                    asPattern.Name),

            ConcretizedTypes.Pattern.ParenthesizedPattern parenthesizedPattern =>
                new Pattern.ParenthesizedPattern(
                    ConvertNode(parenthesizedPattern.Pattern, fromStil4mConcretized)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected pattern type: " + pattern.GetType().Name),
        };

    /// <summary>
    /// Converts a Stil4mConcretized.QualifiedNameRef to a Stil4mElmSyntax7.QualifiedNameRef.
    /// </summary>
    public static QualifiedNameRef fromStil4mConcretized(
        ConcretizedTypes.QualifiedNameRef qualifiedNameRef) =>
        new(
            ModuleName: qualifiedNameRef.ModuleName,
            Name: qualifiedNameRef.Name);

    /// <summary>
    /// Converts a Stil4mConcretized.Expression to a Stil4mElmSyntax7.Expression.
    /// Note: For RecordExpr and RecordUpdateExpression, use ConvertExpressionNode instead
    /// as they need the containing node's range to compute the closing brace location.
    /// </summary>
    public static Expression fromStil4mConcretized(
        ConcretizedTypes.Expression expression) =>
        expression switch
        {
            ConcretizedTypes.Expression.UnitExpr =>
                new Expression.UnitExpr(),

            ConcretizedTypes.Expression.Literal literal =>
                new Expression.Literal(literal.Value, literal.IsTripleQuoted),

            ConcretizedTypes.Expression.CharLiteral charLiteral =>
                new Expression.CharLiteral(charLiteral.Value),

            ConcretizedTypes.Expression.Integer integer =>
                new Expression.Integer(integer.Value),

            ConcretizedTypes.Expression.Hex hex =>
                new Expression.Hex(hex.Value),

            ConcretizedTypes.Expression.Floatable floatable =>
                new Expression.Floatable(floatable.Value),

            ConcretizedTypes.Expression.Negation negation =>
                new Expression.Negation(
                    ConvertExpressionNode(negation.Expression)),

            ConcretizedTypes.Expression.ListExpr listExpr =>
                new Expression.ListExpr(
                    ConvertExpressionNodes(listExpr.Elements)),

            ConcretizedTypes.Expression.FunctionOrValue functionOrValue =>
                new Expression.FunctionOrValue(
                    functionOrValue.ModuleName,
                    functionOrValue.Name),

            ConcretizedTypes.Expression.IfBlock ifBlock =>
                new Expression.IfBlock(
                    ConvertExpressionNode(ifBlock.Condition),
                    ConvertExpressionNode(ifBlock.ThenBlock),
                    ConvertExpressionNode(ifBlock.ElseBlock)),

            ConcretizedTypes.Expression.PrefixOperator prefixOperator =>
                new Expression.PrefixOperator(prefixOperator.Operator),

            ConcretizedTypes.Expression.ParenthesizedExpression parenthesizedExpression =>
                new Expression.ParenthesizedExpression(
                    ConvertExpressionNode(parenthesizedExpression.Expression)),

            ConcretizedTypes.Expression.Application application =>
                new Expression.Application(
                    ConvertExpressionNodes(application.Arguments)),

            ConcretizedTypes.Expression.OperatorApplication operatorApplication =>
                new Expression.OperatorApplication(
                    operatorApplication.Operator,
                    operatorApplication.Direction,
                    ConvertExpressionNode(operatorApplication.Left),
                    ConvertExpressionNode(operatorApplication.Right)),

            ConcretizedTypes.Expression.TupledExpression tupledExpression =>
                new Expression.TupledExpression(
                    ConvertExpressionNodes(tupledExpression.Elements)),

            ConcretizedTypes.Expression.LambdaExpression lambdaExpression =>
                new Expression.LambdaExpression(
                    fromStil4mConcretized(lambdaExpression.Lambda)),

            ConcretizedTypes.Expression.CaseExpression caseExpression =>
                new Expression.CaseExpression(
                    fromStil4mConcretized(caseExpression.CaseBlock)),

            ConcretizedTypes.Expression.LetExpression letExpression =>
                new Expression.LetExpression(
                    fromStil4mConcretized(letExpression.Value)),

            // RecordExpr and RecordUpdateExpression need the containing node's range,
            // so they should be converted via ConvertExpressionNode
            ConcretizedTypes.Expression.RecordExpr =>
                throw new System.InvalidOperationException(
                    "RecordExpr must be converted via ConvertExpressionNode to access the containing node's range"),

            ConcretizedTypes.Expression.RecordAccess recordAccess =>
                new Expression.RecordAccess(
                    ConvertExpressionNode(recordAccess.Record),
                    recordAccess.FieldName),

            ConcretizedTypes.Expression.RecordAccessFunction recordAccessFunction =>
                new Expression.RecordAccessFunction(recordAccessFunction.FunctionName),

            ConcretizedTypes.Expression.RecordUpdateExpression =>
                throw new System.InvalidOperationException(
                    "RecordUpdateExpression must be converted via ConvertExpressionNode to access the containing node's range"),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected expression type: " + expression.GetType().Name),
        };

    /// <summary>
    /// Converts a Node&lt;Expression&gt; to Node&lt;Stil4mElmSyntax7.Expression&gt;.
    /// This method handles RecordExpr and RecordUpdateExpression specially by using
    /// the node's range to compute the closing brace location.
    /// </summary>
    private static Node<Expression> ConvertExpressionNode(
        Node<ConcretizedTypes.Expression> node)
    {
        // For RecordExpr and RecordUpdateExpression, we need the node's range to compute closing brace location
        // The closing brace is at Range.End with column - 1 (since Range.End is after the brace)
        var closeBraceLocation = new Location(node.Range.End.Row, node.Range.End.Column - 1);

        var convertedValue = node.Value switch
        {
            ConcretizedTypes.Expression.RecordExpr recordExpr =>
                new Expression.RecordExpr(
                    ConvertRecordFields(recordExpr.Fields, closeBraceLocation, isRecordUpdateExpression: false)),

            ConcretizedTypes.Expression.RecordUpdateExpression recordUpdateExpression =>
                new Expression.RecordUpdateExpression(
                    recordUpdateExpression.RecordName,
                    ConvertRecordFields(recordUpdateExpression.Fields, closeBraceLocation, isRecordUpdateExpression: true)),

            // For all other expression types, use the standard conversion
            var expr => fromStil4mConcretized(expr)
        };

        return new Node<Expression>(node.Range, convertedValue);
    }

    /// <summary>
    /// Converts a list of expression nodes.
    /// </summary>
    private static IReadOnlyList<Node<Expression>> ConvertExpressionNodes(
        IReadOnlyList<Node<ConcretizedTypes.Expression>> nodes) =>
        [.. nodes.Select(ConvertExpressionNode)];

    /// <summary>
    /// Converts a SeparatedSyntaxList of expression nodes to an IReadOnlyList.
    /// </summary>
    private static IReadOnlyList<Node<Expression>> ConvertExpressionNodes(
        ConcretizedTypes.SeparatedSyntaxList<Node<ConcretizedTypes.Expression>> separatedList) =>
        [.. ToList(separatedList).Select(ConvertExpressionNode)];

    /// <summary>
    /// Converts a Stil4mConcretized.LambdaStruct to a Stil4mElmSyntax7.LambdaStruct.
    /// </summary>
    public static LambdaStruct fromStil4mConcretized(
        ConcretizedTypes.LambdaStruct lambdaStruct) =>
        new(
            Arguments: ConvertNodes(lambdaStruct.Arguments, fromStil4mConcretized),
            Expression: ConvertExpressionNode(lambdaStruct.Expression));

    /// <summary>
    /// Converts a Stil4mConcretized.CaseBlock to a Stil4mElmSyntax7.CaseBlock.
    /// </summary>
    public static CaseBlock fromStil4mConcretized(
        ConcretizedTypes.CaseBlock caseBlock) =>
        new(
            Expression: ConvertExpressionNode(caseBlock.Expression),
            Cases: [.. caseBlock.Cases.Select(fromStil4mConcretized)]);

    /// <summary>
    /// Converts a Stil4mConcretized.Case to a Stil4mElmSyntax7.Case.
    /// </summary>
    public static Case fromStil4mConcretized(
        ConcretizedTypes.Case @case) =>
        new(
            Pattern: ConvertNode(@case.Pattern, fromStil4mConcretized),
            Expression: ConvertExpressionNode(@case.Expression));

    /// <summary>
    /// Converts a Stil4mConcretized.Expression.LetBlock to a Stil4mElmSyntax7.Expression.LetBlock.
    /// </summary>
    public static Expression.LetBlock fromStil4mConcretized(
        ConcretizedTypes.Expression.LetBlock letBlock) =>
        new(
            Declarations: ConvertNodes(letBlock.Declarations, fromStil4mConcretized),
            Expression: ConvertExpressionNode(letBlock.Expression));

    /// <summary>
    /// Converts a Stil4mConcretized.Expression.LetDeclaration to a Stil4mElmSyntax7.Expression.LetDeclaration.
    /// </summary>
    public static Expression.LetDeclaration fromStil4mConcretized(
        ConcretizedTypes.Expression.LetDeclaration letDeclaration) =>
        letDeclaration switch
        {
            ConcretizedTypes.Expression.LetDeclaration.LetFunction letFunction =>
                new Expression.LetDeclaration.LetFunction(
                    fromStil4mConcretized(letFunction.Function)),

            ConcretizedTypes.Expression.LetDeclaration.LetDestructuring letDestructuring =>
                new Expression.LetDeclaration.LetDestructuring(
                    ConvertNode(letDestructuring.Pattern, fromStil4mConcretized),
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
        ConcretizedTypes.SeparatedSyntaxList<TNode> separatedList) =>
        separatedList switch
        {
            ConcretizedTypes.SeparatedSyntaxList<TNode>.Empty => [],
            ConcretizedTypes.SeparatedSyntaxList<TNode>.NonEmpty nonEmpty =>
                [nonEmpty.First, .. nonEmpty.Rest.Select(r => r.Node)],
            _ => throw new System.NotImplementedException(
                "Unexpected SeparatedSyntaxList type: " + separatedList.GetType().Name),
        };

    /// <summary>
    /// Converts a SeparatedSyntaxList of nodes to an IReadOnlyList, applying a converter to each node's value.
    /// </summary>
    public static IReadOnlyList<Node<TResult>> ConvertSeparatedNodes<TSource, TResult>(
        ConcretizedTypes.SeparatedSyntaxList<Node<TSource>> separatedList,
        System.Func<TSource, TResult> converter) =>
        [.. ToList(separatedList).Select(node => ConvertNode(node, converter))];

    private static IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>
        ConvertRecordFields(
            ConcretizedTypes.SeparatedSyntaxList<ConcretizedTypes.RecordExprField> fields,
            Location closeBraceLocation,
            bool isRecordUpdateExpression = false)
    {
        if (fields is not ConcretizedTypes.SeparatedSyntaxList<ConcretizedTypes.RecordExprField>.NonEmpty nonEmpty)
        {
            return [];
        }

        var result = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

        bool isSingleField = nonEmpty.Rest.Count == 0;

        // Elm parser quirk: The behavior differs between single-line and multi-line records.
        // Detect multi-line by comparing the first field's row with the closing brace row.
        bool isMultiLine = closeBraceLocation.Row > nonEmpty.First.FieldName.Range.Start.Row;

        // Process first field
        {
            var convertedValueExpr = ConvertExpressionNode(nonEmpty.First.ValueExpr);
            Location fieldRangeEnd;
            if (isSingleField)
            {
                // Single-field records:
                // - RecordUpdateExpression: use closing brace position
                // - RecordExpr: use expression end
                fieldRangeEnd = isRecordUpdateExpression
                    ? closeBraceLocation
                    : convertedValueExpr.Range.End;
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
            result.Add(new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                new Range(nonEmpty.First.FieldName.Range.Start, fieldRangeEnd),
                (nonEmpty.First.FieldName, convertedValueExpr)));
        }

        // Process remaining fields
        for (int i = 0; i < nonEmpty.Rest.Count; i++)
        {
            var (_, field) = nonEmpty.Rest[i];
            var convertedValueExpr = ConvertExpressionNode(field.ValueExpr);

            bool isLastField = i == nonEmpty.Rest.Count - 1;
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

            result.Add(new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                new Range(field.FieldName.Range.Start, fieldRangeEnd),
                (field.FieldName, convertedValueExpr)));
        }

        return result;
    }
}
