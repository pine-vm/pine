using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Linq;

using FullTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Conversion functions from Stil4mElmSyntax7 types to the full syntax model types.
/// This is the inverse of <see cref="FromFullSyntaxModel"/>.
/// Token locations are invented using a default location (row 1, column 1).
/// The slight mismatch doesn't matter because the formatter will rearrange tokens afterward.
/// </summary>
public static class ToFullSyntaxModel
{
    /// <summary>
    /// A default location used for invented token positions.
    /// All nodes are given the same row so the formatter treats them as single-line,
    /// then the formatter will properly position them.
    /// </summary>
    private static readonly Location s_defaultLocation =
        new(Row: 1, Column: 1);

    /// <summary>
    /// A default range used for invented node positions.
    /// All nodes get the same row so the formatter treats them as single-line.
    /// </summary>
    private static readonly Range s_defaultRange =
        new(Start: s_defaultLocation, End: s_defaultLocation);

    /// <summary>
    /// Converts a File.
    /// </summary>
    public static FullTypes.File Convert(
        File file)
    {
        return
            new FullTypes.File(
                ModuleDefinition: ConvertNode(file.ModuleDefinition, Convert),
                Imports: ConvertNodes(file.Imports, Convert),
                Declarations: ConvertNodes(file.Declarations, Convert),
                Comments: ConvertComments(file.Comments),
                IncompleteDeclarations: []);
    }

    /// <summary>
    /// Converts a Module.
    /// </summary>
    public static FullTypes.Module Convert(
        Module module) =>
        module switch
        {
            Module.NormalModule normalModule =>
            new FullTypes.Module.NormalModule(
                ModuleTokenLocation: s_defaultLocation,
                ModuleData: Convert(normalModule.ModuleData)),

            Module.PortModule portModule =>
            new FullTypes.Module.PortModule(
                PortTokenLocation: s_defaultLocation,
                ModuleTokenLocation: s_defaultLocation,
                ModuleData: Convert(portModule.ModuleData)),

            Module.EffectModule effectModule =>
            new FullTypes.Module.EffectModule(
                EffectTokenLocation: s_defaultLocation,
                ModuleTokenLocation: s_defaultLocation,
                ModuleData: Convert(effectModule.ModuleData)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().Name),
        };

    /// <summary>
    /// Converts a DefaultModuleData.
    /// </summary>
    public static FullTypes.DefaultModuleData Convert(
        DefaultModuleData moduleData) =>
        new(
            ModuleName: ConvertNodePreserveValue(moduleData.ModuleName),
            ExposingTokenLocation: s_defaultLocation,
            ExposingList: ConvertNode(moduleData.ExposingList, Convert));

    /// <summary>
    /// Converts a EffectModuleData.
    /// </summary>
    public static FullTypes.EffectModuleData Convert(
        EffectModuleData moduleData) =>
        new(
            ModuleName: ConvertNodePreserveValue(moduleData.ModuleName),
            ExposingTokenLocation: s_defaultLocation,
            ExposingList: ConvertNode(moduleData.ExposingList, Convert),
            Command: moduleData.Command is { } cmd ? ConvertNodePreserveValue(cmd) : null,
            Subscription: moduleData.Subscription is { } sub ? ConvertNodePreserveValue(sub) : null);

    /// <summary>
    /// Converts a Exposing.
    /// </summary>
    public static FullTypes.Exposing Convert(
        Exposing exposing) =>
        exposing switch
        {
            Exposing.All _ =>
            new FullTypes.Exposing.All(s_defaultRange),

            Exposing.Explicit @explicit =>
            new FullTypes.Exposing.Explicit(
                OpenParenLocation: s_defaultLocation,
                Nodes: ToSeparatedList(@explicit.Nodes, Convert),
                CloseParenLocation: s_defaultLocation),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().Name),
        };

    /// <summary>
    /// Converts a TopLevelExpose.
    /// </summary>
    public static FullTypes.TopLevelExpose Convert(
        TopLevelExpose expose) =>
        expose switch
        {
            TopLevelExpose.InfixExpose infixExpose =>
            new FullTypes.TopLevelExpose.InfixExpose(infixExpose.Name),

            TopLevelExpose.FunctionExpose functionExpose =>
            new FullTypes.TopLevelExpose.FunctionExpose(functionExpose.Name),

            TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
            new FullTypes.TopLevelExpose.TypeOrAliasExpose(typeOrAliasExpose.Name),

            TopLevelExpose.TypeExpose typeExpose =>
            new FullTypes.TopLevelExpose.TypeExpose(
                Convert(typeExpose.ExposedType)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected top level expose type: " + expose.GetType().Name),
        };

    /// <summary>
    /// Converts a ExposedType.
    /// </summary>
    public static FullTypes.ExposedType Convert(
        ExposedType exposedType) =>
        new(
            Name: exposedType.Name,
            Open: exposedType.Open is { } ? s_defaultRange : null);

    /// <summary>
    /// Converts a Import.
    /// </summary>
    public static FullTypes.Import Convert(
        Import import) =>
        new(
            ImportTokenLocation: s_defaultLocation,
            ModuleName: ConvertNodePreserveValue(import.ModuleName),
            ModuleAlias: import.ModuleAlias is { } alias
            ?
            (s_defaultLocation, ConvertNodePreserveValue(alias))
            :
            null,
            ExposingList: import.ExposingList is { } exposingList
            ?
            (s_defaultLocation, ConvertNode(exposingList, Convert))
            :
            null);

    /// <summary>
    /// Converts a Declaration.
    /// </summary>
    public static FullTypes.Declaration Convert(
        Declaration declaration) =>
        declaration switch
        {
            Declaration.FunctionDeclaration functionDeclaration =>
            new FullTypes.Declaration.FunctionDeclaration(
                Convert(functionDeclaration.Function)),

            Declaration.CustomTypeDeclaration customTypeDeclaration =>
            new FullTypes.Declaration.CustomTypeDeclaration(
                Convert(customTypeDeclaration.TypeDeclaration)),

            Declaration.AliasDeclaration aliasDeclaration =>
            new FullTypes.Declaration.AliasDeclaration(
                Convert(aliasDeclaration.TypeAlias)),

            Declaration.PortDeclaration portDeclaration =>
            new FullTypes.Declaration.PortDeclaration(
                PortTokenLocation: s_defaultLocation,
                Signature: Convert(portDeclaration.Signature)),

            Declaration.InfixDeclaration infixDeclaration =>
            new FullTypes.Declaration.InfixDeclaration(
                Convert(infixDeclaration.Infix)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().Name),
        };

    /// <summary>
    /// Converts a Infix.
    /// </summary>
    public static FullTypes.Infix Convert(
        Infix infix) =>
        new(
            InfixTokenLocation: s_defaultLocation,
            Direction: ConvertNodePreserveValue(infix.Direction),
            Precedence: ConvertNodePreserveValue(infix.Precedence),
            Operator: ConvertNodePreserveValue(infix.Operator),
            EqualsTokenLocation: s_defaultLocation,
            FunctionName: ConvertNodePreserveValue(infix.FunctionName));

    /// <summary>
    /// Converts a TypeAlias.
    /// </summary>
    public static FullTypes.TypeAlias Convert(
        TypeAlias typeAlias) =>
        new(
            Documentation: typeAlias.Documentation is { } doc ? ConvertNodePreserveValue(doc) : null,
            TypeTokenLocation: s_defaultLocation,
            AliasTokenLocation: s_defaultLocation,
            Name: ConvertNodePreserveValue(typeAlias.Name),
            Generics: ConvertNodesPreserveValue(typeAlias.Generics),
            EqualsTokenLocation: s_defaultLocation,
            TypeAnnotation: ConvertNode(typeAlias.TypeAnnotation, Convert));

    /// <summary>
    /// Converts a TypeStruct.
    /// </summary>
    public static FullTypes.TypeStruct Convert(
        TypeStruct typeStruct) =>
        new(
            Documentation: typeStruct.Documentation is { } doc ? ConvertNodePreserveValue(doc) : null,
            TypeTokenLocation: s_defaultLocation,
            Name: ConvertNodePreserveValue(typeStruct.Name),
            Generics: ConvertNodesPreserveValue(typeStruct.Generics),
            EqualsTokenLocation: s_defaultLocation,
            Constructors:
            [
            .. typeStruct.Constructors
                .Select(
                (c, i) => (PipeTokenLocation: i > 0 ? s_defaultLocation : (Location?)null,
                Constructor: ConvertNode(c, Convert)))
            ]);

    /// <summary>
    /// Converts a ValueConstructor.
    /// </summary>
    public static FullTypes.ValueConstructor Convert(
        ValueConstructor constructor) =>
        new(
            Name: ConvertNodePreserveValue(constructor.Name),
            Arguments: ConvertNodes(constructor.Arguments, Convert));

    /// <summary>
    /// Converts a TypeAnnotation.
    /// </summary>
    public static FullTypes.TypeAnnotation Convert(
        TypeAnnotation typeAnnotation) =>
        typeAnnotation switch
        {
            TypeAnnotation.GenericType genericType =>
            new FullTypes.TypeAnnotation.GenericType(genericType.Name),

            TypeAnnotation.Typed typed =>
            new FullTypes.TypeAnnotation.Typed(
                ConvertNodePreserveValue(typed.TypeName),
                ConvertNodes(typed.TypeArguments, Convert)),

            TypeAnnotation.Unit =>
            new FullTypes.TypeAnnotation.Unit(),

            TypeAnnotation.Tupled tupled =>
            new FullTypes.TypeAnnotation.Tupled(
                TypeAnnotations: ToSeparatedList(tupled.TypeAnnotations, Convert)),

            TypeAnnotation.Record record =>
            new FullTypes.TypeAnnotation.Record(
                RecordDefinition: Convert(record.RecordDefinition)),

            TypeAnnotation.GenericRecord genericRecord =>
            new FullTypes.TypeAnnotation.GenericRecord(
                GenericName: ConvertNodePreserveValue(genericRecord.GenericName),
                PipeLocation: s_defaultLocation,
                RecordDefinition: ConvertNode(genericRecord.RecordDefinition, Convert)),

            TypeAnnotation.FunctionTypeAnnotation functionType =>
            new FullTypes.TypeAnnotation.FunctionTypeAnnotation(
                ArgumentType: ConvertFunctionTypeArgument(functionType.ArgumentType),
                ArrowLocation: s_defaultLocation,
                ReturnType: ConvertNode(functionType.ReturnType, Convert)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type annotation type: " + typeAnnotation.GetType().Name),
        };

    /// <summary>
    /// Converts a RecordDefinition.
    /// </summary>
    public static FullTypes.RecordDefinition Convert(
        RecordDefinition recordDefinition) =>
        new(
            Fields: ToSeparatedList(recordDefinition.Fields, Convert));

    /// <summary>
    /// Converts a RecordField.
    /// </summary>
    public static FullTypes.RecordField Convert(
        RecordField recordField) =>
        new(
            FieldName: ConvertNodePreserveValue(recordField.FieldName),
            ColonLocation: s_defaultLocation,
            FieldType: ConvertNode(recordField.FieldType, Convert));

    /// <summary>
    /// Converts a FunctionStruct.
    /// </summary>
    public static FullTypes.FunctionStruct Convert(
        FunctionStruct functionStruct) =>
        new(
            Documentation: functionStruct.Documentation is { } doc ? ConvertNodePreserveValue(doc) : null,
            Signature: functionStruct.Signature is { } sig
            ?
            ConvertNode(sig, Convert)
            :
            null,
            Declaration: ConvertNode(functionStruct.Declaration, Convert));

    /// <summary>
    /// Converts a FunctionImplementation.
    /// </summary>
    public static FullTypes.FunctionImplementation Convert(
        FunctionImplementation functionImplementation) =>
        new(
            Name: ConvertNodePreserveValue(functionImplementation.Name),
            Arguments: ConvertNodes(functionImplementation.Arguments, Convert),
            EqualsTokenLocation: s_defaultLocation,
            Expression: ConvertNode(functionImplementation.Expression, Convert));

    /// <summary>
    /// Converts a Signature.
    /// </summary>
    public static FullTypes.Signature Convert(
        Signature signature) =>
        new(
            Name: ConvertNodePreserveValue(signature.Name),
            ColonLocation: s_defaultLocation,
            TypeAnnotation: ConvertNode(signature.TypeAnnotation, Convert));

    /// <summary>
    /// Converts a Pattern.
    /// </summary>
    public static FullTypes.Pattern Convert(
        Pattern pattern) =>
        pattern switch
        {
            Pattern.AllPattern =>
            new FullTypes.Pattern.AllPattern(),

            Pattern.VarPattern varPattern =>
            new FullTypes.Pattern.VarPattern(varPattern.Name),

            Pattern.UnitPattern =>
            new FullTypes.Pattern.UnitPattern(),

            Pattern.CharPattern charPattern =>
            new FullTypes.Pattern.CharPattern(charPattern.Value),

            Pattern.StringPattern stringPattern =>
            new FullTypes.Pattern.StringPattern(stringPattern.Value),

            Pattern.IntPattern intPattern =>
            new FullTypes.Pattern.IntPattern(intPattern.Value),

            Pattern.HexPattern hexPattern =>
            new FullTypes.Pattern.HexPattern(hexPattern.Value),

            Pattern.FloatPattern floatPattern =>
            new FullTypes.Pattern.FloatPattern(floatPattern.Value),

            Pattern.TuplePattern tuplePattern =>
            new FullTypes.Pattern.TuplePattern(
                Elements: ToSeparatedList(tuplePattern.Elements, Convert)),

            Pattern.RecordPattern recordPattern =>
            new FullTypes.Pattern.RecordPattern(
                Fields: ToSeparatedListPreserveValue(recordPattern.Fields)),

            Pattern.UnConsPattern unConsPattern =>
            new FullTypes.Pattern.UnConsPattern(
                Head: ConvertNode(unConsPattern.Head, Convert),
                ConsOperatorLocation: s_defaultLocation,
                Tail: ConvertNode(unConsPattern.Tail, Convert)),

            Pattern.ListPattern listPattern =>
            new FullTypes.Pattern.ListPattern(
                Elements: ToSeparatedList(listPattern.Elements, Convert)),

            Pattern.NamedPattern namedPattern =>
            new FullTypes.Pattern.NamedPattern(
                Name: Convert(namedPattern.Name),
                Arguments: ConvertNodes(namedPattern.Arguments, Convert)),

            Pattern.AsPattern asPattern =>
            new FullTypes.Pattern.AsPattern(
                Pattern: ConvertNode(asPattern.Pattern, Convert),
                AsTokenLocation: s_defaultLocation,
                Name: ConvertNodePreserveValue(asPattern.Name)),

            Pattern.ParenthesizedPattern parenthesizedPattern =>
            new FullTypes.Pattern.ParenthesizedPattern(
                Pattern: ConvertNode(parenthesizedPattern.Pattern, Convert)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected pattern type: " + pattern.GetType().Name),
        };

    /// <summary>
    /// Converts a QualifiedNameRef.
    /// </summary>
    public static FullTypes.QualifiedNameRef Convert(
        QualifiedNameRef qualifiedNameRef) =>
        new(
            ModuleName: qualifiedNameRef.ModuleName,
            Name: qualifiedNameRef.Name);

    /// <summary>
    /// Converts a Expression.
    /// </summary>
    public static FullTypes.Expression Convert(
        Expression expression) =>
        expression switch
        {
            Expression.UnitExpr =>
            new FullTypes.Expression.UnitExpr(),

            Expression.Literal literal =>
            new FullTypes.Expression.Literal(literal.Value, literal.IsTripleQuoted),

            Expression.CharLiteral charLiteral =>
            new FullTypes.Expression.CharLiteral(charLiteral.Value),

            Expression.Integer integer =>
            new FullTypes.Expression.Integer(integer.Value.ToString()),

            Expression.Hex hex =>
            new FullTypes.Expression.Integer(FormatHexLiteral(hex.Value)),

            Expression.Floatable floatable =>
            new FullTypes.Expression.Floatable(floatable.LiteralText),

            Expression.Negation negation =>
            new FullTypes.Expression.Negation(
                ConvertNode(negation.Expression, Convert)),

            Expression.ListExpr listExpr =>
            new FullTypes.Expression.ListExpr(
                Elements: ToSeparatedList(listExpr.Elements, Convert)),

            Expression.FunctionOrValue functionOrValue =>
            new FullTypes.Expression.FunctionOrValue(
                functionOrValue.ModuleName,
                functionOrValue.Name),

            Expression.IfBlock ifBlock =>
            new FullTypes.Expression.IfBlock(
                IfTokenLocation: s_defaultLocation,
                Condition: ConvertNode(ifBlock.Condition, Convert),
                ThenTokenLocation: s_defaultLocation,
                ThenBlock: ConvertNode(ifBlock.ThenBlock, Convert),
                ElseTokenLocation: s_defaultLocation,
                ElseBlock: ConvertNode(ifBlock.ElseBlock, Convert)),

            Expression.PrefixOperator prefixOperator =>
            new FullTypes.Expression.PrefixOperator(prefixOperator.Operator),

            Expression.ParenthesizedExpression parenthesizedExpression =>
            new FullTypes.Expression.ParenthesizedExpression(
                Expression: ConvertNode(parenthesizedExpression.Expression, Convert)),

            Expression.Application application =>
            new FullTypes.Expression.Application(
                ConvertNodes(application.Arguments, Convert)),

            Expression.OperatorApplication operatorApplication =>
            new FullTypes.Expression.OperatorApplication(
                new Node<string>(s_defaultRange, operatorApplication.Operator),
                operatorApplication.Direction,
                ConvertNode(operatorApplication.Left, Convert),
                ConvertNode(operatorApplication.Right, Convert)),

            Expression.TupledExpression tupledExpression =>
            new FullTypes.Expression.TupledExpression(
                Elements: ToSeparatedList(tupledExpression.Elements, Convert)),

            Expression.LambdaExpression lambdaExpression =>
            new FullTypes.Expression.LambdaExpression(
                Convert(lambdaExpression.Lambda)),

            Expression.CaseExpression caseExpression =>
            new FullTypes.Expression.CaseExpression(
                Convert(caseExpression.CaseBlock)),

            Expression.LetExpression letExpression =>
            new FullTypes.Expression.LetExpression(
                Convert(letExpression.Value)),

            Expression.RecordExpr recordExpr =>
            new FullTypes.Expression.RecordExpr(
                Fields: ToSeparatedRecordFields(recordExpr.Fields)),

            Expression.RecordAccess recordAccess =>
            new FullTypes.Expression.RecordAccess(
                ConvertNode(recordAccess.Record, Convert),
                ConvertNodePreserveValue(recordAccess.FieldName)),

            Expression.RecordAccessFunction recordAccessFunction =>
            new FullTypes.Expression.RecordAccessFunction(recordAccessFunction.FunctionName),

            Expression.RecordUpdateExpression recordUpdateExpression =>
            new FullTypes.Expression.RecordUpdateExpression(
                RecordName: ConvertNodePreserveValue(recordUpdateExpression.RecordName),
                PipeLocation: s_defaultLocation,
                Fields: ToSeparatedRecordFields(recordUpdateExpression.Fields)),

            Expression.GLSLExpression glslExpression =>
            new FullTypes.Expression.GLSLExpression(glslExpression.ShaderCode),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected expression type: " + expression.GetType().Name),
        };

    /// <summary>
    /// Converts a LambdaStruct.
    /// </summary>
    public static FullTypes.LambdaStruct Convert(
        LambdaStruct lambdaStruct) =>
        new(
            BackslashLocation: s_defaultLocation,
            Arguments: ConvertNodes(lambdaStruct.Arguments, Convert),
            ArrowLocation: s_defaultLocation,
            Expression: ConvertNode(lambdaStruct.Expression, Convert));

    /// <summary>
    /// Converts a CaseBlock.
    /// </summary>
    public static FullTypes.CaseBlock Convert(
        CaseBlock caseBlock) =>
        new(
            CaseTokenLocation: s_defaultLocation,
            Expression: ConvertNode(caseBlock.Expression, Convert),
            OfTokenLocation: s_defaultLocation,
            Cases: [.. caseBlock.Cases.Select(Convert)]);

    /// <summary>
    /// Converts a Case.
    /// </summary>
    public static FullTypes.Case Convert(
        Case @case) =>
        new(
            Pattern: ConvertNode(@case.Pattern, Convert),
            ArrowLocation: s_defaultLocation,
            Expression: ConvertNode(@case.Expression, Convert));

    /// <summary>
    /// Converts a Expression.LetBlock.
    /// </summary>
    public static FullTypes.Expression.LetBlock Convert(
        Expression.LetBlock letBlock) =>
        new(
            LetTokenLocation: s_defaultLocation,
            Declarations: ConvertNodes(letBlock.Declarations, Convert),
            InTokenLocation: s_defaultLocation,
            Expression: ConvertNode(letBlock.Expression, Convert));

    /// <summary>
    /// Converts a Expression.LetDeclaration.
    /// </summary>
    public static FullTypes.Expression.LetDeclaration Convert(
        Expression.LetDeclaration letDeclaration) =>
        letDeclaration switch
        {
            Expression.LetDeclaration.LetFunction letFunction =>
            new FullTypes.Expression.LetDeclaration.LetFunction(
                Convert(letFunction.Function)),

            Expression.LetDeclaration.LetDestructuring letDestructuring =>
            new FullTypes.Expression.LetDeclaration.LetDestructuring(
                ConvertNode(letDestructuring.Pattern, Convert),
                EqualsTokenLocation: s_defaultLocation,
                Expression: ConvertNode(letDestructuring.Expression, Convert)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected let declaration type: " + letDeclaration.GetType().Name),
        };

    // Helper methods for converting nodes and lists

    /// <summary>
    /// Converts a function type argument, wrapping it in parentheses if it's itself a function type.
    /// This is necessary because in Elm, function types are right-associative, so
    /// `(a -> b) -> c` is different from `a -> b -> c` (which equals `a -> (b -> c)`).
    /// 
    /// Implementation note: In the full syntax tree, parentheses around type annotations
    /// are represented using <see cref="FullTypes.TypeAnnotation.Tupled"/> with a single
    /// element. The formatter recognizes this pattern and renders it as `(element)` rather than
    /// `( element )` which would be used for multi-element tuples.
    /// </summary>
    private static Node<FullTypes.TypeAnnotation> ConvertFunctionTypeArgument(
        Node<TypeAnnotation> node)
    {
        if (node.Value is TypeAnnotation.FunctionTypeAnnotation)
        {
            // Wrap in parentheses using Tupled with a single element
            var convertedInner = Convert(node.Value);
            var innerNode = new Node<FullTypes.TypeAnnotation>(s_defaultRange, convertedInner);

            var tupled =
                new FullTypes.TypeAnnotation.Tupled(
                    TypeAnnotations: new SeparatedSyntaxList<Node<FullTypes.TypeAnnotation>>.NonEmpty(
                        innerNode,
                        []));

            return new Node<FullTypes.TypeAnnotation>(s_defaultRange, tupled);
        }

        return ConvertNode(node, Convert);
    }

    private static Node<TResult> ConvertNode<TSource, TResult>(
        Node<TSource> node,
        System.Func<TSource, TResult> converter) =>
        new(s_defaultRange, converter(node.Value));

    private static IReadOnlyList<Node<TResult>> ConvertNodes<TSource, TResult>(
        IReadOnlyList<Node<TSource>> nodes,
        System.Func<TSource, TResult> converter) =>
        [.. nodes.Select(node => ConvertNode(node, converter))];

    /// <summary>
    /// Converts a node preserving its value type (identity conversion for the value).
    /// </summary>
    private static Node<T> ConvertNodePreserveValue<T>(
        Node<T> node) =>
        new(s_defaultRange, node.Value);

    /// <summary>
    /// Converts nodes preserving their value types (identity conversion for the values).
    /// </summary>
    private static IReadOnlyList<Node<T>> ConvertNodesPreserveValue<T>(
        IReadOnlyList<Node<T>> nodes) =>
        [.. nodes.Select(ConvertNodePreserveValue)];

    /// <summary>
    /// Converts comments to use the default range.
    /// </summary>
    private static IReadOnlyList<Node<string>> ConvertComments(
        IReadOnlyList<Node<string>> comments) =>
        [.. comments.Select(c => new Node<string>(s_defaultRange, c.Value))];

    /// <summary>
    /// Converts an IReadOnlyList to a SeparatedSyntaxList, using DefaultLocation for separator locations.
    /// </summary>
    public static SeparatedSyntaxList<Node<TResult>> ToSeparatedList<TSource, TResult>(
        IReadOnlyList<Node<TSource>> nodes,
        System.Func<TSource, TResult> converter)
    {
        if (nodes.Count == 0)
            return new SeparatedSyntaxList<Node<TResult>>.Empty();

        var first = ConvertNode(nodes[0], converter);

        var rest =
            nodes.Skip(1)
            .Select(
                node => (SeparatorLocation: s_defaultLocation,
                Node: ConvertNode(node, converter)))
            .ToList();

        return new SeparatedSyntaxList<Node<TResult>>.NonEmpty(first, rest);
    }

    /// <summary>
    /// Converts an IReadOnlyList of nodes to a SeparatedSyntaxList, preserving values (no conversion).
    /// </summary>
    public static SeparatedSyntaxList<Node<T>> ToSeparatedListPreserveValue<T>(
        IReadOnlyList<Node<T>> nodes)
    {
        if (nodes.Count == 0)
            return new SeparatedSyntaxList<Node<T>>.Empty();

        var first = ConvertNodePreserveValue(nodes[0]);

        var rest =
            nodes.Skip(1)
            .Select(
                node => (SeparatorLocation: s_defaultLocation,
                Node: ConvertNodePreserveValue(node)))
            .ToList();

        return new SeparatedSyntaxList<Node<T>>.NonEmpty(first, rest);
    }

    /// <summary>
    /// Converts record fields from IReadOnlyList to SeparatedSyntaxList format.
    /// </summary>
    private static SeparatedSyntaxList<RecordExprField>
        ToSeparatedRecordFields(
        IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> fields)
    {
        if (fields.Count == 0)
            return new SeparatedSyntaxList<RecordExprField>.Empty();

        var convertedFields =
            fields.Select(
                field =>
                new RecordExprField(
                    ConvertNodePreserveValue(field.Value.fieldName),
                    s_defaultLocation, // EqualsLocation
                    ConvertNode(field.Value.valueExpr, Convert)))
            .ToList();

        var first = convertedFields[0];

        var rest =
            convertedFields.Skip(1)
            .Select(
                recordField => (SeparatorLocation: s_defaultLocation,
                Node: recordField))
            .ToList();

        return new SeparatedSyntaxList<RecordExprField>.NonEmpty(first, rest);
    }

    /// <summary>
    /// Formats a hex integer value as a literal string (e.g., "0x49" or "-0x1A").
    /// Uses the same formatting as <see cref="Rendering.RenderHexPattern"/>.
    /// </summary>
    private static string FormatHexLiteral(long value)
    {
        if (value < 0)
        {
            return "-" + Rendering.RenderHexPattern(-value);
        }

        return Rendering.RenderHexPattern(value);
    }
}
