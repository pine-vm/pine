using System.Collections.Generic;
using System.Linq;

using ConcretizedTypes = Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Provides conversion methods from Stil4mElmSyntax7 types to Stil4mConcretized types.
/// This is the inverse of <see cref="FromStil4mConcretized"/>.
/// Token locations are invented using a default location (row 1, column 1).
/// The slight mismatch doesn't matter because the formatter will rearrange tokens afterward.
/// </summary>
public static class ToStil4mConcretized
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
    /// Converts a File to a Stil4mConcretized.File.
    /// </summary>
    public static ConcretizedTypes.File ToConcretized(
        File file)
    {
        return
            new ConcretizedTypes.File(
                ModuleDefinition: ConvertNode(file.ModuleDefinition, ToConcretized),
                Imports: ConvertNodes(file.Imports, ToConcretized),
                Declarations: ConvertNodes(file.Declarations, ToConcretized),
                Comments: ConvertComments(file.Comments),
                IncompleteDeclarations: []);
    }

    /// <summary>
    /// Converts a Module to a Stil4mConcretized.Module.
    /// </summary>
    public static ConcretizedTypes.Module ToConcretized(
        Module module) =>
        module switch
        {
            Module.NormalModule normalModule =>
                new ConcretizedTypes.Module.NormalModule(
                    ModuleTokenLocation: s_defaultLocation,
                    ModuleData: ToConcretized(normalModule.ModuleData)),

            Module.PortModule portModule =>
                new ConcretizedTypes.Module.PortModule(
                    PortTokenLocation: s_defaultLocation,
                    ModuleTokenLocation: s_defaultLocation,
                    ModuleData: ToConcretized(portModule.ModuleData)),

            Module.EffectModule effectModule =>
                new ConcretizedTypes.Module.EffectModule(
                    EffectTokenLocation: s_defaultLocation,
                    ModuleTokenLocation: s_defaultLocation,
                    ModuleData: ToConcretized(effectModule.ModuleData)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected module type: " + module.GetType().Name),
        };

    /// <summary>
    /// Converts a DefaultModuleData to a Stil4mConcretized.DefaultModuleData.
    /// </summary>
    public static ConcretizedTypes.DefaultModuleData ToConcretized(
        DefaultModuleData moduleData) =>
        new(
            ModuleName: ConvertNodePreserveValue(moduleData.ModuleName),
            ExposingTokenLocation: s_defaultLocation,
            ExposingList: ConvertNode(moduleData.ExposingList, ToConcretized));

    /// <summary>
    /// Converts a EffectModuleData to a Stil4mConcretized.EffectModuleData.
    /// </summary>
    public static ConcretizedTypes.EffectModuleData ToConcretized(
        EffectModuleData moduleData) =>
        new(
            ModuleName: ConvertNodePreserveValue(moduleData.ModuleName),
            ExposingTokenLocation: s_defaultLocation,
            ExposingList: ConvertNode(moduleData.ExposingList, ToConcretized),
            Command: moduleData.Command is { } cmd ? ConvertNodePreserveValue(cmd) : null,
            Subscription: moduleData.Subscription is { } sub ? ConvertNodePreserveValue(sub) : null);

    /// <summary>
    /// Converts a Exposing to a Stil4mConcretized.Exposing.
    /// </summary>
    public static ConcretizedTypes.Exposing ToConcretized(
        Exposing exposing) =>
        exposing switch
        {
            Exposing.All _ =>
                new ConcretizedTypes.Exposing.All(s_defaultRange),

            Exposing.Explicit @explicit =>
                new ConcretizedTypes.Exposing.Explicit(
                    OpenParenLocation: s_defaultLocation,
                    Nodes: ToSeparatedList(@explicit.Nodes, ToConcretized),
                    CloseParenLocation: s_defaultLocation),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected exposing type: " + exposing.GetType().Name),
        };

    /// <summary>
    /// Converts a TopLevelExpose to a Stil4mConcretized.TopLevelExpose.
    /// </summary>
    public static ConcretizedTypes.TopLevelExpose ToConcretized(
        TopLevelExpose expose) =>
        expose switch
        {
            TopLevelExpose.InfixExpose infixExpose =>
                new ConcretizedTypes.TopLevelExpose.InfixExpose(infixExpose.Name),

            TopLevelExpose.FunctionExpose functionExpose =>
                new ConcretizedTypes.TopLevelExpose.FunctionExpose(functionExpose.Name),

            TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
                new ConcretizedTypes.TopLevelExpose.TypeOrAliasExpose(typeOrAliasExpose.Name),

            TopLevelExpose.TypeExpose typeExpose =>
                new ConcretizedTypes.TopLevelExpose.TypeExpose(
                    ToConcretized(typeExpose.ExposedType)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected top level expose type: " + expose.GetType().Name),
        };

    /// <summary>
    /// Converts a ExposedType to a Stil4mConcretized.ExposedType.
    /// </summary>
    public static ConcretizedTypes.ExposedType ToConcretized(
        ExposedType exposedType) =>
        new(
            Name: exposedType.Name,
            Open: exposedType.Open is { } ? s_defaultRange : null);

    /// <summary>
    /// Converts a Import to a Stil4mConcretized.Import.
    /// </summary>
    public static ConcretizedTypes.Import ToConcretized(
        Import import) =>
        new(
            ImportTokenLocation: s_defaultLocation,
            ModuleName: ConvertNodePreserveValue(import.ModuleName),
            ModuleAlias: import.ModuleAlias is { } alias
                ? (s_defaultLocation, ConvertNodePreserveValue(alias))
                : null,
            ExposingList: import.ExposingList is { } exposingList
                ? (s_defaultLocation, ConvertNode(exposingList, ToConcretized))
                : null);

    /// <summary>
    /// Converts a Declaration to a Stil4mConcretized.Declaration.
    /// </summary>
    public static ConcretizedTypes.Declaration ToConcretized(
        Declaration declaration) =>
        declaration switch
        {
            Declaration.FunctionDeclaration functionDeclaration =>
                new ConcretizedTypes.Declaration.FunctionDeclaration(
                    ToConcretized(functionDeclaration.Function)),

            Declaration.CustomTypeDeclaration customTypeDeclaration =>
                new ConcretizedTypes.Declaration.CustomTypeDeclaration(
                    ToConcretized(customTypeDeclaration.TypeDeclaration)),

            Declaration.AliasDeclaration aliasDeclaration =>
                new ConcretizedTypes.Declaration.AliasDeclaration(
                    ToConcretized(aliasDeclaration.TypeAlias)),

            Declaration.PortDeclaration portDeclaration =>
                new ConcretizedTypes.Declaration.PortDeclaration(
                    PortTokenLocation: s_defaultLocation,
                    Signature: ToConcretized(portDeclaration.Signature)),

            Declaration.InfixDeclaration infixDeclaration =>
                new ConcretizedTypes.Declaration.InfixDeclaration(
                    ToConcretized(infixDeclaration.Infix)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected declaration type: " + declaration.GetType().Name),
        };

    /// <summary>
    /// Converts a Infix to a Stil4mConcretized.Infix.
    /// </summary>
    public static ConcretizedTypes.Infix ToConcretized(
        Infix infix) =>
        new(
            InfixTokenLocation: s_defaultLocation,
            Direction: ConvertNodePreserveValue(infix.Direction),
            Precedence: ConvertNodePreserveValue(infix.Precedence),
            Operator: ConvertNodePreserveValue(infix.Operator),
            EqualsTokenLocation: s_defaultLocation,
            FunctionName: ConvertNodePreserveValue(infix.FunctionName));

    /// <summary>
    /// Converts a TypeAlias to a Stil4mConcretized.TypeAlias.
    /// </summary>
    public static ConcretizedTypes.TypeAlias ToConcretized(
        TypeAlias typeAlias) =>
        new(
            Documentation: typeAlias.Documentation is { } doc ? ConvertNodePreserveValue(doc) : null,
            TypeTokenLocation: s_defaultLocation,
            AliasTokenLocation: s_defaultLocation,
            Name: ConvertNodePreserveValue(typeAlias.Name),
            Generics: ConvertNodesPreserveValue(typeAlias.Generics),
            EqualsTokenLocation: s_defaultLocation,
            TypeAnnotation: ConvertNode(typeAlias.TypeAnnotation, ToConcretized));

    /// <summary>
    /// Converts a TypeStruct to a Stil4mConcretized.TypeStruct.
    /// </summary>
    public static ConcretizedTypes.TypeStruct ToConcretized(
        TypeStruct typeStruct) =>
        new(
            Documentation: typeStruct.Documentation is { } doc ? ConvertNodePreserveValue(doc) : null,
            TypeTokenLocation: s_defaultLocation,
            Name: ConvertNodePreserveValue(typeStruct.Name),
            Generics: ConvertNodesPreserveValue(typeStruct.Generics),
            EqualsTokenLocation: s_defaultLocation,
            Constructors: [.. typeStruct.Constructors
                .Select((c, i) => (
                    PipeTokenLocation: i > 0 ? s_defaultLocation : (Location?)null,
                    Constructor: ConvertNode(c, ToConcretized)))]);

    /// <summary>
    /// Converts a ValueConstructor to a Stil4mConcretized.ValueConstructor.
    /// </summary>
    public static ConcretizedTypes.ValueConstructor ToConcretized(
        ValueConstructor constructor) =>
        new(
            Name: ConvertNodePreserveValue(constructor.Name),
            Arguments: ConvertNodes(constructor.Arguments, ToConcretized));

    /// <summary>
    /// Converts a TypeAnnotation to a Stil4mConcretized.TypeAnnotation.
    /// </summary>
    public static ConcretizedTypes.TypeAnnotation ToConcretized(
        TypeAnnotation typeAnnotation) =>
        typeAnnotation switch
        {
            TypeAnnotation.GenericType genericType =>
                new ConcretizedTypes.TypeAnnotation.GenericType(genericType.Name),

            TypeAnnotation.Typed typed =>
                new ConcretizedTypes.TypeAnnotation.Typed(
                    ConvertNodePreserveValue(typed.TypeName),
                    ConvertNodes(typed.TypeArguments, ToConcretized)),

            TypeAnnotation.Unit =>
                new ConcretizedTypes.TypeAnnotation.Unit(),

            TypeAnnotation.Tupled tupled =>
                new ConcretizedTypes.TypeAnnotation.Tupled(
                    OpenParenLocation: s_defaultLocation,
                    TypeAnnotations: ToSeparatedList(tupled.TypeAnnotations, ToConcretized),
                    CloseParenLocation: s_defaultLocation),

            TypeAnnotation.Record record =>
                new ConcretizedTypes.TypeAnnotation.Record(
                    OpenBraceLocation: s_defaultLocation,
                    RecordDefinition: ToConcretized(record.RecordDefinition),
                    CloseBraceLocation: s_defaultLocation),

            TypeAnnotation.GenericRecord genericRecord =>
                new ConcretizedTypes.TypeAnnotation.GenericRecord(
                    OpenBraceLocation: s_defaultLocation,
                    GenericName: ConvertNodePreserveValue(genericRecord.GenericName),
                    PipeLocation: s_defaultLocation,
                    RecordDefinition: ConvertNode(genericRecord.RecordDefinition, ToConcretized),
                    CloseBraceLocation: s_defaultLocation),

            TypeAnnotation.FunctionTypeAnnotation functionType =>
                new ConcretizedTypes.TypeAnnotation.FunctionTypeAnnotation(
                    ArgumentType: ConvertFunctionTypeArgument(functionType.ArgumentType),
                    ArrowLocation: s_defaultLocation,
                    ReturnType: ConvertNode(functionType.ReturnType, ToConcretized)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected type annotation type: " + typeAnnotation.GetType().Name),
        };

    /// <summary>
    /// Converts a RecordDefinition to a Stil4mConcretized.RecordDefinition.
    /// </summary>
    public static ConcretizedTypes.RecordDefinition ToConcretized(
        RecordDefinition recordDefinition) =>
        new(
            Fields: ToSeparatedList(recordDefinition.Fields, ToConcretized));

    /// <summary>
    /// Converts a RecordField to a Stil4mConcretized.RecordField.
    /// </summary>
    public static ConcretizedTypes.RecordField ToConcretized(
        RecordField recordField) =>
        new(
            FieldName: ConvertNodePreserveValue(recordField.FieldName),
            ColonLocation: s_defaultLocation,
            FieldType: ConvertNode(recordField.FieldType, ToConcretized));

    /// <summary>
    /// Converts a FunctionStruct to a Stil4mConcretized.FunctionStruct.
    /// </summary>
    public static ConcretizedTypes.FunctionStruct ToConcretized(
        FunctionStruct functionStruct) =>
        new(
            Documentation: functionStruct.Documentation is { } doc ? ConvertNodePreserveValue(doc) : null,
            Signature: functionStruct.Signature is { } sig
                ? ConvertNode(sig, ToConcretized)
                : null,
            Declaration: ConvertNode(functionStruct.Declaration, ToConcretized));

    /// <summary>
    /// Converts a FunctionImplementation to a Stil4mConcretized.FunctionImplementation.
    /// </summary>
    public static ConcretizedTypes.FunctionImplementation ToConcretized(
        FunctionImplementation functionImplementation) =>
        new(
            Name: ConvertNodePreserveValue(functionImplementation.Name),
            Arguments: ConvertNodes(functionImplementation.Arguments, ToConcretized),
            EqualsTokenLocation: s_defaultLocation,
            Expression: ConvertNode(functionImplementation.Expression, ToConcretized));

    /// <summary>
    /// Converts a Signature to a Stil4mConcretized.Signature.
    /// </summary>
    public static ConcretizedTypes.Signature ToConcretized(
        Signature signature) =>
        new(
            Name: ConvertNodePreserveValue(signature.Name),
            ColonLocation: s_defaultLocation,
            TypeAnnotation: ConvertNode(signature.TypeAnnotation, ToConcretized));

    /// <summary>
    /// Converts a Pattern to a Stil4mConcretized.Pattern.
    /// </summary>
    public static ConcretizedTypes.Pattern ToConcretized(
        Pattern pattern) =>
        pattern switch
        {
            Pattern.AllPattern =>
                new ConcretizedTypes.Pattern.AllPattern(),

            Pattern.VarPattern varPattern =>
                new ConcretizedTypes.Pattern.VarPattern(varPattern.Name),

            Pattern.UnitPattern =>
                new ConcretizedTypes.Pattern.UnitPattern(),

            Pattern.CharPattern charPattern =>
                new ConcretizedTypes.Pattern.CharPattern(charPattern.Value),

            Pattern.StringPattern stringPattern =>
                new ConcretizedTypes.Pattern.StringPattern(stringPattern.Value),

            Pattern.IntPattern intPattern =>
                new ConcretizedTypes.Pattern.IntPattern(intPattern.Value),

            Pattern.HexPattern hexPattern =>
                new ConcretizedTypes.Pattern.HexPattern(hexPattern.Value),

            Pattern.FloatPattern floatPattern =>
                new ConcretizedTypes.Pattern.FloatPattern(floatPattern.Value),

            Pattern.TuplePattern tuplePattern =>
                new ConcretizedTypes.Pattern.TuplePattern(
                    Elements: ToSeparatedList(tuplePattern.Elements, ToConcretized)),

            Pattern.RecordPattern recordPattern =>
                new ConcretizedTypes.Pattern.RecordPattern(
                    Fields: ToSeparatedListPreserveValue(recordPattern.Fields)),

            Pattern.UnConsPattern unConsPattern =>
                new ConcretizedTypes.Pattern.UnConsPattern(
                    Head: ConvertNode(unConsPattern.Head, ToConcretized),
                    ConsOperatorLocation: s_defaultLocation,
                    Tail: ConvertNode(unConsPattern.Tail, ToConcretized)),

            Pattern.ListPattern listPattern =>
                new ConcretizedTypes.Pattern.ListPattern(
                    Elements: ToSeparatedList(listPattern.Elements, ToConcretized)),

            Pattern.NamedPattern namedPattern =>
                new ConcretizedTypes.Pattern.NamedPattern(
                    Name: ToConcretized(namedPattern.Name),
                    Arguments: ConvertNodes(namedPattern.Arguments, ToConcretized)),

            Pattern.AsPattern asPattern =>
                new ConcretizedTypes.Pattern.AsPattern(
                    Pattern: ConvertNode(asPattern.Pattern, ToConcretized),
                    AsTokenLocation: s_defaultLocation,
                    Name: ConvertNodePreserveValue(asPattern.Name)),

            Pattern.ParenthesizedPattern parenthesizedPattern =>
                new ConcretizedTypes.Pattern.ParenthesizedPattern(
                    Pattern: ConvertNode(parenthesizedPattern.Pattern, ToConcretized)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected pattern type: " + pattern.GetType().Name),
        };

    /// <summary>
    /// Converts a QualifiedNameRef to a Stil4mConcretized.QualifiedNameRef.
    /// </summary>
    public static ConcretizedTypes.QualifiedNameRef ToConcretized(
        QualifiedNameRef qualifiedNameRef) =>
        new(
            ModuleName: qualifiedNameRef.ModuleName,
            Name: qualifiedNameRef.Name);

    /// <summary>
    /// Converts a Expression to a Stil4mConcretized.Expression.
    /// </summary>
    public static ConcretizedTypes.Expression ToConcretized(
        Expression expression) =>
        expression switch
        {
            Expression.UnitExpr =>
                new ConcretizedTypes.Expression.UnitExpr(),

            Expression.Literal literal =>
                new ConcretizedTypes.Expression.Literal(literal.Value, literal.IsTripleQuoted),

            Expression.CharLiteral charLiteral =>
                new ConcretizedTypes.Expression.CharLiteral(charLiteral.Value),

            Expression.Integer integer =>
                new ConcretizedTypes.Expression.Integer(integer.Value),

            Expression.Hex hex =>
                new ConcretizedTypes.Expression.Hex(hex.Value),

            Expression.Floatable floatable =>
                new ConcretizedTypes.Expression.Floatable(floatable.Value),

            Expression.Negation negation =>
                new ConcretizedTypes.Expression.Negation(
                    ConvertNode(negation.Expression, ToConcretized)),

            Expression.ListExpr listExpr =>
                new ConcretizedTypes.Expression.ListExpr(
                    Elements: ToSeparatedList(listExpr.Elements, ToConcretized)),

            Expression.FunctionOrValue functionOrValue =>
                new ConcretizedTypes.Expression.FunctionOrValue(
                    functionOrValue.ModuleName,
                    functionOrValue.Name),

            Expression.IfBlock ifBlock =>
                new ConcretizedTypes.Expression.IfBlock(
                    IfTokenLocation: s_defaultLocation,
                    Condition: ConvertNode(ifBlock.Condition, ToConcretized),
                    ThenTokenLocation: s_defaultLocation,
                    ThenBlock: ConvertNode(ifBlock.ThenBlock, ToConcretized),
                    ElseTokenLocation: s_defaultLocation,
                    ElseBlock: ConvertNode(ifBlock.ElseBlock, ToConcretized)),

            Expression.PrefixOperator prefixOperator =>
                new ConcretizedTypes.Expression.PrefixOperator(prefixOperator.Operator),

            Expression.ParenthesizedExpression parenthesizedExpression =>
                new ConcretizedTypes.Expression.ParenthesizedExpression(
                    Expression: ConvertNode(parenthesizedExpression.Expression, ToConcretized)),

            Expression.Application application =>
                new ConcretizedTypes.Expression.Application(
                    ConvertNodes(application.Arguments, ToConcretized)),

            Expression.OperatorApplication operatorApplication =>
                new ConcretizedTypes.Expression.OperatorApplication(
                    new Node<string>(s_defaultRange, operatorApplication.Operator),
                    operatorApplication.Direction,
                    ConvertNode(operatorApplication.Left, ToConcretized),
                    ConvertNode(operatorApplication.Right, ToConcretized)),

            Expression.TupledExpression tupledExpression =>
                new ConcretizedTypes.Expression.TupledExpression(
                    Elements: ToSeparatedList(tupledExpression.Elements, ToConcretized)),

            Expression.LambdaExpression lambdaExpression =>
                new ConcretizedTypes.Expression.LambdaExpression(
                    ToConcretized(lambdaExpression.Lambda)),

            Expression.CaseExpression caseExpression =>
                new ConcretizedTypes.Expression.CaseExpression(
                    ToConcretized(caseExpression.CaseBlock)),

            Expression.LetExpression letExpression =>
                new ConcretizedTypes.Expression.LetExpression(
                    ToConcretized(letExpression.Value)),

            Expression.RecordExpr recordExpr =>
                new ConcretizedTypes.Expression.RecordExpr(
                    Fields: ToSeparatedRecordFields(recordExpr.Fields)),

            Expression.RecordAccess recordAccess =>
                new ConcretizedTypes.Expression.RecordAccess(
                    ConvertNode(recordAccess.Record, ToConcretized),
                    ConvertNodePreserveValue(recordAccess.FieldName)),

            Expression.RecordAccessFunction recordAccessFunction =>
                new ConcretizedTypes.Expression.RecordAccessFunction(recordAccessFunction.FunctionName),

            Expression.RecordUpdateExpression recordUpdateExpression =>
                new ConcretizedTypes.Expression.RecordUpdateExpression(
                    RecordName: ConvertNodePreserveValue(recordUpdateExpression.RecordName),
                    PipeLocation: s_defaultLocation,
                    Fields: ToSeparatedRecordFields(recordUpdateExpression.Fields)),

            _ =>
                throw new System.NotImplementedException(
                    "Unexpected expression type: " + expression.GetType().Name),
        };

    /// <summary>
    /// Converts a LambdaStruct to a Stil4mConcretized.LambdaStruct.
    /// </summary>
    public static ConcretizedTypes.LambdaStruct ToConcretized(
        LambdaStruct lambdaStruct) =>
        new(
            BackslashLocation: s_defaultLocation,
            Arguments: ConvertNodes(lambdaStruct.Arguments, ToConcretized),
            ArrowLocation: s_defaultLocation,
            Expression: ConvertNode(lambdaStruct.Expression, ToConcretized));

    /// <summary>
    /// Converts a CaseBlock to a Stil4mConcretized.CaseBlock.
    /// </summary>
    public static ConcretizedTypes.CaseBlock ToConcretized(
        CaseBlock caseBlock) =>
        new(
            CaseTokenLocation: s_defaultLocation,
            Expression: ConvertNode(caseBlock.Expression, ToConcretized),
            OfTokenLocation: s_defaultLocation,
            Cases: [.. caseBlock.Cases.Select(toStil4mConcretized)]);

    /// <summary>
    /// Converts a Case to a Stil4mConcretized.Case.
    /// </summary>
    public static ConcretizedTypes.Case toStil4mConcretized(
        Case @case) =>
        new(
            Pattern: ConvertNode(@case.Pattern, ToConcretized),
            ArrowLocation: s_defaultLocation,
            Expression: ConvertNode(@case.Expression, ToConcretized));

    /// <summary>
    /// Converts a Expression.LetBlock to a Stil4mConcretized.Expression.LetBlock.
    /// </summary>
    public static ConcretizedTypes.Expression.LetBlock ToConcretized(
        Expression.LetBlock letBlock) =>
        new(
            LetTokenLocation: s_defaultLocation,
            Declarations: ConvertNodes(letBlock.Declarations, ToConcretized),
            InTokenLocation: s_defaultLocation,
            Expression: ConvertNode(letBlock.Expression, ToConcretized));

    /// <summary>
    /// Converts a Expression.LetDeclaration to a Stil4mConcretized.Expression.LetDeclaration.
    /// </summary>
    public static ConcretizedTypes.Expression.LetDeclaration ToConcretized(
        Expression.LetDeclaration letDeclaration) =>
        letDeclaration switch
        {
            Expression.LetDeclaration.LetFunction letFunction =>
                new ConcretizedTypes.Expression.LetDeclaration.LetFunction(
                    ToConcretized(letFunction.Function)),

            Expression.LetDeclaration.LetDestructuring letDestructuring =>
                new ConcretizedTypes.Expression.LetDeclaration.LetDestructuring(
                    ConvertNode(letDestructuring.Pattern, ToConcretized),
                    EqualsTokenLocation: s_defaultLocation,
                    Expression: ConvertNode(letDestructuring.Expression, ToConcretized)),

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
    /// Implementation note: In the concretized syntax tree, parentheses around type annotations
    /// are represented using <see cref="ConcretizedTypes.TypeAnnotation.Tupled"/> with a single
    /// element. The formatter recognizes this pattern and renders it as `(element)` rather than
    /// `( element )` which would be used for multi-element tuples.
    /// </summary>
    private static Node<ConcretizedTypes.TypeAnnotation> ConvertFunctionTypeArgument(
        Node<TypeAnnotation> node)
    {
        if (node.Value is TypeAnnotation.FunctionTypeAnnotation)
        {
            // Wrap in parentheses using Tupled with a single element
            var convertedInner = ToConcretized(node.Value);
            var innerNode = new Node<ConcretizedTypes.TypeAnnotation>(s_defaultRange, convertedInner);
            var tupled = new ConcretizedTypes.TypeAnnotation.Tupled(
                OpenParenLocation: s_defaultLocation,
                TypeAnnotations: new ConcretizedTypes.SeparatedSyntaxList<Node<ConcretizedTypes.TypeAnnotation>>.NonEmpty(
                    innerNode, []),
                CloseParenLocation: s_defaultLocation);
            return new Node<ConcretizedTypes.TypeAnnotation>(s_defaultRange, tupled);
        }

        return ConvertNode(node, ToConcretized);
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
    public static ConcretizedTypes.SeparatedSyntaxList<Node<TResult>> ToSeparatedList<TSource, TResult>(
        IReadOnlyList<Node<TSource>> nodes,
        System.Func<TSource, TResult> converter)
    {
        if (nodes.Count == 0)
            return new ConcretizedTypes.SeparatedSyntaxList<Node<TResult>>.Empty();

        var first = ConvertNode(nodes[0], converter);
        var rest = nodes.Skip(1)
            .Select(node => (
                SeparatorLocation: s_defaultLocation,
                Node: ConvertNode(node, converter)))
            .ToList();

        return new ConcretizedTypes.SeparatedSyntaxList<Node<TResult>>.NonEmpty(first, rest);
    }

    /// <summary>
    /// Converts an IReadOnlyList of nodes to a SeparatedSyntaxList, preserving values (no conversion).
    /// </summary>
    public static ConcretizedTypes.SeparatedSyntaxList<Node<T>> ToSeparatedListPreserveValue<T>(
        IReadOnlyList<Node<T>> nodes)
    {
        if (nodes.Count == 0)
            return new ConcretizedTypes.SeparatedSyntaxList<Node<T>>.Empty();

        var first = ConvertNodePreserveValue(nodes[0]);
        var rest = nodes.Skip(1)
            .Select(node => (
                SeparatorLocation: s_defaultLocation,
                Node: ConvertNodePreserveValue(node)))
            .ToList();

        return new ConcretizedTypes.SeparatedSyntaxList<Node<T>>.NonEmpty(first, rest);
    }

    /// <summary>
    /// Converts record fields from IReadOnlyList to SeparatedSyntaxList format.
    /// </summary>
    private static ConcretizedTypes.SeparatedSyntaxList<ConcretizedTypes.RecordExprField>
        ToSeparatedRecordFields(
            IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> fields)
    {
        if (fields.Count == 0)
            return new ConcretizedTypes.SeparatedSyntaxList<ConcretizedTypes.RecordExprField>.Empty();

        var convertedFields = fields.Select(field =>
            new ConcretizedTypes.RecordExprField(
                ConvertNodePreserveValue(field.Value.fieldName),
                s_defaultLocation,  // EqualsLocation
                ConvertNode(field.Value.valueExpr, ToConcretized)))
            .ToList();

        var first = convertedFields[0];
        var rest = convertedFields.Skip(1)
            .Select(recordField => (
                SeparatorLocation: s_defaultLocation,
                Node: recordField))
            .ToList();

        return new ConcretizedTypes.SeparatedSyntaxList<ConcretizedTypes.RecordExprField>.NonEmpty(first, rest);
    }
}
