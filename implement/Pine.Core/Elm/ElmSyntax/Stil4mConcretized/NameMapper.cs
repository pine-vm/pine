using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

/// <summary>
/// Maps qualified names in an Elm module file.
/// This class processes a file and transforms <see cref="QualifiedNameRef"/> instances
/// according to a provided mapping function or dictionary.
/// </summary>
public class NameMapper
{
    /// <summary>
    /// Maps qualified names in the file using the provided mapping function.
    /// </summary>
    /// <param name="file">The file to process.</param>
    /// <param name="mapQualifiedName">
    /// A function that transforms qualified names.
    /// If the function returns a different name, the name in the file is updated.
    /// </param>
    /// <returns>A new file with mapped qualified names.</returns>
    public static File MapNames(
        File file,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new File(
            ModuleDefinition: file.ModuleDefinition,
            Imports: file.Imports,
            Declarations: [.. file.Declarations.Select(d => MapDeclaration(d, mapQualifiedName))],
            Comments: file.Comments);
    }

    /// <summary>
    /// Maps qualified names in the file using a dictionary.
    /// Names not found in the dictionary are left unchanged.
    /// </summary>
    /// <param name="file">The file to process.</param>
    /// <param name="namesMap">
    /// A dictionary mapping original names to their replacements.
    /// </param>
    /// <returns>A new file with mapped qualified names.</returns>
    public static File MapNames(
        File file,
        IReadOnlyDictionary<QualifiedNameRef, QualifiedNameRef> namesMap)
    {
        return MapNames(file, originalName =>
            namesMap.TryGetValue(originalName, out var mappedName)
            ? mappedName
            : originalName);
    }

    private static Stil4mElmSyntax7.Node<Declaration> MapDeclaration(
        Stil4mElmSyntax7.Node<Declaration> declNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var mappedDecl = declNode.Value switch
        {
            Declaration.FunctionDeclaration funcDecl =>
                new Declaration.FunctionDeclaration(MapFunction(funcDecl.Function, mapQualifiedName)),

            Declaration.AliasDeclaration aliasDecl =>
                new Declaration.AliasDeclaration(MapTypeAlias(aliasDecl.TypeAlias, mapQualifiedName)),

            Declaration.CustomTypeDeclaration customTypeDecl =>
                new Declaration.CustomTypeDeclaration(MapTypeStruct(customTypeDecl.TypeDeclaration, mapQualifiedName)),

            Declaration.PortDeclaration portDecl =>
                new Declaration.PortDeclaration(
                    portDecl.PortTokenLocation,
                    MapSignature(portDecl.Signature, mapQualifiedName)),

            Declaration.InfixDeclaration =>
                declNode.Value,

            _ =>
                throw new NotImplementedException(
                    $"Mapping for declaration type '{declNode.Value.GetType().Name}' is not implemented.")
        };

        return new Stil4mElmSyntax7.Node<Declaration>(declNode.Range, mappedDecl);
    }

    private static FunctionStruct MapFunction(
        FunctionStruct func,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var impl = func.Declaration.Value;
        var mappedExpression = MapExpression(impl.Expression, mapQualifiedName);

        var mappedImpl = new FunctionImplementation(
            Name: impl.Name,
            Arguments: impl.Arguments,
            EqualsTokenLocation: impl.EqualsTokenLocation,
            Expression: mappedExpression);

        return new FunctionStruct(
            Documentation: func.Documentation,
            Signature: func.Signature is { } sig
                ? new Stil4mElmSyntax7.Node<Signature>(sig.Range, MapSignature(sig.Value, mapQualifiedName))
                : null,
            Declaration: new Stil4mElmSyntax7.Node<FunctionImplementation>(func.Declaration.Range, mappedImpl));
    }

    private static Signature MapSignature(
        Signature sig,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new Signature(
            Name: sig.Name,
            ColonLocation: sig.ColonLocation,
            TypeAnnotation: MapTypeAnnotation(sig.TypeAnnotation, mapQualifiedName));
    }

    private static TypeAlias MapTypeAlias(
        TypeAlias alias,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new TypeAlias(
            Documentation: alias.Documentation,
            TypeTokenLocation: alias.TypeTokenLocation,
            AliasTokenLocation: alias.AliasTokenLocation,
            Name: alias.Name,
            Generics: alias.Generics,
            EqualsTokenLocation: alias.EqualsTokenLocation,
            TypeAnnotation: MapTypeAnnotation(alias.TypeAnnotation, mapQualifiedName));
    }

    private static TypeStruct MapTypeStruct(
        TypeStruct typeStruct,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new TypeStruct(
            Documentation: typeStruct.Documentation,
            TypeTokenLocation: typeStruct.TypeTokenLocation,
            Name: typeStruct.Name,
            Generics: typeStruct.Generics,
            EqualsTokenLocation: typeStruct.EqualsTokenLocation,
            Constructors: [.. typeStruct.Constructors.Select(c => MapValueConstructor(c, mapQualifiedName))]);
    }

    private static (Stil4mElmSyntax7.Location? PipeTokenLocation, Stil4mElmSyntax7.Node<ValueConstructor> Constructor) MapValueConstructor(
        (Stil4mElmSyntax7.Location? PipeTokenLocation, Stil4mElmSyntax7.Node<ValueConstructor> Constructor) constructorItem,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var constructor = constructorItem.Constructor.Value;
        return (
            constructorItem.PipeTokenLocation,
            new Stil4mElmSyntax7.Node<ValueConstructor>(
                constructorItem.Constructor.Range,
                new ValueConstructor(
                    Name: constructor.Name,
                    Arguments: [.. constructor.Arguments.Select(a => MapTypeAnnotation(a, mapQualifiedName))])));
    }

    private static Stil4mElmSyntax7.Node<TypeAnnotation> MapTypeAnnotation(
        Stil4mElmSyntax7.Node<TypeAnnotation> typeAnnotNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var mapped = typeAnnotNode.Value switch
        {
            TypeAnnotation.Typed typed =>
                MapTypedAnnotation(typed, mapQualifiedName),

            TypeAnnotation.FunctionTypeAnnotation funcType =>
                new TypeAnnotation.FunctionTypeAnnotation(
                    ArgumentType: MapTypeAnnotation(funcType.ArgumentType, mapQualifiedName),
                    ArrowLocation: funcType.ArrowLocation,
                    ReturnType: MapTypeAnnotation(funcType.ReturnType, mapQualifiedName)),

            TypeAnnotation.Tupled tupled =>
                new TypeAnnotation.Tupled(
                    OpenParenLocation: tupled.OpenParenLocation,
                    TypeAnnotations: MapSeparatedList(tupled.TypeAnnotations, t => MapTypeAnnotation(t, mapQualifiedName)),
                    CloseParenLocation: tupled.CloseParenLocation),

            TypeAnnotation.Record record =>
                new TypeAnnotation.Record(
                    OpenBraceLocation: record.OpenBraceLocation,
                    RecordDefinition: MapRecordDefinition(record.RecordDefinition, mapQualifiedName),
                    CloseBraceLocation: record.CloseBraceLocation),

            TypeAnnotation.GenericRecord genericRecord =>
                new TypeAnnotation.GenericRecord(
                    OpenBraceLocation: genericRecord.OpenBraceLocation,
                    GenericName: genericRecord.GenericName,
                    PipeLocation: genericRecord.PipeLocation,
                    RecordDefinition: new Stil4mElmSyntax7.Node<RecordDefinition>(
                        genericRecord.RecordDefinition.Range,
                        MapRecordDefinition(genericRecord.RecordDefinition.Value, mapQualifiedName)),
                    CloseBraceLocation: genericRecord.CloseBraceLocation),

            // These don't contain type names to map
            TypeAnnotation.GenericType or TypeAnnotation.Unit =>
                typeAnnotNode.Value,

            _ =>
                throw new NotImplementedException(
                    $"Mapping for type annotation '{typeAnnotNode.Value.GetType().Name}' is not implemented.")
        };

        return new Stil4mElmSyntax7.Node<TypeAnnotation>(typeAnnotNode.Range, mapped);
    }

    private static TypeAnnotation.Typed MapTypedAnnotation(
        TypeAnnotation.Typed typed,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var originalQualifiedName = new QualifiedNameRef(
            ModuleName: typed.TypeName.Value.ModuleName,
            Name: typed.TypeName.Value.Name);

        var mappedQualifiedName = mapQualifiedName(originalQualifiedName);

        var newTypeName = new Stil4mElmSyntax7.Node<(IReadOnlyList<string> ModuleName, string Name)>(
            typed.TypeName.Range,
            (mappedQualifiedName.ModuleName, mappedQualifiedName.Name));

        return new TypeAnnotation.Typed(
            TypeName: newTypeName,
            TypeArguments: [.. typed.TypeArguments.Select(a => MapTypeAnnotation(a, mapQualifiedName))]);
    }

    private static RecordDefinition MapRecordDefinition(
        RecordDefinition recordDef,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new RecordDefinition(
            MapSeparatedList(recordDef.Fields, f => MapRecordField(f, mapQualifiedName)));
    }

    private static Stil4mElmSyntax7.Node<RecordField> MapRecordField(
        Stil4mElmSyntax7.Node<RecordField> fieldNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var field = fieldNode.Value;
        return new Stil4mElmSyntax7.Node<RecordField>(
            fieldNode.Range,
            new RecordField(
                FieldName: field.FieldName,
                ColonLocation: field.ColonLocation,
                FieldType: MapTypeAnnotation(field.FieldType, mapQualifiedName)));
    }

    // Expression mapping methods

    private static Stil4mElmSyntax7.Node<Expression> MapExpression(
        Stil4mElmSyntax7.Node<Expression> exprNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var mappedExpr = exprNode.Value switch
        {
            Expression.LetExpression letExpr =>
                new Expression.LetExpression(MapLetBlock(letExpr.Value, mapQualifiedName)),

            Expression.LambdaExpression lambdaExpr =>
                new Expression.LambdaExpression(MapLambda(lambdaExpr.Lambda, mapQualifiedName)),

            Expression.CaseExpression caseExpr =>
                new Expression.CaseExpression(MapCaseBlock(caseExpr.CaseBlock, mapQualifiedName)),

            Expression.IfBlock ifBlock =>
                new Expression.IfBlock(
                    IfTokenLocation: ifBlock.IfTokenLocation,
                    Condition: MapExpression(ifBlock.Condition, mapQualifiedName),
                    ThenTokenLocation: ifBlock.ThenTokenLocation,
                    ThenBlock: MapExpression(ifBlock.ThenBlock, mapQualifiedName),
                    ElseTokenLocation: ifBlock.ElseTokenLocation,
                    ElseBlock: MapExpression(ifBlock.ElseBlock, mapQualifiedName)),

            Expression.Application app =>
                new Expression.Application(
                    [.. app.Arguments.Select(a => MapExpression(a, mapQualifiedName))]),

            Expression.ListExpr listExpr =>
                new Expression.ListExpr(
                    Elements: MapSeparatedList(listExpr.Elements, e => MapExpression(e, mapQualifiedName))),

            Expression.TupledExpression tupled =>
                new Expression.TupledExpression(
                    OpenParenLocation: tupled.OpenParenLocation,
                    Elements: MapSeparatedList(tupled.Elements, e => MapExpression(e, mapQualifiedName)),
                    CloseParenLocation: tupled.CloseParenLocation),

            Expression.RecordExpr recordExpr =>
                new Expression.RecordExpr(
                    Fields: MapSeparatedList(recordExpr.Fields, f => MapRecordExprField(f, mapQualifiedName))),

            Expression.RecordUpdateExpression recordUpdate =>
                new Expression.RecordUpdateExpression(
                    RecordName: recordUpdate.RecordName,
                    PipeLocation: recordUpdate.PipeLocation,
                    Fields: MapSeparatedList(recordUpdate.Fields, f => MapRecordExprField(f, mapQualifiedName))),

            Expression.OperatorApplication opApp =>
                new Expression.OperatorApplication(
                    Operator: opApp.Operator,
                    Direction: opApp.Direction,
                    Left: MapExpression(opApp.Left, mapQualifiedName),
                    Right: MapExpression(opApp.Right, mapQualifiedName)),

            Expression.Negation negation =>
                new Expression.Negation(MapExpression(negation.Expression, mapQualifiedName)),

            Expression.ParenthesizedExpression parenExpr =>
                new Expression.ParenthesizedExpression(
                    OpenParenLocation: parenExpr.OpenParenLocation,
                    Expression: MapExpression(parenExpr.Expression, mapQualifiedName),
                    CloseParenLocation: parenExpr.CloseParenLocation),

            Expression.RecordAccess recordAccess =>
                new Expression.RecordAccess(
                    Record: MapExpression(recordAccess.Record, mapQualifiedName),
                    FieldName: recordAccess.FieldName),

            // These expression types don't contain type annotations or expressions to traverse
            Expression.Integer or Expression.Literal or Expression.CharLiteral or
            Expression.Hex or Expression.Floatable or Expression.UnitExpr or
            Expression.FunctionOrValue or Expression.RecordAccessFunction or
            Expression.PrefixOperator =>
                exprNode.Value,

            _ =>
                exprNode.Value // Return unchanged for unhandled expression types
        };

        return new Stil4mElmSyntax7.Node<Expression>(exprNode.Range, mappedExpr);
    }

    private static Expression.LetBlock MapLetBlock(
        Expression.LetBlock letBlock,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new Expression.LetBlock(
            LetTokenLocation: letBlock.LetTokenLocation,
            Declarations: [.. letBlock.Declarations.Select(d => MapLetDeclaration(d, mapQualifiedName))],
            InTokenLocation: letBlock.InTokenLocation,
            Expression: MapExpression(letBlock.Expression, mapQualifiedName));
    }

    private static Stil4mElmSyntax7.Node<Expression.LetDeclaration> MapLetDeclaration(
        Stil4mElmSyntax7.Node<Expression.LetDeclaration> declNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        Expression.LetDeclaration mappedDecl = declNode.Value switch
        {
            Expression.LetDeclaration.LetFunction letFunc =>
                new Expression.LetDeclaration.LetFunction(MapFunction(letFunc.Function, mapQualifiedName)),

            Expression.LetDeclaration.LetDestructuring letDestr =>
                new Expression.LetDeclaration.LetDestructuring(
                    Pattern: letDestr.Pattern,
                    EqualsTokenLocation: letDestr.EqualsTokenLocation,
                    Expression: MapExpression(letDestr.Expression, mapQualifiedName)),

            _ =>
                throw new NotImplementedException(
                    $"Mapping for let declaration type '{declNode.Value.GetType().Name}' is not implemented.")
        };

        return new Stil4mElmSyntax7.Node<Expression.LetDeclaration>(declNode.Range, mappedDecl);
    }

    private static LambdaStruct MapLambda(
        LambdaStruct lambda,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new LambdaStruct(
            BackslashLocation: lambda.BackslashLocation,
            Arguments: lambda.Arguments,
            ArrowLocation: lambda.ArrowLocation,
            Expression: MapExpression(lambda.Expression, mapQualifiedName));
    }

    private static CaseBlock MapCaseBlock(
        CaseBlock caseBlock,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new CaseBlock(
            CaseTokenLocation: caseBlock.CaseTokenLocation,
            Expression: MapExpression(caseBlock.Expression, mapQualifiedName),
            OfTokenLocation: caseBlock.OfTokenLocation,
            Cases: [.. caseBlock.Cases.Select(c => MapCase(c, mapQualifiedName))]);
    }

    private static Case MapCase(
        Case caseItem,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new Case(
            Pattern: caseItem.Pattern,
            ArrowLocation: caseItem.ArrowLocation,
            Expression: MapExpression(caseItem.Expression, mapQualifiedName));
    }

    private static RecordExprField MapRecordExprField(
        RecordExprField field,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new RecordExprField(field.FieldName, field.EqualsLocation, MapExpression(field.ValueExpr, mapQualifiedName));
    }

    /// <summary>
    /// Helper to map over a SeparatedSyntaxList.
    /// </summary>
    private static SeparatedSyntaxList<TNode> MapSeparatedList<TNode>(
        SeparatedSyntaxList<TNode> list,
        Func<TNode, TNode> mapper)
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
                throw new NotImplementedException(
                    $"Unexpected SeparatedSyntaxList type: {list.GetType().Name}")
        };
    }
}
