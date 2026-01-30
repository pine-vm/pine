using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Linq;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;

namespace Pine.Core.Elm.ElmSyntax;

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
            Comments: file.Comments,
            IncompleteDeclarations: file.IncompleteDeclarations);
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

    private static Node<Declaration> MapDeclaration(
        Node<Declaration> declNode,
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

        return new Node<Declaration>(declNode.Range, mappedDecl);
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
                ? new Node<Signature>(sig.Range, MapSignature(sig.Value, mapQualifiedName))
                : null,
            Declaration: new Node<FunctionImplementation>(func.Declaration.Range, mappedImpl));
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

    private static (Location? PipeTokenLocation, Node<ValueConstructor> Constructor) MapValueConstructor(
        (Location? PipeTokenLocation, Node<ValueConstructor> Constructor) constructorItem,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var constructor = constructorItem.Constructor.Value;
        return (
            constructorItem.PipeTokenLocation,
            new Node<ValueConstructor>(
                constructorItem.Constructor.Range,
                new ValueConstructor(
                    Name: constructor.Name,
                    Arguments: [.. constructor.Arguments.Select(a => MapTypeAnnotation(a, mapQualifiedName))])));
    }

    private static Node<TypeAnnotation> MapTypeAnnotation(
        Node<TypeAnnotation> typeAnnotNode,
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
                    TypeAnnotations: MapSeparatedList(tupled.TypeAnnotations, t => MapTypeAnnotation(t, mapQualifiedName))),

            TypeAnnotation.Record record =>
                new TypeAnnotation.Record(
                    RecordDefinition: MapRecordDefinition(record.RecordDefinition, mapQualifiedName)),

            TypeAnnotation.GenericRecord genericRecord =>
                new TypeAnnotation.GenericRecord(
                    GenericName: genericRecord.GenericName,
                    PipeLocation: genericRecord.PipeLocation,
                    RecordDefinition: new Node<RecordDefinition>(
                        genericRecord.RecordDefinition.Range,
                        MapRecordDefinition(genericRecord.RecordDefinition.Value, mapQualifiedName))),

            // These don't contain type names to map
            TypeAnnotation.GenericType or TypeAnnotation.Unit =>
                typeAnnotNode.Value,

            _ =>
                throw new NotImplementedException(
                    $"Mapping for type annotation '{typeAnnotNode.Value.GetType().Name}' is not implemented.")
        };

        return new Node<TypeAnnotation>(typeAnnotNode.Range, mapped);
    }

    private static TypeAnnotation.Typed MapTypedAnnotation(
        TypeAnnotation.Typed typed,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var originalQualifiedName = new QualifiedNameRef(
            ModuleName: typed.TypeName.Value.ModuleName,
            Name: typed.TypeName.Value.Name);

        var mappedQualifiedName = mapQualifiedName(originalQualifiedName);

        var newTypeName = new Node<(IReadOnlyList<string> ModuleName, string Name)>(
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

    private static Node<RecordField> MapRecordField(
        Node<RecordField> fieldNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var field = fieldNode.Value;
        return new Node<RecordField>(
            fieldNode.Range,
            new RecordField(
                FieldName: field.FieldName,
                ColonLocation: field.ColonLocation,
                FieldType: MapTypeAnnotation(field.FieldType, mapQualifiedName)));
    }

    // Expression mapping methods

    private static Node<ExpressionSyntax> MapExpression(
        Node<ExpressionSyntax> exprNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var mappedExpr = exprNode.Value switch
        {
            ExpressionSyntax.LetExpression letExpr =>
                new ExpressionSyntax.LetExpression(MapLetBlock(letExpr.Value, mapQualifiedName)),

            ExpressionSyntax.LambdaExpression lambdaExpr =>
                new ExpressionSyntax.LambdaExpression(MapLambda(lambdaExpr.Lambda, mapQualifiedName)),

            ExpressionSyntax.CaseExpression caseExpr =>
                new ExpressionSyntax.CaseExpression(MapCaseBlock(caseExpr.CaseBlock, mapQualifiedName)),

            ExpressionSyntax.IfBlock ifBlock =>
                new ExpressionSyntax.IfBlock(
                    IfTokenLocation: ifBlock.IfTokenLocation,
                    Condition: MapExpression(ifBlock.Condition, mapQualifiedName),
                    ThenTokenLocation: ifBlock.ThenTokenLocation,
                    ThenBlock: MapExpression(ifBlock.ThenBlock, mapQualifiedName),
                    ElseTokenLocation: ifBlock.ElseTokenLocation,
                    ElseBlock: MapExpression(ifBlock.ElseBlock, mapQualifiedName)),

            ExpressionSyntax.Application app =>
                new ExpressionSyntax.Application(
                    [.. app.Arguments.Select(a => MapExpression(a, mapQualifiedName))]),

            ExpressionSyntax.ListExpr listExpr =>
                new ExpressionSyntax.ListExpr(
                    Elements: MapSeparatedList(listExpr.Elements, e => MapExpression(e, mapQualifiedName))),

            ExpressionSyntax.TupledExpression tupled =>
                new ExpressionSyntax.TupledExpression(
                    Elements: MapSeparatedList(tupled.Elements, e => MapExpression(e, mapQualifiedName))),

            ExpressionSyntax.RecordExpr recordExpr =>
                new ExpressionSyntax.RecordExpr(
                    Fields: MapSeparatedList(recordExpr.Fields, f => MapRecordExprField(f, mapQualifiedName))),

            ExpressionSyntax.RecordUpdateExpression recordUpdate =>
                new ExpressionSyntax.RecordUpdateExpression(
                    RecordName: recordUpdate.RecordName,
                    PipeLocation: recordUpdate.PipeLocation,
                    Fields: MapSeparatedList(recordUpdate.Fields, f => MapRecordExprField(f, mapQualifiedName))),

            ExpressionSyntax.OperatorApplication opApp =>
                new ExpressionSyntax.OperatorApplication(
                    Operator: opApp.Operator,
                    Direction: opApp.Direction,
                    Left: MapExpression(opApp.Left, mapQualifiedName),
                    Right: MapExpression(opApp.Right, mapQualifiedName)),

            ExpressionSyntax.Negation negation =>
                new ExpressionSyntax.Negation(MapExpression(negation.Expression, mapQualifiedName)),

            ExpressionSyntax.ParenthesizedExpression parenExpr =>
                new ExpressionSyntax.ParenthesizedExpression(
                    Expression: MapExpression(parenExpr.Expression, mapQualifiedName)),

            ExpressionSyntax.RecordAccess recordAccess =>
                new ExpressionSyntax.RecordAccess(
                    Record: MapExpression(recordAccess.Record, mapQualifiedName),
                    FieldName: recordAccess.FieldName),

            // These expression types don't contain type annotations or expressions to traverse
            ExpressionSyntax.Integer or ExpressionSyntax.Literal or ExpressionSyntax.CharLiteral or
            ExpressionSyntax.Hex or ExpressionSyntax.Floatable or ExpressionSyntax.UnitExpr or
            ExpressionSyntax.FunctionOrValue or ExpressionSyntax.RecordAccessFunction or
            ExpressionSyntax.PrefixOperator =>
                exprNode.Value,

            _ =>
            throw new NotImplementedException(
                $"Mapping for expression type '{exprNode.Value.GetType().Name}' is not implemented.")
        };

        return new Node<ExpressionSyntax>(exprNode.Range, mappedExpr);
    }

    private static ExpressionSyntax.LetBlock MapLetBlock(
        ExpressionSyntax.LetBlock letBlock,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new ExpressionSyntax.LetBlock(
            LetTokenLocation: letBlock.LetTokenLocation,
            Declarations: [.. letBlock.Declarations.Select(d => MapLetDeclaration(d, mapQualifiedName))],
            InTokenLocation: letBlock.InTokenLocation,
            Expression: MapExpression(letBlock.Expression, mapQualifiedName));
    }

    private static Node<ExpressionSyntax.LetDeclaration> MapLetDeclaration(
        Node<ExpressionSyntax.LetDeclaration> declNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        ExpressionSyntax.LetDeclaration mappedDecl = declNode.Value switch
        {
            ExpressionSyntax.LetDeclaration.LetFunction letFunc =>
                new ExpressionSyntax.LetDeclaration.LetFunction(MapFunction(letFunc.Function, mapQualifiedName)),

            ExpressionSyntax.LetDeclaration.LetDestructuring letDestr =>
                new ExpressionSyntax.LetDeclaration.LetDestructuring(
                    Pattern: letDestr.Pattern,
                    EqualsTokenLocation: letDestr.EqualsTokenLocation,
                    Expression: MapExpression(letDestr.Expression, mapQualifiedName)),

            _ =>
                throw new NotImplementedException(
                    $"Mapping for let declaration type '{declNode.Value.GetType().Name}' is not implemented.")
        };

        return new Node<ExpressionSyntax.LetDeclaration>(declNode.Range, mappedDecl);
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
