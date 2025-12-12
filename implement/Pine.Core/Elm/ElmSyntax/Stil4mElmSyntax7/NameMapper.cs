using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Maps qualified names in an Elm syntax tree.
/// This class processes a file and transforms <see cref="QualifiedNameRef"/> instances
/// according to a provided mapping function or dictionary.
/// </summary>
/// <remarks>
/// Similar to <see cref="Avh4Format"/>, this class operates on the file level,
/// transforming the syntax tree before rendering.
/// </remarks>
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
                new Declaration.PortDeclaration(MapSignature(portDecl.Signature, mapQualifiedName)),

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
            TypeAnnotation: MapTypeAnnotation(sig.TypeAnnotation, mapQualifiedName));
    }

    private static TypeAlias MapTypeAlias(
        TypeAlias alias,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new TypeAlias(
            Documentation: alias.Documentation,
            Name: alias.Name,
            Generics: alias.Generics,
            TypeAnnotation: MapTypeAnnotation(alias.TypeAnnotation, mapQualifiedName));
    }

    private static TypeStruct MapTypeStruct(
        TypeStruct typeStruct,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new TypeStruct(
            Documentation: typeStruct.Documentation,
            Name: typeStruct.Name,
            Generics: typeStruct.Generics,
            Constructors: [.. typeStruct.Constructors.Select(c => MapValueConstructor(c, mapQualifiedName))]);
    }

    private static Node<ValueConstructor> MapValueConstructor(
        Node<ValueConstructor> constructorNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var constructor = constructorNode.Value;
        return new Node<ValueConstructor>(
            constructorNode.Range,
            new ValueConstructor(
                Name: constructor.Name,
                Arguments: [.. constructor.Arguments.Select(a => MapTypeAnnotation(a, mapQualifiedName))]));
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
                    ReturnType: MapTypeAnnotation(funcType.ReturnType, mapQualifiedName)),

            TypeAnnotation.Tupled tupled =>
                new TypeAnnotation.Tupled(
                    [.. tupled.TypeAnnotations.Select(t => MapTypeAnnotation(t, mapQualifiedName))]),

            TypeAnnotation.Record record =>
                new TypeAnnotation.Record(MapRecordDefinition(record.RecordDefinition, mapQualifiedName)),

            TypeAnnotation.GenericRecord genericRecord =>
                new TypeAnnotation.GenericRecord(
                    GenericName: genericRecord.GenericName,
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
            [.. recordDef.Fields.Select(f => MapRecordField(f, mapQualifiedName))]);
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
                FieldType: MapTypeAnnotation(field.FieldType, mapQualifiedName)));
    }

    // Expression mapping methods

    private static Node<Expression> MapExpression(
        Node<Expression> exprNode,
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
                    Condition: MapExpression(ifBlock.Condition, mapQualifiedName),
                    ThenBlock: MapExpression(ifBlock.ThenBlock, mapQualifiedName),
                    ElseBlock: MapExpression(ifBlock.ElseBlock, mapQualifiedName)),

            Expression.Application app =>
                new Expression.Application(
                    [.. app.Arguments.Select(a => MapExpression(a, mapQualifiedName))]),

            Expression.ListExpr listExpr =>
                new Expression.ListExpr(
                    [.. listExpr.Elements.Select(e => MapExpression(e, mapQualifiedName))]),

            Expression.TupledExpression tupled =>
                new Expression.TupledExpression(
                    [.. tupled.Elements.Select(e => MapExpression(e, mapQualifiedName))]),

            Expression.RecordExpr recordExpr =>
                new Expression.RecordExpr(
                    [.. recordExpr.Fields.Select(f => MapRecordExprField(f, mapQualifiedName))]),

            Expression.RecordUpdateExpression recordUpdate =>
                new Expression.RecordUpdateExpression(
                    RecordName: recordUpdate.RecordName,
                    Fields: [.. recordUpdate.Fields.Select(f => MapRecordExprField(f, mapQualifiedName))]),

            Expression.OperatorApplication opApp =>
                new Expression.OperatorApplication(
                    Operator: opApp.Operator,
                    Direction: opApp.Direction,
                    Left: MapExpression(opApp.Left, mapQualifiedName),
                    Right: MapExpression(opApp.Right, mapQualifiedName)),

            Expression.Negation negation =>
                new Expression.Negation(MapExpression(negation.Expression, mapQualifiedName)),

            Expression.ParenthesizedExpression parenExpr =>
                new Expression.ParenthesizedExpression(MapExpression(parenExpr.Expression, mapQualifiedName)),

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

        return new Node<Expression>(exprNode.Range, mappedExpr);
    }

    private static Expression.LetBlock MapLetBlock(
        Expression.LetBlock letBlock,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new Expression.LetBlock(
            Declarations: [.. letBlock.Declarations.Select(d => MapLetDeclaration(d, mapQualifiedName))],
            Expression: MapExpression(letBlock.Expression, mapQualifiedName));
    }

    private static Node<Expression.LetDeclaration> MapLetDeclaration(
        Node<Expression.LetDeclaration> declNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        Expression.LetDeclaration mappedDecl = declNode.Value switch
        {
            Expression.LetDeclaration.LetFunction letFunc =>
                new Expression.LetDeclaration.LetFunction(MapFunction(letFunc.Function, mapQualifiedName)),

            Expression.LetDeclaration.LetDestructuring letDestr =>
                new Expression.LetDeclaration.LetDestructuring(
                    Pattern: letDestr.Pattern,
                    Expression: MapExpression(letDestr.Expression, mapQualifiedName)),

            _ =>
                throw new NotImplementedException(
                    $"Mapping for let declaration type '{declNode.Value.GetType().Name}' is not implemented.")
        };

        return new Node<Expression.LetDeclaration>(declNode.Range, mappedDecl);
    }

    private static LambdaStruct MapLambda(
        LambdaStruct lambda,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new LambdaStruct(
            Arguments: lambda.Arguments,
            Expression: MapExpression(lambda.Expression, mapQualifiedName));
    }

    private static CaseBlock MapCaseBlock(
        CaseBlock caseBlock,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new CaseBlock(
            Expression: MapExpression(caseBlock.Expression, mapQualifiedName),
            Cases: [.. caseBlock.Cases.Select(c => MapCase(c, mapQualifiedName))]);
    }

    private static Case MapCase(
        Case caseItem,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        return new Case(
            Pattern: caseItem.Pattern,
            Expression: MapExpression(caseItem.Expression, mapQualifiedName));
    }

    private static Node<(Node<string> fieldName, Node<Expression> valueExpr)> MapRecordExprField(
        Node<(Node<string> fieldName, Node<Expression> valueExpr)> fieldNode,
        Func<QualifiedNameRef, QualifiedNameRef> mapQualifiedName)
    {
        var field = fieldNode.Value;
        return new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
            fieldNode.Range,
            (field.fieldName, MapExpression(field.valueExpr, mapQualifiedName)));
    }
}
