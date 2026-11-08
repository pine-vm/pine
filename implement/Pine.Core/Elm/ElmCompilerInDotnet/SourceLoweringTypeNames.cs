using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Linq;

using Expression = Pine.Core.Elm.Syntax.Abstract.Expression;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>Separates type-name questions from value-name questions before the lexical name adapter runs.</summary>
internal sealed class SourceLoweringTypeNames(Func<DeclQualifiedName, DeclQualifiedName> map)
{
    public Declaration Apply(Declaration declaration) =>
        declaration switch
        {
            Declaration.FunctionDeclaration function =>
            new Declaration.FunctionDeclaration(Function(function.Function)),

            Declaration.AliasDeclaration alias =>
            new Declaration.AliasDeclaration(
                new(alias.TypeAlias.Name, alias.TypeAlias.Generics, Type(alias.TypeAlias.TypeAnnotation))),

            Declaration.ChoiceTypeDeclaration choice =>
            new Declaration.ChoiceTypeDeclaration(
                new(
                    choice.TypeDeclaration.Name,
                    choice.TypeDeclaration.Generics,
                    [
                        .. choice.TypeDeclaration.Constructors.Select(
                            constructor =>
                            ValueConstructor.Create(constructor.Name, [.. constructor.Arguments.Select(Type)]))
                    ])),

            Declaration.PortDeclaration port => new Declaration.PortDeclaration(Signature(port.Signature)),
            Declaration.InfixDeclaration => declaration,

            _ =>
            throw new NotImplementedException(
                nameof(Apply) + " does not handle declaration variant: " + declaration.GetType().Name)
        };

    private Signature Signature(Signature signature) => new(signature.Name, Type(signature.TypeAnnotation));

    private FunctionStruct Function(FunctionStruct function) =>
        new(
            function.Signature is { } signature ? Signature(signature) : null,
            new(function.Declaration.Name, function.Declaration.Arguments, Rewrite(function.Declaration.Expression)));

    private TypeAnnotation Type(TypeAnnotation type) =>
        type switch
        {
            TypeAnnotation.Typed typed => NamedType(typed),

            TypeAnnotation.FunctionTypeAnnotation function =>
            new TypeAnnotation.FunctionTypeAnnotation(
                Type(function.ArgumentType),
                Type(function.ReturnType)),

            TypeAnnotation.Tupled tuple => new TypeAnnotation.Tupled([.. tuple.TypeAnnotations.Select(Type)]),
            TypeAnnotation.Record record => new TypeAnnotation.Record(Record(record.RecordDefinition)),

            TypeAnnotation.GenericRecord record =>
            new TypeAnnotation.GenericRecord(record.GenericName, Record(record.RecordDefinition)),

            TypeAnnotation.GenericType => type,
            TypeAnnotation.Unit => type,

            _ =>
            throw new NotImplementedException(
                nameof(Type) + " does not handle type annotation variant: " + type.GetType().Name)
        };

    private TypeAnnotation NamedType(TypeAnnotation.Typed typed)
    {
        var name = map(DeclQualifiedName.Create(typed.ModuleName, typed.Name));
        return new TypeAnnotation.Typed(name.Namespaces, name.DeclName, [.. typed.TypeArguments.Select(Type)]);
    }

    private RecordDefinition Record(RecordDefinition record) =>
        new([.. record.Fields.Select(field => RecordField.Create(field.FieldName, Type(field.FieldType)))]);

    private Expression Rewrite(Expression expression) =>
        expression switch
        {
            Expression.Application application =>
            new Expression.Application(
                Rewrite(application.Function),
                [.. application.Arguments.Select(Rewrite)]),

            Expression.OperatorApplication operation =>
            new Expression.OperatorApplication(
                operation.Operator,
                operation.Direction,
                Rewrite(operation.Left),
                Rewrite(operation.Right)),

            Expression.IfBlock conditional =>
            new Expression.IfBlock(
                Rewrite(conditional.Condition),
                Rewrite(conditional.ThenBlock),
                Rewrite(conditional.ElseBlock)),

            Expression.Negation negation => new Expression.Negation(Rewrite(negation.Expression)),

            Expression.TupledExpression tuple =>
            new Expression.TupledExpression([.. tuple.Elements.Select(Rewrite)]),

            Expression.ListExpr list => new Expression.ListExpr([.. list.Elements.Select(Rewrite)]),

            Expression.RecordExpr record =>
            new Expression.RecordExpr(
                [
                    .. record.Fields
                    .Select(
                        field => new RecordSetter(field.FieldName, field.FieldNameValue, Rewrite(field.Value)))
                ]),

            Expression.RecordAccess access =>
            new Expression.RecordAccess(
                Rewrite(access.Record),
                access.FieldName,
                access.FieldNameValue),

            Expression.RecordUpdateExpression update =>
            new Expression.RecordUpdateExpression(
                update.RecordName,
                [
                    .. update.Fields
                    .Select(
                        field => new RecordSetter(field.FieldName, field.FieldNameValue, Rewrite(field.Value)))
                ]),

            Expression.LambdaExpression lambda =>
            new Expression.LambdaExpression(lambda.Arguments, Rewrite(lambda.Expression)),

            Expression.CaseExpression cases =>
            new Expression.CaseExpression(
                Rewrite(cases.Expression),
                [.. cases.Cases.Select(branch => new Case(branch.Pattern, Rewrite(branch.Expression)))]),

            Expression.LetExpression let =>
            new Expression.LetExpression(
                [.. let.Declarations.Select(Let)],
                Rewrite(let.Expression)),

            Expression.UnitExpr => expression,
            Expression.StringLiteral => expression,
            Expression.CharLiteral => expression,
            Expression.IntegerLiteral => expression,
            Expression.FloatLiteral => expression,
            Expression.Identifier => expression,
            Expression.PrefixOperator => expression,
            Expression.RecordAccessFunction => expression,
            Expression.GLSLExpression => expression,

            _ =>
            throw new NotImplementedException(
                nameof(Rewrite) + " does not handle expression variant: " + expression.GetType().Name)
        };

    private LetDeclaration Let(LetDeclaration declaration) =>
        declaration switch
        {
            LetDeclaration.LetFunction function => new LetDeclaration.LetFunction(Function(function.Function)),

            LetDeclaration.LetDestructuring binding =>
            new LetDeclaration.LetDestructuring(binding.Pattern, Rewrite(binding.Expression)),

            _ =>
            throw new NotImplementedException(
                nameof(Let) + " does not handle let declaration variant: " + declaration.GetType().Name)
        };
}
