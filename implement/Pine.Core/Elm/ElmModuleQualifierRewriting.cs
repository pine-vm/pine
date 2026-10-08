using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Linq;

using File = Pine.Core.Elm.ElmSyntax.SyntaxModel.File;

namespace Pine.Core.Elm;

using Expression = ElmSyntax.SyntaxModel.Expression;

/// <summary>Rewrites only qualified syntax references, never strings, comments or unqualified local bindings.</summary>
internal sealed class ElmModuleQualifierRewriting(IReadOnlyDictionary<string, string> aliases)
{
    public File Rewrite(File file) =>
        file with
        {
            Declarations = [.. file.Declarations.Select(node => node with { Value = Declaration(node.Value) })],
        };

    private IReadOnlyList<string> Qualifier(IReadOnlyList<string> name) =>
        aliases.TryGetValue(string.Join(".", name), out var alias) ? alias.Split('.') : name;

    private Declaration Declaration(Declaration declaration) =>
        declaration switch
        {
            Declaration.FunctionDeclaration function => function with { Function = Function(function.Function) },

            Declaration.AliasDeclaration alias =>
            alias with
            {
                TypeAlias = alias.TypeAlias with { TypeAnnotation = Type(alias.TypeAlias.TypeAnnotation) },
            },

            Declaration.ChoiceTypeDeclaration choice =>
            choice with
            {
                TypeDeclaration =
                choice.TypeDeclaration with
                {
                    Constructors =
                    Map(
                        choice.TypeDeclaration.Constructors,
                        node => node with
                        {
                            Value = node.Value with { Arguments = [.. node.Value.Arguments.Select(Type)] },
                        }),
                },
            },

            Declaration.PortDeclaration port => port with { Signature = Signature(port.Signature) },
            Declaration.InfixDeclaration infix => infix,

            _ =>
            throw Missing(nameof(Declaration), declaration),
        };

    private FunctionStruct Function(FunctionStruct function) =>
        function with
        {
            Signature =
            function.Signature is { } signature ? signature with { Value = Signature(signature.Value) } : null,
            Declaration =
            function.Declaration with
            {
                Value =
                function.Declaration.Value with
                {
                    Arguments = [.. function.Declaration.Value.Arguments.Select(Pattern)],
                    Expression = Expression(function.Declaration.Value.Expression),
                },
            },
        };

    private Signature Signature(Signature signature) =>
        signature with { TypeAnnotation = Type(signature.TypeAnnotation) };

    private Node<TypeAnnotation> Type(Node<TypeAnnotation> node) =>
        node with
        {
            Value =
            node.Value switch
            {
                TypeAnnotation.GenericType generic => generic,
                TypeAnnotation.Unit unit => unit,

                TypeAnnotation.Typed typed =>
                typed with
                {
                    TypeName =
                    typed.TypeName with { Value = (Qualifier(typed.TypeName.Value.ModuleName), typed.TypeName.Value.Name) },
                    TypeArguments = [.. typed.TypeArguments.Select(Type)],
                },

                TypeAnnotation.Tupled tuple => tuple with { TypeAnnotations = Map(tuple.TypeAnnotations, Type) },
                TypeAnnotation.Record record => record with { RecordDefinition = Record(record.RecordDefinition) },

                TypeAnnotation.GenericRecord record =>
                record with
                {
                    RecordDefinition = record.RecordDefinition with { Value = Record(record.RecordDefinition.Value) },
                },

                TypeAnnotation.FunctionTypeAnnotation function =>
                function with
                {
                    ArgumentType = Type(function.ArgumentType),
                    ReturnType = Type(function.ReturnType),
                },

                _ =>
                throw Missing(nameof(Type), node.Value),
            },
        };

    private RecordDefinition Record(RecordDefinition record) =>
        record with
        {
            Fields =
            Map(
                record.Fields,
                node => node with { Value = node.Value with { FieldType = Type(node.Value.FieldType) } }),
        };

    private Node<Pattern> Pattern(Node<Pattern> node) =>
        node with
        {
            Value =
            node.Value switch
            {
                Pattern.AllPattern all => all,
                Pattern.VarPattern variable => variable,
                Pattern.UnitPattern unit => unit,
                Pattern.CharPattern character => character,
                Pattern.StringPattern text => text,
                Pattern.IntPattern integer => integer,
                Pattern.HexPattern hex => hex,
                Pattern.FloatPattern number => number,
                Pattern.RecordPattern record => record,
                Pattern.TuplePattern tuple => tuple with { Elements = Map(tuple.Elements, Pattern) },
                Pattern.ListPattern list => list with { Elements = Map(list.Elements, Pattern) },
                Pattern.UnConsPattern cons => cons with { Head = Pattern(cons.Head), Tail = Pattern(cons.Tail) },

                Pattern.NamedPattern named =>
                named with
                {
                    Name = named.Name with { ModuleName = Qualifier(named.Name.ModuleName) },
                    Arguments = [.. named.Arguments.Select(Pattern)],
                },

                Pattern.AsPattern alias => alias with { Pattern = Pattern(alias.Pattern) },
                Pattern.ParenthesizedPattern parentheses => parentheses with { Pattern = Pattern(parentheses.Pattern) },

                _ =>
                throw Missing(nameof(Pattern), node.Value),
            },
        };

    private Node<Expression> Expression(Node<Expression> node) =>
        node with
        {
            Value =
            node.Value switch
            {
                Expression.UnitExpr unit => unit,
                Expression.StringLiteral text => text,
                Expression.MultilineStringLiteral text => text,
                Expression.CharLiteral character => character,
                Expression.IntegerLiteral integer => integer,
                Expression.FloatLiteral number => number,
                Expression.PrefixOperator prefix => prefix,
                Expression.RecordAccessFunction access => access,
                Expression.GLSLExpression shader => shader,
                Expression.Identifier identifier => identifier with { ModuleName = Qualifier(identifier.ModuleName) },
                Expression.Negation negation => negation with { Expression = Expression(negation.Expression) },

                Expression.Parenthesized parentheses =>
                parentheses with { Expression = Expression(parentheses.Expression) },

                Expression.ListExpr list => list with { Elements = Map(list.Elements, Expression) },
                Expression.TupledExpression tuple => tuple with { Elements = Map(tuple.Elements, Expression) },

                Expression.IfBlock conditional =>
                conditional with
                {
                    Condition = Expression(conditional.Condition),
                    ThenBlock = Expression(conditional.ThenBlock),
                    ElseBlock = Expression(conditional.ElseBlock),
                },

                Expression.Application application =>
                application with
                {
                    Function = Expression(application.Function),
                    Arguments = [.. application.Arguments.Select(Expression)],
                },

                Expression.OperatorApplication operation =>
                operation with
                {
                    Left = Expression(operation.Left),
                    Right = Expression(operation.Right),
                },

                Expression.LambdaExpression lambda =>
                lambda with
                {
                    Lambda =
                    lambda.Lambda with
                    {
                        Arguments = [.. lambda.Lambda.Arguments.Select(Pattern)],
                        Expression = Expression(lambda.Lambda.Expression),
                    },
                },

                Expression.CaseExpression match =>
                match with
                {
                    CaseBlock =
                    match.CaseBlock with
                    {
                        Expression = Expression(match.CaseBlock.Expression),
                        Cases =
                        [
                        .. match.CaseBlock.Cases.Select(
                            branch => branch with
                            {
                                Pattern = Pattern(branch.Pattern),
                                Expression = Expression(branch.Expression),
                            })
                        ],
                    },
                },

                Expression.LetExpression let =>
                let with
                {
                    Value =
                    let.Value with
                    {
                        Expression = Expression(let.Value.Expression),
                        Declarations =
                        [
                        .. let.Value.Declarations.Select(
                            declaration => declaration with
                            {
                                Value =
                                declaration.Value switch
                                {
                                    Expression.LetDeclaration.LetFunction function =>
                                    function with { Function = Function(function.Function) },

                                    Expression.LetDeclaration.LetDestructuring destructuring =>
                                    destructuring with
                                    {
                                        Pattern = Pattern(destructuring.Pattern),
                                        Expression = Expression(destructuring.Expression),
                                    },

                                    _ =>
                                    throw Missing("LetDeclaration", declaration.Value),
                                },
                            })
                        ],
                    },
                },

                Expression.RecordExpr record => record with { Fields = Map(record.Fields, Field) },
                Expression.RecordUpdateExpression update => update with { Fields = Map(update.Fields, Field) },
                Expression.RecordAccess access => access with { Record = Expression(access.Record) },

                _ =>
                throw Missing(nameof(Expression), node.Value),
            },
        };

    private RecordExprField Field(RecordExprField field) => field with { ValueExpr = Expression(field.ValueExpr) };

    private static SeparatedSyntaxList<T> Map<T>(SeparatedSyntaxList<T> list, Func<T, T> transform) =>
        list switch
        {
            SeparatedSyntaxList<T>.Empty empty => empty,

            SeparatedSyntaxList<T>.NonEmpty nonEmpty =>
            nonEmpty with
            {
                First = transform(nonEmpty.First),
                Rest = [.. nonEmpty.Rest.Select(item => (item.SeparatorLocation, transform(item.Node)))],
            },

            _ =>
            throw Missing(nameof(Map), list),
        };

    private static NotImplementedException Missing(string method, object variant) =>
        new($"{method} does not handle syntax variant: {variant.GetType().Name}");
}
