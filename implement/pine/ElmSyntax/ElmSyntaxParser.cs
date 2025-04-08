using Pine.Core;
using Pine.Core.Elm;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.ElmSyntax;

public class ElmSyntaxParser
{
    /*
    type alias File =
        { moduleDefinition : Node Module
        , imports : List (Node Import)
        , declarations : List (Node Declaration)
        , comments : List (Node Comment)
        }
     * */
    private record File(
        Node<Module> ModuleDefinition,
        IReadOnlyList<Node<Import>> Imports,
        IReadOnlyList<Node<Declaration>> Declarations,
        IReadOnlyList<Node<string>> Comments);

    private record Node<T>(
        Range Range,
        T Value);

    private record Range(
        Location Start,
        Location End);

    public record Location(
        int Row,
        int Column);

    /*
    type alias Import =
        { moduleName : Node ModuleName
        , moduleAlias : Maybe (Node ModuleName)
        , exposingList : Maybe (Node Exposing)
        }
     * */

    private record Import(
        Node<ModuleName> ModuleName,
        Node<ModuleName>? ModuleAlias,
        Node<Exposing>? ExposingList);

    private abstract record Module
    {
        public sealed record NormalModule(
            DefaultModuleData ModuleData)
            : Module;
    }

    private record DefaultModuleData(
        Node<ModuleName> ModuleName,
        Node<Exposing> ExposingList);

    private abstract record Exposing
    {
        public sealed record All(
            Range Range)
            : Exposing;
    }

    private abstract record Declaration
    {
        /*
        type Declaration
            = FunctionDeclaration Function
            | AliasDeclaration TypeAlias
            | CustomTypeDeclaration Type
            | PortDeclaration Signature
            | InfixDeclaration Infix
            | Destructuring (Node Pattern) (Node Expression)
         * */

        public sealed record FunctionDeclaration(
            FunctionStruct Function)
            : Declaration;

        public sealed record CustomTypeDeclaration(
            TypeStruct TypeDeclaration)
            : Declaration;
    }

    /*
    type alias Type =
        { documentation : Maybe (Node Documentation)
        , name : Node String
        , generics : List (Node String)
        , constructors : List (Node ValueConstructor)
        }


    type alias ValueConstructor =
        { name : Node String
        , arguments : List (Node TypeAnnotation)
        }

     * */

    private record TypeStruct(
        Node<string> TypeName,
        IReadOnlyList<Node<string>> Generics,
        IReadOnlyList<Node<ValueConstructor>> Constructors);

    private record ValueConstructor(
        Node<string> Name,
        IReadOnlyList<Node<TypeAnnotation>> Arguments);

    /*
    type TypeAnnotation
        = GenericType String
        | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
        | Unit
        | Tupled (List (Node TypeAnnotation))
        | Record RecordDefinition
        | GenericRecord (Node String) (Node RecordDefinition)
        | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)

     * */

    private abstract record TypeAnnotation
    {
        /*
        type TypeAnnotation
            = GenericType String
            | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
            | Unit
            | Tupled (List (Node TypeAnnotation))
            | Record RecordDefinition
            | GenericRecord (Node String) (Node RecordDefinition)
            | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)
         * */

        public sealed record GenericType(
            string Name)
            : TypeAnnotation;

        public sealed record Typed(
            Node<(ModuleName ModuleName, string Name)> TypeName,
            IReadOnlyList<Node<TypeAnnotation>> TypeArguments)
            : TypeAnnotation;

        public sealed record Unit
            : TypeAnnotation;

        public sealed record Tupled(
            IReadOnlyList<Node<TypeAnnotation>> TypeAnnotations)
            : TypeAnnotation;

        public sealed record Record(
            RecordDefinition RecordDefinition)
            : TypeAnnotation;

        public sealed record GenericRecord(
            Node<string> GenericName,
            Node<RecordDefinition> RecordDefinition)
            : TypeAnnotation;

        public sealed record FunctionTypeAnnotation(
            Node<TypeAnnotation> ArgumentType,
            Node<TypeAnnotation> ReturnType)
            : TypeAnnotation;
    }

    /*
    type alias RecordDefinition =
        List (Node RecordField)
     * */

    private record RecordDefinition(
        IReadOnlyList<Node<RecordField>> Fields);

    /*
    type alias RecordField =
        ( Node String, Node TypeAnnotation )
     * */

    private record RecordField(
        Node<string> FieldName,
        Node<TypeAnnotation> FieldType);

    /*
    type alias Function =
        { documentation : Maybe (Node Documentation)
        , signature : Maybe (Node Signature)
        , declaration : Node FunctionImplementation
        }
     * */
    private record FunctionStruct(
        Node<FunctionImplementation> Declaration,
        Node<Signature>? Signature);

    /*
    type alias FunctionImplementation =
        { name : Node String
        , arguments : List (Node Pattern)
        , expression : Node Expression
        }
     * */
    private record FunctionImplementation(
        Node<string> Name,
        IReadOnlyList<Node<Pattern>> Arguments,
        Node<Expression> Expression);

    /*
    type alias Signature =
        { name : Node String
        , typeAnnotation : Node TypeAnnotation
        }
     * */
    private record Signature(
        Node<string> Name,
        Node<TypeAnnotation> TypeAnnotation);

    private abstract record Pattern
    {
        /*
        type Pattern
            = AllPattern
            | UnitPattern
            | CharPattern Char
            | StringPattern String
            | IntPattern Int
            | HexPattern Int
            | FloatPattern Float
            | TuplePattern (List (Node Pattern))
            | RecordPattern (List (Node String))
            | UnConsPattern (Node Pattern) (Node Pattern)
            | ListPattern (List (Node Pattern))
            | VarPattern String
            | NamedPattern QualifiedNameRef (List (Node Pattern))
            | AsPattern (Node Pattern) (Node String)
            | ParenthesizedPattern (Node Pattern)         * 
         * */

        public sealed record AllPattern
            : Pattern;

        public sealed record VarPattern(
            string Name)
            : Pattern;

        public sealed record UnitPattern
            : Pattern;

        public sealed record CharPattern(
            char Value)
            : Pattern;

        public sealed record StringPattern(
            string Value)
            : Pattern;

        public sealed record IntPattern(
            int Value)
            : Pattern;

        public sealed record HexPattern(
            int Value)
            : Pattern;

        public sealed record FloatPattern(
            float Value)
            : Pattern;

        public sealed record TuplePattern(
            IReadOnlyList<Node<Pattern>> Elements)
            : Pattern;

        public sealed record RecordPattern(
            IReadOnlyList<Node<string>> Fields)
            : Pattern;

        public sealed record UnConsPattern(
            Node<Pattern> Head,
            Node<Pattern> Tail)
            : Pattern;

        public sealed record ListPattern(
            IReadOnlyList<Node<Pattern>> Elements)
            : Pattern;

        public sealed record NamedPattern(
            QualifiedNameRef Name,
            IReadOnlyList<Node<Pattern>> Arguments)
            : Pattern;
    }

    /*
    type alias QualifiedNameRef =
        { moduleName : List String
        , name : String
        }
     * */
    private record QualifiedNameRef(
        ModuleName ModuleName,
        string Name);

    private abstract record Expression
    {
        /*
        type Expression
            = UnitExpr
            | Application (List (Node Expression))
            | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
            | FunctionOrValue ModuleName String
            | IfBlock (Node Expression) (Node Expression) (Node Expression)
            | PrefixOperator String
            | Operator String
            | Integer Int
            | Hex Int
            | Floatable Float
            | Negation (Node Expression)
            | Literal String
            | CharLiteral Char
            | TupledExpression (List (Node Expression))
            | ParenthesizedExpression (Node Expression)
            | LetExpression LetBlock
            | CaseExpression CaseBlock
            | LambdaExpression Lambda
            | RecordExpr (List (Node RecordSetter))
            | ListExpr (List (Node Expression))
            | RecordAccess (Node Expression) (Node String)
            | RecordAccessFunction String
            | RecordUpdateExpression (Node String) (List (Node RecordSetter))
            | GLSLExpression String

         * */

        public sealed record Literal(
            string Value)
            : Expression;

        public sealed record UnitExpr
            : Expression;

        public sealed record Integer(
            int Value)
            : Expression;

        public sealed record ListExpr(
            IReadOnlyList<Node<Expression>> Elements)
            : Expression;

        public sealed record FunctionOrValue(
            ModuleName ModuleName,
            string Name)
            : Expression;

        public sealed record IfBlock(
            Node<Expression> Condition,
            Node<Expression> ThenBlock,
            Node<Expression> ElseBlock)
            : Expression;

        public sealed record PrefixOperator(
            string Operator)
            : Expression;

        public sealed record ParenthesizedExpression(
            Node<Expression> Expression)
            : Expression;

        public sealed record Application(
            IReadOnlyList<Node<Expression>> Arguments)
            : Expression;

        public sealed record OperatorApplication(
            string Operator,
            InfixDirection Direction,
            Node<Expression> Left,
            Node<Expression> Right)
            : Expression;

        public sealed record TupledExpression(
            IReadOnlyList<Node<Expression>> Elements)
            : Expression;

        public sealed record LambdaExpression(
            LambdaStruct Lambda)
            : Expression;

        public sealed record CaseExpression(
            CaseBlock CaseBlock)
            : Expression;

        public sealed record RecordExpr(
            IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
            : Expression;

        public sealed record RecordAccess(
            Node<Expression> Record,
            Node<string> FieldName)
            : Expression;
    }

    /*
    type alias Lambda =
        { args : List (Node Pattern)
        , expression : Node Expression
        }
     * */
    private record LambdaStruct(
        IReadOnlyList<Node<Pattern>> Arguments,
        Node<Expression> Expression);

    /*
    type alias CaseBlock =
        { expression : Node Expression
        , cases : Cases
        }
     * */
    private record CaseBlock(
        Node<Expression> Expression,
        IReadOnlyList<Case> Cases);

    /*
    type alias Case =
        ( Node Pattern, Node Expression )
     * */
    private record Case(
        Node<Pattern> Pattern,
        Node<Expression> Expression);

    /*
    type InfixDirection
        = Left
        | Right
        | Non
     * */
    private enum InfixDirection
    {
        Left,
        Right,
        Non,
    }

    private record InfixOperatorInfo(
        int Precedence,
        InfixDirection Direction)
    {
        public static InfixOperatorInfo GetInfo(string symbol) =>
            /*
             * module Basics:

            infix right 0 (<|) = apL
            infix left  0 (|>) = apR
            infix right 2 (||) = or
            infix right 3 (&&) = and
            infix non   4 (==) = eq
            infix non   4 (/=) = neq
            infix non   4 (<) = lt
            infix non   4 (>) = gt
            infix non   4 (<=) = le
            infix non   4 (>=) = ge
            infix right 5 (++) = append
            infix left  6 (+) = add
            infix left  6 (-) = sub
            infix left  7 (*) = mul
            infix left  7 (//) = idiv
            infix right 8 (^) = pow
            infix left  9 (<<) = composeL
            infix right 9 (>>) = composeR

            * module List:
            infix right 5 (::) = cons

             * */
            symbol switch
            {
                "<|" => new InfixOperatorInfo(0, InfixDirection.Right),
                "|>" => new InfixOperatorInfo(0, InfixDirection.Left),

                "||" => new InfixOperatorInfo(2, InfixDirection.Right),
                "&&" => new InfixOperatorInfo(3, InfixDirection.Right),

                "==" => new InfixOperatorInfo(4, InfixDirection.Non),
                "/=" => new InfixOperatorInfo(4, InfixDirection.Non),

                "<" => new InfixOperatorInfo(4, InfixDirection.Non),
                ">" => new InfixOperatorInfo(4, InfixDirection.Non),

                "<=" => new InfixOperatorInfo(4, InfixDirection.Non),
                ">=" => new InfixOperatorInfo(4, InfixDirection.Non),

                "++" => new InfixOperatorInfo(5, InfixDirection.Right),

                "::" => new InfixOperatorInfo(5, InfixDirection.Right),

                "+" => new InfixOperatorInfo(6, InfixDirection.Left),
                "-" => new InfixOperatorInfo(6, InfixDirection.Left),
                "*" => new InfixOperatorInfo(7, InfixDirection.Left),
                "//" => new InfixOperatorInfo(7, InfixDirection.Left),
                "^" => new InfixOperatorInfo(8, InfixDirection.Right),

                "<<" => new InfixOperatorInfo(9, InfixDirection.Left),
                ">>" => new InfixOperatorInfo(9, InfixDirection.Right),

                _ =>
                throw new ArgumentException("Unknown operator: " + symbol),
            };
    }


    public static Result<string, ElmValue> ParseModuleTextAsElmSyntaxElmValue(
        string elmModuleText)
    {
        var parseResult =
            ParseModuleText(elmModuleText);

        if (parseResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (parseResult.IsOkOrNull() is not { } parseOk)
        {
            throw new NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().Name);
        }

        return EncodeAsElmValue(parseOk);
    }

    private static ElmValue EncodeAsElmValue(File file)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("comments",
                ElmValue.ListInstance(
                    [..file.Comments.Select(c => EncodeNode(EncodeString, c))])),

                ("declarations",
                ElmValue.ListInstance(
                    [..file.Declarations.Select(d => EncodeNode(EncodeDeclaration, d))])),

                ("imports",
                ElmValue.ListInstance(
                    [..file.Imports.Select(i => EncodeNode(EncodeImport, i))])),

                ("moduleDefinition",
                EncodeNode(EncodeModule, file.ModuleDefinition)),
                ]);
    }

    private static ElmValue EncodeModule(Module module)
    {
        return module switch
        {
            Module.NormalModule moduleData =>
            ElmValue.TagInstance(
                "NormalModule",
                [new ElmValue.ElmRecord(
                    [
                    ("exposingList",
                    EncodeNode(EncodeExposing, moduleData.ModuleData.ExposingList)),

                    ("moduleName",
                    EncodeNode(EncodeModuleName, moduleData.ModuleData.ModuleName)),
                    ])
                ]),

            _ =>
            throw new NotImplementedException(
                "Unexpected module type: " + module.GetType().Name),
        };
    }

    private static ElmValue EncodeModuleName(ModuleName moduleName)
    {
        return
            new ElmValue.ElmList(
                [.. moduleName.Select(ElmValue.StringInstance)]);
    }

    private static ElmValue EncodeExposing(Exposing exposing)
    {
        return exposing switch
        {
            Exposing.All range =>
                ElmValue.TagInstance(
                    "All",
                    [EncodeRange(range.Range)]),

            _ =>
            throw new NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().Name),
        };
    }

    private static ElmValue EncodeImport(Import import)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("exposingList",
                import.ExposingList is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance(
                        "Just",
                        [EncodeNode(EncodeExposing, import.ExposingList)])),

                ("moduleAlias",
                import.ModuleAlias is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance(
                        "Just",
                        [EncodeNode(EncodeModuleName, import.ModuleAlias)])),

                ("moduleName",
                EncodeNode(EncodeModuleName, import.ModuleName)),
                ]);
    }

    private static ElmValue EncodeDeclaration(Declaration declaration)
    {
        return declaration switch
        {
            Declaration.FunctionDeclaration functionDeclaration =>
                ElmValue.TagInstance(
                    "FunctionDeclaration",
                    [EncodeFunction(functionDeclaration.Function)]),

            Declaration.CustomTypeDeclaration typeDeclaration =>
                ElmValue.TagInstance(
                    "CustomTypeDeclaration",
                    [EncodeTypeStruct(typeDeclaration.TypeDeclaration)]),

            _ =>
            throw new NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().Name),
        };
    }

    private static ElmValue EncodeTypeStruct(TypeStruct type)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("constructors",
                ElmValue.ListInstance(
                    [..type.Constructors.Select(c => EncodeNode(EncodeValueConstructor, c))])),

                ("documentation",
                ElmValue.TagInstance("Nothing",[])),

                ("generics",
                ElmValue.ListInstance(
                    [..type.Generics.Select(g => EncodeNode(EncodeString, g))])),

                ("name",
                EncodeNode(EncodeString, type.TypeName)),
                ]);
    }

    private static ElmValue EncodeValueConstructor(ValueConstructor constructor)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("arguments",
                ElmValue.ListInstance(
                    [..constructor.Arguments.Select(a => EncodeNode(EncodeTypeAnnotation, a))])),

                ("name",
                EncodeNode(EncodeString, constructor.Name)),
                ]);
    }

    private static ElmValue EncodeTypeAnnotation(TypeAnnotation type)
    {
        return type switch
        {
            // | GenericType String
            TypeAnnotation.GenericType name =>
                ElmValue.TagInstance(
                    "GenericType",
                    [EncodeString(name.Name)]),

            TypeAnnotation.Typed typeName =>
            // | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
                ElmValue.TagInstance(
                    "Typed",
                    [
                        EncodeNode(
                            aggregateName =>
                            ElmValue.ListInstance(
                                [
                                EncodeModuleName(aggregateName.ModuleName),
                                EncodeString(aggregateName.Name),
                                ]),
                            typeName.TypeName),

                        ElmValue.ListInstance(
                            [..typeName.TypeArguments.Select(a => EncodeNode(EncodeTypeAnnotation, a))]),
                    ]),

            TypeAnnotation.Tupled tupled =>
            // | Tupled (List (Node TypeAnnotation))
                ElmValue.TagInstance(
                    "Tupled",
                    [ElmValue.ListInstance(
                        [..tupled.TypeAnnotations.Select(a => EncodeNode(EncodeTypeAnnotation, a))])]),

            TypeAnnotation.Unit =>
            // | Unit
                ElmValue.TagInstance(
                    "Unit",
                    []),

            TypeAnnotation.FunctionTypeAnnotation
            functionType =>
            // | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)
                ElmValue.TagInstance(
                    "FunctionTypeAnnotation",
                    [
                        EncodeNode(EncodeTypeAnnotation, functionType.ArgumentType),
                        EncodeNode(EncodeTypeAnnotation, functionType.ReturnType),
                    ]),

            _ =>
            throw new NotImplementedException(
                "Unexpected type annotation type: " + type.GetType().Name),
        };
    }

    private static ElmValue EncodeFunction(FunctionStruct function)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("declaration",
                EncodeNode(EncodeFunctionImplementation, function.Declaration)),

                ("documentation",
                ElmValue.TagInstance("Nothing",[])),

                ("signature",
                function.Signature is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance(
                        "Just",
                        [EncodeNode(EncodeSignature, function.Signature)])),
                ]);
    }

    private static ElmValue EncodeSignature(Signature signature)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("name",
                EncodeNode(EncodeString, signature.Name)),
                ("typeAnnotation",
                EncodeNode(EncodeTypeAnnotation, signature.TypeAnnotation)),
                ]);
    }

    private static ElmValue EncodeFunctionImplementation(FunctionImplementation function)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("arguments",
                ElmValue.ListInstance(
                    [..function.Arguments.Select(a => EncodeNode(EncodePattern, a))])),

                ("expression",
                EncodeNode(EncodeExpression, function.Expression)),

                ("name",
                EncodeNode(EncodeString, function.Name)),
                ]);
    }

    private static ElmValue EncodePattern(Pattern pattern)
    {
        return pattern switch
        {
            Pattern.VarPattern name =>
                ElmValue.TagInstance(
                    "VarPattern",
                    [EncodeString(name.Name)]),

            Pattern.AllPattern =>
                ElmValue.TagInstance(
                    "AllPattern",
                    []),

            Pattern.UnitPattern =>
                ElmValue.TagInstance(
                    "UnitPattern",
                    []),

            Pattern.CharPattern value =>
                ElmValue.TagInstance(
                    "CharPattern",
                    [ElmValue.StringInstance(value.Value.ToString())]),

            Pattern.StringPattern value =>
                ElmValue.TagInstance(
                    "StringPattern",
                    [EncodeString(value.Value)]),

            Pattern.IntPattern value =>
                ElmValue.TagInstance(
                    "IntPattern",
                    [ElmValue.Integer(value.Value)]),

            Pattern.HexPattern value =>
                ElmValue.TagInstance(
                    "HexPattern",
                    [ElmValue.Integer(value.Value)]),

            Pattern.NamedPattern name =>
                ElmValue.TagInstance(
                    "NamedPattern",
                    [
                        EncodeQualifiedNameRef(name.Name),

                        ElmValue.ListInstance(
                            [..name.Arguments.Select(a => EncodeNode(EncodePattern, a))]),
                    ]),

            Pattern.TuplePattern elements =>
                ElmValue.TagInstance(
                    "TuplePattern",
                    [ElmValue.ListInstance(
                        [..elements.Elements.Select(e => EncodeNode(EncodePattern, e))])]),

            _ =>
            throw new NotImplementedException(
                "Unexpected pattern type: " + pattern.GetType().Name),
        };
    }

    private static ElmValue EncodeQualifiedNameRef(QualifiedNameRef qualifiedNameRef)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("moduleName",
                ElmValue.ListInstance(
                    [..qualifiedNameRef.ModuleName.Select(ElmValue.StringInstance)])),
                ("name",
                EncodeString(qualifiedNameRef.Name)),
                ]);
    }

    private static ElmValue EncodeExpression(Expression expression)
    {
        return expression switch
        {
            // | Literal String
            Expression.Literal value =>
                ElmValue.TagInstance(
                    "Literal",
                    [EncodeString(value.Value)]),

            // | Integer Int
            Expression.Integer value =>
                ElmValue.TagInstance(
                    "Integer",
                    [ElmValue.Integer(value.Value)]),

            // | ListExpr (List (Node Expression))
            Expression.ListExpr elements =>
                ElmValue.TagInstance(
                    "ListExpr",
                    [ElmValue.ListInstance(
                        [..elements.Elements.Select(e => EncodeNode(EncodeExpression, e))])]),

            // | FunctionOrValue ModuleName String
            Expression.FunctionOrValue functionOrValue =>
                ElmValue.TagInstance(
                    "FunctionOrValue",
                    [
                        EncodeModuleName(functionOrValue.ModuleName),
                        EncodeString(functionOrValue.Name),
                    ]),

            // | Application (List (Node Expression))
            Expression.Application arguments =>
                ElmValue.TagInstance(
                    "Application",
                    [ElmValue.ListInstance(
                        [..arguments.Arguments.Select(a => EncodeNode(EncodeExpression, a))])]),

            // | ParenthesizedExpression (Node Expression)
            Expression.ParenthesizedExpression expressionNode =>
                ElmValue.TagInstance(
                    "ParenthesizedExpression",
                    [EncodeNode(EncodeExpression, expressionNode.Expression)]),

            // | TupledExpression (List (Node Expression))
            Expression.TupledExpression elements =>
                ElmValue.TagInstance(
                    "TupledExpression",
                    [ElmValue.ListInstance(
                        [..elements.Elements.Select(e => EncodeNode(EncodeExpression, e))])]),

            // | LambdaExpression Lambda
            Expression.LambdaExpression lambda =>
                ElmValue.TagInstance(
                    "LambdaExpression",
                    [EncodeLambda(lambda.Lambda)]),

            // | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
            Expression.OperatorApplication operatorApplication =>
                ElmValue.TagInstance(
                    "OperatorApplication",
                    [
                        EncodeString(operatorApplication.Operator),
                        ElmValue.TagInstance(
                            operatorApplication.Direction.ToString(),
                            []),
                        EncodeNode(EncodeExpression, operatorApplication.Left),
                        EncodeNode(EncodeExpression, operatorApplication.Right),
                    ]),

            // | IfBlock (Node Expression) (Node Expression) (Node Expression)
            Expression.IfBlock ifBlock =>
                ElmValue.TagInstance(
                    "IfBlock",
                    [
                        EncodeNode(EncodeExpression, ifBlock.Condition),
                        EncodeNode(EncodeExpression, ifBlock.ThenBlock),
                        EncodeNode(EncodeExpression, ifBlock.ElseBlock),
                    ]),

            // | UnitExpr
            Expression.UnitExpr =>
                ElmValue.TagInstance(
                    "UnitExpr",
                    []),

            // | CaseExpression CaseBlock
            Expression.CaseExpression caseBlock =>
                ElmValue.TagInstance(
                    "CaseExpression",
                    [EncodeCaseBlock(caseBlock.CaseBlock)]),

            // | PrefixOperator String
            Expression.PrefixOperator prefixOperator =>
                ElmValue.TagInstance(
                    "PrefixOperator",
                    [EncodeString(prefixOperator.Operator)]),

            // | RecordExpr (List (Node RecordSetter))
            Expression.RecordExpr fields =>
                ElmValue.TagInstance(
                    "RecordExpr",
                    [ElmValue.ListInstance(
                        [..fields.Fields.Select(rs => EncodeNode(EncodeRecordSetter, rs))])]),

            // | RecordAccess (Node Expression) (Node String)
            Expression.RecordAccess recordAccess =>
                ElmValue.TagInstance(
                    "RecordAccess",
                    [
                        EncodeNode(EncodeExpression, recordAccess.Record),
                        EncodeNode(EncodeString, recordAccess.FieldName),
                    ]),

            _ =>
                throw new NotImplementedException(
                    "Unexpected expression type: " + expression.GetType().Name),
        };
    }

    private static ElmValue EncodeCaseBlock(CaseBlock caseBlock)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("cases",
                ElmValue.ListInstance(
                    [..caseBlock.Cases.Select(EncodeCase)])),

                ("expression",
                EncodeNode(EncodeExpression, caseBlock.Expression)),
                ]);
    }

    private static ElmValue EncodeCase(Case caseBlock)
    {
        return
            ElmValue.ListInstance(
                [
                EncodeNode(EncodePattern, caseBlock.Pattern),
                EncodeNode(EncodeExpression, caseBlock.Expression),
                ]);
    }

    private static ElmValue EncodeLambda(LambdaStruct lambda)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("args",
                ElmValue.ListInstance(
                    [..lambda.Arguments.Select(a => EncodeNode(EncodePattern, a))])),
                ("expression",
                EncodeNode(EncodeExpression, lambda.Expression)),
                ]);
    }

    private static ElmValue EncodeRecordSetter(
        (Node<string> fieldName, Node<Expression> expr) field)
    {
        return
            ElmValue.ListInstance(
                [
                EncodeNode(EncodeString, field.fieldName),
                EncodeNode(EncodeExpression, field.expr),
                ]);
    }

    private static ElmValue EncodeString(string value)
    {
        return ElmValue.StringInstance(value);
    }

    private static ElmValue EncodeNode<T>(
        Func<T, ElmValue> encodeValue,
        Node<T> node)
    {
        return
            ElmValue.TagInstance(
                "Node",
                [
                EncodeRange(node.Range),
                encodeValue(node.Value),
                ]);
    }

    private static ElmValue EncodeRange(Range range)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("end", EncodeLocation(range.End)),
                ("start", EncodeLocation(range.Start)),
                ]);
    }

    private static ElmValue EncodeLocation(Location location)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("column", ElmValue.Integer(location.Column)),
                ("row", ElmValue.Integer(location.Row)),
            ]);
    }

    public record Token(
        TokenType Type,
        string Lexeme,
        Location Start,
        Location End);

    public enum TokenType
    {
        /*
         * TODO: Explore using dedicated tokens for keywords like 'case', 'of', 'let', 'in', etc.
         * This might simplify expression parsing code.
         * */

        Identifier,
        StringLiteral,
        NumberLiteral,
        OpenParen,
        CloseParen,
        OpenBrace,
        CloseBrace,
        OpenBracket,
        CloseBracket,
        Comma,
        Dot,
        DotDot,
        Equal,
        Arrow,
        Colon,
        Pipe,
        Comment,
        Whitespace,
        Newline,
        Lambda,
        InfixOperator,
        Unknown,
    }

    private static bool IsKeyword(Token token)
    {
        if (token.Type is TokenType.Identifier)
        {
            return token.Lexeme switch
            {
                "case" or "of" or "let" or "in" or "if" or "then" or "else" or "import" or "module" or "exposing" =>
                true,

                _ => false,
            };
        }

        return false;
    }

    private static bool CharCanAppearInOperator(char Char) =>
        Char switch
        {
            '|' or '>' or '<' or '.' or ':' or '%' or '^' or '&' or '*' or '/' or '-' or '+' or '=' =>
            true,

            _ => false,
        };

    public class Tokenizer(string input)
    {
        private readonly string _input = input;
        private int _position;
        private int _line = 1;
        private int _column = 1;

        public IEnumerable<Token> Tokenize()
        {
            while (!IsAtEnd())
            {
                var token = NextToken();

                if (token != null)
                {
                    yield return token;
                }
            }
        }

        private bool IsAtEnd() =>
            _position >= _input.Length;

        private char Peek() =>
            IsAtEnd()
            ?
            '\0'
            :
            _input[_position];

        private char PeekNext() =>
            (_position + 1 < _input.Length)
            ?
            _input[_position + 1]
            :
            '\0';

        private char Advance()
        {
            char current = _input[_position];
            _position++;

            if (current is '\n')
            {
                _line++;
                _column = 1;
            }
            else
            {
                _column++;
            }

            return current;
        }

        private Token NextToken()
        {
            // Capture the start location for this token.
            Location start = new(_line, _column);
            char current = Peek();

            // Handle whitespace and newlines
            if (char.IsWhiteSpace(current))
            {
                if (current is '\n')
                {
                    Advance();
                    Location end = new(_line, _column);
                    return new Token(TokenType.Newline, "\n", start, end);
                }
                else
                {
                    string whitespace = "";

                    while (!IsAtEnd() && char.IsWhiteSpace(Peek()) && Peek() != '\n')
                    {
                        whitespace += Advance();
                    }

                    Location end = new(_line, _column);

                    return new Token(TokenType.Whitespace, whitespace, start, end);
                }
            }

            // Handle comments (for example, starting with "//")
            if (current is '-' && PeekNext() is '-')
            {
                string comment = "";

                while (!IsAtEnd() && Peek() is not '\n')
                {
                    comment += Advance();
                }

                Location end = new(_line, _column);

                return new Token(TokenType.Comment, comment, start, end);
            }

            // Handle string literals
            if (current is '"')
            {
                Advance(); // Consume the opening quote
                StringBuilder sb = new();
                while (!IsAtEnd() && Peek() != '"')
                {
                    // If we see a backslash, check for an escaped character.
                    if (Peek() is '\\')
                    {
                        Advance(); // Consume the backslash
                        if (!IsAtEnd())
                        {
                            char escaped = Advance(); // Consume the character after the backslash
                                                      // Handle specific escape sequences
                            if (escaped is '"')
                            {
                                sb.Append('"'); // Escaped quote
                            }
                            else if (escaped is 'n')
                            {
                                sb.Append('\n'); // Newline escape
                            }
                            else if (escaped is 't')
                            {
                                sb.Append('\t'); // Tab escape
                            }
                            else if (escaped is '\\')
                            {
                                sb.Append('\\'); // Backslash escape
                            }
                            else
                            {
                                // For any other escape, just append the character (or handle error)
                                sb.Append(escaped);
                            }
                        }
                    }
                    else
                    {
                        sb.Append(Advance());
                    }
                }

                if (IsAtEnd())
                {
                    // Unterminated string literal; here you might want to throw an error.
                    return new Token(TokenType.Unknown, sb.ToString(), start, new Location(_line, _column));
                }

                Advance(); // Consume the closing quote
                Location end = new(_line, _column);
                return new Token(TokenType.StringLiteral, sb.ToString(), start, end);
            }

            // Handle number literals
            if (char.IsDigit(current))
            {
                string number = "";
                while (!IsAtEnd() && char.IsDigit(Peek()))
                {
                    number += Advance();
                }
                // Optionally handle fractional parts
                if (!IsAtEnd() && Peek() is '.')
                {
                    number += Advance(); // Consume the dot
                    while (!IsAtEnd() && char.IsDigit(Peek()))
                    {
                        number += Advance();
                    }
                }
                Location end = new(_line, _column);
                return new Token(TokenType.NumberLiteral, number, start, end);
            }

            // Handle identifiers (start with letter or underscore, then letters, digits or underscores)
            if (char.IsLetter(current) || current is '_')
            {
                string identifier = "";
                while (!IsAtEnd() && (char.IsLetterOrDigit(Peek()) || Peek() is '_'))
                {
                    identifier += Advance();
                }
                Location end = new(_line, _column);
                return new Token(TokenType.Identifier, identifier, start, end);
            }

            // Handle single-character tokens and multi-character tokens like the arrow "->"
            switch (current)
            {
                case '(':
                    Advance();
                    return new Token(TokenType.OpenParen, "(", start, new Location(_line, _column));

                case ')':
                    Advance();
                    return new Token(TokenType.CloseParen, ")", start, new Location(_line, _column));

                case '{':
                    Advance();
                    return new Token(TokenType.OpenBrace, "{", start, new Location(_line, _column));

                case '}':
                    Advance();
                    return new Token(TokenType.CloseBrace, "}", start, new Location(_line, _column));

                case '[':
                    Advance();
                    return new Token(TokenType.OpenBracket, "[", start, new Location(_line, _column));

                case ']':
                    Advance();
                    return new Token(TokenType.CloseBracket, "]", start, new Location(_line, _column));

                case ',':
                    Advance();
                    return new Token(TokenType.Comma, ",", start, new Location(_line, _column));

                case '.':
                    Advance();

                    if (Peek() is '.')
                    {
                        Advance();
                        return new Token(TokenType.DotDot, "..", start, new Location(_line, _column));
                    }

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        string operatorToken = "." + Advance();

                        return new Token(
                            TokenType.InfixOperator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    return new Token(TokenType.Dot, ".", start, new Location(_line, _column));

                case '=':

                    Advance();

                    // Check if this is part of a two-character operator:
                    if (CharCanAppearInOperator(Peek()))
                    {
                        string operatorToken = "=" + Advance();

                        return new Token(
                            TokenType.InfixOperator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    return new Token(TokenType.Equal, "=", start, new Location(_line, _column));

                case '|':

                    Advance();

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        string operatorToken = "|" + Advance();

                        return new Token(
                            TokenType.InfixOperator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    return new Token(TokenType.Pipe, "|", start, new Location(_line, _column));

                case '-':

                    // Check if this is part of an arrow token "->"
                    if (PeekNext() is '>')
                    {
                        Advance(); // Consume '-'
                        Advance(); // Consume '>'
                        return new Token(TokenType.Arrow, "->", start, new Location(_line, _column));
                    }
                    else
                    {
                        Advance();
                        return new Token(TokenType.InfixOperator, "-", start, new Location(_line, _column));
                    }

                case '\\':
                    Advance();

                    return new Token(TokenType.Lambda, "\\", start, new Location(_line, _column));

                case '<':

                    Advance();

                    // Check if this is part of a two-character operator:
                    if (CharCanAppearInOperator(Peek()))
                    {
                        string operatorToken = "<" + Advance();
                        return new Token(
                            TokenType.InfixOperator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    return new Token(
                        TokenType.InfixOperator,
                        "<",
                        start,
                        new Location(_line, _column));

                case '>':

                    Advance();

                    // Check if this is part of a two-character operator:
                    if (CharCanAppearInOperator(Peek()))
                    {
                        string operatorToken = ">" + Advance();

                        return new Token(
                            TokenType.InfixOperator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    return new Token(
                        TokenType.InfixOperator,
                        ">",
                        start,
                        new Location(_line, _column));

                case ':':

                    Advance();

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        string operatorToken = ":" + Advance();

                        return new Token(
                            TokenType.InfixOperator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    return new Token(TokenType.Colon, ":", start, new Location(_line, _column));

                default:
                    // For any unknown character, consume it and return an Unknown token.
                    Advance();
                    return new Token(TokenType.Unknown, current.ToString(), start, new Location(_line, _column));
            }
        }
    }

    private class Parser(ReadOnlyMemory<Token> tokens)
    {
        private int _current = 0;

        // Entry point: parse the entire file and return a File record.
        public Result<string, File> ParseFile()
        {
            try
            {
                ConsumeAllTrivia();

                // Parse the module header
                Node<Module> moduleDefinition = ParseModule();

                ConsumeAllTrivia();

                // Parse the imports (if any)

                var imports = new List<Node<Import>>();

                while (Peek.Type is TokenType.Identifier && Peek.Lexeme is "import")
                {
                    var import = ParseImport();

                    imports.Add(import);

                    ConsumeAllTrivia();
                }

                // Parse the declarations until we've consumed all tokens.
                var declarations = new List<Node<Declaration>>();

                ConsumeAllTrivia();

                while (!IsAtEnd())
                {
                    declarations.Add(ParseDeclaration());

                    ConsumeAllTrivia();
                }

                IReadOnlyList<Token> allComments =
                    [.. tokens.ToArray().Where(t => t.Type is TokenType.Comment)];

                return new File(
                    moduleDefinition,
                    imports,
                    declarations,
                    [.. allComments.Select(c => new Node<string>(new Range(c.Start, c.End), c.Lexeme))]);
            }
            catch (Exception ex)
            {
                return Result<string, File>.err(ex.Message);
            }
        }

        // Parses the module header and returns a Node<Module>
        private Node<Module> ParseModule()
        {
            // Expect the "module" keyword (this could be a token with Type Identifier "module")

            var keywordToken = ConsumeKeyword("module");

            ConsumeAllTrivia();

            // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)
            var moduleNameParts = new List<string>();

            var firstModuleNamePart =
                ConsumeAnyIdentifier("module name");

            moduleNameParts.Add(firstModuleNamePart.Lexeme);

            while (Peek.Type is TokenType.Dot)
            {
                Consume(TokenType.Dot);

                var moduleNamePart = ConsumeAnyIdentifier("module name part");

                moduleNameParts.Add(moduleNamePart.Lexeme);
            }

            // Create a Node<IReadOnlyList<string>> for the module name.
            var moduleNameNode =
                new Node<ModuleName>(
                    new Range(firstModuleNamePart.Start, Peek.Start),
                    moduleNameParts.AsReadOnly());

            // Parse the exposing clause (here we use a dummy implementation).
            Node<Exposing> exposingNode = ParseExposing();

            // Build the module data and wrap it in a Module.NormalModule.
            var moduleData = new DefaultModuleData(moduleNameNode, exposingNode);
            var moduleNodeValue = new Module.NormalModule(moduleData);
            var moduleNode = new Node<Module>(new Range(keywordToken.Start, Peek.Start), moduleNodeValue);
            return moduleNode;
        }

        // Parses an import statement and returns a Node<Import>
        private Node<Import> ParseImport()
        {
            /*
             * Examples of covered syntax:
             * 
            import Bytes.Encode
            import CompilerGenerated.Base64 as Base64
            import WorkspaceState_2021_01
             * */

            ConsumeAllTrivia();

            var keyword = ConsumeKeyword("import");

            ConsumeAllTrivia();
            // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)

            var firstModuleNamePart =
                ConsumeAnyIdentifier("module name");

            var moduleNameParts = new List<Token>([firstModuleNamePart]);

            while (Peek.Type is TokenType.Dot)
            {
                Consume(TokenType.Dot);

                var moduleNamePart = ConsumeAnyIdentifier("module name part");

                moduleNameParts.Add(moduleNamePart);
            }

            var moduleNameNode =
                new Node<ModuleName>(
                    new Range(firstModuleNamePart.Start, moduleNameParts.Last().End),
                    [.. moduleNameParts.Select(t => t.Lexeme)]);

            ConsumeAllTrivia();

            // Parse the optional alias (e.g. "as Base64")

            Node<ModuleName>? moduleAliasNode = null;

            if (Peek.Type is TokenType.Identifier && Peek.Lexeme is "as")
            {
                ConsumeKeyword("as");

                ConsumeAllTrivia();

                var aliasToken = ConsumeAnyIdentifier("module alias");

                moduleAliasNode =
                    new Node<ModuleName>(
                        new Range(aliasToken.Start, Peek.Start),
                        new[] { aliasToken.Lexeme }.AsReadOnly());
            }

            var importRangeEnd =
                moduleAliasNode is null
                ?
                moduleNameNode.Range.End
                :
                moduleAliasNode.Range.End;

            var importNode =
                new Node<Import>(
                    new Range(keyword.Start, importRangeEnd),
                    new Import(
                        moduleNameNode,
                        moduleAliasNode,
                        null));

            return importNode;
        }

        private Node<Exposing> ParseExposing()
        {
            ConsumeAllTrivia();

            var keyword = ConsumeKeyword("exposing");

            ConsumeAllWhitespace(false);

            Consume(TokenType.OpenParen);

            ConsumeAllTrivia();

            if (Peek.Type is TokenType.DotDot)
            {
                var dotDotToken = Consume(TokenType.DotDot);

                ConsumeAllTrivia();

                var closeParen = Consume(TokenType.CloseParen);

                return new Node<Exposing>(
                    new Range(keyword.Start, closeParen.End),
                    new Exposing.All(new Range(dotDotToken.Start, dotDotToken.End)));
            }

            throw ExceptionForCurrentLocation("Unsupported exposing clause.");
        }

        // Parses a declaration (e.g. a function declaration)
        private Node<Declaration> ParseDeclaration()
        {
            // For example, a function declaration might start with a function name.
            Token start = Peek;

            Token identifierToken = ConsumeAnyIdentifier("function name");

            if (identifierToken.Lexeme is "type")
            {
                /*
                 * Parse type declaration, like:
                 * 
                type FileTreeNode blobStructure
                    = BlobNode blobStructure
                    | TreeNode (List ( String, FileTreeNode blobStructure ))
                 * */

                ConsumeAllTrivia();

                // Parse type name

                Token typeNameToken = ConsumeAnyIdentifier("type name");

                ConsumeAllTrivia();

                var typeParameters = new List<Node<string>>();

                // Type parameters are optional

                if (Peek.Type is TokenType.Identifier)
                {
                    if (Peek.Lexeme is "alias")
                    {
                        ConsumeKeyword("alias");

                        throw new NotImplementedException(
                            "Type alias not implemented.");
                    }

                    // Parse type parameters
                    while (Peek.Type is TokenType.Identifier)
                    {
                        var typeParameterToken = ConsumeAnyIdentifier("type parameter");

                        typeParameters.Add(
                            new Node<string>(
                                new Range(typeParameterToken.Start, typeParameterToken.End),
                                typeParameterToken.Lexeme));

                        ConsumeAllTrivia();
                    }
                }

                // Parse the equal sign
                Consume(TokenType.Equal);

                ConsumeAllTrivia();

                // Parse the type definition

                var constructors = new List<Node<ValueConstructor>>();

                while (true)
                {
                    ConsumeAllTrivia();

                    var constructorNameToken = ConsumeAnyIdentifier("constructor name");

                    ConsumeAllTrivia();

                    var constructorArguments = new List<Node<TypeAnnotation>>();

                    while (
                        (Peek.Type is TokenType.Identifier || Peek.Type is TokenType.OpenParen) &&
                        constructorNameToken.Start.Column <= Peek.Start.Column)
                    {
                        var argumentAnnotation =
                            ParseTypeAnnotation(minIndent: constructorNameToken.Start.Column);

                        constructorArguments.Add(argumentAnnotation);
                    }

                    var constructorEnd =
                        constructorArguments.Count is 0
                        ?
                        constructorNameToken.End
                        :
                        constructorArguments.Last().Range.End;

                    constructors.Add(
                        new Node<ValueConstructor>(
                            new Range(constructorNameToken.Start, constructorEnd),
                            new ValueConstructor(
                                new Node<string>(
                                    new Range(constructorNameToken.Start, constructorNameToken.End),
                                    constructorNameToken.Lexeme),
                                constructorArguments)));

                    ConsumeAllTrivia();

                    if (Peek.Type is TokenType.Pipe)
                    {
                        Consume(TokenType.Pipe);
                    }
                    else
                    {
                        break;
                    }
                }

                return
                    new Node<Declaration>(
                        new Range(start.Start, constructors.Last().Range.End),
                        new Declaration.CustomTypeDeclaration(
                            new TypeStruct(
                                new Node<string>(
                                    new Range(typeNameToken.Start, typeNameToken.End),
                                    typeNameToken.Lexeme),
                                typeParameters,
                                constructors)));
            }

            var functionNameToken = identifierToken;

            ConsumeAllTrivia();

            Node<Signature>? signature = null;

            if (Peek.Type is TokenType.Colon)
            {
                // Parse the optional signature (e.g. "toLower : String -> String")

                Consume(TokenType.Colon);

                ConsumeAllTrivia();

                var signatureTypeAnnotation =
                    ParseTypeAnnotation(minIndent: identifierToken.Start.Column);

                signature =
                    new Node<Signature>(
                        new Range(identifierToken.Start, signatureTypeAnnotation.Range.End),
                        new Signature(
                            Name:
                            new Node<string>(
                                new Range(identifierToken.Start, identifierToken.End),
                                identifierToken.Lexeme),
                            TypeAnnotation: signatureTypeAnnotation));

                ConsumeAllTrivia();

                var declNameAgain =
                    ConsumeAnyIdentifier("function name");

                if (declNameAgain.Lexeme != identifierToken.Lexeme)
                {
                    throw ExceptionForCurrentLocation(
                        "Function name does not match signature: " +
                        declNameAgain.Lexeme + " != " + identifierToken.Lexeme);
                }

                functionNameToken = declNameAgain;

                ConsumeAllTrivia();
            }

            var arguments = new List<Node<Pattern>>();

            while (NextTokenMatches(t => t.Type is TokenType.Identifier or TokenType.OpenParen))
            {
                var argument = ParsePattern(minIndent: 0);

                ConsumeAllTrivia();

                arguments.Add(argument);
            }

            ConsumeAllTrivia();

            Consume(TokenType.Equal);

            ConsumeAllTrivia();

            Node<Expression> expression = ParseExpression(1);

            var functionImpl =
                new FunctionImplementation(
                    new Node<string>(new Range(functionNameToken.Start, functionNameToken.End), functionNameToken.Lexeme),
                    arguments,
                    expression);

            var functionStruct =
                new FunctionStruct(
                    new Node<FunctionImplementation>(new Range(functionNameToken.Start, expression.Range.End), functionImpl),
                    signature);

            var declaration =
                new Declaration.FunctionDeclaration(functionStruct);

            return new Node<Declaration>(
                new Range(start.Start, expression.Range.End),
                declaration);
        }

        private Node<TypeAnnotation> ParseTypeAnnotation(int minIndent)
        {
            var lessFunction =
                ParseTypeAnnotationLessFunction(minIndent: minIndent);

            ConsumeAllTrivia();

            if (Peek.Type is TokenType.Arrow)
            {
                Consume(TokenType.Arrow);

                ConsumeAllTrivia();

                var returnType =
                    ParseTypeAnnotation(lessFunction.Range.Start.Column);

                var range =
                    new Range(
                        lessFunction.Range.Start,
                        returnType.Range.End);

                return
                    new Node<TypeAnnotation>(
                        range,
                        new TypeAnnotation.FunctionTypeAnnotation(
                            lessFunction,
                            returnType));
            }

            return lessFunction;
        }

        private Node<TypeAnnotation> ParseTypeAnnotationLessFunction(int minIndent)
        {
            Token start = Peek;

            if (start.Type is TokenType.OpenParen)
            {
                // Is either Tupled or Typed

                var openToken = Consume(TokenType.OpenParen);

                ConsumeAllTrivia();

                var firstTypeAnnotation =
                    ParseTypeAnnotation(minIndent: minIndent);

                ConsumeAllTrivia();

                if (Peek.Type is TokenType.Comma)
                {
                    // | Tupled (List (Node TypeAnnotation))

                    Consume(TokenType.Comma);

                    ConsumeAllTrivia();

                    var tupleItems = new List<Node<TypeAnnotation>>
                    {
                        firstTypeAnnotation
                    };

                    while (Peek.Type is not TokenType.CloseParen)
                    {
                        var typeAnnotation =
                            ParseTypeAnnotation(minIndent: minIndent);

                        tupleItems.Add(typeAnnotation);

                        ConsumeAllTrivia();
                    }

                    var closingToken = Consume(TokenType.CloseParen);

                    var range =
                        new Range(openToken.Start, closingToken.End);

                    if (tupleItems.Count is 1)
                    {
                        return tupleItems[0];
                    }

                    return
                        new Node<TypeAnnotation>(
                            range,
                            new TypeAnnotation.Tupled(tupleItems));
                }

                {
                    var closingToken = Consume(TokenType.CloseParen);

                    return
                        new Node<TypeAnnotation>(
                            new Range(openToken.Start, closingToken.End),
                            firstTypeAnnotation.Value);
                }
            }

            if (start.Type is TokenType.Identifier)
            {
                var firstIdentifierToken =
                    ConsumeAnyIdentifier("first identifier");

                if (char.IsLower(firstIdentifierToken.Lexeme.First()))
                {
                    // GenericType String

                    return new Node<TypeAnnotation>(
                        new Range(start.Start, start.End),
                        new TypeAnnotation.GenericType(firstIdentifierToken.Lexeme));
                }

                // Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))

                // https://github.com/stil4m/elm-syntax/blob/c99a05ac96d3fa15fb3a8dc5ca39eaf78d1e510a/src/Elm/Parser/TypeAnnotation.elm#L336-L357

                var namespaces = new List<Token>();

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var namespaceToken = ConsumeAnyIdentifier("namespace item");

                    namespaces.Add(namespaceToken);
                }

                ConsumeAllTrivia();

                IReadOnlyList<Token> moduleName =
                    [.. namespaces.Prepend(firstIdentifierToken).SkipLast(1)];

                var typeNameToken =
                    namespaces.LastOrDefault() ?? firstIdentifierToken;

                var typeArguments =
                    new List<Node<TypeAnnotation>>();

                while (
                    !IsAtEnd() &&
                    firstIdentifierToken.Start.Column <= Peek.Start.Column &&
                    minIndent <= Peek.Start.Column &&
                    Peek.Type is not TokenType.Arrow &&
                    Peek.Type is not TokenType.Comma &&
                    Peek.Type is not TokenType.CloseParen &&
                    Peek.Type is not TokenType.CloseBracket &&
                    Peek.Type is not TokenType.CloseBrace)
                {
                    var typeArgument =
                        ParseTypeAnnotationLessFunction(minIndent: minIndent);

                    typeArguments.Add(typeArgument);

                    ConsumeAllTrivia();
                }

                ConsumeAllTrivia();

                var rangeEnd =
                    typeArguments.Count is 0
                    ?
                    typeNameToken.End
                    :
                    typeArguments.Last().Range.End;

                var range =
                    new Range(start.Start, rangeEnd);

                var instantiatedRangeStart =
                    moduleName.Count is 0
                    ?
                    typeNameToken.Start
                    :
                    moduleName[0].Start;

                return
                    new Node<TypeAnnotation>(
                        range,
                        new TypeAnnotation.Typed(
                            new Node<(ModuleName ModuleName, string Name)>(
                                new Range(instantiatedRangeStart, typeNameToken.End),
                                (
                                [.. moduleName.Select(t => t.Lexeme)],
                                typeNameToken.Lexeme)),
                            typeArguments));
            }

            throw ExceptionForCurrentLocation(
                "Unsupported type annotation type: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private Node<Expression> ParseExpression(
            int indentMin,
            int minPrecedence = 0)
        {
            // Parse a primary expression first.
            Node<Expression> left = ParsePrimaryExpression(indentMin);

            ConsumeAllTrivia();

            if (!IsAtEnd() && Peek.Type is TokenType.Dot)
            {
                // | RecordAccess (Node Expression) (Node String)

                Consume(TokenType.Dot);

                var recordFieldToken =
                    ConsumeAnyIdentifier("record field");

                var recordAccessRange =
                    new Range(
                        left.Range.Start,
                        recordFieldToken.End);

                var recordAccessExpr =
                    new Expression.RecordAccess(
                        left,
                        new Node<string>(
                            new Range(recordFieldToken.Start, recordFieldToken.End),
                            recordFieldToken.Lexeme));

                return
                    new Node<Expression>(
                        recordAccessRange,
                        recordAccessExpr);
            }

            // Loop while the next token is an infix operator.
            while (!IsAtEnd() && Peek.Type == TokenType.InfixOperator)
            {
                var opToken = Peek;

                InfixOperatorInfo opInfo = InfixOperatorInfo.GetInfo(opToken.Lexeme);

                // If the operator's precedence is less than the minimum required, break.
                if (opInfo.Precedence < minPrecedence)
                {
                    break;
                }

                // Consume the operator token.
                Advance();

                // Determine the next minimum precedence for the right-hand side.
                // For left-associative operators, use (precedence + 1).
                int nextMinPrecedence =
                    opInfo.Direction == InfixDirection.Left
                    ?
                    opInfo.Precedence + 1
                    :
                    opInfo.Precedence;

                ConsumeAllTrivia();

                // Recursively parse the right-hand side with the adjusted precedence.
                Node<Expression> right =
                    ParseExpression(
                        indentMin: indentMin,
                        minPrecedence: nextMinPrecedence);

                // Combine the left and right expressions into an OperatorApplication node.
                left =
                    new Node<Expression>(
                        new Range(left.Range.Start, right.Range.End),
                        new Expression.OperatorApplication(opToken.Lexeme, opInfo.Direction, left, right));

                ConsumeAllTrivia();
            }

            return left;
        }

        private Node<Expression> ParsePrimaryExpression(
            int indentMin)
        {
            var functionExpr =
                ParseBasicPrimaryExpression(indentMin);

            ConsumeAllTrivia();

            /*
             * Following expressions are arguments if indented at least as much as the first identifier
             * and not matching any keyword.
             * */

            var argumentsNodes =
                new List<Node<Expression>>();

            while (
                !IsAtEnd() &&
                !IsKeyword(Peek) &&
                indentMin < Peek.Start.Column &&
                CanStartPrimary(Peek))
            {
                var argumentExpr = ParseBasicPrimaryExpression(indentMin);

                argumentsNodes.Add(argumentExpr);

                ConsumeAllTrivia();
            }

            if (0 < argumentsNodes.Count)
            {
                var applicationRange =
                    new Range(
                        functionExpr.Range.Start,
                        argumentsNodes.Last().Range.End);

                var applicationExpr =
                    new Expression.Application([functionExpr, .. argumentsNodes]);

                return
                    new Node<Expression>(
                        applicationRange,
                        applicationExpr);
            }

            return functionExpr;
        }

        private static bool CanStartPrimary(Token token)
        {
            // You want to allow tokens that can start expressions but not tokens that are infix operators,
            // commas, closing parentheses/brackets, etc.
            return token.Type switch
            {
                TokenType.StringLiteral or TokenType.NumberLiteral or TokenType.Identifier or TokenType.OpenParen or TokenType.OpenBrace or TokenType.OpenBracket =>
                true,

                _ =>
                false,
            };
        }

        private Node<Expression> ParseBasicPrimaryExpression(
            int indentMin)
        {
            Token start = Peek;

            if (start.Type is TokenType.StringLiteral)
            {
                string literal = start.Lexeme;

                Advance();

                var literalExpr = new Expression.Literal(literal);

                return new Node<Expression>(new Range(start.Start, start.End), literalExpr);
            }

            if (start.Type is TokenType.OpenBrace)
            {
                return ParseRecordExpr(indentMin);
            }

            if (start.Type is TokenType.Identifier)
            {
                var firstIdentifierToken = ConsumeAnyIdentifier("first identifier");

                // | FunctionOrValue ModuleName String
                // | Application (List (Node Expression))

                if (firstIdentifierToken.Lexeme is "let")
                {
                    throw new NotImplementedException(
                        "Let expression not implemented.");
                }

                if (firstIdentifierToken.Lexeme is "if")
                {
                    ConsumeAllTrivia();

                    var condition = ParseExpression(indentMin);

                    ConsumeAllTrivia();

                    ConsumeKeyword("then");

                    ConsumeAllTrivia();

                    var thenBranch = ParseExpression(indentMin);

                    ConsumeAllTrivia();

                    ConsumeKeyword("else");

                    ConsumeAllTrivia();

                    var elseBranch = ParseExpression(indentMin);

                    var ifBlockRange =
                        new Range(
                            firstIdentifierToken.Start,
                            elseBranch.Range.End);

                    var ifBlockExpr =
                        new Expression.IfBlock(
                            condition,
                            thenBranch,
                            elseBranch);

                    return
                        new Node<Expression>(
                            ifBlockRange,
                            ifBlockExpr);
                }

                if (firstIdentifierToken.Lexeme is "case")
                {
                    // case valueToEncode of

                    ConsumeAllTrivia();

                    var caseValue = ParseExpression(indentMin);

                    ConsumeAllTrivia();

                    var caseOfToken = ConsumeKeyword("of");

                    ConsumeAllTrivia();

                    var casesIndentMin = Peek.Start.Column;

                    var caseBranches = new List<Node<Case>>();

                    while (
                        !IsAtEnd() &&
                        casesIndentMin <= Peek.Start.Column &&
                        Peek.Type is not TokenType.Comma &&
                        Peek.Type is not TokenType.CloseParen &&
                        Peek.Type is not TokenType.CloseBracket &&
                        Peek.Type is not TokenType.CloseBrace)
                    {
                        var caseBranch = ParseCaseBranch(casesIndentMin);

                        caseBranches.Add(caseBranch);

                        ConsumeAllTrivia();
                    }

                    var caseBlockRange =
                        new Range(
                            firstIdentifierToken.Start,
                            caseBranches.Last().Range.End);

                    var caseBlockExpr =
                        new CaseBlock(
                            caseValue,
                            [.. caseBranches.Select(cb => cb.Value)]);

                    return
                        new Node<Expression>(
                            caseBlockRange,
                            new Expression.CaseExpression(caseBlockExpr));
                }

                if (char.IsLower(firstIdentifierToken.Lexeme[0]) && Peek.Type is TokenType.Dot)
                {
                    // | RecordAccess (Node Expression) (Node String)

                    Consume(TokenType.Dot);

                    var recordFieldToken =
                        ConsumeAnyIdentifier("record field");

                    var recordAccessRange =
                        new Range(
                            firstIdentifierToken.Start,
                            recordFieldToken.End);

                    var recordAccessExpr =
                        new Expression.RecordAccess(
                            new Node<Expression>(
                                new Range(firstIdentifierToken.Start, firstIdentifierToken.End),
                                new Expression.FunctionOrValue(
                                    [],
                                    firstIdentifierToken.Lexeme)),
                            new Node<string>(
                                new Range(recordFieldToken.Start, recordFieldToken.End),
                                recordFieldToken.Lexeme));

                    return
                        new Node<Expression>(
                            recordAccessRange,
                            recordAccessExpr);
                }

                var furtherIdentifiers = new List<Token>();

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var furtherNamePart =
                        ConsumeAnyIdentifier("function or value name part");

                    furtherIdentifiers.Add(furtherNamePart);
                }

                IReadOnlyList<Token> aggregateNameParts =
                    [firstIdentifierToken, .. furtherIdentifiers];

                var firstExpr =
                    new Node<Expression>(
                        new Range(
                            firstIdentifierToken.Start,
                            aggregateNameParts[aggregateNameParts.Count - 1].End),
                        new Expression.FunctionOrValue(
                            [.. aggregateNameParts.SkipLast(1).Select(t => t.Lexeme)],
                            aggregateNameParts[aggregateNameParts.Count - 1].Lexeme));

                return firstExpr;
            }

            if (start.Type is TokenType.OpenBracket)
            {
                // | ListExpr (List (Node Expression))

                var listOpenToken =
                    Consume(TokenType.OpenBracket);

                ConsumeAllTrivia();

                var elements = new List<Node<Expression>>();

                while (Peek.Type is not TokenType.CloseBracket)
                {
                    var elementExpr = ParseExpression(indentMin);

                    elements.Add(elementExpr);

                    ConsumeAllTrivia();

                    if (Peek.Type is TokenType.Comma)
                    {
                        Consume(TokenType.Comma);

                        ConsumeAllTrivia();
                    }
                }

                var listCloseToken =
                    Consume(TokenType.CloseBracket);

                var listRange =
                    new Range(listOpenToken.Start, listCloseToken.End);

                var listExpr =
                    new Expression.ListExpr(elements);

                return new Node<Expression>(listRange, listExpr);
            }

            if (start.Type is TokenType.OpenParen)
            {
                /*
                 * Can be either of:
                 * | TupledExpression (List (Node Expression))
                 * | ParenthesizedExpression (Node Expression)
                 * */

                var parenOpenToken =
                    Consume(TokenType.OpenParen);

                var nextTwoTokens =
                    EnumerateFollowingTokens().Take(2).ToArray();

                if (nextTwoTokens.Length is 2)
                {
                    if (nextTwoTokens[0].Type is TokenType.InfixOperator &&
                        nextTwoTokens[1].Type is TokenType.CloseParen)
                    {
                        var operatorToken = Consume(TokenType.InfixOperator);

                        var parenCloseToken =
                            Consume(TokenType.CloseParen);

                        return
                            new Node<Expression>(
                                new Range(parenOpenToken.Start, parenCloseToken.End),
                                new Expression.PrefixOperator(operatorToken.Lexeme));

                    }
                }

                ConsumeAllTrivia();

                var firstItemExpr = ParseExpression(indentMin);

                ConsumeAllTrivia();

                var furtherItems = new List<Node<Expression>>();

                while (Peek.Type is TokenType.Comma)
                {
                    Consume(TokenType.Comma);

                    ConsumeAllTrivia();

                    var furtherItemExpr = ParseExpression(indentMin);

                    furtherItems.Add(furtherItemExpr);

                    ConsumeAllTrivia();
                }

                {
                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

                    var parenRange =
                        new Range(parenOpenToken.Start, parenCloseToken.End);

                    if (furtherItems.Count is 0)
                    {
                        var parenthesizedExpr =
                            new Expression.ParenthesizedExpression(firstItemExpr);

                        return new Node<Expression>(parenRange, parenthesizedExpr);
                    }

                    var tupledExpr =
                        new Expression.TupledExpression(
                            [firstItemExpr, .. furtherItems]);

                    return new Node<Expression>(parenRange, tupledExpr);
                }
            }

            if (start.Type is TokenType.NumberLiteral)
            {
                if (int.TryParse(start.Lexeme, out var number))
                {
                    Advance();

                    var literalExpr = new Expression.Integer(number);

                    return new Node<Expression>(new Range(start.Start, start.End), literalExpr);
                }
            }

            if (start.Type is TokenType.Lambda)
            {
                // | Lambda (Node Expression)

                var lambdaToken = Consume(TokenType.Lambda);

                ConsumeAllTrivia();

                /*
                 * Example:
                 * 
                 * (\type_arg -> json_encode_Bytes type_arg)
                 * */

                var arguments = new List<Node<Pattern>>();

                while (Peek.Type is TokenType.Identifier)
                {
                    var argumentToken = ConsumeAnyIdentifier("argument name");

                    ConsumeAllTrivia();

                    var argument =
                        new Node<Pattern>(
                            new Range(argumentToken.Start, argumentToken.End),
                            new Pattern.VarPattern(argumentToken.Lexeme));

                    arguments.Add(argument);
                }

                ConsumeAllTrivia();

                var arrowToken = Consume(TokenType.Arrow);

                ConsumeAllTrivia();

                var expression = ParseExpression(indentMin);

                var lambdaRange =
                    new Range(lambdaToken.Start, expression.Range.End);

                var lambdaExpr =
                    new LambdaStruct(
                        arguments,
                        expression);

                return
                    new Node<Expression>(
                        lambdaRange,
                        new Expression.LambdaExpression(lambdaExpr));
            }

            throw ExceptionForCurrentLocation(
                "Unsupported token type in expression: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private Node<Case> ParseCaseBranch(
            int minIndent)
        {
            /*
             * Example:
             * 
                Nothing ->
                    [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object
             */

            Token start = Peek;

            var pattern = ParsePattern(minIndent);

            ConsumeAllTrivia();

            var equalToken = Consume(TokenType.Arrow);

            ConsumeAllTrivia();

            var expression = ParseExpression(minIndent);

            var caseRange =
                new Range(pattern.Range.Start, expression.Range.End);

            var caseBranch =
                new Case(pattern, expression);

            return
                new Node<Case>(
                    caseRange,
                    caseBranch);
        }

        private Node<Pattern> ParsePattern(
            int minIndent)
        {
            Token start = Peek;

            if (start.Type is TokenType.Identifier)
            {
                var identifierToken = ConsumeAnyIdentifier("pattern identifier");

                if (char.IsLower(identifierToken.Lexeme[0]))
                {
                    if (identifierToken.Lexeme is "_")
                    {
                        // | AllPattern
                        return new Node<Pattern>(
                            new Range(identifierToken.Start, identifierToken.End),
                            new Pattern.AllPattern());
                    }

                    // | VarPattern String
                    return new Node<Pattern>(
                        new Range(identifierToken.Start, identifierToken.End),
                        new Pattern.VarPattern(identifierToken.Lexeme));
                }

                /*
                 * | NamedPattern QualifiedNameRef (List (Node Pattern))
                 * 
                 * like
                 * "Nothing"
                 * or
                 * "Just (Node Pattern)"
                 * or
                 * "Just just"
                 * */

                var namespaces = new List<Token>();

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var namespaceToken = ConsumeAnyIdentifier("namespace item");

                    namespaces.Add(namespaceToken);
                }

                var patternNameToken =
                    namespaces.LastOrDefault() ?? identifierToken;

                ConsumeAllTrivia();

                var patternArguments = new List<Node<Pattern>>();

                while (
                    !IsAtEnd() &&
                    patternNameToken.Start.Column <= Peek.Start.Column &&
                    Peek.Type is not TokenType.Comma &&
                    Peek.Type is not TokenType.Arrow &&
                    Peek.Type is not TokenType.CloseParen &&
                    Peek.Type is not TokenType.CloseBracket &&
                    Peek.Type is not TokenType.CloseBrace)
                {
                    var patternArgument = ParsePattern(minIndent);

                    patternArguments.Add(patternArgument);

                    ConsumeAllTrivia();
                }

                var patternRangeEnd =
                    patternArguments.Count is 0
                    ?
                    patternNameToken.End
                    :
                    patternArguments.Last().Range.End;

                var patternRange =
                    new Range(start.Start, patternRangeEnd);

                var namedPattern =
                    new Pattern.NamedPattern(
                        new QualifiedNameRef(
                            [.. namespaces.Select(t => t.Lexeme)],
                            patternNameToken.Lexeme),
                        patternArguments);

                return
                    new Node<Pattern>(
                        patternRange,
                        namedPattern);
            }

            if (start.Type is TokenType.OpenParen)
            {
                // | TupledPattern (List (Node Pattern))

                /*
                 * Example:
                 * ( a, b )
                 * */

                var parenOpenToken =
                    Consume(TokenType.OpenParen);

                ConsumeAllTrivia();

                var firstPattern = ParsePattern(minIndent);

                ConsumeAllTrivia();

                var furtherPatterns = new List<Node<Pattern>>();

                while (Peek.Type is TokenType.Comma)
                {
                    Consume(TokenType.Comma);

                    ConsumeAllTrivia();

                    var furtherPattern = ParsePattern(minIndent);

                    furtherPatterns.Add(furtherPattern);

                    ConsumeAllTrivia();
                }

                var parenCloseToken =
                    Consume(TokenType.CloseParen);

                var parenRange =
                    new Range(parenOpenToken.Start, parenCloseToken.End);

                if (furtherPatterns.Count is 0)
                {
                    return firstPattern;
                }

                var tupledPattern =
                    new Pattern.TuplePattern(
                        [firstPattern, .. furtherPatterns]);

                return
                    new Node<Pattern>(
                        parenRange,
                        tupledPattern);
            }

            throw ExceptionForCurrentLocation(
                "Unsupported pattern type: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private Node<Expression> ParseRecordExpr(
            int indentMin)
        {
            Token start = Peek;

            Consume(TokenType.OpenBrace);

            var fields = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

            ConsumeAllTrivia();

            while (Peek.Type is not TokenType.CloseBrace)
            {
                ConsumeAllTrivia();

                var fieldName = ConsumeAnyIdentifier("field name");

                ConsumeAllTrivia();

                Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var valueExpr = ParseExpression(indentMin);

                fields.Add(
                    new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                    new Range(fieldName.Start, valueExpr.Range.End),
                    (new Node<string>(new Range(fieldName.Start, fieldName.End), fieldName.Lexeme), valueExpr)));

                ConsumeAllTrivia();

                if (Peek.Type is TokenType.Comma)
                {
                    Consume(TokenType.Comma);
                }
            }

            var end = Peek;

            Consume(TokenType.CloseBrace);

            return new Node<Expression>(
                new Range(start.Start, end.End),
                new Expression.RecordExpr(fields));
        }

        // Helper methods

        private bool IsAtEnd() =>
            !EnumerateFollowingTokens().Any();

        private Token Peek =>
            EnumerateFollowingTokens().First();

        private Token Advance() =>
            tokens.Span[_current++];

        private bool NextTokenMatches(Func<Token, bool> predicate)
        {
            if (IsAtEnd())
            {
                return false;
            }

            return predicate(Peek);
        }

        private IEnumerable<Token> EnumerateFollowingTokens()
        {
            var pointer = _current;

            while (pointer < tokens.Length)
            {
                yield return tokens.Span[pointer++];
            }
        }

        private Token ConsumeKeyword(string expectedLexeme)
        {
            return
                Consume(TokenType.Identifier, expectedLexeme);
        }

        private Token ConsumeAnyIdentifier(string description)
        {
            return
                Consume(
                    TokenType.Identifier,
                    expectedLexeme: null,
                    tokenDescription: description);
        }

        // Consume a token of a given type, throwing an error if the token does not match.
        private Token Consume(
            TokenType expectedType,
            string? expectedLexeme = null,
            string? tokenDescription = null)
        {
            var nextToken = Peek;

            if (nextToken.Type != expectedType)
            {
                throw new ElmSyntaxParserException(
                    "Expected " + (tokenDescription ?? "token") + " of type " + expectedType +
                    " but found " + nextToken.Type,
                    lineNumber: nextToken.Start.Row,
                    columnNumber: nextToken.Start.Column);
            }

            if (expectedLexeme is not null && nextToken.Lexeme != expectedLexeme)
            {
                var errorDescription =
                    (expectedType is TokenType.Identifier
                    ?
                    "Expected keyword '" + expectedLexeme
                    :
                    "Expected token with lexeme " + expectedLexeme) +
                    "' but found '" + nextToken.Lexeme + "'";

                throw new ElmSyntaxParserException(
                    errorDescription,
                    lineNumber: nextToken.Start.Row,
                    columnNumber: nextToken.Start.Column);
            }

            return Advance();
        }

        private ElmSyntaxParserException ExceptionForCurrentLocation(string message)
        {
            var token =
            IsAtEnd() ?
            tokens.Span[tokens.Length - 1] :
            Peek;

            return new ElmSyntaxParserException(
            message,
            lineNumber: token.Start.Row,
            columnNumber: token.Start.Column);
        }

        private IReadOnlyList<Token> ConsumeAllTrivia()
        {
            return
            [.. ConsumeWhileLazy(
                token =>
                token.Type is TokenType.Comment ||
                token.Type is TokenType.Whitespace ||
                token.Type is TokenType.Newline)
            ];
        }

        private IReadOnlyList<Token> ConsumeAllWhitespace(
            bool includingNewline)
        {
            return [.. ConsumeAllWhitespaceLazy(includingNewline)];
        }

        private IEnumerable<Token> ConsumeAllWhitespaceLazy(
            bool includingNewline)
        {
            bool tokenMatches(Token token) =>
                (token.Type is TokenType.Whitespace)
                ||
                (includingNewline && token.Type is TokenType.Newline);

            while (!IsAtEnd() && tokenMatches(Peek))
            {
                yield return Advance();
            }
        }

        private IEnumerable<Token> ConsumeWhileLazy(
            Func<Token, bool> predicate)
        {
            while (!IsAtEnd() && predicate(Peek))
            {
                yield return Advance();
            }
        }
    }

    private static Result<string, File> ParseModuleText(
        string elmModuleText)
    {
        var tokenizer = new Tokenizer(elmModuleText);

        var tokens = tokenizer.Tokenize().ToArray();

        var parser = new Parser(tokens);

        return parser.ParseFile();
    }
}
