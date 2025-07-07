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

    /*
    type Module
        = NormalModule DefaultModuleData
        | PortModule DefaultModuleData
        | EffectModule EffectModuleData


    type alias DefaultModuleData =
        { moduleName : Node ModuleName
        , exposingList : Node Exposing
        }


    type alias EffectModuleData =
        { moduleName : Node ModuleName
        , exposingList : Node Exposing
        , command : Maybe (Node String)
        , subscription : Maybe (Node String)
        }

     * */

    private abstract record Module
    {
        public sealed record NormalModule(
            DefaultModuleData ModuleData)
            : Module;

        public sealed record PortModule(
            DefaultModuleData ModuleData)
            : Module;

        public sealed record EffectModule(
            EffectModuleData ModuleData)
            : Module;
    }

    private record DefaultModuleData(
        Node<ModuleName> ModuleName,
        Node<Exposing> ExposingList);

    private record EffectModuleData(
        Node<ModuleName> ModuleName,
        Node<Exposing> ExposingList,
        Node<string>? Command,
        Node<string>? Subscription);

    /*
    type Exposing
        = All Range
        | Explicit (List (Node TopLevelExpose))


    type TopLevelExpose
        = InfixExpose String
        | FunctionExpose String
        | TypeOrAliasExpose String
        | TypeExpose ExposedType


    type alias ExposedType =
        { name : String
        , open : Maybe Range
        }

    */
    private abstract record Exposing
    {
        public sealed record All(
            Range Range)
            : Exposing;

        public sealed record Explicit(
            IReadOnlyList<Node<TopLevelExpose>> Nodes)
            : Exposing;
    }

    private abstract record TopLevelExpose
    {
        public sealed record InfixExpose(
            string Name)
            : TopLevelExpose;

        public sealed record FunctionExpose(
            string Name)
            : TopLevelExpose;

        public sealed record TypeOrAliasExpose(
            string Name)
            : TopLevelExpose;

        public sealed record TypeExpose(
            ExposedType ExposedType)
            : TopLevelExpose;
    }

    private record ExposedType(
        string Name,
        Range? Open);

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

        public sealed record AliasDeclaration(
            TypeAlias TypeAlias)
            : Declaration;

        public sealed record PortDeclaration(
            Signature Signature)
            : Declaration;

        public sealed record InfixDeclaration(
            Infix Infix)
            : Declaration;
    }

    /*
    type alias Infix =
        { direction : Node InfixDirection
        , precedence : Node Int
        , operator : Node String
        , function : Node String
        }
     * */
    private record Infix(
        Node<InfixDirection> Direction,
        Node<int> Precedence,
        Node<string> Operator,
        Node<string> FunctionName);

    /*
    type alias TypeAlias =
        { documentation : Maybe (Node Documentation)
        , name : Node String
        , generics : List (Node String)
        , typeAnnotation : Node TypeAnnotation
        }
     * */
    private record TypeAlias(
        Node<string>? Documentation,
        Node<string> Name,
        IReadOnlyList<Node<string>> Generics,
        Node<TypeAnnotation> TypeAnnotation);

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
        Node<string>? Documentation,
        Node<string> Name,
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
        Node<string>? Documentation,
        Node<Signature>? Signature,
        Node<FunctionImplementation> Declaration);

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
            | ParenthesizedPattern (Node Pattern)
         * */

        public sealed record AllPattern
            : Pattern;

        public sealed record VarPattern(
            string Name)
            : Pattern;

        public sealed record UnitPattern
            : Pattern;

        public sealed record CharPattern(
            int Value)
            : Pattern;

        public sealed record StringPattern(
            string Value)
            : Pattern;

        public sealed record IntPattern(
            long Value)
            : Pattern;

        public sealed record HexPattern(
            long Value)
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

        public sealed record AsPattern(
            Node<Pattern> Pattern,
            Node<string> Name)
            : Pattern;

        public sealed record ParenthesizedPattern(
            Node<Pattern> Pattern)
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

        public sealed record UnitExpr
            : Expression;

        public sealed record Literal(
            string Value)
            : Expression;

        public sealed record CharLiteral(
            int Value)
            : Expression;

        public sealed record Integer(
            long Value)
            : Expression;

        public sealed record Hex(
            long Value)
            : Expression;

        public sealed record Floatable(
            double Value)
            : Expression;

        public sealed record Negation(
            Node<Expression> Expression)
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

        public sealed record LetExpression(
            LetBlock Value)
            : Expression;

        /*
        {-| Expression for a let block
        -}
        type alias LetBlock =
            { declarations : List (Node LetDeclaration)
            , expression : Node Expression
            }


        {-| Union type for all possible declarations in a let block
        -}
        type LetDeclaration
            = LetFunction Function
            | LetDestructuring (Node Pattern) (Node Expression)

         * */

        public sealed record LetBlock(
            IReadOnlyList<Node<LetDeclaration>> Declarations,
            Node<Expression> Expression);

        public abstract record LetDeclaration
        {
            public sealed record LetFunction(
                FunctionStruct Function)
                : LetDeclaration;

            public sealed record LetDestructuring(
                Node<Pattern> Pattern,
                Node<Expression> Expression)
                : LetDeclaration;
        }

        public sealed record RecordExpr(
            IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
            : Expression;

        public sealed record RecordAccess(
            Node<Expression> Record,
            Node<string> FieldName)
            : Expression;

        public sealed record RecordAccessFunction(
            string FunctionName)
            : Expression;

        public sealed record RecordUpdateExpression(
            Node<string> RecordName,
            IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
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
             * ----
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
            * ----
            infix right 5 (::) = cons

            * module Parser
            * ----
            infix left  5 (|=) = keeper
            infix left  6 (|.) = ignorer

            https://github.com/elm/url/blob/384b1dcf84065a500a71402ec367f3982b35093d/src/Url/Parser.elm#L49-L50
            infix right 7 (</>) = slash
            infix left  8 (<?>) = questionMark

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

                "|=" => new InfixOperatorInfo(5, InfixDirection.Left),
                "|." => new InfixOperatorInfo(6, InfixDirection.Left),

                "</>" => new InfixOperatorInfo(7, InfixDirection.Right),
                "<?>" => new InfixOperatorInfo(8, InfixDirection.Left),

                _ =>
                throw new ArgumentException("Unknown operator: " + symbol),
            };
    }

    private record Node<T>(
        Range Range,
        T Value)
    {
        public Node<TOther> Cast<TOther>()
        {
            return new Node<TOther>(Range, (TOther)(object)Value);
        }
    }

    public record Range(
        Location Start,
        Location End);

    public record Location(
        int Row,
        int Column);

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

            Module.PortModule moduleData =>
                ElmValue.TagInstance(
                    "PortModule",
                    [new ElmValue.ElmRecord(
                        [
                        ("exposingList",
                        EncodeNode(EncodeExposing, moduleData.ModuleData.ExposingList)),
                        ("moduleName",
                        EncodeNode(EncodeModuleName, moduleData.ModuleData.ModuleName)),
                        ])
                    ]),

            Module.EffectModule moduleData =>
                ElmValue.TagInstance(
                    "EffectModule",
                    [new ElmValue.ElmRecord(
                        [
                        ("command",
                        moduleData.ModuleData.Command is not { } moduleCommand
                            ? maybeNothingInstance
                            : ElmValue.TagInstance(
                                "Just",
                                [EncodeNode(EncodeString, moduleCommand)])),

                        ("exposingList",
                        EncodeNode(EncodeExposing, moduleData.ModuleData.ExposingList)),

                        ("moduleName",
                        EncodeNode(EncodeModuleName, moduleData.ModuleData.ModuleName)),

                        ("subscription",
                        moduleData.ModuleData.Subscription is not { } moduleSubscription
                            ? maybeNothingInstance
                            : ElmValue.TagInstance(
                                "Just",
                                [EncodeNode(EncodeString, moduleSubscription)])),
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

            Exposing.Explicit explicitExp =>
                ElmValue.TagInstance(
                    "Explicit",
                    [ElmValue.ListInstance(
                        [.. explicitExp.Nodes.Select(n => EncodeNode(EncodeTopLevelExpose, n))])]),

            _ =>
            throw new NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().Name),
        };
    }

    private static ElmValue EncodeTopLevelExpose(TopLevelExpose topLevelExpose)
    {
        if (topLevelExpose is TopLevelExpose.InfixExpose infixExpose)
        {
            return
                ElmValue.TagInstance(
                    "InfixExpose",
                    [ElmValue.StringInstance(infixExpose.Name)]);
        }

        if (topLevelExpose is TopLevelExpose.FunctionExpose functionExpose)
        {
            return
                ElmValue.TagInstance(
                    "FunctionExpose",
                    [ElmValue.StringInstance(functionExpose.Name)]);
        }

        if (topLevelExpose is TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose)
        {
            return
                ElmValue.TagInstance(
                    "TypeOrAliasExpose",
                    [ElmValue.StringInstance(typeOrAliasExpose.Name)]);
        }

        if (topLevelExpose is TopLevelExpose.TypeExpose typeExpose)
        {
            return
                ElmValue.TagInstance(
                    "TypeExpose",
                    [EncodeExposedType(typeExpose.ExposedType)]);
        }

        throw new NotImplementedException(
            "Unexpected variant: " + topLevelExpose.GetType());
    }

    private static ElmValue EncodeExposedType(ExposedType exposedType)
    {
        return new ElmValue.ElmRecord(
            [
                ("name",
                ElmValue.StringInstance(exposedType.Name)),

                ("open",
                EncodeMaybe(EncodeRange, exposedType.Open)),
            ]);
    }

    private static ElmValue EncodeImport(Import import)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("exposingList",
                import.ExposingList is null
                    ? maybeNothingInstance
                    : ElmValue.TagInstance(
                        "Just",
                        [EncodeNode(EncodeExposing, import.ExposingList)])),

                ("moduleAlias",
                import.ModuleAlias is null
                    ? maybeNothingInstance
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

            Declaration.InfixDeclaration infixDeclaration =>
                ElmValue.TagInstance(
                    "InfixDeclaration",
                    [EncodeInfix(infixDeclaration.Infix)]),

            Declaration.PortDeclaration portDeclaration =>
                ElmValue.TagInstance(
                    "PortDeclaration",
                    [EncodeSignature(portDeclaration.Signature)]),

            Declaration.AliasDeclaration aliasDeclaration =>
                ElmValue.TagInstance(
                    "AliasDeclaration",
                    [EncodeTypeAlias(aliasDeclaration.TypeAlias)]),

            _ =>
            throw new NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().Name),
        };
    }

    private static ElmValue EncodeInfix(Infix infix)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("direction",
                EncodeNode(EncodeInfixDirection, infix.Direction)),
                ("function",
                EncodeNode(EncodeString, infix.FunctionName)),
                ("operator",
                EncodeNode(EncodeString, infix.Operator)),
                ("precedence",
                EncodeNode(EncodeInteger, infix.Precedence)),
                ]);
    }

    private static ElmValue EncodeTypeAlias(TypeAlias typeAlias)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("documentation",
                EncodeMaybe(node => EncodeNode(EncodeString, node), typeAlias.Documentation)),

                ("generics",
                ElmValue.ListInstance(
                    [..typeAlias.Generics.Select(g => EncodeNode(EncodeString, g))])),

                ("name",
                EncodeNode(EncodeString, typeAlias.Name)),

                ("typeAnnotation",
                EncodeNode(EncodeTypeAnnotation, typeAlias.TypeAnnotation)),
                ]);
    }

    private static ElmValue EncodeInfixDirection(InfixDirection direction)
    {
        return
            ElmValue.TagInstance(
                direction.ToString(),
                []);
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
                EncodeMaybe(node => EncodeNode(EncodeString, node), type.Documentation)),

                ("generics",
                ElmValue.ListInstance(
                    [..type.Generics.Select(g => EncodeNode(EncodeString, g))])),

                ("name",
                EncodeNode(EncodeString, type.Name)),
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

            // | Tupled (List (Node TypeAnnotation))
            TypeAnnotation.Tupled tupled =>
            ElmValue.TagInstance(
                "Tupled",
                [ElmValue.ListInstance(
                    [..tupled.TypeAnnotations.Select(a => EncodeNode(EncodeTypeAnnotation, a))])]),

            // | Unit
            TypeAnnotation.Unit =>
            ElmValue.TagInstance(
                "Unit",
                []),

            // | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)
            TypeAnnotation.FunctionTypeAnnotation functionType =>
            ElmValue.TagInstance(
                "FunctionTypeAnnotation",
                [
                    EncodeNode(EncodeTypeAnnotation, functionType.ArgumentType),
                    EncodeNode(EncodeTypeAnnotation, functionType.ReturnType),
                ]),

            // | Record RecordDefinition
            TypeAnnotation.Record record =>
            ElmValue.TagInstance(
                "Record",
                [EncodeRecordDefinition(record.RecordDefinition)]),

            // | GenericRecord (Node String) (Node RecordDefinition)
            TypeAnnotation.GenericRecord genericRecord =>
            ElmValue.TagInstance(
                "GenericRecord",
                [
                    EncodeNode(EncodeString, genericRecord.GenericName),
                    EncodeNode(EncodeRecordDefinition, genericRecord.RecordDefinition),
                ]),

            _ =>
            throw new NotImplementedException(
                "Unexpected type annotation type: " + type.GetType().Name),
        };
    }

    private static ElmValue EncodeRecordDefinition(RecordDefinition record)
    {
        return
            ElmValue.ListInstance(
                [.. record.Fields.Select(f => EncodeNode(EncodeRecordField, f))]);
    }

    private static ElmValue EncodeRecordField(RecordField field)
    {
        return
            // | ( Node String, Node TypeAnnotation )
            ElmValue.ListInstance(
                [
                    EncodeNode(EncodeString, field.FieldName),
                    EncodeNode(EncodeTypeAnnotation, field.FieldType),
                ]);
    }

    private static ElmValue EncodeFunction(FunctionStruct function)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("declaration",
                EncodeNode(EncodeFunctionImplementation, function.Declaration)),

                ("documentation",
                EncodeMaybe(node => EncodeNode(EncodeString, node), function.Documentation)),

                ("signature",
                function.Signature is null
                    ? maybeNothingInstance
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
                    [ElmValue.CharInstance(value.Value)]),

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

            Pattern.RecordPattern fields =>
                ElmValue.TagInstance(
                    "RecordPattern",
                    [ElmValue.ListInstance(
                        [..fields.Fields.Select(f => EncodeNode(EncodeString, f))])]),

            Pattern.UnConsPattern unCons =>
                ElmValue.TagInstance(
                    "UnConsPattern",
                    [
                        EncodeNode(EncodePattern, unCons.Head),
                        EncodeNode(EncodePattern, unCons.Tail),
                    ]),

            Pattern.ListPattern elements =>
                ElmValue.TagInstance(
                    "ListPattern",
                    [ElmValue.ListInstance(
                        [..elements.Elements.Select(e => EncodeNode(EncodePattern, e))])]),

            Pattern.AsPattern asPattern =>
                ElmValue.TagInstance(
                    "AsPattern",
                    [
                        EncodeNode(EncodePattern, asPattern.Pattern),
                        EncodeNode(EncodeString, asPattern.Name),
                    ]),

            Pattern.ParenthesizedPattern parenthesized =>
                ElmValue.TagInstance(
                    "ParenthesizedPattern",
                    [EncodeNode(EncodePattern, parenthesized.Pattern)]),

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

            // | CharLiteral Char
            Expression.CharLiteral value =>
                ElmValue.TagInstance(
                    "CharLiteral",
                    [ElmValue.CharInstance(value.Value)]),

            // | Integer Int
            Expression.Integer value =>
                ElmValue.TagInstance(
                    "Integer",
                    [ElmValue.Integer(value.Value)]),

            // | Hex Int
            Expression.Hex value =>
                ElmValue.TagInstance(
                    "Hex",
                    [ElmValue.Integer(value.Value)]),

            // | Floatable Float
            Expression.Floatable value =>
                ElmValue.TagInstance(
                    "Floatable",
                    [ElmValue.ElmFloat.Convert(value.Value)]),

            // | Negation (Node Expression)
            Expression.Negation negation =>
                ElmValue.TagInstance(
                    "Negation",
                    [EncodeNode(EncodeExpression, negation.Expression)]),

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

            // | LetExpression LetBlock
            Expression.LetExpression letBlock =>
                ElmValue.TagInstance(
                    "LetExpression",
                    [EncodeLetBlock(letBlock.Value)]),

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

            // | RecordAccessFunction String
            Expression.RecordAccessFunction functionName =>
                ElmValue.TagInstance(
                    "RecordAccessFunction",
                    [EncodeString(functionName.FunctionName)]),

            // | RecordUpdateExpression (Node String) (List (Node RecordSetter))
            Expression.RecordUpdateExpression recordUpdate =>
                ElmValue.TagInstance(
                    "RecordUpdateExpression",
                    [
                        EncodeNode(EncodeString, recordUpdate.RecordName),
                        ElmValue.ListInstance(
                            [..recordUpdate.Fields.Select(rs => EncodeNode(EncodeRecordSetter, rs))]),
                    ]),

            _ =>
                throw new NotImplementedException(
                    "Unexpected expression type: " + expression.GetType().Name),
        };
    }

    private static ElmValue EncodeLetBlock(Expression.LetBlock letBlock)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("declarations",
                ElmValue.ListInstance(
                    [..letBlock.Declarations.Select(d => EncodeNode(EncodeLetDeclaration, d))])),
                ("expression",
                EncodeNode(EncodeExpression, letBlock.Expression)),
                ]);
    }

    private static ElmValue EncodeLetDeclaration(Expression.LetDeclaration declaration)
    {
        return declaration switch
        {
            Expression.LetDeclaration.LetFunction function =>
                ElmValue.TagInstance(
                    "LetFunction",
                    [EncodeFunction(function.Function)]),

            Expression.LetDeclaration.LetDestructuring destructuring =>
                ElmValue.TagInstance(
                    "LetDestructuring",
                    [
                        EncodeNode(EncodePattern, destructuring.Pattern),
                        EncodeNode(EncodeExpression, destructuring.Expression),
                    ]),
            _ =>
            throw new NotImplementedException(
                "Unexpected let declaration type: " + declaration.GetType().Name),
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

    private static ElmValue EncodeMaybe<JustT>(
        Func<JustT, ElmValue> encodeJust,
        Maybe<JustT> maybe)
    {
        if (maybe is Maybe<JustT>.Nothing)
        {
            return maybeNothingInstance;
        }

        if (maybe is Maybe<JustT>.Just just)
        {
            return ElmValue.TagInstance("Just", [encodeJust(just.Value)]);
        }

        throw new NotImplementedException(
            "Unexpected variant: " + maybe.GetType());
    }

    private static ElmValue EncodeMaybe<JustT>(
        Func<JustT, ElmValue> encodeJust,
        JustT? maybe)
    {
        if (maybe is { } just)
        {
            return ElmValue.TagInstance("Just", [encodeJust(just)]);
        }

        return maybeNothingInstance;
    }

    private static ElmValue EncodeInteger(int value)
    {
        return ElmValue.Integer(value);
    }

    private static readonly ElmValue maybeNothingInstance = ElmValue.TagInstance("Nothing", []);

    public record Token(
        TokenType Type,
        string Lexeme,
        Location Start,
        Location End)
    {
        public Range Range =>
            new(Start, End);
    }

    public enum TokenType
    {
        /*
         * TODO: Explore using dedicated tokens for keywords like 'case', 'of', 'let', 'in', etc.
         * This might simplify expression parsing code.
         * */

        Identifier,
        StringLiteral,
        CharLiteral,
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
        Operator,
        Negation,
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
            '|' or '>' or '<' or '.' or ':' or '%' or '^' or '&' or '*' or '/' or '-' or '+' or '=' or '?' =>
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

                if (token is not null)
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
            PeekNext(1);

        private char PeekNext(int offset) =>
            (_position + offset < _input.Length)
            ?
            _input[_position + offset]
            :
            '\0';

        private char Advance()
        {
            var current = _input[_position];
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
            var current = Peek();

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
                    var whitespace = "";

                    while (!IsAtEnd() && char.IsWhiteSpace(Peek()) && Peek() != '\n')
                    {
                        whitespace += Advance();
                    }

                    Location end = new(_line, _column);

                    return new Token(TokenType.Whitespace, whitespace, start, end);
                }
            }

            // Handle single-line comments
            if (current is '-' && PeekNext() is '-')
            {
                var comment = "";

                while (!IsAtEnd() && Peek() is not '\n')
                {
                    comment += Advance();
                }

                Location end = new(_line, _column);

                return new Token(TokenType.Comment, comment, start, end);
            }

            /*
             * Parse multi-line comments, which start with '{-' and end with '-}'
             * Multi-line comments can be nested.
             * */
            if (Peek() is '{' && PeekNext() is '-')
            {
                var comment = new StringBuilder(capacity: 100);

                comment.Append(Advance()); // Consume '{'

                comment.Append(Advance()); // Consume '-'

                var nestedLevel = 1;

                while (!IsAtEnd() && nestedLevel > 0)
                {
                    if (Peek() is '{' && PeekNext() is '-')
                    {
                        comment.Append(Advance()); // Consume '{'
                        comment.Append(Advance()); // Consume '-'
                        nestedLevel++;
                    }
                    else if (Peek() is '-' && PeekNext() is '}')
                    {
                        comment.Append(Advance()); // Consume '-'
                        comment.Append(Advance()); // Consume '}'
                        nestedLevel--;
                    }
                    else
                    {
                        comment.Append(Advance());
                    }
                }

                if (nestedLevel > 0)
                {
                    // Unterminated multi-line comment; here you might want to throw an error.
                    return new Token(TokenType.Unknown, comment.ToString(), start, new Location(_line, _column));
                }

                Location end = new(_line, _column);

                return new Token(TokenType.Comment, comment.ToString(), start, end);
            }

            // Handle string literals
            if (current is '"')
            {
                Advance(); // Consume the opening quote

                // Check if this is a triple-quoted string

                if (Peek() is '"' && PeekNext() is '"')
                {
                    Advance(); // Consume the first quote
                    Advance(); // Consume the second quote

                    var literal = ParseStringLiteral(termination: "\"\"\"");

                    if (literal is not null)
                    {
                        Location end = new(_line, _column);

                        return new Token(TokenType.StringLiteral, literal, start, end);
                    }
                }
                else
                {
                    var literal = ParseStringLiteral(termination: "\"");

                    if (literal is not null)
                    {
                        Location end = new(_line, _column);

                        return new Token(TokenType.StringLiteral, literal, start, end);
                    }
                }

                // Unterminated string literal; here you might want to throw an error.
                throw new ElmSyntaxParserException(
                    "Unterminated string literal",
                    lineNumber: _line,
                    columnNumber: _column);
            }

            // Handle character literals
            if (current is '\'' && PeekNext() is not '\'')
            {
                Advance(); // Consume the opening quote

                var literal = ParseStringLiteral(termination: "'");

                if (literal is not null)
                {
                    Location end = new(_line, _column);
                    return new Token(TokenType.CharLiteral, literal, start, end);
                }

                // Unterminated character literal; here you might want to throw an error.
                throw new ElmSyntaxParserException(
                    "Unterminated character literal",
                    lineNumber: _line,
                    columnNumber: _column);
            }

            // Handle number literals
            if (char.IsDigit(current))
            {
                var number = new StringBuilder(capacity: 16);

                var isHex = false;

                if (current is '0' && PeekNext() is 'x')
                {
                    number.Append(Advance()); // Consume '0'
                    number.Append(Advance()); // Consume 'x'

                    isHex = true;
                }

                while (!IsAtEnd())
                {
                    if (!(isHex && char.IsAsciiHexDigit(Peek()) || char.IsDigit(Peek())))
                    {
                        break;
                    }

                    number.Append(Advance());
                }

                Location end = new(_line, _column);

                return new Token(TokenType.NumberLiteral, number.ToString(), start, end);
            }

            // Handle identifiers (start with letter or underscore, then letters, digits or underscores)
            if (char.IsLetter(current) || current is '_')
            {
                var identifier = new StringBuilder(capacity: 16);

                while (!IsAtEnd() && (char.IsLetterOrDigit(Peek()) || Peek() is '_'))
                {
                    identifier.Append(Advance());
                }

                Location end = new(_line, _column);

                return new Token(TokenType.Identifier, identifier.ToString(), start, end);
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
                        var operatorToken = "." + Advance();

                        return new Token(
                            TokenType.Operator,
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
                        var operatorToken = "=" + Advance();

                        return new Token(
                            TokenType.Operator,
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
                        var operatorToken = "|" + Advance();

                        return new Token(
                            TokenType.Operator,
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

                    switch (PeekNext())
                    {
                        case ' ':
                        case '\n':
                        case '\r':
                        case '\t':
                        case ')':
                            Advance();
                            return new Token(TokenType.Operator, "-", start, new Location(_line, _column));
                    }

                    {
                        Advance();
                        return new Token(TokenType.Negation, "-", start, new Location(_line, _column));
                    }

                case '\\':
                    Advance();

                    return new Token(TokenType.Lambda, "\\", start, new Location(_line, _column));

                case ':':

                    Advance();

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        var operatorToken = ":" + Advance();

                        return new Token(
                            TokenType.Operator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    return new Token(TokenType.Colon, ":", start, new Location(_line, _column));

                case '<':
                case '>':
                default:

                    if (CharCanAppearInOperator(current))
                    {
                        var operatorToken = Advance().ToString();

                        if (CharCanAppearInOperator(Peek()))
                        {
                            operatorToken += Advance();
                        }

                        if (CharCanAppearInOperator(Peek()))
                        {
                            operatorToken += Advance();
                        }

                        return new Token(
                            TokenType.Operator,
                            operatorToken,
                            start,
                            new Location(_line, _column));
                    }

                    // For any unknown character, consume it and return an Unknown token.
                    Advance();
                    return new Token(TokenType.Unknown, current.ToString(), start, new Location(_line, _column));
            }
        }

        private string? ParseStringLiteral(string termination)
        {
            var terminationFirstChar = termination[0];

            var sb = new StringBuilder();

            while (!IsAtEnd())
            {
                if (Peek() == terminationFirstChar)
                {
                    var matchesTermination = true;

                    for (var i = 1; i < termination.Length; i++)
                    {
                        if (PeekNext(i) != termination[i])
                        {
                            matchesTermination = false;
                            break;
                        }
                    }

                    if (matchesTermination)
                    {
                        for (var i = 0; i < termination.Length; i++)
                        {
                            Advance(); // Consume the termination characters
                        }

                        return sb.ToString();
                    }
                }

                // If we see a backslash, check for an escaped character.
                if (Peek() is '\\')
                {
                    Advance(); // Consume the backslash

                    if (!IsAtEnd())
                    {
                        var escaped = Advance(); // Consume the character after the backslash
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
                        // pattern like  "\u{000C}"
                        else if (escaped is 'u' && Peek() is '{')
                        {
                            Advance(); // Consume the '{'

                            var unicode = "";

                            while (!IsAtEnd() && Peek() is not '}')
                            {
                                unicode += Advance();
                            }

                            if (!IsAtEnd())
                            {
                                Advance(); // Consume the '}'

                                var codePoint = int.Parse(unicode, System.Globalization.NumberStyles.HexNumber);

                                sb.Append(char.ConvertFromUtf32(codePoint));
                            }
                        }
                        else if (escaped is 'r')
                        {
                            sb.Append('\r'); // Carriage return escape
                        }
                        else if (escaped is 'b')
                        {
                            sb.Append('\b'); // Backspace escape
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

            return null;
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
                IReadOnlyList<Token> allComments =
                    [.. tokens.ToArray().Where(t => t.Type is TokenType.Comment)];

                IReadOnlyList<Token> docComments =
                    [.. allComments.Where(c => c.Lexeme.StartsWith("{-|"))];

                ConsumeAllTrivia();

                // Parse the module header
                var moduleDefinition = ParseModule();

                ConsumeAllTrivia();

                // Parse the imports (if any)

                var imports = new List<Node<Import>>();

                while (NextTokenMatches(t => t.Type is TokenType.Identifier && t.Lexeme is "import"))
                {
                    var import = ParseImport();

                    imports.Add(import);

                    ConsumeAllTrivia();
                }

                var declarations = new List<Node<Declaration>>();

                ConsumeAllTrivia();

                // Parse the declarations until we've consumed all tokens.
                while (!IsAtEnd())
                {
                    if (Peek.Start.Column is not 1)
                    {
                        throw ExceptionForCurrentLocation(
                            "Unexpected token '" + Peek.Lexeme + "' after parsing " +
                            declarations.Count + " declarations");
                    }

                    bool canAttachComment(Token commentToken)
                    {
                        if (declarations.Count is 0)
                        {
                            if (imports.LastOrDefault() is { } lastImport &&
                                lastImport.Range.End.Row < commentToken.End.Row)
                            {
                                return commentToken.End.Row < Peek.Start.Row;
                            }
                        }

                        return commentToken.End.Row == Peek.Start.Row - 1;
                    }

                    var docComment =
                        docComments
                        .Where(canAttachComment)
                        .LastOrDefault();

                    declarations.Add(ParseDeclaration(docComment));

                    ConsumeAllTrivia();
                }

                IReadOnlyList<Node<string>> commentsOnDeclarations =
                    [..declarations.SelectWhereNotNull(decl =>
                    decl.Value switch
                    {
                        Declaration.FunctionDeclaration functionDeclaration =>
                        functionDeclaration.Function.Documentation,

                        Declaration.CustomTypeDeclaration typeDecl =>
                        typeDecl.TypeDeclaration.Documentation,

                        Declaration.AliasDeclaration aliasDecl =>
                        aliasDecl.TypeAlias.Documentation,

                        Declaration.InfixDeclaration =>
                        null,

                        Declaration.PortDeclaration =>
                        null,

                        _ =>
                        throw new NotImplementedException(
                            "Unexpected declaration type: " + decl.GetType().Name),
                    })];

                bool commentEmittedInGlobalList(Token commentToken)
                {
                    if (commentsOnDeclarations.Any(onDecl => onDecl.Range == commentToken.Range))
                    {
                        return false;
                    }

                    return true;
                }

                IReadOnlyList<Node<string>> commentsGlobalList =
                    [.. allComments
                    .Where(commentEmittedInGlobalList)
                    .Select(token => new Node<string>(token.Range,token.Lexeme))
                    ];

                return new File(
                    moduleDefinition,
                    imports,
                    declarations,
                    Comments: commentsGlobalList);
            }
            catch (Exception ex)
            {
                return Result<string, File>.err(ex.Message);
            }
        }

        // Parses the module header and returns a Node<Module>
        private Node<Module> ParseModule()
        {
            if (NextTokenMatches(p => p.Lexeme is "effect"))
            {
                /*
                 * Example syntax:
                 * ----
                 * effect module Time where { subscription = MySub } exposing
                 * */

                var effectKeyword = ConsumeKeyword("effect");

                ConsumeAllTrivia();

                var moduleKeyword = ConsumeKeyword("module");

                ConsumeAllTrivia();

                var moduleNameParts = new List<Token>();

                var firstModuleNamePart =
                    ConsumeAnyIdentifier("module name");

                moduleNameParts.Add(firstModuleNamePart);

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

                Node<string>? command = null;
                Node<string>? subscription = null;

                if (NextTokenMatches(peek => peek.Lexeme is "where"))
                {
                    ConsumeKeyword("where");

                    ConsumeAllTrivia();

                    var recordExprNode = ParseRecordExpr(indentMin: 1);

                    if (recordExprNode.Value is not Expression.RecordExpr recordExpr)
                    {
                        throw ExceptionForCurrentLocation(
                            "Expected record expression after 'where', found: " +
                            recordExprNode.Value.GetType().Name);
                    }

                    foreach (var recordField in recordExpr.Fields)
                    {
                        if (recordField.Value.fieldName.Value is "command")
                        {
                            if (recordField.Value.valueExpr.Value is not Expression.FunctionOrValue functionOrValue)
                            {
                                throw ExceptionForCurrentLocation(
                                    "Expected function or value for 'command', found: " +
                                    recordField.Value.valueExpr.GetType().Name);
                            }

                            command = new Node<string>(
                                recordField.Value.valueExpr.Range,
                                functionOrValue.Name);
                        }

                        if (recordField.Value.fieldName.Value is "subscription")
                        {
                            if (recordField.Value.valueExpr.Value is not Expression.FunctionOrValue functionOrValue)
                            {
                                throw ExceptionForCurrentLocation(
                                    "Expected function or value for 'subscription', found: " +
                                    recordField.Value.valueExpr.GetType().Name);
                            }

                            subscription = new Node<string>(
                                recordField.Value.valueExpr.Range,
                                functionOrValue.Name);
                        }
                    }

                    ConsumeAllTrivia();
                }

                var exposingNode = ParseExposing();

                var moduleData =
                    new EffectModuleData(
                        ModuleName: moduleNameNode,
                        ExposingList: exposingNode,
                        Command: command,
                        Subscription: subscription);

                var moduleNodeValue = new Module.EffectModule(moduleData);
                var moduleNode = new Node<Module>(new Range(effectKeyword.Start, exposingNode.Range.End), moduleNodeValue);

                return moduleNode;
            }

            {
                /*
                 * Example syntax:
                 * ----
                 * module CompilationInterface.ElmMake exposing (..)
                 * */

                // Expect the "module" keyword (this could be a token with Type Identifier "module")

                var keywordToken = ConsumeKeyword("module");

                ConsumeAllTrivia();

                // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)
                var moduleNameParts = new List<Token>();

                var firstModuleNamePart =
                    ConsumeAnyIdentifier("module name");

                moduleNameParts.Add(firstModuleNamePart);

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var moduleNamePart = ConsumeAnyIdentifier("module name part");

                    moduleNameParts.Add(moduleNamePart);
                }

                // Create a Node<IReadOnlyList<string>> for the module name.
                var moduleNameNode =
                    new Node<ModuleName>(
                        new Range(firstModuleNamePart.Start, moduleNameParts.Last().End),
                        [.. moduleNameParts.Select(t => t.Lexeme)]);

                ConsumeAllTrivia();

                var exposingNode = ParseExposing();

                // Build the module data and wrap it in a Module.NormalModule.
                var moduleData = new DefaultModuleData(moduleNameNode, exposingNode);
                var moduleNodeValue = new Module.NormalModule(moduleData);

                var moduleNode = new Node<Module>(
                    new Range(keywordToken.Start, exposingNode.Range.End),
                    moduleNodeValue);

                return moduleNode;
            }
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
            import Json.Encode exposing (Value(..))
             * */

            ConsumeAllTrivia();

            var importKeyword = ConsumeKeyword("import");

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

                ConsumeAllTrivia();

                moduleAliasNode =
                    new Node<ModuleName>(
                        aliasToken.Range,
                        new[] { aliasToken.Lexeme }.AsReadOnly());
            }

            Node<Exposing>? exposingList = null;

            // Parse the optional exposing clause (e.g. "exposing (Value(..))")
            if (Peek.Type is TokenType.Identifier && Peek.Lexeme is "exposing")
            {
                exposingList = ParseExposing();

                ConsumeAllTrivia();
            }

            var importRangeEnd =
                exposingList is null
                ?
                moduleAliasNode is null
                ?
                moduleNameNode.Range.End
                :
                moduleAliasNode.Range.End
                :
                exposingList.Range.End;

            var importNode =
                new Node<Import>(
                    new Range(importKeyword.Start, importRangeEnd),
                    new Import(
                        moduleNameNode,
                        moduleAliasNode,
                        ExposingList: exposingList));

            return importNode;
        }

        private Node<Exposing> ParseExposing()
        {
            var keyword = ConsumeKeyword("exposing");

            ConsumeAllTrivia();

            var openParen = Consume(TokenType.OpenParen);

            ConsumeAllTrivia();

            if (Peek.Type is TokenType.DotDot)
            {
                var dotDotToken = Consume(TokenType.DotDot);

                ConsumeAllTrivia();

                var closeParen = Consume(TokenType.CloseParen);

                return new Node<Exposing>(
                    new Range(keyword.Start, closeParen.End),
                    new Exposing.All(
                        new Range(
                            dotDotToken.Start,
                            new Location(Row: dotDotToken.End.Row, Column: closeParen.End.Column - 1))));
            }

            {
                var explicitNodes = new List<Node<TopLevelExpose>>();

                while (Peek.Type is not TokenType.CloseParen)
                {
                    if (0 < explicitNodes.Count)
                    {
                        Consume(TokenType.Comma);

                        ConsumeAllTrivia();
                    }

                    var topLevelExposeNode = ParseTopLevelExpose();

                    explicitNodes.Add(topLevelExposeNode);

                    ConsumeAllTrivia();
                }

                var closeParen = Consume(TokenType.CloseParen);

                return new Node<Exposing>(
                    new Range(keyword.Start, closeParen.End),
                    new Exposing.Explicit(explicitNodes));
            }
        }

        Node<TopLevelExpose> ParseTopLevelExpose()
        {
            if (Peek.Type is TokenType.OpenParen)
            {
                var topLevelOpenParen = Consume(TokenType.OpenParen);

                var operatorToken = Consume(TokenType.Operator);

                var topLevelClosingParen = Consume(TokenType.CloseParen);

                return new Node<TopLevelExpose>(
                    new Range(topLevelOpenParen.Start, topLevelClosingParen.End),
                    new TopLevelExpose.InfixExpose(operatorToken.Lexeme));
            }

            if (Peek.Type is TokenType.Identifier)
            {
                var topLevelIdentifier = Consume(TokenType.Identifier);

                ConsumeAllTrivia();

                if (char.IsLower(topLevelIdentifier.Lexeme[0]))
                {
                    return new Node<TopLevelExpose>(
                        new Range(topLevelIdentifier.Start, topLevelIdentifier.End),
                        new TopLevelExpose.FunctionExpose(topLevelIdentifier.Lexeme));
                }

                if (Peek.Type is TokenType.OpenParen)
                {
                    var openParen = Consume(TokenType.OpenParen);

                    ConsumeAllTrivia();

                    var open = false;

                    if (Peek.Type is TokenType.DotDot)
                    {
                        Consume(TokenType.DotDot);

                        open = true;
                    }

                    ConsumeAllTrivia();

                    var closeParen = Consume(TokenType.CloseParen);

                    var openRange =
                        open
                        ?
                        new Range(openParen.Start, closeParen.End)
                        :
                        null;

                    return new Node<TopLevelExpose>(
                        new Range(topLevelIdentifier.Start, closeParen.End),
                        new TopLevelExpose.TypeExpose(
                            new ExposedType(
                                topLevelIdentifier.Lexeme,
                                Open: openRange)));
                }

                return new Node<TopLevelExpose>(
                    topLevelIdentifier.Range,
                    new TopLevelExpose.TypeOrAliasExpose(
                        topLevelIdentifier.Lexeme));
            }

            throw ExceptionForCurrentLocation(
                "Unexpected token in exposing list: " + Peek.Type);
        }

        /// <summary>
        /// Parse next declaration (infix, type, function, etc.)
        /// </summary>
        private Node<Declaration> ParseDeclaration(
            Token? docComment)
        {
            if (Peek.Lexeme is "infix")
            {
                var infixKeywordToken = ConsumeKeyword("infix");

                /*
                infix right 0 (<|) = apL
                infix left  0 (|>) = apR
                infix right 2 (||) = or
                 * */

                ConsumeAllTrivia();

                var infixDirectionToken = ConsumeAnyIdentifier("infix direction");

                var infixDirection =
                    infixDirectionToken.Lexeme switch
                    {
                        "left" =>
                        InfixDirection.Left,

                        "right" =>
                        InfixDirection.Right,

                        "non" =>
                        InfixDirection.Non,

                        _ =>
                        throw ExceptionForCurrentLocation(
                            "Infix direction is not a valid value: " +
                            infixDirectionToken.Lexeme),
                    };

                ConsumeAllTrivia();

                var precedenceToken = Consume(TokenType.NumberLiteral);

                if (!int.TryParse(precedenceToken.Lexeme, out var precedence))
                {
                    throw ExceptionForCurrentLocation(
                        "Infix precedence is not a number: " + precedenceToken.Lexeme);
                }

                ConsumeAllTrivia();

                var operatorOpenParen = Consume(TokenType.OpenParen);

                var operatorToken = Consume(TokenType.Operator);

                var operatorCloseParen = Consume(TokenType.CloseParen);

                ConsumeAllTrivia();

                var equalToken = Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var functionNameToken = ConsumeAnyIdentifier("function name");

                ConsumeAllTrivia();

                return
                    new Node<Declaration>(
                        new Range(infixKeywordToken.Start, functionNameToken.End),
                        new Declaration.InfixDeclaration(
                            new Infix(
                                Direction:
                                new Node<InfixDirection>(
                                    infixDirectionToken.Range,
                                    infixDirection),
                                Precedence:
                                new Node<int>(precedenceToken.Range, precedence),
                                Operator:
                                new Node<string>(
                                    new Range(operatorOpenParen.Start, operatorCloseParen.End), operatorToken.Lexeme),
                                FunctionName:
                                new Node<string>(functionNameToken.Range, functionNameToken.Lexeme))));
            }

            if (Peek.Lexeme is "type")
            {
                return ParseTypeDeclaration(docComment);
            }

            return
                ParseFunctionDeclaration(docComment)
                .Cast<Declaration>();
        }

        private Node<Declaration> ParseTypeDeclaration(Token? docComment)
        {
            var typeKeywordToken = ConsumeKeyword("type");

            /*
             * Parse type declaration, like:
             * 
            type FileTreeNode blobStructure
                = BlobNode blobStructure
                | TreeNode (List ( String, FileTreeNode blobStructure ))
             * */

            ConsumeAllTrivia();

            if (NextTokenMatches(peek => peek.Type is TokenType.Identifier && peek.Lexeme is "alias"))
            {
                // Parse type alias

                ConsumeKeyword("alias");

                ConsumeAllTrivia();

                var typeAliasToken = ConsumeAnyIdentifier("type alias");

                ConsumeAllTrivia();

                var generics = new List<Node<string>>();

                while (Peek.Type is TokenType.Identifier)
                {
                    var genericToken = ConsumeAnyIdentifier("generic type parameter");

                    generics.Add(
                        new Node<string>(
                            genericToken.Range,
                            genericToken.Lexeme));

                    ConsumeAllTrivia();
                }

                var equalToken = Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var typeAliasTypeAnnotation =
                    ParseTypeAnnotation(indentMin: 0);

                ConsumeAllTrivia();

                var rangeStart =
                    docComment is null
                    ?
                    typeKeywordToken.Start
                    :
                    docComment.Range.Start;

                var typeAliasNode =
                    new Node<Declaration>(
                        new Range(rangeStart, typeAliasTypeAnnotation.Range.End),
                        new Declaration.AliasDeclaration(
                            new TypeAlias(
                                Documentation:
                                docComment is null
                                ?
                                null
                                :
                                new Node<string>(
                                    docComment.Range,
                                    docComment.Lexeme),
                                Name:
                                new Node<string>(
                                    new Range(typeAliasToken.Start, typeAliasToken.End),
                                    typeAliasToken.Lexeme),
                                Generics: generics,
                                TypeAnnotation: typeAliasTypeAnnotation)));

                return typeAliasNode;
            }

            {
                // Parse type name

                var typeNameToken = ConsumeAnyIdentifier("type name");

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

                Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var constructors = new List<Node<ValueConstructor>>();

                while (true)
                {
                    ConsumeAllTrivia();

                    var constructorNameToken = ConsumeAnyIdentifier("constructor name");

                    ConsumeAllTrivia();

                    var constructorArguments = new List<Node<TypeAnnotation>>();

                    while (
                        NextTokenMatches(peek =>
                        constructorNameToken.Start.Column <= Peek.Start.Column &&
                        CanStartTypeAnnotation(peek)))
                    {
                        var argumentAnnotation =
                            ParseTypeAnnotationLessApplication(
                                indentMin: constructorNameToken.Start.Column);

                        constructorArguments.Add(argumentAnnotation);

                        ConsumeAllTrivia();
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

                    if (NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                    {
                        Consume(TokenType.Pipe);
                    }
                    else
                    {
                        break;
                    }
                }

                var rangeStart =
                    docComment is null
                    ?
                    typeKeywordToken.Start
                    :
                    docComment.Range.Start;

                return
                    new Node<Declaration>(
                        new Range(rangeStart, constructors.Last().Range.End),
                        new Declaration.CustomTypeDeclaration(
                            new TypeStruct(
                                Documentation:
                                docComment is null
                                ?
                                null
                                :
                                new Node<string>(
                                    new Range(docComment.Start, docComment.End),
                                    docComment.Lexeme),
                                Name:
                                new Node<string>(
                                    new Range(typeNameToken.Start, typeNameToken.End),
                                    typeNameToken.Lexeme),
                                typeParameters,
                                constructors)));
            }
        }


        private Node<Declaration.FunctionDeclaration> ParseFunctionDeclaration(
            Token? docComment)
        {
            var functionFirstNameToken = ConsumeAnyIdentifier("function first identifier");

            var functionLastNameToken = functionFirstNameToken;

            ConsumeAllTrivia();

            Node<Signature>? signature = null;

            if (Peek.Type is TokenType.Colon)
            {
                // Parse the optional signature (e.g. "toLower : String -> String")

                Consume(TokenType.Colon);

                ConsumeAllTrivia();

                var signatureTypeAnnotation =
                    ParseTypeAnnotation(indentMin: functionFirstNameToken.Start.Column);

                signature =
                    new Node<Signature>(
                        new Range(functionFirstNameToken.Start, signatureTypeAnnotation.Range.End),
                        new Signature(
                            Name:
                            new Node<string>(
                                new Range(functionFirstNameToken.Start, functionFirstNameToken.End),
                                functionFirstNameToken.Lexeme),
                            TypeAnnotation: signatureTypeAnnotation));

                ConsumeAllTrivia();

                var declNameAgain =
                    ConsumeAnyIdentifier("function name");

                if (declNameAgain.Lexeme != functionFirstNameToken.Lexeme)
                {
                    throw ExceptionForCurrentLocation(
                        "Function name does not match signature: " +
                        declNameAgain.Lexeme + " != " + functionFirstNameToken.Lexeme);
                }

                functionLastNameToken = declNameAgain;

                ConsumeAllTrivia();
            }

            var arguments = new List<Node<Pattern>>();

            while (NextTokenMatches(CanStartArgumentPattern))
            {
                var argument =
                    ParsePatternLessUncons(indentMin: functionFirstNameToken.Start.Column);

                ConsumeAllTrivia();

                arguments.Add(argument);
            }

            ConsumeAllTrivia();

            Consume(TokenType.Equal);

            ConsumeAllTrivia();

            var expression =
                ParseExpression(indentMin: functionFirstNameToken.Start.Column + 1);

            var functionImpl =
                new FunctionImplementation(
                    new Node<string>(functionLastNameToken.Range, functionFirstNameToken.Lexeme),
                    arguments,
                    expression);

            var functionStruct =
                new FunctionStruct(
                    Documentation:
                    docComment is null
                    ?
                    null
                    :
                    new Node<string>(
                        docComment.Range,
                        docComment.Lexeme),
                    Signature:
                    signature,
                    Declaration:
                    new Node<FunctionImplementation>(
                        new Range(functionLastNameToken.Start, expression.Range.End), functionImpl));

            var declaration =
                new Declaration.FunctionDeclaration(functionStruct);

            var rangeStart =
                docComment is null
                ?
                functionFirstNameToken.Start
                :
                docComment.Start;

            return new Node<Declaration.FunctionDeclaration>(
                new Range(rangeStart, expression.Range.End),
                declaration);
        }

        private Node<TypeAnnotation> ParseTypeAnnotation(int indentMin)
        {
            var lessFunction =
                ParseTypeAnnotationLessFunction(indentMin: indentMin);

            ConsumeAllTrivia();

            if (NextTokenMatches(peek => peek.Type is TokenType.Arrow))
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

        private Node<TypeAnnotation> ParseTypeAnnotationLessFunction(int indentMin)
        {
            var lessApplication = ParseTypeAnnotationLessApplication(indentMin);

            if (lessApplication.Value is TypeAnnotation.Typed typedLessApp &&
                typedLessApp.TypeArguments.Count is 0)
            {
                ConsumeAllTrivia();

                var typeArguments =
                    new List<Node<TypeAnnotation>>();

                while (
                    NextTokenMatches(peek =>
                    lessApplication.Range.Start.Column <= peek.Start.Column &&
                    indentMin <= peek.Start.Column &&
                    CanStartTypeAnnotation(peek)))
                {
                    var typeArgument =
                        ParseTypeAnnotationLessApplication(indentMin: indentMin);

                    typeArguments.Add(typeArgument);

                    ConsumeAllTrivia();
                }

                ConsumeAllTrivia();

                var range =
                    typeArguments.Count is 0
                    ?
                    lessApplication.Range
                    :
                    lessApplication.Range
                    with
                    {
                        End = typeArguments.Last().Range.End
                    };

                return
                    new Node<TypeAnnotation>(
                        range,
                        new TypeAnnotation.Typed(
                            TypeName: typedLessApp.TypeName,
                            typeArguments));
            }

            return lessApplication;
        }

        private Node<TypeAnnotation> ParseTypeAnnotationLessApplication(int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.OpenParen)
            {
                // Is either Tupled or Typed

                var openToken = Consume(TokenType.OpenParen);

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    var closeToken = Consume(TokenType.CloseParen);

                    return
                        new Node<TypeAnnotation>(
                            new Range(openToken.Start, closeToken.End),
                            new TypeAnnotation.Unit());
                }

                var firstTypeAnnotation =
                    ParseTypeAnnotation(indentMin: indentMin);

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

                    while (true)
                    {
                        var typeAnnotation =
                            ParseTypeAnnotation(indentMin: indentMin);

                        tupleItems.Add(typeAnnotation);

                        ConsumeAllTrivia();

                        if (Peek.Type is TokenType.Comma)
                        {
                            Consume(TokenType.Comma);
                            ConsumeAllTrivia();
                        }
                        else
                        {
                            break;
                        }
                    }

                    var closingToken = Consume(TokenType.CloseParen);

                    var range =
                        new Range(openToken.Start, closingToken.End);

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

            if (start.Type is TokenType.OpenBrace)
            {
                var openToken = Consume(TokenType.OpenBrace);

                ConsumeAllTrivia();

                var fields = new List<Node<RecordField>>();

                Token? genericName = null;

                while (NextTokenMatches(peek => peek.Type is not TokenType.CloseBrace))
                {
                    var fieldNameToken = ConsumeAnyIdentifier("record field name");

                    ConsumeAllTrivia();

                    if (fields.Count is 0 && genericName is null &&
                        NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                    {
                        // | GenericRecord (Node String) (Node RecordDefinition)

                        genericName = fieldNameToken;

                        Consume(TokenType.Pipe);

                        ConsumeAllTrivia();

                        continue;
                    }

                    Consume(TokenType.Colon);

                    ConsumeAllTrivia();

                    var fieldTypeAnnotation =
                        ParseTypeAnnotation(indentMin: fieldNameToken.Start.Column);

                    ConsumeAllTrivia();

                    var fieldRangeEnd =
                        fields.Count is 0
                        ?
                        fieldTypeAnnotation.Range.End
                        :
                        EnumeratePrecedingTokensBackwards().First().End;

                    var field =
                        new Node<RecordField>(
                            new Range(fieldNameToken.Start, fieldRangeEnd),
                            new RecordField(
                                new Node<string>(
                                    fieldNameToken.Range,
                                    fieldNameToken.Lexeme),
                                fieldTypeAnnotation));

                    fields.Add(field);

                    if (Peek.Type is TokenType.Comma)
                    {
                        Consume(TokenType.Comma);
                        ConsumeAllTrivia();
                    }
                    else
                    {
                        break;
                    }
                }

                var closingToken = Consume(TokenType.CloseBrace);

                var range =
                    new Range(
                        openToken.Start,
                        closingToken.End);

                if (genericName is not null)
                {
                    return
                        new Node<TypeAnnotation>(
                            range,
                            new TypeAnnotation.GenericRecord(
                                new Node<string>(
                                    genericName.Range,
                                    genericName.Lexeme),
                                new Node<RecordDefinition>(
                                    new Range(
                                        Start:
                                        genericName.Range.End
                                        with
                                        {
                                            Column = genericName.Range.End.Column + 2
                                        },
                                        End: fields.Last().Range.End),
                                    new RecordDefinition(fields))));
                }

                return
                    new Node<TypeAnnotation>(
                        range,
                        new TypeAnnotation.Record(new RecordDefinition(fields)));
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

                var instantiatedRangeStart =
                    moduleName.Count is 0
                    ?
                    typeNameToken.Start
                    :
                    moduleName[0].Start;

                var range =
                    new Range(instantiatedRangeStart, typeNameToken.End);

                return
                    new Node<TypeAnnotation>(
                        range,
                        new TypeAnnotation.Typed(
                            new Node<(ModuleName ModuleName, string Name)>(
                                new Range(instantiatedRangeStart, typeNameToken.End),
                                (
                                [.. moduleName.Select(t => t.Lexeme)],
                                typeNameToken.Lexeme)),
                            TypeArguments: []));
            }

            throw ExceptionForCurrentLocation(
                "Unsupported type annotation type: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private static bool CanStartTypeAnnotation(Token token)
        {
            return token.Type switch
            {
                TokenType.Identifier or
                TokenType.OpenParen or
                TokenType.OpenBrace =>
                true,

                _ =>
                false,
            };
        }

        private Node<Expression> ParseExpression(
            int indentMin,
            int minPrecedence = 0)
        {
            // Parse a primary expression first.
            var left = ParsePrimaryExpression(indentMin);

            ConsumeAllTrivia();

            // Loop while the next token is an infix operator.
            while (
                NextTokenMatches(peek =>
                peek.Type is TokenType.Operator &&
                InfixOperatorInfo.GetInfo(peek.Lexeme).Precedence >= minPrecedence))
            {
                var opToken = Consume(TokenType.Operator);

                var opInfo = InfixOperatorInfo.GetInfo(opToken.Lexeme);

                // Determine the next minimum precedence for the right-hand side.
                // For left-associative operators, use (precedence + 1).
                var nextMinPrecedence =
                    opInfo.Direction is InfixDirection.Left
                    ?
                    opInfo.Precedence + 1
                    :
                    opInfo.Precedence;

                ConsumeAllTrivia();

                // Recursively parse the right-hand side with the adjusted precedence.
                var right =
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
                NextTokenMatches(peek =>
                indentMin < peek.Start.Column &&
                CanStartArgumentExpression(peek)))
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

        private static bool CanStartArgumentExpression(Token token)
        {
            if (IsKeyword(token))
            {
                return false;
            }

            // You want to allow tokens that can start expressions but not tokens that are infix operators,
            // commas, closing parentheses/brackets, etc.
            return token.Type switch
            {
                TokenType.StringLiteral or
                TokenType.NumberLiteral or
                TokenType.CharLiteral or
                TokenType.Identifier or
                TokenType.OpenParen or
                TokenType.OpenBrace or
                TokenType.OpenBracket or
                TokenType.Negation or
                TokenType.Dot =>
                true,

                _ =>
                false,
            };
        }

        private Node<Expression> ParseBasicPrimaryExpression(
            int indentMin)
        {
            var lessRecordAccess =
                ParseBasicPrimaryExpressionLessRecordAccess(indentMin: indentMin);

            if (NextTokenMatches(peek => peek.Type is TokenType.Dot))
            {
                // | RecordAccess (Node Expression) (Node String)

                var lastRecordAccess = lessRecordAccess;

                while (NextTokenMatches(peek => peek.Type is TokenType.Dot))
                {
                    Consume(TokenType.Dot);

                    var recordFieldToken =
                        ConsumeAnyIdentifier("record field");

                    var recordAccessRange =
                        new Range(
                            lastRecordAccess.Range.Start,
                            recordFieldToken.End);

                    var recordAccessExpr =
                        new Expression.RecordAccess(
                            lastRecordAccess,
                            new Node<string>(
                                new Range(recordFieldToken.Start, recordFieldToken.End),
                                recordFieldToken.Lexeme));

                    lastRecordAccess =
                        new Node<Expression>(
                            recordAccessRange,
                            recordAccessExpr);
                }

                return lastRecordAccess;
            }

            return lessRecordAccess;
        }

        private Node<Expression> ParseBasicPrimaryExpressionLessRecordAccess(
            int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.StringLiteral)
            {
                var stringLiteral =
                    Consume(TokenType.StringLiteral);

                var literalExpr = new Expression.Literal(stringLiteral.Lexeme);

                return new Node<Expression>(stringLiteral.Range, literalExpr);
            }

            if (start.Type is TokenType.CharLiteral)
            {
                var charToken = Consume(TokenType.CharLiteral);

                var literalExpr =
                    new Expression.CharLiteral(char.ConvertToUtf32(charToken.Lexeme, 0));

                return new Node<Expression>(charToken.Range, literalExpr);
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
                    /*
                     * Example A:
                     * 
                        let
                            alfa =
                                13

                            beta =
                                71
                        in
                     * --------
                     * 
                     * Example B:
                     * 
                        let
                            ( itemNodeCount, itemByteCount ) =
                                countValueContent item
                        in
                     * ---------
                     * 
                     * Example C:
                     * 
                        let
                            (EvalEnvironment environmentValue) =
                                context
                        in
                     * ---------
                     *
                     * */

                    ConsumeAllTrivia();

                    var letDecls = new List<Node<Expression.LetDeclaration>>();

                    while (
                        NextTokenMatches(
                            peek =>
                            firstIdentifierToken.Range.Start.Column < peek.Start.Column &&
                            peek.Lexeme is not "in"))
                    {
                        var letDecl =
                            ParseLetDeclaration(indentMin: firstIdentifierToken.Range.Start.Column + 1);

                        letDecls.Add(letDecl);

                        ConsumeAllTrivia();
                    }

                    var letInToken = ConsumeKeyword("in");

                    ConsumeAllTrivia();

                    var letInExpr = ParseExpression(letInToken.Start.Column);

                    var letBlockRange =
                        new Range(
                            firstIdentifierToken.Start,
                            letInExpr.Range.End);

                    var letBlockExpr =
                        new Expression.LetBlock(
                            letDecls,
                            letInExpr);

                    return
                        new Node<Expression>(
                            letBlockRange,
                            new Expression.LetExpression(letBlockExpr));
                }

                if (firstIdentifierToken.Lexeme is "if")
                {
                    ConsumeAllTrivia();

                    var condition =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

                    ConsumeAllTrivia();

                    ConsumeKeyword("then");

                    ConsumeAllTrivia();

                    var thenBranch =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

                    ConsumeAllTrivia();

                    ConsumeKeyword("else");

                    ConsumeAllTrivia();

                    var elseBranch =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

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

                    var caseValue =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

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

                var identifiers = new List<Token>([firstIdentifierToken]);

                /*
                 * If last identifier does not start with uppercase, means record access starts here.
                 * Example of record access following FunctionOrValue:
                 * 
                 * Alfa.Beta.gamma.delta
                 * */
                while (
                    char.IsUpper(identifiers.Last().Lexeme[0]) &&
                    NextTokenMatches(peek => peek.Type is TokenType.Dot))
                {
                    Consume(TokenType.Dot);

                    var furtherNamePart =
                        ConsumeAnyIdentifier("function or value name part");

                    identifiers.Add(furtherNamePart);
                }

                var firstExpr =
                    new Node<Expression>(
                        new Range(
                            firstIdentifierToken.Start,
                            identifiers[^1].End),
                        new Expression.FunctionOrValue(
                            [.. identifiers.SkipLast(1).Select(t => t.Lexeme)],
                            identifiers[^1].Lexeme));

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
                    if (nextTwoTokens[0].Type is TokenType.Operator &&
                        nextTwoTokens[1].Type is TokenType.CloseParen)
                    {
                        var operatorToken = Consume(TokenType.Operator);

                        var parenCloseToken =
                            Consume(TokenType.CloseParen);

                        return
                            new Node<Expression>(
                                new Range(parenOpenToken.Start, parenCloseToken.End),
                                new Expression.PrefixOperator(operatorToken.Lexeme));

                    }
                }

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

                    var parenRange =
                        new Range(parenOpenToken.Start, parenCloseToken.End);

                    return
                        new Node<Expression>(
                            parenRange,
                            new Expression.UnitExpr());
                }

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
                Consume(TokenType.NumberLiteral);

                return
                    new Node<Expression>(
                        start.Range,
                        ParseNumber(start.Lexeme));
            }

            if (start.Type is TokenType.Negation)
            {
                var negationToken = Consume(TokenType.Negation);

                ConsumeAllTrivia();

                var negatedExpr = ParseBasicPrimaryExpression(indentMin);

                var negationRange =
                    new Range(negationToken.Start, negatedExpr.Range.End);

                return
                    new Node<Expression>(
                        negationRange,
                        new Expression.Negation(negatedExpr));
            }

            if (start.Type is TokenType.Lambda)
            {
                return
                    ParseLambdaExpression(indentMin)
                    .Cast<Expression>();
            }

            if (start.Type is TokenType.Dot)
            {
                // | RecordAccessFunction String

                var dotToken = Consume(TokenType.Dot);

                var recordFieldToken =
                    ConsumeAnyIdentifier("record field");

                var recordAccessRange =
                    new Range(dotToken.Start, recordFieldToken.End);

                var recordAccessExpr =
                    new Expression.RecordAccessFunction(
                        /*
                         * elm-syntax currently includes the dot:
                         * https://github.com/stil4m/elm-syntax/pull/188
                         * */
                        dotToken.Lexeme + recordFieldToken.Lexeme);

                return
                    new Node<Expression>(
                        recordAccessRange,
                        recordAccessExpr);
            }

            throw ExceptionForCurrentLocation(
                "Unsupported token type in expression: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private Node<Expression.LambdaExpression> ParseLambdaExpression(int indentMin)
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

            while (NextTokenMatches(CanStartArgumentPattern))
            {
                var argument =
                    ParsePatternLessUncons(indentMin: indentMin);

                ConsumeAllTrivia();

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
                new Node<Expression.LambdaExpression>(
                    lambdaRange,
                    new Expression.LambdaExpression(lambdaExpr));
        }

        private Node<Expression.LetDeclaration> ParseLetDeclaration(int indentMin)
        {
            /*
             * LetDeclaration can be either LetDestructuring or LetFunction:
             * 
                (fst, snd) = identifier

                function arg = identifier
             */

            if (Peek.Type is TokenType.Identifier)
            {
                // LetFunction

                var parsedDecl = ParseFunctionDeclaration(docComment: null);

                return
                    new Node<Expression.LetDeclaration>(
                        parsedDecl.Range,
                        new Expression.LetDeclaration.LetFunction(
                            parsedDecl.Value.Function));
            }

            {
                // LetDestructuring

                var pattern =
                    ParsePatternLessUncons(indentMin: indentMin);

                ConsumeAllTrivia();

                var equalToken = Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var expression =
                    ParseExpression(indentMin: pattern.Range.Start.Column + 1);

                var letDeclRange =
                    new Range(pattern.Range.Start, expression.Range.End);

                var letDecl =
                    new Expression.LetDeclaration.LetDestructuring(
                        pattern,
                        expression);

                return
                    new Node<Expression.LetDeclaration>(
                        letDeclRange,
                        letDecl);
            }
        }

        private static Expression ParseNumber(string expression)
        {
            if (expression.StartsWith("-0x"))
            {
                // Hexadecimal number

                var hexNumber = expression[3..];

                var abs = long.Parse(hexNumber, System.Globalization.NumberStyles.HexNumber);

                return new Expression.Hex(-abs);
            }

            if (expression.StartsWith("0x"))
            {
                // Hexadecimal number

                var hexNumber = expression[2..];

                return new Expression.Hex(
                    long.Parse(hexNumber, System.Globalization.NumberStyles.HexNumber));
            }

            var dec = long.Parse(expression);

            return new Expression.Integer(dec);
        }

        private Node<Case> ParseCaseBranch(int indentMin)
        {
            /*
             * Example:
             * 
                Nothing ->
                    [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object
             */

            var pattern = ParsePattern(indentMin);

            ConsumeAllTrivia();

            var equalToken = Consume(TokenType.Arrow);

            ConsumeAllTrivia();

            var expression = ParseExpression(indentMin);

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
            int indentMin)
        {
            var lessUncons = ParsePatternLessUncons(indentMin: indentMin);

            ConsumeAllTrivia();

            if (NextTokenMatches(peek => peek.Lexeme is "::"))
            {
                // | UnConsPattern (Node Pattern) (Node Pattern)

                var unconsSymbol = Consume(TokenType.Operator);

                ConsumeAllTrivia();

                var tailPattern = ParsePattern(indentMin: unconsSymbol.End.Column);

                ConsumeAllTrivia();

                return
                    new Node<Pattern>(
                        new Range(lessUncons.Range.Start, tailPattern.Range.End),
                        new Pattern.UnConsPattern(
                            lessUncons,
                            tailPattern));
            }

            if (NextTokenMatches(peek => peek.Lexeme is "as"))
            {
                // | NamedPattern (Node Pattern) (Node String)

                var asToken = ConsumeKeyword("as");

                ConsumeAllTrivia();

                var nameToken = ConsumeAnyIdentifier("pattern name");

                var asPattern =
                    new Pattern.AsPattern(
                        lessUncons,
                        new Node<string>(
                            new Range(nameToken.Start, nameToken.End),
                            nameToken.Lexeme));

                return
                    new Node<Pattern>(
                        new Range(lessUncons.Range.Start, nameToken.End),
                        asPattern);
            }

            if (lessUncons.Value is Pattern.NamedPattern namedLeft &&
                namedLeft.Arguments.Count is 0 &&
                NextTokenMatches(CanStartPattern))
            {
                ConsumeAllTrivia();

                var patternArguments = new List<Node<Pattern>>();

                while (
                    NextTokenMatches(peek =>
                    lessUncons.Range.Start.Column <= peek.Start.Column &&
                    CanStartPattern(peek)))
                {
                    var patternArgument = ParsePatternLessUncons(indentMin);

                    patternArguments.Add(patternArgument);

                    ConsumeAllTrivia();
                }

                if (patternArguments.Count is 0)
                {
                    return lessUncons;
                }

                var patternRangeEnd =
                    patternArguments.Last().Range.End;

                var patternRange =
                    lessUncons.Range
                    with
                    {
                        End = patternRangeEnd
                    };

                var namedPattern =
                    new Pattern.NamedPattern(
                        namedLeft.Name,
                        patternArguments);

                return
                    new Node<Pattern>(
                        patternRange,
                        namedPattern);
            }

            return lessUncons;
        }

        private Node<Pattern> ParsePatternLessUncons(
            int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.Identifier)
            {
                var identifierToken = ConsumeAnyIdentifier("pattern identifier");

                if (identifierToken.Lexeme is "_")
                {
                    // | AllPattern

                    return new Node<Pattern>(
                        identifierToken.Range,
                        new Pattern.AllPattern());
                }

                if (char.IsLower(identifierToken.Lexeme[0]))
                {
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

                var namespaces = new List<Token>([identifierToken]);

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var namespaceToken = ConsumeAnyIdentifier("namespace item");

                    namespaces.Add(namespaceToken);
                }

                var patternNameToken = namespaces.Last();

                ConsumeAllTrivia();

                var patternRange =
                    new Range(start.Start, patternNameToken.End);

                var namedPattern =
                    new Pattern.NamedPattern(
                        new QualifiedNameRef(
                            [.. namespaces.SkipLast(1).Select(t => t.Lexeme)],
                            patternNameToken.Lexeme),
                        Arguments: []);

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

                var parenOpenToken = Consume(TokenType.OpenParen);

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

                    var parenRange =
                        new Range(parenOpenToken.Start, parenCloseToken.End);

                    return
                        new Node<Pattern>(
                            parenRange,
                            new Pattern.UnitPattern());
                }

                {
                    var firstPattern = ParsePattern(indentMin);

                    ConsumeAllTrivia();

                    var furtherPatterns = new List<Node<Pattern>>();

                    while (Peek.Type is TokenType.Comma)
                    {
                        Consume(TokenType.Comma);

                        ConsumeAllTrivia();

                        var furtherPattern = ParsePattern(indentMin);

                        furtherPatterns.Add(furtherPattern);

                        ConsumeAllTrivia();
                    }

                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

                    var parenRange =
                        new Range(parenOpenToken.Start, parenCloseToken.End);

                    if (furtherPatterns.Count is 0)
                    {
                        return
                            new Node<Pattern>(
                                parenRange,
                                new Pattern.ParenthesizedPattern(firstPattern));
                    }

                    var tupledPattern =
                        new Pattern.TuplePattern(
                            [firstPattern, .. furtherPatterns]);

                    return
                        new Node<Pattern>(
                            parenRange,
                            tupledPattern);
                }
            }

            if (start.Type is TokenType.StringLiteral)
            {
                // | StringPattern String

                var literalToken = Consume(TokenType.StringLiteral);

                var stringPattern =
                    new Pattern.StringPattern(literalToken.Lexeme);

                return
                    new Node<Pattern>(
                        new Range(start.Start, literalToken.End),
                        stringPattern);
            }


            if (start.Type is TokenType.CharLiteral)
            {
                // | CharPattern Char

                var literalToken = Consume(TokenType.CharLiteral);

                var charPattern =
                    new Pattern.CharPattern(literalToken.Lexeme.Single());

                return
                    new Node<Pattern>(
                        new Range(start.Start, literalToken.End),
                        charPattern);
            }

            if (start.Type is TokenType.NumberLiteral)
            {
                if (start.Lexeme.StartsWith("0x"))
                {
                    // | HexPattern Int

                    var literalToken = Consume(TokenType.NumberLiteral);

                    var hexPattern =
                        new Pattern.HexPattern(
                            long.Parse(literalToken.Lexeme[2..], System.Globalization.NumberStyles.HexNumber));

                    return
                        new Node<Pattern>(
                            new Range(start.Start, literalToken.End),
                            hexPattern);
                }

                // | IntegerPattern Int
                if (long.TryParse(start.Lexeme, out var number))
                {
                    var literalToken = Consume(TokenType.NumberLiteral);

                    var integerPattern =
                        new Pattern.IntPattern(number);

                    return
                        new Node<Pattern>(
                            new Range(start.Start, literalToken.End),
                            integerPattern);
                }
            }

            if (start.Type is TokenType.OpenBracket)
            {
                // | ListPattern (List (Node Pattern))

                var listOpenToken =
                    Consume(TokenType.OpenBracket);

                ConsumeAllTrivia();

                var items = new List<Node<Pattern>>();

                while (NextTokenMatches(peek => peek.Type is not TokenType.CloseBracket))
                {
                    var itemPattern = ParsePattern(indentMin);

                    items.Add(itemPattern);

                    ConsumeAllTrivia();

                    if (Peek.Type is TokenType.Comma)
                    {
                        Consume(TokenType.Comma);

                        ConsumeAllTrivia();
                    }
                    else
                    {
                        break;
                    }
                }

                var listCloseToken =
                    Consume(TokenType.CloseBracket);

                var listRange =
                    new Range(listOpenToken.Start, listCloseToken.End);

                var listExpr =
                    new Pattern.ListPattern(items);

                return new Node<Pattern>(listRange, listExpr);
            }

            if (start.Type is TokenType.OpenBrace)
            {
                // | RecordPattern (List (Node String))

                var recordOpenToken = Consume(TokenType.OpenBrace);

                ConsumeAllTrivia();

                var fields = new List<Node<string>>();

                while (Peek.Type is not TokenType.CloseBrace)
                {
                    var fieldName = ConsumeAnyIdentifier("field name");

                    fields.Add(
                        new Node<string>(
                            fieldName.Range,
                            fieldName.Lexeme));

                    ConsumeAllTrivia();

                    if (Peek.Type is TokenType.Comma)
                    {
                        Consume(TokenType.Comma);
                        ConsumeAllTrivia();
                    }
                    else
                    {
                        break;
                    }
                }

                var recordCloseToken = Consume(TokenType.CloseBrace);

                var recordRange =
                    new Range(recordOpenToken.Start, recordCloseToken.End);

                var recordPattern =
                    new Pattern.RecordPattern(fields);

                return new Node<Pattern>(recordRange, recordPattern);
            }

            throw ExceptionForCurrentLocation(
                "Unsupported pattern type: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private static bool CanStartArgumentPattern(Token token)
        {
            if (token.Type is
                TokenType.StringLiteral or TokenType.NumberLiteral or TokenType.CharLiteral)
            {
                return false;
            }

            return CanStartPattern(token);
        }

        private static bool CanStartPattern(Token token)
        {
            // You want to allow tokens that can start patterns but not tokens that are infix operators,
            // commas, closing parentheses/brackets, etc.

            return token.Type switch
            {
                TokenType.StringLiteral or
                TokenType.CharLiteral or
                TokenType.NumberLiteral or
                TokenType.Identifier or
                TokenType.OpenParen or
                TokenType.OpenBrace or
                TokenType.OpenBracket =>
                true,

                _ =>
                false,
            };
        }

        private Node<Expression> ParseRecordExpr(
            int indentMin)
        {
            var start = Peek;

            Consume(TokenType.OpenBrace);

            var fields = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

            ConsumeAllTrivia();

            Token? updatedRecord = null;

            while (Peek.Type is not TokenType.CloseBrace)
            {
                ConsumeAllTrivia();

                var fieldName = ConsumeAnyIdentifier("field name");

                ConsumeAllTrivia();

                if (fields.Count is 0 && NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                {
                    // | RecordUpdateExpression (Node String) (List (Node RecordSetter))

                    updatedRecord = fieldName;

                    Consume(TokenType.Pipe);

                    ConsumeAllTrivia();

                    continue;
                }

                Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var valueExpr = ParseExpression(indentMin);

                ConsumeAllTrivia();

                var fieldRangeEnd =
                    fields.Count is 0 && updatedRecord is null
                    ?
                    valueExpr.Range.End
                    :
                    EnumeratePrecedingTokensBackwards().First().End;

                fields.Add(
                    new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                    new Range(fieldName.Start, fieldRangeEnd),
                    (new Node<string>(fieldName.Range, fieldName.Lexeme), valueExpr)));

                if (Peek.Type is TokenType.Comma)
                {
                    Consume(TokenType.Comma);
                }
                else
                {
                    break;
                }
            }

            var closeBrace = Consume(TokenType.CloseBrace);

            var range = new Range(start.Start, closeBrace.End);

            if (updatedRecord is not null)
            {
                var recordUpdateExpr =
                    new Expression.RecordUpdateExpression(
                        new Node<string>(
                            updatedRecord.Range,
                            updatedRecord.Lexeme),
                        fields);

                return new Node<Expression>(range, recordUpdateExpr);
            }

            return new Node<Expression>(
                range,
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

        private IEnumerable<Token> EnumeratePrecedingTokensBackwards()
        {
            var pointer = _current - 1;

            while (pointer >= 0)
            {
                yield return tokens.Span[pointer--];
            }
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
