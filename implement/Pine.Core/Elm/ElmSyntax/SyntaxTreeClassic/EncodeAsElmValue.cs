using System;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.SyntaxTreeClassic;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

internal class EncodeAsElmValue
{
    public static ElmValue EncodeFile(File file)
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
                            ? s_maybeNothingInstance
                            : ElmValue.TagInstance(
                                "Just",
                                [EncodeNode(EncodeString, moduleCommand)])),

                        ("exposingList",
                        EncodeNode(EncodeExposing, moduleData.ModuleData.ExposingList)),

                        ("moduleName",
                        EncodeNode(EncodeModuleName, moduleData.ModuleData.ModuleName)),

                        ("subscription",
                        moduleData.ModuleData.Subscription is not { } moduleSubscription
                            ? s_maybeNothingInstance
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
                    ? s_maybeNothingInstance
                    : ElmValue.TagInstance(
                        "Just",
                        [EncodeNode(EncodeExposing, import.ExposingList)])),

                ("moduleAlias",
                import.ModuleAlias is null
                    ? s_maybeNothingInstance
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
                    ? s_maybeNothingInstance
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

    private static ElmValue EncodeRange(SyntaxTreeClassic.Range range)
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
            return s_maybeNothingInstance;
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

        return s_maybeNothingInstance;
    }

    private static ElmValue EncodeInteger(int value)
    {
        return ElmValue.Integer(value);
    }

    private static readonly ElmValue s_maybeNothingInstance =
        ElmValue.TagInstance("Nothing", []);

}
