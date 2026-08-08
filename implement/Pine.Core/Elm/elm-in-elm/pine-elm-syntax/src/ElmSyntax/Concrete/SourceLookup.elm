module ElmSyntax.Concrete.SourceLookup exposing (..)

{-| Projection of range-free structural paths (`ElmSyntax.Path`) onto the
concrete syntax tree they were derived from, and the inverse lookup from a
source location to a structural path.

This is the only place that combines a path with concrete syntax. Semantic
analysis stays range-free; a request handler resolves a path to a range only
for the results it actually returns.

Normalization rules implemented here (mirroring
`ElmSyntax.Abstract.ConvertFromConcrete`):

  - `Expression.Parenthesized` and `Pattern.ParenthesizedPattern` are skipped
    when descending into a child, so a path addresses the same node in both
    models. A resolved range therefore never includes the enclosing
    parentheses.
  - `SeparatedSyntaxList` values are flattened preserving source order.
  - Record fields are addressed by name plus occurrence index, because the
    abstract model sorts record setters by field name.
  - A path that does not match the tree resolves to `Nothing`; no range is
    invented.

-}

import ElmSyntax.Concrete.Declaration as Declaration exposing (Declaration)
import ElmSyntax.Concrete.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import ElmSyntax.Concrete.Expression as Expression exposing (Expression)
import ElmSyntax.Concrete.File as File
import ElmSyntax.Concrete.Import as Import
import ElmSyntax.Concrete.Module as Module exposing (Module)
import ElmSyntax.Concrete.Node as Node exposing (Node(..))
import ElmSyntax.Concrete.Pattern as Pattern exposing (Pattern)
import ElmSyntax.Concrete.Range as Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList exposing (SeparatedSyntaxList)
import ElmSyntax.Concrete.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import ElmSyntax.Path as Path exposing (Path, Selection(..), Step(..))


{-| A node of the concrete tree reached by a path.

Constructors exist for the syntax records that the concrete model does not wrap
in a `Node`; their range is derived from the ranges of their parts.

-}
type SyntaxNode
    = FileNode File.File
    | ModuleNode (Node Module)
    | ImportNode (Node Import.Import)
    | ModuleNameNode (Node (List String))
    | ExposeNode (Node TopLevelExpose)
    | DeclarationNode (Node Declaration)
    | SignatureNode Expression.Signature
    | FunctionImplementationNode (Node Expression.FunctionImplementation)
    | ConstructorNode (Node Declaration.ValueConstructor)
    | LetDeclarationNode (Node Expression.LetDeclaration)
    | CaseBranchNode Expression.Case
    | ExpressionNode (Node Expression)
    | PatternNode (Node Pattern)
    | TypeAnnotationNode (Node TypeAnnotation)
    | RecordSetterNode Expression.RecordExprField
    | RecordTypeFieldNode (Node TypeAnnotation.RecordField)
    | NameNode (Node String)



-- Path resolution


{-| Resolves a path against a concrete file.
-}
nodeAtPath : Path -> File.File -> Maybe SyntaxNode
nodeAtPath path file =
    nodeAtPathFrom path (FileNode file)


nodeAtPathFrom : Path -> SyntaxNode -> Maybe SyntaxNode
nodeAtPathFrom path node =
    case path of
        [] ->
            Just node

        step :: rest ->
            case childAtStep step node of
                Nothing ->
                    Nothing

                Just child ->
                    nodeAtPathFrom rest child


{-| Resolves a path and projects the requested part of the addressed node to a
source range.
-}
rangeAtPath : Path -> Selection -> File.File -> Maybe Range
rangeAtPath path selection file =
    case nodeAtPath path file of
        Nothing ->
            Nothing

        Just node ->
            rangeOfSelection selection node


{-| Source range of the addressed node, without enclosing parentheses.
-}
rangeOfNode : SyntaxNode -> Range
rangeOfNode node =
    case node of
        FileNode file ->
            Range.combine
                (Node.range file.moduleDefinition
                    :: declarationRanges file.declarations
                )

        ModuleNode (Node range _) ->
            range

        ImportNode (Node range _) ->
            range

        ModuleNameNode (Node range _) ->
            range

        ExposeNode (Node range _) ->
            range

        DeclarationNode (Node range _) ->
            range

        SignatureNode signature ->
            Range.combine
                [ Node.range signature.name
                , Node.range signature.typeAnnotation
                ]

        FunctionImplementationNode (Node range _) ->
            range

        ConstructorNode (Node range _) ->
            range

        LetDeclarationNode (Node range _) ->
            range

        CaseBranchNode caseBranch ->
            Range.combine
                [ Node.range caseBranch.pattern
                , Node.range caseBranch.expression
                ]

        ExpressionNode (Node range _) ->
            range

        PatternNode (Node range _) ->
            range

        TypeAnnotationNode (Node range _) ->
            range

        RecordSetterNode field ->
            Range.combine
                [ Node.range field.fieldName
                , Node.range field.valueExpr
                ]

        RecordTypeFieldNode (Node range _) ->
            range

        NameNode (Node range _) ->
            range


declarationRanges : List (Node Declaration) -> List Range
declarationRanges declarations =
    case declarations of
        declaration :: rest ->
            Node.range declaration :: declarationRanges rest

        [] ->
            []


{-| Projects the requested part of a node to a source range.

Returns `Nothing` when the selection does not apply to the node, for example
`SelectQualifier` on an unqualified reference or `SelectDocumentation` on a
declaration without documentation.

-}
rangeOfSelection : Selection -> SyntaxNode -> Maybe Range
rangeOfSelection selection node =
    case selection of
        SelectWhole ->
            Just (rangeOfNode node)

        SelectName ->
            nameRangeOfNode node

        SelectQualifier ->
            qualifierRangeOfNode node

        SelectDeclarationWithoutDocumentation ->
            case node of
                DeclarationNode declarationNode ->
                    Just (declarationRangeWithoutDocumentation declarationNode)

                _ ->
                    Just (expandRangeToLineStart (rangeOfNode node))

        SelectDocumentation ->
            case node of
                DeclarationNode (Node _ declaration) ->
                    case documentationOfDeclaration declaration of
                        Just (Node range _) ->
                            Just range

                        Nothing ->
                            Nothing

                _ ->
                    Nothing



-- Children


childAtStep : Step -> SyntaxNode -> Maybe SyntaxNode
childAtStep step node =
    case node of
        FileNode file ->
            case step of
                StepModuleDefinition ->
                    Just (ModuleNode file.moduleDefinition)

                StepImport index ->
                    case listItemAt index file.imports of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (ImportNode item)

                StepDeclaration index ->
                    case listItemAt index file.declarations of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (DeclarationNode item)

                _ ->
                    Nothing

        ModuleNode (Node _ moduleValue) ->
            let
                moduleName : Node (List String)
                moduleName =
                    moduleNameNodeOfModule moduleValue

                exposingList : Node Exposing
                exposingList =
                    exposingNodeOfModule moduleValue
            in
            case step of
                StepModuleName ->
                    Just (ModuleNameNode moduleName)

                StepExposingEntry index ->
                    case exposingEntryAt index (Node.value exposingList) of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (ExposeNode item)

                _ ->
                    Nothing

        ImportNode (Node _ importValue) ->
            case step of
                StepModuleName ->
                    Just (ModuleNameNode importValue.moduleName)

                StepModuleAlias ->
                    case importValue.moduleAlias of
                        Just ( _, aliasNode ) ->
                            Just (ModuleNameNode aliasNode)

                        Nothing ->
                            Nothing

                StepExposingEntry index ->
                    case importValue.exposingList of
                        Just ( _, exposingNode ) ->
                            case exposingEntryAt index (Node.value exposingNode) of
                                Nothing ->
                                    Nothing

                                Just item ->
                                    Just (ExposeNode item)

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        DeclarationNode (Node _ declaration) ->
            childOfDeclaration step declaration

        SignatureNode signature ->
            case step of
                StepTypeAnnotation ->
                    Just (TypeAnnotationNode signature.typeAnnotation)

                _ ->
                    Nothing

        FunctionImplementationNode (Node _ implementation) ->
            case step of
                StepArgument index ->
                    case listItemAt index implementation.arguments of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (PatternNode (unwrapPattern item))

                StepBody ->
                    Just (ExpressionNode (unwrapExpression implementation.expression))

                _ ->
                    Nothing

        ConstructorNode (Node _ constructor) ->
            case step of
                StepArgument index ->
                    case listItemAt index constructor.arguments of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (TypeAnnotationNode item)

                _ ->
                    Nothing

        LetDeclarationNode (Node _ letDeclaration) ->
            case letDeclaration of
                Expression.LetFunction functionStruct ->
                    childOfFunctionStruct step functionStruct

                Expression.LetDestructuring pattern _ expression ->
                    case step of
                        StepPattern ->
                            Just (PatternNode (unwrapPattern pattern))

                        StepBody ->
                            Just (ExpressionNode (unwrapExpression expression))

                        _ ->
                            Nothing

        CaseBranchNode caseBranch ->
            case step of
                StepPattern ->
                    Just (PatternNode (unwrapPattern caseBranch.pattern))

                StepBody ->
                    Just (ExpressionNode (unwrapExpression caseBranch.expression))

                _ ->
                    Nothing

        ExpressionNode (Node _ expression) ->
            childOfExpression step expression

        PatternNode (Node _ pattern) ->
            childOfPattern step pattern

        TypeAnnotationNode (Node _ typeAnnotation) ->
            childOfTypeAnnotation step typeAnnotation

        RecordSetterNode field ->
            case step of
                StepChild 0 ->
                    Just (ExpressionNode (unwrapExpression field.valueExpr))

                _ ->
                    Nothing

        RecordTypeFieldNode (Node _ field) ->
            case step of
                StepChild 0 ->
                    Just (TypeAnnotationNode field.fieldType)

                _ ->
                    Nothing

        ModuleNameNode _ ->
            Nothing

        ExposeNode _ ->
            Nothing

        NameNode _ ->
            Nothing


childOfDeclaration : Step -> Declaration -> Maybe SyntaxNode
childOfDeclaration step declaration =
    case declaration of
        Declaration.FunctionDeclaration functionStruct ->
            childOfFunctionStruct step functionStruct

        Declaration.ChoiceTypeDeclaration choiceStruct ->
            case step of
                StepConstructor index ->
                    case listItemAt index (separatedToList choiceStruct.constructors) of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (ConstructorNode item)

                _ ->
                    Nothing

        Declaration.AliasDeclaration typeAlias ->
            case step of
                StepTypeAnnotation ->
                    Just (TypeAnnotationNode typeAlias.typeAnnotation)

                _ ->
                    Nothing

        Declaration.PortDeclaration _ signature ->
            case step of
                StepSignature ->
                    Just (SignatureNode signature)

                StepTypeAnnotation ->
                    Just (TypeAnnotationNode signature.typeAnnotation)

                _ ->
                    Nothing

        Declaration.InfixDeclaration _ ->
            Nothing


childOfFunctionStruct : Step -> Expression.FunctionStruct -> Maybe SyntaxNode
childOfFunctionStruct step functionStruct =
    case step of
        StepSignature ->
            case functionStruct.signature of
                Just signatureNode ->
                    Just (SignatureNode (Node.value signatureNode))

                Nothing ->
                    Nothing

        StepImplementation ->
            Just (FunctionImplementationNode functionStruct.declaration)

        _ ->
            Nothing


childOfExpression : Step -> Expression -> Maybe SyntaxNode
childOfExpression step expression =
    case expression of
        Expression.Parenthesized inner ->
            childOfExpression step (Node.value inner)

        Expression.Negation inner ->
            expressionChildAtList step [ inner ]

        Expression.ListExpr items ->
            expressionChildAtList step (separatedToList items)

        Expression.TupledExpression items ->
            expressionChildAtList step (separatedToList items)

        Expression.IfBlock _ condition _ thenBranch _ elseBranch ->
            expressionChildAtList step [ condition, thenBranch, elseBranch ]

        Expression.Application function arguments ->
            expressionChildAtList step (function :: arguments)

        Expression.OperatorApplication _ _ left right ->
            expressionChildAtList step [ left, right ]

        Expression.RecordAccess record _ ->
            expressionChildAtList step [ record ]

        Expression.LambdaExpression lambda ->
            case step of
                StepArgument index ->
                    case listItemAt index lambda.arguments of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (PatternNode (unwrapPattern item))

                StepBody ->
                    Just (ExpressionNode (unwrapExpression lambda.expression))

                _ ->
                    Nothing

        Expression.CaseExpression caseBlock ->
            case step of
                StepChild 0 ->
                    Just (ExpressionNode (unwrapExpression caseBlock.expression))

                StepCaseBranch index ->
                    case listItemAt index caseBlock.cases of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (CaseBranchNode item)

                _ ->
                    Nothing

        Expression.LetExpression letBlock ->
            case step of
                StepLetDeclaration index ->
                    case listItemAt index letBlock.declarations of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (LetDeclarationNode item)

                StepBody ->
                    Just (ExpressionNode (unwrapExpression letBlock.expression))

                _ ->
                    Nothing

        Expression.RecordExpr fields ->
            recordSetterAtStep step (separatedToList fields)

        Expression.RecordUpdateExpression _ _ fields ->
            recordSetterAtStep step (separatedToList fields)

        _ ->
            Nothing


expressionChildAtList : Step -> List (Node Expression) -> Maybe SyntaxNode
expressionChildAtList step items =
    case step of
        StepChild index ->
            case listItemAt index items of
                Nothing ->
                    Nothing

                Just item ->
                    Just (ExpressionNode (unwrapExpression item))

        _ ->
            Nothing


recordSetterAtStep : Step -> List Expression.RecordExprField -> Maybe SyntaxNode
recordSetterAtStep step fields =
    case step of
        StepRecordField fieldName occurrence ->
            case recordSetterWithNameAt fieldName occurrence fields of
                Nothing ->
                    Nothing

                Just item ->
                    Just (RecordSetterNode item)

        _ ->
            Nothing


recordSetterWithNameAt :
    String
    -> Int
    -> List Expression.RecordExprField
    -> Maybe Expression.RecordExprField
recordSetterWithNameAt fieldName occurrence fields =
    case fields of
        field :: rest ->
            if Node.value field.fieldName == fieldName then
                if occurrence == 0 then
                    Just field

                else
                    recordSetterWithNameAt fieldName (occurrence - 1) rest

            else
                recordSetterWithNameAt fieldName occurrence rest

        [] ->
            Nothing


childOfPattern : Step -> Pattern -> Maybe SyntaxNode
childOfPattern step pattern =
    case pattern of
        Pattern.ParenthesizedPattern inner ->
            childOfPattern step (Node.value inner)

        Pattern.TuplePattern items ->
            patternChildAtList step (separatedToList items)

        Pattern.ListPattern items ->
            patternChildAtList step (separatedToList items)

        Pattern.UnConsPattern head _ tail ->
            patternChildAtList step [ head, tail ]

        Pattern.NamedPattern _ arguments ->
            patternChildAtList step arguments

        Pattern.AsPattern inner _ _ ->
            patternChildAtList step [ inner ]

        Pattern.RecordPattern fields ->
            case step of
                StepChild index ->
                    case listItemAt index (separatedToList fields) of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just (NameNode item)

                _ ->
                    Nothing

        _ ->
            Nothing


patternChildAtList : Step -> List (Node Pattern) -> Maybe SyntaxNode
patternChildAtList step items =
    case step of
        StepChild index ->
            case listItemAt index items of
                Nothing ->
                    Nothing

                Just item ->
                    Just (PatternNode (unwrapPattern item))

        _ ->
            Nothing


childOfTypeAnnotation : Step -> TypeAnnotation -> Maybe SyntaxNode
childOfTypeAnnotation step typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.Typed _ arguments ->
            typeAnnotationChildAtList step arguments

        TypeAnnotation.Tupled items ->
            typeAnnotationChildAtList step (separatedToList items)

        TypeAnnotation.FunctionTypeAnnotation left _ right ->
            typeAnnotationChildAtList step [ left, right ]

        TypeAnnotation.Record fields ->
            recordTypeFieldAtStep step (separatedToList fields)

        TypeAnnotation.GenericRecord _ _ fields ->
            recordTypeFieldAtStep step (separatedToList (Node.value fields))

        _ ->
            Nothing


typeAnnotationChildAtList : Step -> List (Node TypeAnnotation) -> Maybe SyntaxNode
typeAnnotationChildAtList step items =
    case step of
        StepChild index ->
            case listItemAt index items of
                Nothing ->
                    Nothing

                Just item ->
                    Just (TypeAnnotationNode item)

        _ ->
            Nothing


recordTypeFieldAtStep : Step -> List (Node TypeAnnotation.RecordField) -> Maybe SyntaxNode
recordTypeFieldAtStep step fields =
    case step of
        StepRecordField fieldName occurrence ->
            case recordTypeFieldWithNameAt fieldName occurrence fields of
                Nothing ->
                    Nothing

                Just item ->
                    Just (RecordTypeFieldNode item)

        _ ->
            Nothing


recordTypeFieldWithNameAt :
    String
    -> Int
    -> List (Node TypeAnnotation.RecordField)
    -> Maybe (Node TypeAnnotation.RecordField)
recordTypeFieldWithNameAt fieldName occurrence fields =
    case fields of
        ((Node _ field) as fieldNode) :: rest ->
            if Node.value field.fieldName == fieldName then
                if occurrence == 0 then
                    Just fieldNode

                else
                    recordTypeFieldWithNameAt fieldName (occurrence - 1) rest

            else
                recordTypeFieldWithNameAt fieldName occurrence rest

        [] ->
            Nothing



-- Name and qualifier subranges


{-| Range of the declared or referenced name of a node, without module
qualifier.
-}
nameRangeOfNode : SyntaxNode -> Maybe Range
nameRangeOfNode node =
    case node of
        ModuleNode (Node _ moduleValue) ->
            Just (Node.range (moduleNameNodeOfModule moduleValue))

        ImportNode (Node _ importValue) ->
            Just (Node.range importValue.moduleName)

        ModuleNameNode (Node range _) ->
            Just range

        NameNode (Node range _) ->
            Just range

        ExposeNode (Node range expose) ->
            Just (rangeOfNameAtStart range (nameOfTopLevelExpose expose))

        DeclarationNode (Node _ declaration) ->
            declarationNameRange declaration

        SignatureNode signature ->
            Just (Node.range signature.name)

        FunctionImplementationNode (Node _ implementation) ->
            Just (Node.range implementation.name)

        ConstructorNode (Node _ constructor) ->
            Just (Node.range constructor.name)

        LetDeclarationNode (Node _ letDeclaration) ->
            case letDeclaration of
                Expression.LetFunction functionStruct ->
                    Just (Node.range (Node.value functionStruct.declaration).name)

                Expression.LetDestructuring _ _ _ ->
                    Nothing

        ExpressionNode (Node range expression) ->
            case expression of
                Expression.Identifier _ name ->
                    Just (rangeOfNameAtEnd range name)

                Expression.RecordUpdateExpression recordName _ _ ->
                    Just (Node.range recordName)

                Expression.RecordAccess _ fieldName ->
                    Just (Node.range fieldName)

                Expression.RecordAccessFunction name ->
                    Just (rangeOfNameAtEnd range (String.dropLeft 1 name))

                _ ->
                    Nothing

        PatternNode (Node range pattern) ->
            case pattern of
                Pattern.VarPattern _ ->
                    Just range

                Pattern.NamedPattern qualifiedNameRef _ ->
                    Just (rangeOfNameAtStartOffset range qualifiedNameRef.moduleName qualifiedNameRef.name)

                Pattern.AsPattern _ _ nameNode ->
                    Just (Node.range nameNode)

                _ ->
                    Nothing

        TypeAnnotationNode (Node range typeAnnotation) ->
            case typeAnnotation of
                TypeAnnotation.Typed (Node typeNameRange ( _, name )) _ ->
                    Just (rangeOfNameAtEnd typeNameRange name)

                TypeAnnotation.GenericType name ->
                    Just (rangeOfNameAtStart range name)

                TypeAnnotation.GenericRecord nameNode _ _ ->
                    Just (Node.range nameNode)

                _ ->
                    Nothing

        RecordSetterNode field ->
            Just (Node.range field.fieldName)

        RecordTypeFieldNode (Node _ field) ->
            Just (Node.range field.fieldName)

        CaseBranchNode _ ->
            Nothing

        FileNode _ ->
            Nothing


{-| Range of the module qualifier tokens of a qualified reference, excluding
the trailing dot. `Nothing` for unqualified references.
-}
qualifierRangeOfNode : SyntaxNode -> Maybe Range
qualifierRangeOfNode node =
    case node of
        ExpressionNode (Node range expression) ->
            case expression of
                Expression.Identifier moduleName name ->
                    qualifierRangeBefore range moduleName name

                _ ->
                    Nothing

        PatternNode (Node range pattern) ->
            case pattern of
                Pattern.NamedPattern qualifiedNameRef _ ->
                    qualifierRangeBeforeStart range qualifiedNameRef.moduleName

                _ ->
                    Nothing

        TypeAnnotationNode (Node _ typeAnnotation) ->
            case typeAnnotation of
                TypeAnnotation.Typed (Node typeNameRange ( moduleName, name )) _ ->
                    qualifierRangeBefore typeNameRange moduleName name

                _ ->
                    Nothing

        _ ->
            Nothing


qualifierRangeBefore : Range -> List String -> String -> Maybe Range
qualifierRangeBefore range moduleName name =
    if moduleName == [] then
        Nothing

    else
        let
            nameRange : Range
            nameRange =
                rangeOfNameAtEnd range name
        in
        Just
            { start = range.start
            , end =
                { row = nameRange.start.row
                , column = nameRange.start.column - 1
                }
            }


qualifierRangeBeforeStart : Range -> List String -> Maybe Range
qualifierRangeBeforeStart range moduleName =
    if moduleName == [] then
        Nothing

    else
        Just
            { start = range.start
            , end =
                { row = range.start.row
                , column = range.start.column + String.length (String.join "." moduleName)
                }
            }


{-| Range covering the first `String.length name` characters of the given
range.
-}
rangeOfNameAtStart : Range -> String -> Range
rangeOfNameAtStart range name =
    { start = range.start
    , end =
        { row = range.start.row
        , column = range.start.column + String.length name
        }
    }


rangeOfNameAtStartOffset : Range -> List String -> String -> Range
rangeOfNameAtStartOffset range moduleName name =
    let
        offset : Int
        offset =
            if moduleName == [] then
                0

            else
                String.length (String.join "." moduleName) + 1
    in
    { start =
        { row = range.start.row
        , column = range.start.column + offset
        }
    , end =
        { row = range.start.row
        , column = range.start.column + offset + String.length name
        }
    }


{-| Range covering the last `String.length name` characters of the given range.

Qualified references are written on a single line, so the name token always
ends where the whole reference ends.

-}
rangeOfNameAtEnd : Range -> String -> Range
rangeOfNameAtEnd range name =
    { start =
        { row = range.end.row
        , column = range.end.column - String.length name
        }
    , end = range.end
    }



-- Declaration helpers


declarationNameRange : Declaration -> Maybe Range
declarationNameRange declaration =
    case declaration of
        Declaration.FunctionDeclaration functionStruct ->
            Just (Node.range (Node.value functionStruct.declaration).name)

        Declaration.ChoiceTypeDeclaration choiceStruct ->
            Just (Node.range choiceStruct.name)

        Declaration.AliasDeclaration typeAlias ->
            Just (Node.range typeAlias.name)

        Declaration.PortDeclaration _ signature ->
            Just (Node.range signature.name)

        Declaration.InfixDeclaration infix ->
            Just (Node.range infix.operator)


documentationOfDeclaration : Declaration -> Maybe (Node String)
documentationOfDeclaration declaration =
    case declaration of
        Declaration.FunctionDeclaration functionStruct ->
            functionStruct.documentation

        Declaration.ChoiceTypeDeclaration choiceStruct ->
            choiceStruct.documentation

        Declaration.AliasDeclaration typeAlias ->
            typeAlias.documentation

        _ ->
            Nothing


{-| Range of a declaration without its documentation comment, expanded to the
beginning of the line.
-}
declarationRangeWithoutDocumentation : Node Declaration -> Range
declarationRangeWithoutDocumentation (Node range declaration) =
    case documentationOfDeclaration declaration of
        Just (Node documentationRange _) ->
            { start =
                { row = documentationRange.end.row + 1
                , column = 1
                }
            , end = range.end
            }

        Nothing ->
            expandRangeToLineStart range


expandRangeToLineStart : Range -> Range
expandRangeToLineStart range =
    { start = { row = range.start.row, column = 1 }
    , end = range.end
    }



-- Cursor lookup


{-| Structural path of the most specific node containing the given location.

Returns the empty path when the location is outside every top-level construct,
which addresses the file itself.

-}
pathAtLocation : Location -> File.File -> Path
pathAtLocation location file =
    pathAtLocationFrom location (FileNode file) []


pathAtLocationFrom : Location -> SyntaxNode -> Path -> Path
pathAtLocationFrom location node pathSoFar =
    case childStepsOfNode node of
        [] ->
            pathSoFar

        steps ->
            case firstChildContaining location node steps of
                Nothing ->
                    pathSoFar

                Just ( step, child ) ->
                    pathAtLocationFrom location child (pathSoFar ++ [ step ])


firstChildContaining : Location -> SyntaxNode -> List Step -> Maybe ( Step, SyntaxNode )
firstChildContaining location node steps =
    case steps of
        [] ->
            Nothing

        step :: rest ->
            case childAtStep step node of
                Nothing ->
                    firstChildContaining location node rest

                Just child ->
                    if rangeContainsLocation (rangeOfNode child) location then
                        Just ( step, child )

                    else
                        firstChildContaining location node rest


{-| All steps that can lead to a child of the given node, in source order.
-}
childStepsOfNode : SyntaxNode -> List Step
childStepsOfNode node =
    case node of
        FileNode file ->
            StepModuleDefinition
                :: List.append
                    (importSteps 0 file.imports)
                    (declarationSteps 0 file.declarations)

        ModuleNode (Node _ moduleValue) ->
            StepModuleName
                :: exposingEntrySteps
                    0
                    (exposingEntries (Node.value (exposingNodeOfModule moduleValue)))

        ImportNode (Node _ importValue) ->
            StepModuleName
                :: StepModuleAlias
                :: (case importValue.exposingList of
                        Just ( _, exposingNode ) ->
                            exposingEntrySteps
                                0
                                (exposingEntries (Node.value exposingNode))

                        Nothing ->
                            []
                   )

        DeclarationNode (Node _ declaration) ->
            case declaration of
                Declaration.FunctionDeclaration _ ->
                    [ StepSignature, StepImplementation ]

                Declaration.ChoiceTypeDeclaration choiceStruct ->
                    constructorSteps
                        0
                        (separatedToList choiceStruct.constructors)

                Declaration.AliasDeclaration _ ->
                    [ StepTypeAnnotation ]

                Declaration.PortDeclaration _ _ ->
                    [ StepSignature ]

                Declaration.InfixDeclaration _ ->
                    []

        SignatureNode _ ->
            [ StepTypeAnnotation ]

        FunctionImplementationNode (Node _ implementation) ->
            List.append
                (argumentSteps 0 implementation.arguments)
                [ StepBody ]

        ConstructorNode (Node _ constructor) ->
            argumentSteps 0 constructor.arguments

        LetDeclarationNode (Node _ letDeclaration) ->
            case letDeclaration of
                Expression.LetFunction _ ->
                    [ StepSignature, StepImplementation ]

                Expression.LetDestructuring _ _ _ ->
                    [ StepPattern, StepBody ]

        CaseBranchNode _ ->
            [ StepPattern, StepBody ]

        ExpressionNode (Node _ expression) ->
            expressionChildSteps expression

        PatternNode (Node _ pattern) ->
            patternChildSteps pattern

        TypeAnnotationNode (Node _ typeAnnotation) ->
            typeAnnotationChildSteps typeAnnotation

        RecordSetterNode _ ->
            [ StepChild 0 ]

        RecordTypeFieldNode _ ->
            [ StepChild 0 ]

        ModuleNameNode _ ->
            []

        ExposeNode _ ->
            []

        NameNode _ ->
            []


expressionChildSteps : Expression -> List Step
expressionChildSteps expression =
    case expression of
        Expression.Parenthesized (Node _ inner) ->
            expressionChildSteps inner

        Expression.Negation _ ->
            [ StepChild 0 ]

        Expression.ListExpr items ->
            indexSteps (separatedToList items)

        Expression.TupledExpression items ->
            indexSteps (separatedToList items)

        Expression.IfBlock _ _ _ _ _ _ ->
            [ StepChild 0, StepChild 1, StepChild 2 ]

        Expression.Application function arguments ->
            indexSteps (function :: arguments)

        Expression.OperatorApplication _ _ _ _ ->
            [ StepChild 0, StepChild 1 ]

        Expression.RecordAccess _ _ ->
            [ StepChild 0 ]

        Expression.LambdaExpression lambda ->
            List.append
                (argumentSteps 0 lambda.arguments)
                [ StepBody ]

        Expression.CaseExpression caseBlock ->
            StepChild 0
                :: caseBranchSteps 0 caseBlock.cases

        Expression.LetExpression letBlock ->
            List.append
                (letDeclarationSteps 0 letBlock.declarations)
                [ StepBody ]

        Expression.RecordExpr fields ->
            recordExpressionFieldSteps (separatedToList fields)

        Expression.RecordUpdateExpression _ _ fields ->
            recordExpressionFieldSteps (separatedToList fields)

        _ ->
            []


patternChildSteps : Pattern -> List Step
patternChildSteps pattern =
    case pattern of
        Pattern.ParenthesizedPattern (Node _ inner) ->
            patternChildSteps inner

        Pattern.TuplePattern items ->
            indexSteps (separatedToList items)

        Pattern.ListPattern items ->
            indexSteps (separatedToList items)

        Pattern.UnConsPattern _ _ _ ->
            [ StepChild 0, StepChild 1 ]

        Pattern.NamedPattern _ arguments ->
            indexSteps arguments

        Pattern.AsPattern _ _ _ ->
            [ StepChild 0 ]

        Pattern.RecordPattern fields ->
            indexSteps (separatedToList fields)

        _ ->
            []


typeAnnotationChildSteps : TypeAnnotation -> List Step
typeAnnotationChildSteps typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.Typed _ arguments ->
            indexSteps arguments

        TypeAnnotation.Tupled items ->
            indexSteps (separatedToList items)

        TypeAnnotation.FunctionTypeAnnotation _ _ _ ->
            [ StepChild 0, StepChild 1 ]

        TypeAnnotation.Record fields ->
            recordTypeFieldSteps (separatedToList fields)

        TypeAnnotation.GenericRecord _ _ fields ->
            recordTypeFieldSteps (separatedToList (Node.value fields))

        _ ->
            []


indexSteps : List a -> List Step
indexSteps items =
    indexStepsFrom 0 items


indexStepsFrom : Int -> List a -> List Step
indexStepsFrom index items =
    case items of
        _ :: rest ->
            StepChild index :: indexStepsFrom (index + 1) rest

        [] ->
            []


importSteps : Int -> List a -> List Step
importSteps index items =
    case items of
        _ :: rest ->
            StepImport index :: importSteps (index + 1) rest

        [] ->
            []


declarationSteps : Int -> List a -> List Step
declarationSteps index items =
    case items of
        _ :: rest ->
            StepDeclaration index :: declarationSteps (index + 1) rest

        [] ->
            []


exposingEntrySteps : Int -> List a -> List Step
exposingEntrySteps index items =
    case items of
        _ :: rest ->
            StepExposingEntry index :: exposingEntrySteps (index + 1) rest

        [] ->
            []


constructorSteps : Int -> List a -> List Step
constructorSteps index items =
    case items of
        _ :: rest ->
            StepConstructor index :: constructorSteps (index + 1) rest

        [] ->
            []


argumentSteps : Int -> List a -> List Step
argumentSteps index items =
    case items of
        _ :: rest ->
            StepArgument index :: argumentSteps (index + 1) rest

        [] ->
            []


caseBranchSteps : Int -> List a -> List Step
caseBranchSteps index items =
    case items of
        _ :: rest ->
            StepCaseBranch index :: caseBranchSteps (index + 1) rest

        [] ->
            []


letDeclarationSteps : Int -> List a -> List Step
letDeclarationSteps index items =
    case items of
        _ :: rest ->
            StepLetDeclaration index :: letDeclarationSteps (index + 1) rest

        [] ->
            []


recordExpressionFieldSteps : List Expression.RecordExprField -> List Step
recordExpressionFieldSteps fields =
    recordExpressionFieldStepsHelp fields []


recordExpressionFieldStepsHelp :
    List Expression.RecordExprField
    -> List String
    -> List Step
recordExpressionFieldStepsHelp remaining seen =
    case remaining of
        [] ->
            []

        field :: rest ->
            let
                fieldName : String
                fieldName =
                    Node.value field.fieldName
            in
            StepRecordField fieldName (countString fieldName seen)
                :: recordExpressionFieldStepsHelp rest (fieldName :: seen)


recordTypeFieldSteps : List (Node TypeAnnotation.RecordField) -> List Step
recordTypeFieldSteps fields =
    recordTypeFieldStepsHelp fields []


recordTypeFieldStepsHelp :
    List (Node TypeAnnotation.RecordField)
    -> List String
    -> List Step
recordTypeFieldStepsHelp remaining seen =
    case remaining of
        [] ->
            []

        (Node _ field) :: rest ->
            let
                fieldName : String
                fieldName =
                    Node.value field.fieldName
            in
            StepRecordField fieldName (countString fieldName seen)
                :: recordTypeFieldStepsHelp rest (fieldName :: seen)


countString : String -> List String -> Int
countString searched items =
    case items of
        item :: rest ->
            if item == searched then
                1 + countString searched rest

            else
                countString searched rest

        [] ->
            0



-- Small helpers


rangeContainsLocation : Range -> Location -> Bool
rangeContainsLocation range location =
    case Range.compareLocations range.start location of
        GT ->
            False

        _ ->
            case Range.compareLocations location range.end of
                GT ->
                    False

                _ ->
                    True


listItemAt : Int -> List a -> Maybe a
listItemAt index items =
    if index < 0 then
        Nothing

    else
        case items of
            [] ->
                Nothing

            first :: rest ->
                if index == 0 then
                    Just first

                else
                    listItemAt (index - 1) rest


separatedToList : SeparatedSyntaxList a -> List a
separatedToList separated =
    case separated of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            first :: separatedRestToList rest


separatedRestToList : List ( a, b ) -> List b
separatedRestToList rest =
    case rest of
        ( _, item ) :: remaining ->
            item :: separatedRestToList remaining

        [] ->
            []


unwrapExpression : Node Expression -> Node Expression
unwrapExpression node =
    case node of
        Node _ (Expression.Parenthesized inner) ->
            unwrapExpression inner

        _ ->
            node


unwrapPattern : Node Pattern -> Node Pattern
unwrapPattern node =
    case node of
        Node _ (Pattern.ParenthesizedPattern inner) ->
            unwrapPattern inner

        _ ->
            node


exposingNodeOfModule : Module -> Node Exposing
exposingNodeOfModule moduleValue =
    case moduleValue of
        Module.NormalModule data ->
            data.exposingList

        Module.PortModule data ->
            data.exposingList

        Module.EffectModule data ->
            data.exposingList


moduleNameNodeOfModule : Module -> Node (List String)
moduleNameNodeOfModule moduleValue =
    case moduleValue of
        Module.NormalModule data ->
            data.moduleName

        Module.PortModule data ->
            data.moduleName

        Module.EffectModule data ->
            data.moduleName


exposingEntries : Exposing -> List (Node TopLevelExpose)
exposingEntries exposingValue =
    case exposingValue of
        Exposing.All _ ->
            []

        Exposing.Explicit _ entries _ ->
            separatedToList entries


exposingEntryAt : Int -> Exposing -> Maybe (Node TopLevelExpose)
exposingEntryAt index exposingValue =
    listItemAt index (exposingEntries exposingValue)


nameOfTopLevelExpose : TopLevelExpose -> String
nameOfTopLevelExpose expose =
    case expose of
        Exposing.InfixExpose name ->
            name

        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.TypeExpose exposedType ->
            exposedType.name
