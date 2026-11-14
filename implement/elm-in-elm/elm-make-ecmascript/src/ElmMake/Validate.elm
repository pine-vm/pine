module ElmMake.Validate exposing (moduleFeatures, modules)

import Dict
import ElmMake.Canonicalize as Canonicalize
import ElmMake.Effects as Effects
import ElmMake.Errors as Errors
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmSyntax.Abstract.Declaration as AbstractDeclaration
import ElmSyntax.Abstract.Exposing as AbstractExposing
import ElmSyntax.Abstract.Module as ModuleSyntax
import ElmSyntax.Concrete.Declaration exposing (Declaration(..))
import ElmSyntax.Concrete.Exposing as Exposing exposing (Exposing(..), TopLevelExpose(..))
import ElmSyntax.Concrete.Expression exposing (Expression(..), LetDeclaration(..))
import ElmSyntax.Concrete.Module as ConcreteModule
import ElmSyntax.Concrete.Node as Node exposing (Node(..))
import ElmSyntax.Concrete.Pattern exposing (Pattern(..))
import ElmSyntax.Concrete.SeparatedSyntaxList as Separated
import ElmSyntax.Concrete.TypeAnnotation as Type
import Set exposing (Set)


{-| Resolve the import closure first. Independent modules/declarations accumulate
diagnostics; dependent phases do not run after prerequisite failures.
-}
modules : Environment -> Module -> Result MakeError (List Module)
modules environment entry =
    closure environment [ entry ] Set.empty
        |> Result.andThen (\found -> Errors.collect (validateModule environment) found |> Result.map (\_ -> found))


closure : Environment -> List Module -> Set ( String, String ) -> Result MakeError (List Module)
closure environment pending seen =
    case pending of
        [] ->
            Ok []

        module_ :: rest ->
            let
                key =
                    ( module_.package, module_.name )
            in
            if Set.member key seen then
                closure environment rest seen

            else
                let
                    imports =
                        case module_.source of
                            Just source ->
                                source.imports
                                    |> List.filter (\node -> not (String.startsWith "Elm.Kernel." (String.join "." (Node.value (Node.value node).moduleName))))
                                    |> List.map
                                        (\node ->
                                            let
                                                name =
                                                    (Node.value node).moduleName
                                            in
                                            Canonicalize.resolveModule environment module_ (String.join "." (Node.value name))
                                                |> Errors.at module_.file (Node.range name)
                                        )

                            Nothing ->
                                [ Err (Errors.internal CompilerInvariant ("Missing import source metadata: " ++ module_.name)) ]

                    remaining =
                        closure environment (rest ++ List.filterMap Result.toMaybe imports) (Set.insert key seen)
                in
                case ( Errors.collect identity imports, remaining ) of
                    ( Ok _, Ok found ) ->
                        Ok (module_ :: found)

                    ( Err left, Err right ) ->
                        Err (Errors.combine left right)

                    ( Err error, _ ) ->
                        Err error

                    ( _, Err error ) ->
                        Err error


validateModule : Environment -> Module -> Result MakeError ()
validateModule environment module_ =
    let
        scope =
            Canonicalize.interfaceScope environment module_
    in
    (case module_.source of
        Just source ->
            moduleFeatures module_ |> Errors.at module_.file (Node.range source.moduleDefinition)

        Nothing ->
            moduleFeatures module_
    )
        |> Result.andThen
            (\_ ->
                case module_.source of
                    Nothing ->
                        Err (Errors.internal CompilerInvariant ("Missing source metadata for validation: " ++ module_.name))

                    Just source ->
                        let
                            exposing_ =
                                case Node.value source.moduleDefinition of
                                    ConcreteModule.NormalModule data ->
                                        data.exposingList

                                    ConcreteModule.PortModule data ->
                                        data.exposingList

                                    ConcreteModule.EffectModule data ->
                                        data.exposingList

                            imports =
                                Errors.collect
                                    (\node ->
                                        let
                                            item =
                                                Node.value node
                                        in
                                        if String.startsWith "Elm.Kernel." (String.join "." (Node.value item.moduleName)) then
                                            Ok ()

                                        else
                                            Canonicalize.resolveModule environment module_ (String.join "." (Node.value item.moduleName))
                                                |> Result.andThen
                                                    (\target ->
                                                        case item.exposingList of
                                                            Nothing ->
                                                                Ok ()

                                                            Just ( _, exposed ) ->
                                                                validateExposing module_ True target exposed
                                                    )
                                                |> Errors.at module_.file (Node.range node)
                                    )
                                    source.imports
                        in
                        validateTopBindings module_ source.declarations
                            |> Result.andThen
                                (\_ ->
                                    Errors.collect identity
                                        [ validateExposing module_ False module_ exposing_
                                        , imports |> Result.map (\_ -> ())
                                        , Errors.collect (declaration scope) source.declarations |> Result.map (\_ -> ())
                                        ]
                                        |> Result.map (\_ -> ())
                                )
            )


moduleFeatures : Module -> Result MakeError ()
moduleFeatures module_ =
    let
        reject feature =
            Err (Errors.unsupported (Location.File module_.file) feature)
    in
    case module_.syntax.moduleDefinition of
        ModuleSyntax.PortModule _ ->
            reject Ports

        ModuleSyntax.EffectModule _ ->
            if Effects.isTrusted module_ then
                Ok ()

            else
                reject EffectManagers

        ModuleSyntax.NormalModule _ ->
            if
                List.any
                    (\item ->
                        case item of
                            AbstractDeclaration.PortDeclaration _ ->
                                True

                            _ ->
                                False
                    )
                    module_.syntax.declarations
            then
                reject Ports

            else
                Ok ()


validateTopBindings : Module -> List (Node Declaration) -> Result MakeError ()
validateTopBindings module_ declarations =
    let
        names =
            declarations
                |> List.concatMap
                    (\node ->
                        case Node.value node of
                            FunctionDeclaration function ->
                                [ ( False, (Node.value function.declaration).name ) ]

                            PortDeclaration _ signature ->
                                [ ( False, signature.name ) ]

                            ChoiceTypeDeclaration choice ->
                                ( True, choice.name ) :: List.map (\constructor -> ( False, (Node.value constructor).name )) (separated choice.constructors)

                            AliasDeclaration alias ->
                                [ ( True, alias.name ) ]

                            InfixDeclaration _ ->
                                []
                    )

        check index ( typeName, node ) =
            if List.any (\( previousType, previous ) -> previousType == typeName && Node.value previous == Node.value node) (List.take index names) then
                Err (Errors.naming module_ (DuplicateDeclaration (Node.value node)))
                    |> Errors.at module_.file (Node.range node)

            else
                Ok ()
    in
    Errors.collect (\( index, item ) -> check index item) (List.indexedMap Tuple.pair names) |> Result.map (\_ -> ())


separated : Separated.SeparatedSyntaxList a -> List a
separated values =
    case values of
        Separated.Empty ->
            []

        Separated.NonEmpty first rest ->
            first :: List.map Tuple.second rest


within : Scope -> Node a -> Result MakeError b -> Result MakeError b
within scope node =
    Errors.at scope.module_.file (Node.range node)


bind : Scope -> List (Node String) -> Result MakeError Scope
bind scope bindings =
    let
        names =
            List.map Node.value bindings

        check index node =
            let
                name =
                    Node.value node
            in
            (if List.member name (List.take index names) then
                Err (Errors.naming scope.module_ (DuplicateBinding [ name ]))

             else if Dict.member name scope.locals || Canonicalize.definition scope.module_ name /= Nothing then
                Err (Errors.naming scope.module_ (ShadowedBinding name))

             else
                Ok ()
            )
                |> within scope node
    in
    Errors.collect (\( index, node ) -> check index node) (List.indexedMap Tuple.pair bindings)
        |> Result.map (\_ -> { scope | locals = List.foldl (\name -> Dict.insert name name) scope.locals names })


patternBindings : Node Pattern -> List (Node String)
patternBindings (Node range value) =
    case value of
        VarPattern name ->
            [ Node range name ]

        RecordPattern fields ->
            separated fields

        TuplePattern items ->
            List.concatMap patternBindings (separated items)

        ListPattern items ->
            List.concatMap patternBindings (separated items)

        UnConsPattern head _ tail ->
            patternBindings head ++ patternBindings tail

        NamedPattern _ items ->
            List.concatMap patternBindings items

        AsPattern inner _ name ->
            patternBindings inner ++ [ name ]

        ParenthesizedPattern inner ->
            patternBindings inner

        _ ->
            []


declaration : Scope -> Node Declaration -> Result MakeError ()
declaration scope node =
    (case Node.value node of
        FunctionDeclaration function ->
            validateSignature scope function.signature
                |> Result.andThen (\_ -> functionBody scope function)

        PortDeclaration _ signature ->
            annotation scope signature.typeAnnotation

        ChoiceTypeDeclaration choice ->
            Errors.collect (Node.value >> .arguments >> Errors.collect (annotation scope)) (separated choice.constructors) |> Result.map (\_ -> ())

        AliasDeclaration alias ->
            annotation scope alias.typeAnnotation

        InfixDeclaration infix ->
            Canonicalize.resolve scope [] (Node.value infix.function) |> Result.map (\_ -> ())
    )
        |> within scope node


validateSignature scope signature =
    case signature of
        Nothing ->
            Ok ()

        Just node ->
            annotation scope (Node.value node).typeAnnotation


functionBody scope function =
    let
        implementation =
            Node.value function.declaration
    in
    Errors.collect (pattern scope) implementation.arguments
        |> Result.andThen (\_ -> bind scope (List.concatMap patternBindings implementation.arguments))
        |> Result.andThen (\inner -> expression inner implementation.expression)


annotation : Scope -> Node Type.TypeAnnotation -> Result MakeError ()
annotation scope node =
    let
        children values =
            Errors.collect (annotation scope) values |> Result.map (\_ -> ())

        fields values =
            children (List.map (Node.value >> .fieldType) (separated values))
    in
    (case Node.value node of
        Type.Typed name arguments ->
            let
                ( qualifier, typeName ) =
                    Node.value name
            in
            Errors.collect identity
                [ Canonicalize.resolveType scope qualifier typeName |> Result.map (\_ -> ()) |> within scope name
                , children arguments
                ]
                |> Result.map (\_ -> ())

        Type.Tupled items ->
            children (separated items)

        Type.Record items ->
            fields items

        Type.GenericRecord _ _ items ->
            fields (Node.value items)

        Type.FunctionTypeAnnotation argument _ result ->
            children [ argument, result ]

        Type.GenericType _ ->
            Ok ()

        Type.Unit ->
            Ok ()
    )
        |> within scope node


pattern : Scope -> Node Pattern -> Result MakeError ()
pattern scope node =
    let
        children items =
            Errors.collect (pattern scope) items |> Result.map (\_ -> ())
    in
    (case Node.value node of
        NamedPattern name arguments ->
            Canonicalize.resolve scope name.moduleName name.name
                |> Result.andThen
                    (\( package, home, constructor ) ->
                        case Dict.get ( package, home ) scope.environment.modules |> Maybe.andThen (\module_ -> Canonicalize.definition module_ constructor) of
                            Just (Constructor arity) ->
                                if arity == List.length arguments then
                                    children arguments

                                else
                                    Err (Errors.naming scope.module_ (WrongConstructorArity constructor arity (List.length arguments)))

                            _ ->
                                Err (Errors.naming scope.module_ (NotAConstructor constructor))
                    )

        TuplePattern items ->
            children (separated items)

        ListPattern items ->
            children (separated items)

        UnConsPattern head _ tail ->
            children [ head, tail ]

        AsPattern inner _ _ ->
            pattern scope inner

        ParenthesizedPattern inner ->
            pattern scope inner

        FloatPattern _ ->
            Err (Errors.unsupported (Location.File scope.module_.file) FloatPatterns)

        _ ->
            Ok ()
    )
        |> within scope node


expression : Scope -> Node Expression -> Result MakeError ()
expression scope node =
    let
        children items =
            Errors.collect (expression scope) items |> Result.map (\_ -> ())

        record fields =
            children (List.map .valueExpr (separated fields))
    in
    (case Node.value node of
        Identifier qualifier name ->
            if List.isEmpty qualifier && Dict.member name scope.locals then
                Ok ()

            else
                Canonicalize.resolve scope qualifier name |> Result.map (\_ -> ())

        Application function arguments ->
            children (function :: arguments)

        OperatorApplication _ _ left right ->
            children [ left, right ]

        Negation value ->
            expression scope value

        IfBlock _ condition _ yes _ no ->
            children [ condition, yes, no ]

        ListExpr items ->
            children (separated items)

        TupledExpression items ->
            children (separated items)

        RecordExpr fields ->
            record fields

        RecordAccess target _ ->
            expression scope target

        Parenthesized inner ->
            expression scope inner

        RecordUpdateExpression name _ fields ->
            Errors.collect identity
                [ expression scope (Node (Node.range name) (Identifier [] (Node.value name)))
                , record fields
                ]
                |> Result.map (\_ -> ())

        LambdaExpression lambda ->
            Errors.collect (pattern scope) lambda.arguments
                |> Result.andThen (\_ -> bind scope (List.concatMap patternBindings lambda.arguments))
                |> Result.andThen (\inner -> expression inner lambda.expression)

        CaseExpression case_ ->
            Errors.collect identity
                [ expression scope case_.expression
                , Errors.collect
                    (\branch ->
                        pattern scope branch.pattern
                            |> Result.andThen (\_ -> bind scope (patternBindings branch.pattern))
                            |> Result.andThen (\inner -> expression inner branch.expression)
                    )
                    case_.cases
                    |> Result.map (\_ -> ())
                ]
                |> Result.map (\_ -> ())

        LetExpression let_ ->
            let
                bindings =
                    List.concatMap
                        (\item ->
                            case Node.value item of
                                LetFunction function ->
                                    [ (Node.value function.declaration).name ]

                                LetDestructuring target _ _ ->
                                    patternBindings target
                        )
                        let_.declarations
            in
            bind scope bindings
                |> Result.andThen
                    (\inner ->
                        Errors.collect identity
                            [ Errors.collect
                                (\item ->
                                    case Node.value item of
                                        LetFunction function ->
                                            validateSignature inner function.signature |> Result.andThen (\_ -> functionBody inner function)

                                        LetDestructuring target _ value ->
                                            pattern inner target |> Result.andThen (\_ -> expression inner value)
                                )
                                let_.declarations
                                |> Result.map (\_ -> ())
                            , expression inner let_.expression
                            ]
                            |> Result.map (\_ -> ())
                    )

        GLSLExpression _ ->
            Err (Errors.unsupported (Location.File scope.module_.file) Glsl)

        _ ->
            Ok ()
    )
        |> within scope node


validateExposing : Module -> Bool -> Module -> Node Exposing -> Result MakeError ()
validateExposing importer isImport target exposed =
    let
        primitive name =
            target.package == "elm/core"
                && List.member ( target.name, name )
                    [ ( "Basics", "Int" ), ( "Basics", "Float" ), ( "String", "String" ), ( "Char", "Char" ), ( "List", "List" ) ]

        hasType name =
            primitive name
                || List.any
                    (\item ->
                        case item of
                            AbstractDeclaration.ChoiceTypeDeclaration choice ->
                                choice.name == name

                            AbstractDeclaration.AliasDeclaration alias ->
                                alias.name == name

                            _ ->
                                False
                    )
                    target.syntax.declarations

        isChoice name =
            List.any
                (\item ->
                    case item of
                        AbstractDeclaration.ChoiceTypeDeclaration choice ->
                            choice.name == name

                        _ ->
                            False
                )
                target.syntax.declarations

        constructorsPublic name =
            let
                exposing_ =
                    case target.syntax.moduleDefinition of
                        ModuleSyntax.NormalModule data ->
                            data.exposingList

                        ModuleSyntax.PortModule data ->
                            data.exposingList

                        ModuleSyntax.EffectModule data ->
                            data.exposingList
            in
            case exposing_ of
                AbstractExposing.All ->
                    True

                AbstractExposing.Explicit names ->
                    List.any
                        (\item ->
                            case item of
                                AbstractExposing.TypeExpose value ->
                                    value.name == name && value.exposesConstructors

                                _ ->
                                    False
                        )
                        names

        validate node =
            let
                ( name, exists ) =
                    case Node.value node of
                        FunctionExpose name_ ->
                            ( name_, Canonicalize.definition target name_ /= Nothing )

                        InfixExpose name_ ->
                            ( name_
                            , List.any
                                (\item ->
                                    case item of
                                        AbstractDeclaration.InfixDeclaration infix ->
                                            infix.operator == name_

                                        _ ->
                                            False
                                )
                                target.syntax.declarations
                            )

                        TypeOrAliasExpose name_ ->
                            ( name_, hasType name_ )

                        TypeExpose value ->
                            ( value.name, hasType value.name && (value.open == Nothing || (isChoice value.name && (not isImport || constructorsPublic value.name))) )
            in
            (if exists && (not isImport || primitive name || Canonicalize.exported target name) then
                Ok ()

             else
                Err (Errors.naming importer (InvalidExposure target.name name))
            )
                |> Errors.at importer.file (Node.range node)
    in
    case Node.value exposed of
        All _ ->
            Ok ()

        Explicit _ names _ ->
            Errors.collect validate (separated names) |> Result.map (\_ -> ())
