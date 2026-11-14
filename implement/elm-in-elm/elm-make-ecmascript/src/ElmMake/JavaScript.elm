module ElmMake.JavaScript exposing (Code, compileCyclicValueFrom, compileDefinition, compileDefinitionFrom, expression, quote)

import Dict
import ElmMake.Canonicalize as Canonicalize
import ElmMake.CoreCalls as CoreCalls
import ElmMake.EmissionMode as EmissionMode
import ElmMake.Errors as Errors
import ElmMake.JavaScriptLiterals as Literals
import ElmMake.JavaScriptNames as Names
import ElmMake.JavaScriptSyntax as JS
import ElmMake.LocalDeclarations as LocalDeclarations
import ElmMake.Model exposing (..)
import ElmMake.PatternDecisions as PatternDecisions
import ElmMake.Patterns as Patterns
import ElmMake.SourceLocation as Location
import ElmSyntax.Abstract.Expression exposing (Expression(..), LetDeclaration(..))
import ElmSyntax.Abstract.Pattern exposing (Pattern(..))
import Set exposing (Set)


type alias Code =
    EmittedCode


type Body
    = Value JS.Expr
    | Block (List JS.Stmt)


type alias State =
    { next : Int, dependencies : Set Global, tailCall : Bool }


type alias Tail =
    { name : String, parameters : List ( String, String ) }


type ParameterOrder
    = PatternsBeforeBody
    | PatternsAfterBody


quote : String -> String
quote =
    Literals.string


child : Int -> Scope -> Scope
child index scope =
    { scope | origin = childOrigin index scope.origin }


applicationOrigin : SourceOrigin -> SourceOrigin -> SourceOrigin
applicationOrigin function argument =
    SourceOrigin Nothing [ function, argument ] Nothing


asExpr : Body -> JS.Expr
asExpr code =
    case code of
        Value expr ->
            expr

        Block body ->
            JS.asExpression body


asStmts : Body -> List JS.Stmt
asStmts code =
    case code of
        Value expr ->
            JS.asStatements expr

        Block body ->
            body


runList : (a -> State -> Result MakeError ( b, State )) -> List a -> State -> Result MakeError ( List b, State )
runList compile items state =
    case items of
        [] ->
            Ok ( [], state )

        first :: rest ->
            compile first state
                |> Result.andThen
                    (\( value, next ) ->
                        runList compile rest next |> Result.map (\( values, final ) -> ( value :: values, final ))
                    )


fresh : State -> ( String, State )
fresh state =
    ( "_v" ++ String.fromInt state.next, { state | next = state.next + 1 } )


dependency : Global -> State -> State
dependency global state =
    { state | dependencies = Set.insert global state.dependencies }


kernel : String -> State -> State
kernel name =
    dependency ( "elm/core", "Elm.Kernel." ++ name, "$" )


reference : Scope -> List String -> String -> State -> Result MakeError ( JS.Expr, State )
reference scope qualifier name state =
    case ( qualifier, Dict.get name scope.locals ) of
        ( [], Just local ) ->
            Ok ( JS.Ref local, state )

        _ ->
            Canonicalize.resolve scope qualifier name
                |> Result.map
                    (\global ->
                        ( if Set.member global scope.cyclicValues then
                            JS.Call (JS.Ref (cyclicName global)) []

                          else if global == ( "elm/core", "Basics", "True" ) then
                            JS.Literal "true"

                          else if global == ( "elm/core", "Basics", "False" ) then
                            JS.Literal "false"

                          else
                            case Canonicalize.constructorInfo scope.environment global of
                                Just info ->
                                    if scope.mode.flavor == Optimize && info.enumeration then
                                        JS.Literal (String.fromInt info.index)

                                    else if scope.mode.flavor == Optimize && info.arity == 1 && info.alternatives == 1 then
                                        JS.Ref (globalName ( "elm/core", "Basics", "identity" ))

                                    else
                                        JS.Ref (globalName global)

                                Nothing ->
                                    JS.Ref (globalName global)
                        , case Canonicalize.constructorInfo scope.environment global of
                            Just { arity, alternatives } ->
                                if arity == 1 && alternatives == 1 then
                                    dependency ( "elm/core", "Basics", "identity" ) (dependency global state)

                                else
                                    dependency global state

                            Nothing ->
                                dependency global state
                        )
                    )
                |> Result.andThen
                    (\( value, next ) ->
                        if value == JS.Ref (globalName ( "elm/core", "Debug", "todo" )) then
                            case originRange scope.origin of
                                Just range ->
                                    let
                                        position item =
                                            JS.Object [ ( "line", JS.Literal (String.fromInt item.row) ), ( "column", JS.Literal (String.fromInt item.column) ) ]
                                    in
                                    Ok
                                        ( JS.Call (JS.Ref "_Debug_todo")
                                            [ JS.Literal (quote scope.module_.name)
                                            , JS.Object [ ( "start", position range.start ), ( "end", position range.end ) ]
                                            ]
                                        , next
                                        )

                                Nothing ->
                                    Err (Errors.internal InvalidSourceLocation "Debug.todo requires the source range of its reference.")

                        else
                            Ok ( value, next )
                    )


expression : Scope -> Expression -> Result MakeError Code
expression scope expr =
    lower scope Nothing expr { next = 0, dependencies = Set.empty, tailCall = False }
        |> Result.map (\( body, state ) -> { text = JS.renderExpression (asExpr body), dependencies = state.dependencies })


lower : Scope -> Maybe Tail -> Expression -> State -> Result MakeError ( Body, State )
lower scope tail expr state =
    let
        literal text =
            Ok ( Value (JS.Literal text), state )

        unary op value =
            lower (child 0 scope) Nothing value state |> Result.map (\( body, next ) -> ( Value (JS.Unary op (asExpr body)), next ))
    in
    case expr of
        UnitExpr ->
            Ok
                ( Value
                    (if scope.mode.flavor == Optimize then
                        JS.Literal "0"

                     else
                        JS.Ref "_Utils_Tuple0"
                    )
                , kernel "Utils" state
                )

        StringLiteral text ->
            literal (literalSpelling scope.origin |> Maybe.withDefault (quote text))

        CharLiteral code ->
            let
                value =
                    JS.Literal (Literals.character code)
            in
            Ok
                ( Value
                    (if scope.mode.flavor == Optimize then
                        value

                     else
                        JS.Call (JS.Ref "_Utils_chr") [ value ]
                    )
                , kernel "Utils" state
                )

        IntegerLiteral number ->
            literal (String.fromInt number)

        FloatLiteral number ->
            literal (literalSpelling scope.origin |> Maybe.withDefault (String.fromFloat number))

        Negation value ->
            unary "-" value |> Result.map (\( body, next ) -> ( body, dependency ( "elm/core", "Basics", "negate" ) next ))

        Identifier qualifier name ->
            reference scope qualifier name state |> Result.map (\( value, next ) -> ( Value value, next ))

        PrefixOperator operator ->
            resolveOperator scope operator
                |> Result.map (\global -> ( Value (JS.Ref (globalName global)), dependency global state ))

        Application function arguments ->
            case function of
                PrefixOperator operator ->
                    applyOperator scope operator 1 arguments state

                _ ->
                    apply scope tail function arguments state

        OperatorApplication operator _ left right ->
            if operator == "<|" then
                applyFlattened False scope left right (dependency ( "elm/core", "Basics", "apL" ) state)

            else if operator == "|>" then
                applyFlattened True { scope | origin = applicationOrigin (childOrigin 1 scope.origin) (childOrigin 0 scope.origin) } right left (dependency ( "elm/core", "Basics", "apR" ) state)

            else
                applyOperator scope operator 0 [ left, right ] state

        IfBlock _ _ _ ->
            lowerIf scope tail expr state

        ListExpr entries ->
            lowerArguments scope 0 entries (kernel "List" state)
                |> Result.map
                    (\( values, next ) ->
                        ( Value
                            (if List.isEmpty values then
                                JS.Ref "_List_Nil"

                             else
                                JS.Call (JS.Ref "_List_fromArray") [ JS.Array values ]
                            )
                        , next
                        )
                    )

        TupledExpression entries ->
            if List.length entries < 2 || List.length entries > 3 then
                Err (Errors.unsupported (Location.File scope.module_.file) (TupleArity (List.length entries)))

            else
                lowerArguments scope 0 entries (kernel "Utils" state)
                    |> Result.map (\( values, next ) -> ( Value (JS.Call (JS.Ref ("_Utils_Tuple" ++ String.fromInt (List.length values))) values), next ))

        RecordExpr fields ->
            runList
                (\( index, field ) current ->
                    lower (child index scope) Nothing field.value current |> Result.map (\( value, next ) -> ( ( EmissionMode.field scope.mode field.fieldName, asExpr value ), next ))
                )
                (List.sortBy .fieldName fields |> List.indexedMap Tuple.pair)
                state
                |> Result.map (\( values, next ) -> ( Value (JS.Object values), next ))

        RecordAccess value field ->
            lower (child 0 scope) Nothing value state |> Result.map (\( body, next ) -> ( Value (JS.Access (asExpr body) (EmissionMode.field scope.mode field)), next ))

        RecordAccessFunction field ->
            Ok ( Value (JS.Function [ "$" ] [ JS.Return (JS.Access (JS.Ref "$") (EmissionMode.field scope.mode field)) ]), state )

        RecordUpdateExpression name fields ->
            reference scope [] name (kernel "Utils" state)
                |> Result.andThen
                    (\( target, next ) ->
                        lower scope Nothing (RecordExpr fields) next
                            |> Result.map (\( updates, final ) -> ( Value (JS.Call (JS.Ref "_Utils_update") [ target, asExpr updates ]), final ))
                    )

        LambdaExpression parameters body ->
            lowerFunction (child 0 scope) Nothing parameters body state |> Result.map (\( function, next ) -> ( Value function, next ))

        LetExpression declarations body ->
            lowerLet scope tail declarations body state

        CaseExpression value cases ->
            lowerCase scope tail value cases state

        GLSLExpression _ ->
            Err (Errors.unsupported (Location.File scope.module_.file) Glsl)


lowerIf : Scope -> Maybe Tail -> Expression -> State -> Result MakeError ( Body, State )
lowerIf scope tail expr state =
    let
        flatten current expression_ =
            case expression_ of
                IfBlock condition yes no ->
                    let
                        ( remainingBranches, final ) =
                            flatten (child 2 current) no
                    in
                    ( ( current, condition, yes ) :: remainingBranches, final )

                _ ->
                    ( [], ( current, expression_ ) )

        ( branches, ( finalScope, finalExpression ) ) =
            flatten scope expr

        isBlock body =
            case body of
                Block _ ->
                    True

                Value _ ->
                    False
    in
    runList
        (\( current, condition, yes ) next ->
            lower (child 0 current) Nothing condition next
                |> Result.andThen
                    (\( test, afterTest ) ->
                        lower (child 1 current) tail yes afterTest
                            |> Result.map (\( body, afterBody ) -> ( ( asExpr test, body ), afterBody ))
                    )
        )
        branches
        state
        |> Result.andThen
            (\( compiled, next ) ->
                lower finalScope tail finalExpression next
                    |> Result.map
                        (\( final, finished ) ->
                            ( if isBlock final || List.any (Tuple.second >> isBlock) compiled then
                                List.foldr (\( condition, body ) no -> Block [ JS.If condition (asStmts body) (asStmts no) ]) final compiled

                              else
                                List.foldr (\( condition, body ) no -> Value (JS.Conditional condition (asExpr body) (asExpr no))) final compiled
                            , finished
                            )
                        )
            )


lowerArguments : Scope -> Int -> List Expression -> State -> Result MakeError ( List JS.Expr, State )
lowerArguments scope offset arguments =
    runList
        (\( index, expr ) -> lowerArgument (child (offset + index) scope) expr)
        (List.indexedMap Tuple.pair arguments)


lowerArgument : Scope -> Expression -> State -> Result MakeError ( JS.Expr, State )
lowerArgument scope expr =
    lower scope Nothing expr >> Result.map (\( body, next ) -> ( asExpr body, next ))


lowerIndexedArguments : Scope -> Int -> List ( Int, Expression ) -> State -> Result MakeError ( List JS.Expr, State )
lowerIndexedArguments scope offset arguments =
    runList
        (\( index, expr ) state -> lowerArgument (child (offset + index) scope) expr state |> Result.map (\( value, next ) -> ( ( index, value ), next )))
        arguments
        >> Result.map (\( values, final ) -> ( List.sortBy Tuple.first values |> List.map Tuple.second, final ))


apply : Scope -> Maybe Tail -> Expression -> List Expression -> State -> Result MakeError ( Body, State )
apply =
    applyOrdered False


applyOrdered : Bool -> Scope -> Maybe Tail -> Expression -> List Expression -> State -> Result MakeError ( Body, State )
applyOrdered pipelineInputFirst scope tail function arguments state =
    let
        compileArguments =
            if pipelineInputFirst then
                case List.reverse (List.indexedMap Tuple.pair arguments) of
                    last :: reversed ->
                        lowerIndexedArguments scope 1 (last :: List.reverse reversed)

                    [] ->
                        lowerArguments scope 1 arguments

            else
                lowerArguments scope 1 arguments

        resolved =
            case function of
                Identifier qualifier name ->
                    if List.isEmpty qualifier && Dict.member name scope.locals then
                        Nothing

                    else
                        Canonicalize.resolve scope qualifier name |> Result.toMaybe

                _ ->
                    Nothing

        tailTarget =
            case ( tail, function ) of
                ( Just target, Identifier qualifier name ) ->
                    if
                        name == target.name
                            && (List.isEmpty qualifier || qualifier == String.split "." scope.module_.name)
                            && List.length arguments == List.length target.parameters
                    then
                        Just target

                    else
                        Nothing

                _ ->
                    Nothing

        unboxed =
            resolved
                |> Maybe.andThen (Canonicalize.constructorInfo scope.environment)
                |> Maybe.map (\info -> scope.mode.flavor == Optimize && info.arity == 1 && info.alternatives == 1 && List.length arguments == 1)
                |> Maybe.withDefault False
    in
    if unboxed then
        case ( arguments, resolved ) of
            ( [ argument ], Just global ) ->
                lower (child 1 scope) Nothing argument (dependency ( "elm/core", "Basics", "identity" ) (dependency global state))

            _ ->
                Err (Errors.internal CompilerInvariant "Unboxed constructor calls require one argument and a canonical identity.")

    else
        case tailTarget of
            Just target ->
                compileArguments state
                    |> Result.map
                        (\( values, next ) ->
                            let
                                temps =
                                    List.map2 (\( sourceName, _ ) value -> ( "$temp$" ++ sourceName, value )) target.parameters values
                            in
                            ( Block
                                (JS.Vars temps
                                    :: List.map (\( sourceName, emittedName ) -> JS.Assign emittedName (JS.Ref ("$temp$" ++ sourceName))) target.parameters
                                    ++ [ JS.Continue (Names.local target.name) ]
                                )
                            , { next | tailCall = True }
                            )
                        )

            Nothing ->
                case ( resolved, arguments ) of
                    ( Just ( "elm/core", "Basics", "apL" ), [ fn, arg ] ) ->
                        applyFlattened False { scope | origin = applicationOrigin (childOrigin 1 scope.origin) (childOrigin 2 scope.origin) } fn arg (dependency ( "elm/core", "Basics", "apL" ) state)

                    ( Just ( "elm/core", "Basics", "apR" ), [ arg, fn ] ) ->
                        applyFlattened True { scope | origin = applicationOrigin (childOrigin 2 scope.origin) (childOrigin 1 scope.origin) } fn arg (dependency ( "elm/core", "Basics", "apR" ) state)

                    _ ->
                        compileArguments state
                            |> Result.andThen
                                (\( values, afterArguments ) ->
                                    lower (child 0 scope) Nothing function afterArguments
                                        |> Result.map
                                            (\( target, final ) ->
                                                ( Value
                                                    (case resolved of
                                                        Just ( "elm/core", "Debug", "todo" ) ->
                                                            JS.call (asExpr target) values

                                                        Just ( "elm/core", home, name ) ->
                                                            CoreCalls.compile home name values

                                                        _ ->
                                                            JS.call (asExpr target) values
                                                    )
                                                , final
                                                )
                                            )
                                )


applyFlattened : Bool -> Scope -> Expression -> Expression -> State -> Result MakeError ( Body, State )
applyFlattened pipelineInputFirst scope function argument state =
    case function of
        Application target arguments ->
            case childOrigin 0 scope.origin of
                SourceOrigin _ original _ ->
                    applyOrdered pipelineInputFirst { scope | origin = SourceOrigin Nothing (original ++ [ childOrigin 1 scope.origin ]) Nothing } Nothing target (arguments ++ [ argument ]) state

        RecordAccessFunction field ->
            lower { scope | origin = SourceOrigin Nothing [ childOrigin 1 scope.origin ] Nothing } Nothing (RecordAccess argument field) state

        _ ->
            apply scope Nothing function [ argument ] state


resolveOperator : Scope -> String -> Result MakeError Global
resolveOperator scope operator =
    case operatorName operator of
        Just ( home, name ) ->
            Ok ( "elm/core", home, name )

        Nothing ->
            Canonicalize.resolveOperator scope operator


applyOperator : Scope -> String -> Int -> List Expression -> State -> Result MakeError ( Body, State )
applyOperator scope operator offset arguments state =
    resolveOperator scope operator
        |> Result.andThen
            (\global ->
                lowerArguments scope offset arguments (dependency global state)
                    |> Result.map
                        (\( args, next ) ->
                            ( Value
                                (case global of
                                    ( "elm/core", home, name ) ->
                                        CoreCalls.compile home name args

                                    _ ->
                                        JS.call (JS.Ref (globalName global)) args
                                )
                            , next
                            )
                        )
            )


operatorName : String -> Maybe ( String, String )
operatorName operator =
    Dict.get operator
        (Dict.fromList
            [ ( "+", "add" )
            , ( "-", "sub" )
            , ( "*", "mul" )
            , ( "/", "fdiv" )
            , ( "//", "idiv" )
            , ( "^", "pow" )
            , ( "==", "eq" )
            , ( "/=", "neq" )
            , ( "<", "lt" )
            , ( ">", "gt" )
            , ( "<=", "le" )
            , ( ">=", "ge" )
            , ( "&&", "and" )
            , ( "||", "or" )
            , ( "++", "append" )
            , ( "<|", "apL" )
            , ( "|>", "apR" )
            , ( "<<", "composeL" )
            , ( ">>", "composeR" )
            ]
        )
        |> Maybe.map (\name -> ( "Basics", name ))
        |> (\found ->
                if operator == "::" then
                    Just ( "List", "cons" )

                else
                    found
           )


lowerFunction : Scope -> Maybe String -> List Pattern -> Expression -> State -> Result MakeError ( JS.Expr, State )
lowerFunction =
    lowerFunctionWithPatternOrder PatternsBeforeBody


lowerFunctionWithPatternOrder : ParameterOrder -> Scope -> Maybe String -> List Pattern -> Expression -> State -> Result MakeError ( JS.Expr, State )
lowerFunctionWithPatternOrder parameterOrder scope self arguments body state =
    let
        named pattern =
            case pattern of
                VarPattern name ->
                    Just name

                AsPattern _ name ->
                    Just name

                _ ->
                    Nothing

        locals =
            List.concatMap Patterns.names arguments
                |> List.foldl (\name -> Dict.insert name (Names.local name)) scope.locals

        bindPattern pattern current =
            let
                ( name, afterName ) =
                    case named pattern of
                        Just source ->
                            ( Names.local source, current )

                        Nothing ->
                            fresh current
            in
            Patterns.bindings scope pattern (JS.Ref name) afterName.next
                |> Result.andThen
                    (\match ->
                        if List.isEmpty match.tests then
                            Ok ( ( name, match.variables ), { afterName | next = match.next } )

                        else
                            Err (Errors.unsupported (Location.File scope.module_.file) RefutableArgumentPattern)
                    )

        bindArgument pattern current =
            if parameterOrder == PatternsAfterBody then
                Ok ( ( Maybe.map Names.local (named pattern), [] ), current )

            else
                case pattern of
                    AllPattern ->
                        Ok ( ( Nothing, [] ), current )

                    UnitPattern ->
                        Ok ( ( Nothing, [] ), current )

                    _ ->
                        bindPattern pattern current |> Result.map (\( ( name, variables ), final ) -> ( ( Just name, variables ), final ))

        tailParameters names =
            List.map2 (\pattern name -> ( named pattern |> Maybe.withDefault name, name )) arguments names

        finishArgument ( pattern, ( name, variables ) ) current =
            if parameterOrder == PatternsAfterBody then
                bindPattern pattern current

            else
                case name of
                    Just value ->
                        Ok ( ( value, variables ), current )

                    Nothing ->
                        bindPattern pattern current
    in
    runList bindArgument arguments state
        |> Result.andThen
            (\( parameters, afterArgs ) ->
                let
                    preparedNames =
                        List.filterMap Tuple.first parameters

                    initialTail =
                        if List.length preparedNames == List.length arguments then
                            Maybe.map (\name -> { name = name, parameters = tailParameters preparedNames }) self

                        else
                            Nothing

                    lowerBody tail =
                        lower { scope | locals = locals } tail body { afterArgs | tailCall = False }
                in
                lowerBody initialTail
                    |> Result.andThen
                        (\( initialBody, afterBody ) ->
                            runList finishArgument (List.map2 Tuple.pair arguments parameters) afterBody
                                |> Result.andThen
                                    (\( boundParameters, afterNames ) ->
                                        let
                                            names =
                                                List.map Tuple.first boundParameters

                                            compiled =
                                                case self of
                                                    Just name ->
                                                        if initialTail == Nothing && Set.member ( [], name ) (LocalDeclarations.references body) then
                                                            -- Deferred parameter roots follow body names, but tail-call rewriting needs their names.
                                                            lowerBody (Just { name = name, parameters = tailParameters names })

                                                        else
                                                            Ok ( initialBody, afterBody )

                                                    Nothing ->
                                                        Ok ( initialBody, afterBody )
                                        in
                                        compiled
                                            |> Result.map
                                                (\( code, final ) ->
                                                    let
                                                        bindings =
                                                            List.concatMap Tuple.second boundParameters

                                                        statements =
                                                            List.map (\( name, value ) -> JS.Var name value) bindings ++ asStmts code

                                                        wrapped =
                                                            case ( final.tailCall, self ) of
                                                                ( True, Just functionName ) ->
                                                                    [ JS.Loop (Names.local functionName) statements ]

                                                                _ ->
                                                                    statements
                                                    in
                                                    ( JS.curryFunction names wrapped
                                                    , { final | next = afterNames.next, tailCall = state.tailCall }
                                                    )
                                                )
                                    )
                        )
            )


lowerLet : Scope -> Maybe Tail -> List LetDeclaration -> Expression -> State -> Result MakeError ( Body, State )
lowerLet scope tail declarations body state =
    let
        locals =
            declarations
                |> List.concatMap
                    (\declaration ->
                        case declaration of
                            LetFunction function ->
                                [ function.declaration.name ]

                            LetDestructuring pattern _ ->
                                Patterns.names pattern
                    )
                |> List.foldl (\name -> Dict.insert name (Names.local name)) scope.locals

        inner =
            { scope | locals = locals }

        compile indexed current =
            let
                sourceScope =
                    child indexed.index inner
            in
            case indexed.declaration of
                LetFunction function ->
                    let
                        implementation =
                            function.declaration
                    in
                    (if List.isEmpty implementation.arguments then
                        lower sourceScope Nothing implementation.expression current |> Result.map (\( code, next ) -> ( asExpr code, next ))

                     else
                        lowerFunctionWithPatternOrder
                            (if LocalDeclarations.isSelfRecursive indexed.declaration then
                                PatternsBeforeBody

                             else
                                PatternsAfterBody
                            )
                            sourceScope
                            (Just implementation.name)
                            implementation.arguments
                            implementation.expression
                            current
                    )
                        |> Result.map (\( value, next ) -> ( [ JS.Var (Names.local implementation.name) value ], next ))

                LetDestructuring pattern value ->
                    let
                        ( temporary, afterName ) =
                            fresh current
                    in
                    Patterns.bindings inner pattern (JS.Ref temporary) afterName.next
                        |> Result.andThen
                            (\match ->
                                if not (List.isEmpty match.tests) then
                                    Err (Errors.unsupported (Location.File scope.module_.file) RefutableLetPattern)

                                else
                                    lower sourceScope Nothing value { afterName | next = match.next }
                                        |> Result.map
                                            (\( code, next ) ->
                                                ( JS.Var temporary (asExpr code) :: List.map (\( name, target ) -> JS.Var name target) match.variables, next )
                                            )
                            )

        declarationsBody ordered current =
            case ordered of
                [] ->
                    lower (child (List.length declarations) inner) tail body current

                first :: rest ->
                    let
                        prefix statements code =
                            Block (statements ++ asStmts code)

                        before =
                            case first.declaration of
                                LetDestructuring _ _ ->
                                    True

                                _ ->
                                    LocalDeclarations.isSelfRecursive first.declaration
                    in
                    if before then
                        compile first current
                            |> Result.andThen
                                (\( statements, next ) ->
                                    declarationsBody rest next |> Result.map (\( code, final ) -> ( prefix statements code, final ))
                                )

                    else
                        declarationsBody rest current
                            |> Result.andThen
                                (\( code, next ) ->
                                    compile first next |> Result.map (\( statements, final ) -> ( prefix statements code, final ))
                                )
    in
    LocalDeclarations.order (Location.File scope.module_.file) declarations
        |> Result.andThen (\ordered -> declarationsBody ordered state)


lowerCase : Scope -> Maybe Tail -> Expression -> List ElmSyntax.Abstract.Expression.Case -> State -> Result MakeError ( Body, State )
lowerCase scope tail subject cases state =
    let
        ( temporary, afterName ) =
            fresh state
    in
    lower (child 0 scope) Nothing subject afterName
        |> Result.andThen
            (\( value, afterValue ) ->
                let
                    ( root, prefix ) =
                        case asExpr value of
                            JS.Ref name ->
                                if Dict.values scope.locals |> List.member name then
                                    ( JS.Ref name, [] )

                                else
                                    ( JS.Ref temporary, [ JS.Var temporary (asExpr value) ] )

                            _ ->
                                ( JS.Ref temporary, [ JS.Var temporary (asExpr value) ] )
                in
                runList
                    (\( index, case_ ) current ->
                        Patterns.bindings scope case_.pattern root current.next
                            |> Result.andThen
                                (\match ->
                                    let
                                        locals =
                                            List.foldl (\name -> Dict.insert name (Names.local name)) scope.locals (Patterns.names case_.pattern)
                                    in
                                    lower (child (index + 1) { scope | locals = locals }) tail case_.expression { current | next = match.next }
                                        |> Result.map
                                            (\( code, next ) ->
                                                ( { tests = match.tests
                                                  , body = List.map (\( name, target ) -> JS.Var name target) match.variables ++ asStmts code
                                                  }
                                                , next
                                                )
                                            )
                                )
                    )
                    (List.indexedMap Tuple.pair cases)
                    afterValue
                    |> Result.andThen
                        (\( rows, final ) ->
                            PatternDecisions.compile (Location.File scope.module_.file) temporary rows
                                |> Result.map (\statements -> ( Block (prefix ++ statements), final ))
                        )
            )


compileDefinition : Scope -> String -> Definition -> Result MakeError Code
compileDefinition scope name definition =
    compileDefinitionFrom 0 scope name definition |> Result.map .code


compileCyclicValueFrom : Int -> Scope -> String -> Expression -> Result MakeError { code : Code, next : Int }
compileCyclicValueFrom offset scope name body =
    lower scope Nothing body { next = offset, dependencies = Set.empty, tailCall = False }
        |> Result.map
            (\( value, state ) ->
                { code =
                    { text = JS.renderStatement (JS.FunctionDeclaration (cyclicName ( scope.module_.package, scope.module_.name, name )) [] (asStmts value))
                    , dependencies = state.dependencies
                    }
                , next = state.next
                }
            )


compileDefinitionFrom : Int -> Scope -> String -> Definition -> Result MakeError { code : Code, next : Int }
compileDefinitionFrom offset scope name definition =
    let
        initial =
            { next = offset, dependencies = Set.empty, tailCall = False }

        compile =
            case definition of
                EffectLeaf ->
                    Err (Errors.internal CompilerInvariant "An effect leaf must be normalized to its manager before emission.")

                Function [] body ->
                    lower scope Nothing body initial |> Result.map (\( code, state ) -> ( asExpr code, state ))

                Function arguments body ->
                    lowerFunction scope (Just name) arguments body initial

                Constructor arity ->
                    let
                        parameters =
                            List.range 0 (arity - 1) |> List.map Names.field

                        info =
                            Canonicalize.constructorInfo scope.environment ( scope.module_.package, scope.module_.name, name )

                        tag =
                            EmissionMode.tag scope.mode ( scope.module_.package, scope.module_.name, name ) (info |> Maybe.map .index |> Maybe.withDefault 0)

                        value =
                            JS.Object (( "$", tag ) :: List.map (\arg -> ( arg, JS.Ref arg )) parameters)

                        constructor =
                            case info of
                                Just details ->
                                    if scope.mode.flavor == Optimize && details.enumeration then
                                        tag

                                    else if scope.mode.flavor == Optimize && details.arity == 1 && details.alternatives == 1 then
                                        JS.Ref (globalName ( "elm/core", "Basics", "identity" ))

                                    else
                                        JS.curryFunction parameters [ JS.Return value ]

                                Nothing ->
                                    JS.curryFunction parameters [ JS.Return value ]
                    in
                    if arity > 26 then
                        Err (Errors.unsupported (Location.File scope.module_.file) (ConstructorFieldCount arity))

                    else
                        Ok
                            ( constructor
                            , case info of
                                Just details ->
                                    if details.arity == 1 && details.alternatives == 1 then
                                        dependency ( "elm/core", "Basics", "identity" ) initial

                                    else
                                        initial

                                Nothing ->
                                    initial
                            )
    in
    compile
        |> Result.map
            (\( value, state ) ->
                { code =
                    { text = JS.renderStatement (JS.Var (globalName ( scope.module_.package, scope.module_.name, name )) value)
                    , dependencies = state.dependencies
                    }
                , next = state.next
                }
            )
