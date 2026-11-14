module CompilerTests exposing (tests)

import Dict
import ElmMake
import ElmMake.Canonicalize as Canonicalize
import ElmMake.CoreCalls as CoreCalls
import ElmMake.CyclicDeclarations as CyclicDeclarations
import ElmMake.DependencyGraph as Graph
import ElmMake.Effects as Effects
import ElmMake.EmissionMode as EmissionMode
import ElmMake.EntryPoint as EntryPoint
import ElmMake.Errors as Errors
import ElmMake.JavaScript as JavaScript
import ElmMake.JavaScriptLiterals as Literals
import ElmMake.JavaScriptNames as Names
import ElmMake.JavaScriptSyntax as JS
import ElmMake.Kernel as Kernel
import ElmMake.LocalDeclarations as LocalDeclarations
import ElmMake.MainType as MainType
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmMake.StronglyConnected as StronglyConnected
import ElmMake.Types as Types
import ElmMake.Validate as Validate
import ElmSyntax.Abstract.Expression exposing (Expression(..))
import ElmSyntax.Abstract.Infix exposing (InfixDirection(..))
import ElmSyntax.Abstract.Pattern exposing (Pattern(..))
import ElmSyntax.Concrete.Parser.FromString as Parser
import Expect
import Set
import Test exposing (Test, describe, test)


scope : Result MakeError Scope
scope =
    Canonicalize.parseModule "module Main exposing (main)\nmain = 1\nprivate = 2"
        |> Result.mapError (\( error, _ ) -> Errors.internal CompilerInvariant error.message)
        |> Result.map
            (\syntax ->
                let
                    module_ =
                        { package = "author/project", name = "Main", path = "Main.elm", syntax = syntax, origins = Dict.empty, file = Location.ApplicationFile [ "Main.elm" ], source = Nothing }
                in
                { module_ = module_
                , locals = Dict.empty
                , mode = developmentMode
                , origin = emptyOrigin
                , cyclicValues = Set.empty
                , environment =
                    { modules = Dict.singleton ( "author/project", "Main" ) module_
                    , kernels = Dict.empty
                    , dependencies = Dict.empty
                    , exposedModules = Dict.empty
                    }
                }
            )


compile : Expression -> Result MakeError String
compile expression_ =
    scope |> Result.andThen (\context -> JavaScript.expression context expression_) |> Result.map .text


moduleScope : String -> Result MakeError Scope
moduleScope source =
    Canonicalize.parseSourceModule source
        |> Result.mapError (\( error, _ ) -> Errors.internal CompilerInvariant error.message)
        |> Result.map
            (\parsed ->
                let
                    module_ =
                        { package = "author/project", name = "Main", path = "Main.elm", syntax = parsed.syntax, origins = parsed.origins, file = Location.ApplicationFile [ "Main.elm" ], source = Just parsed.source }
                in
                { module_ = module_
                , locals = Dict.empty
                , mode = developmentMode
                , origin = emptyOrigin
                , cyclicValues = Set.empty
                , environment =
                    { modules = Dict.singleton ( "author/project", "Main" ) module_
                    , kernels = Dict.empty
                    , dependencies = Dict.empty
                    , exposedModules = Dict.empty
                    }
                }
            )


compileDefinition : String -> String -> Result MakeError String
compileDefinition source name =
    compileDefinitionWithFlavor Development source name


compileDefinitionWithFlavor : Flavor -> String -> String -> Result MakeError String
compileDefinitionWithFlavor flavor source name =
    moduleScope source
        |> Result.andThen
            (\context ->
                case Canonicalize.definition context.module_ name of
                    Nothing ->
                        Err (Errors.naming context.module_ (UnknownValue [] name))

                    Just definition ->
                        let
                            prepared =
                                Canonicalize.definitionScope context.environment context.module_ name
                        in
                        JavaScript.compileDefinition { prepared | mode = EmissionMode.prepare flavor context.environment [ context.module_ ] } name definition |> Result.map .text
            )


classifyMain : String -> Result MakeError MainType.Main
classifyMain source =
    Result.map2 Tuple.pair
        (moduleScope source)
        (moduleScope "module Platform exposing (Program)\ntype Program flags model msg = Program")
        |> Result.andThen
            (\( context, platformContext ) ->
                let
                    platform =
                        platformContext.module_

                    core =
                        { platform | package = "elm/core", name = "Platform" }

                    environment =
                        context.environment

                    prepared =
                        { environment
                            | modules = Dict.insert ( "elm/core", "Platform" ) core environment.modules
                            , dependencies = Dict.singleton "author/project" [ "elm/core" ]
                            , exposedModules = Dict.singleton "elm/core" [ "Platform" ]
                        }
                in
                MainType.classify prepared context.module_
            )


sharedQualifierScope : Result MakeError Scope
sharedQualifierScope =
    Result.map3
        (\context first second ->
            let
                environment =
                    context.environment

                bytes =
                    first.module_

                decode =
                    second.module_
            in
            { context
                | environment =
                    { environment
                        | modules =
                            Dict.insert ( "author/project", "Bytes" ) { bytes | name = "Bytes" } environment.modules
                                |> Dict.insert ( "author/project", "Bytes.Decode" ) { decode | name = "Bytes.Decode" }
                    }
            }
        )
        (moduleScope "module Main exposing (main)\nimport Bytes\nimport Bytes.Decode as Bytes\nmain = ()")
        (moduleScope "module Bytes exposing (Bytes, width, shared)\ntype Bytes = Bytes\ntype Hidden = Hidden\nwidth = 1\nshared = 1\nprivate = 1")
        (moduleScope "module Bytes.Decode exposing (Decoder, decode, shared)\ntype Decoder = Decoder\ndecode = 2\nshared = 2")


importedOperatorScope : Result MakeError Scope
importedOperatorScope =
    Result.map2
        (\context parser ->
            let
                environment =
                    context.environment

                module_ =
                    parser.module_
            in
            { context
                | environment =
                    { environment | modules = Dict.insert ( "author/project", "Parser" ) { module_ | name = "Parser" } environment.modules }
            }
        )
        (moduleScope "module Main exposing (main)\nimport Parser exposing ((|.))\nmain = ()")
        (moduleScope "module Parser exposing ((|.))\ninfix left 6 (|.) = keep\nkeep left right = left")


request : Request
request =
    { files = []
    , manifestPath = [ "elm.json" ]
    , entryPoint = [ "src", "Main.elm" ]
    , packages = []
    , outputType = JavaScript
    , flavor = Development
    }


tests : Test
tests =
    describe "Elm make ECMAScript"
        [ test "JavaScript strings escape quotes, backslashes, controls, and line separators" <|
            \_ ->
                JavaScript.quote "'\\\n\t\u{2028}"
                    |> Expect.equal "'\\'\\\\\\n\\t\\u2028'"
        , test "character literals use classic control and whitespace escapes without string-only escaping" <|
            \_ ->
                List.map Literals.character [ 9, 10, 11, 12, 13, 34, 60, 160, 0x2028, 0x1F642 ]
                    |> Expect.equal [ "'\\t'", "'\\n'", "'\\u000B'", "'\\u000C'", "'\\r'", "'\\\"'", "'<'", "'\\u00A0'", "'\\u2028'", "'\u{1F642}'" ]
        , test "character expressions and case patterns retain their classic escaping" <|
            \_ ->
                Result.map2 Tuple.pair
                    (traverse (CharLiteral >> compile) [ 13, 34, 60, 160 ])
                    (compileDefinition "module Main exposing (..)\nchoose char = case char of\n    '\\u{000D}' -> 0\n    '\\u{00A0}' -> 1\n    _ -> 2" "choose"
                        |> Result.map (\text -> String.contains "'\\u000D'" text && String.contains "'\\u00A0'" text)
                    )
                    |> Expect.equal (Ok ( [ "_Utils_chr('\\r')", "_Utils_chr('\\\"')", "_Utils_chr('<')", "_Utils_chr('\\u00A0')" ], True ))
        , test "eager initializer dependencies exclude lambda bodies and let functions" <|
            \_ ->
                moduleScope "module Main exposing (..)\nvalue = let\n    later unit = delayed\n  in (eager, \\unit -> later unit)"
                    |> Result.andThen
                        (\context ->
                            case Canonicalize.definition context.module_ "value" of
                                Just (Function [] body) ->
                                    Ok (LocalDeclarations.eagerReferences body)

                                _ ->
                                    Err (Errors.internal CompilerInvariant "Expected a value initializer")
                        )
                    |> Expect.equal (Ok (Set.singleton ( [], "eager" )))
        , test "anonymous function arguments are allocated after case temporaries in their body" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = \\_ -> case 1 of\n    0 -> 0\n    1 -> 1\n    _ -> 2" "value"
                    |> Expect.equal
                        (Ok
                            ("var $author$project$Main$value = function (_v1) {\n\tvar _v0 = 1;\n"
                                ++ "\tswitch (_v0) {\n\t\tcase 0:\n\t\t\treturn 0;\n\t\tcase 1:\n\t\t\treturn 1;\n\t\tdefault:\n\t\t\treturn 2;\n\t}\n};\n"
                            )
                        )
        , test "call arguments allocate fresh names before the function expression" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = (\\fn -> case [fn ()] of\n    [] -> 0\n    _ -> 1) (\\_ -> 2)" "value"
                    |> Result.map (\text -> String.contains "function (_v0)" text && String.contains "var _v1 =" text)
                    |> Expect.equal (Ok True)
        , test "named argument patterns allocate roots before body temporaries" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nchoose (left, right) = case left of\n    0 -> right\n    _ -> left" "choose"
                    |> Result.map (\text -> String.contains "function (_v0)" text && String.contains "var left = _v0.a" text)
                    |> Expect.equal (Ok True)
        , test "nonrecursive local function patterns allocate roots after body temporaries" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = let\n    choose {left} = case left of\n        0 -> 0\n        _ -> 1\n  in choose {left = 0}" "value"
                    |> Result.map (\text -> String.contains "function (_v1)" text && String.contains "var left = _v1.left" text)
                    |> Expect.equal (Ok True)
        , test "recursive local function patterns allocate roots before body temporaries" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = let\n    loop (x, y) = case x of\n        0 -> y\n        _ -> loop (y, x)\n  in loop (1, 2)" "value"
                    |> Result.map (\text -> String.contains "function (_v0)" text && String.contains "_v0 = $temp$_v0;" text && String.contains "continue loop;" text)
                    |> Expect.equal (Ok True)
        , test "flattened forward pipelines allocate input names before existing function arguments" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = (\\_ -> 2) |> (\\fn arg -> fn arg) (\\_ -> 1)" "value"
                    |> Result.map (\text -> Maybe.map2 (<) (List.head (String.indexes "_v1" text)) (List.head (String.indexes "_v0" text)))
                    |> Expect.equal (Ok (Just True))
        , test "bitwise shifts reverse the Elm shift-count argument order" <|
            \_ ->
                [ "shiftLeftBy", "shiftRightBy", "shiftRightZfBy" ]
                    |> List.map (\name -> CoreCalls.compile "Bitwise" name [ JS.Ref "count", JS.Ref "value" ] |> JS.renderExpression)
                    |> Expect.equal [ "value << count", "value >> count", "value >>> count" ]
        , test "partially applied core calls retain their curried function" <|
            \_ ->
                CoreCalls.compile "Bitwise" "and" [ JS.Literal "7" ]
                    |> JS.renderExpression
                    |> Expect.equal "$elm$core$Bitwise$and(7)"
        , test "inequality with false is the operand rather than double negation" <|
            \_ ->
                [ [ JS.Ref "flag", JS.Literal "false" ], [ JS.Literal "false", JS.Ref "flag" ] ]
                    |> List.map (CoreCalls.compile "Basics" "neq" >> JS.renderExpression)
                    |> Expect.equal [ "flag", "flag" ]
        , test "Program classification substitutes main and flag aliases" <|
            \_ ->
                classifyMain "module Main exposing (main)\ntype alias Flags = ()\ntype alias Main flags = Program flags () ()\nmain : Main Flags\nmain = ()"
                    |> Expect.equal (Ok MainType.ProgramWithUnitFlags)
        , test "generic alias substitution preserves the caller's type identity" <|
            \_ ->
                Result.map2 Tuple.pair
                    (moduleScope "module Main exposing (..)\nimport Imported\ntype Msg = Msg\ntype alias Value = Imported.Box Msg")
                    (moduleScope "module Imported exposing (..)\ntype alias Box a = (a, a)")
                    |> Result.andThen
                        (\( context, imported ) ->
                            let
                                environment =
                                    context.environment

                                parsed =
                                    imported.module_

                                owner =
                                    { parsed | name = "Imported" }

                                prepared =
                                    { environment | modules = Dict.insert ( owner.package, owner.name ) owner environment.modules }
                            in
                            Types.expandAlias prepared ( "author/project", "Main", "Value" ) []
                                |> Result.andThen
                                    (\expanded ->
                                        case expanded of
                                            Just (Types.Typed key arguments) ->
                                                Types.expandAlias prepared key arguments

                                            _ ->
                                                Err (Errors.internal CompilerInvariant "Expected the imported alias")
                                    )
                        )
                    |> Expect.equal
                        (Ok (Just (Types.Tupled [ Types.Typed ( "author/project", "Main", "Msg" ) [], Types.Typed ( "author/project", "Main", "Msg" ) [] ])))
        , test "shared import qualifiers resolve values by the exported member" <|
            \_ ->
                sharedQualifierScope
                    |> Result.andThen
                        (\context ->
                            traverse (Canonicalize.resolve context [ "Bytes" ]) [ "width", "decode" ]
                        )
                    |> Expect.equal
                        (Ok [ ( "author/project", "Bytes", "width" ), ( "author/project", "Bytes.Decode", "decode" ) ])
        , test "shared import qualifiers resolve type names by their module exposure" <|
            \_ ->
                sharedQualifierScope
                    |> Result.andThen
                        (\context ->
                            traverse (Canonicalize.resolveType context [ "Bytes" ]) [ "Bytes", "Decoder" ]
                        )
                    |> Result.map (List.map .name)
                    |> Expect.equal (Ok [ "Bytes", "Bytes.Decode" ])
        , test "shared import qualifiers retain ambiguous and unavailable member errors" <|
            \_ ->
                case sharedQualifierScope of
                    Err error ->
                        Expect.fail (Debug.toString error)

                    Ok context ->
                        Expect.equal
                            ( Err (Errors.naming context.module_ (AmbiguousModule "Bytes"))
                            , Err (Errors.naming context.module_ (UnknownValue [ "Bytes" ] "private"))
                            , Err (Errors.naming context.module_ (UnknownType [ "Bytes" ] "Hidden"))
                            )
                            ( Canonicalize.resolve context [ "Bytes" ] "shared"
                            , Canonicalize.resolve context [ "Bytes" ] "private"
                            , Canonicalize.resolveType context [ "Bytes" ] "Hidden"
                            )
        , test "imported operators use their private implementing function and arity helpers" <|
            \_ ->
                importedOperatorScope
                    |> Result.andThen
                        (\context ->
                            traverse (JavaScript.expression context)
                                [ PrefixOperator "|."
                                , Application (PrefixOperator "|.") [ IntegerLiteral 1 ]
                                , OperatorApplication "|." Left (IntegerLiteral 1) (IntegerLiteral 2)
                                ]
                        )
                    |> Result.map (List.map (\code -> ( code.text, code.dependencies )))
                    |> Expect.equal
                        (Ok
                            [ ( "$author$project$Parser$keep", Set.singleton ( "author/project", "Parser", "keep" ) )
                            , ( "$author$project$Parser$keep(1)", Set.singleton ( "author/project", "Parser", "keep" ) )
                            , ( "A2($author$project$Parser$keep, 1, 2)", Set.singleton ( "author/project", "Parser", "keep" ) )
                            ]
                        )
        , test "undeclared operators report naming errors instead of guessing an implementation" <|
            \_ ->
                case importedOperatorScope of
                    Err error ->
                        Expect.fail (Debug.toString error)

                    Ok context ->
                        Canonicalize.resolveOperator context "|="
                            |> Expect.equal (Err (Errors.naming context.module_ (UnknownValue [] "|=")))
        , test "cyclic value references use thunks only within their recursive component" <|
            \_ ->
                moduleScope "module Main exposing (..)\nvalue = 1"
                    |> Result.andThen
                        (\context ->
                            Result.map2 Tuple.pair
                                (JavaScript.expression { context | cyclicValues = Set.singleton ( "author/project", "Main", "value" ) } (Identifier [] "value"))
                                (JavaScript.expression context (Identifier [] "value"))
                        )
                    |> Result.map (\( inside, outside ) -> ( inside.text, outside.text ))
                    |> Expect.equal (Ok ( "$author$project$Main$cyclic$value()", "$author$project$Main$value" ))
        , test "mixed recursive components emit named thunks with memoized initialization and an explicit failure" <|
            \_ ->
                let
                    helper =
                        ( "author/project", "Main", "helper" )

                    value =
                        ( "author/project", "Main", "value" )

                    nodes =
                        Dict.fromList
                            [ ( helper, { text = "", dependencies = [ value ], isKernel = False, eager = False } )
                            , ( value, { text = "", dependencies = [ helper ], isKernel = False, eager = True } )
                            ]
                in
                moduleScope "module Main exposing (..)\nvalue = helper ()\nhelper unit event = value"
                    |> Result.andThen (\context -> CyclicDeclarations.allocate developmentMode context.environment nodes)
                    |> Result.andThen (\prepared -> Graph.emit prepared value)
                    |> (\result ->
                            case result of
                                Err error ->
                                    Expect.fail (Debug.toString error)

                                Ok ( kernels, declarations ) ->
                                    Expect.all
                                        [ \_ -> Expect.equal "" kernels
                                        , \_ ->
                                            Expect.equal True
                                                (String.startsWith
                                                    ("var $author$project$Main$helper = F2(\n\tfunction (unit, event) {\n\t\treturn $author$project$Main$cyclic$value();\n\t});\n"
                                                        ++ "function $author$project$Main$cyclic$value() {\n\treturn $author$project$Main$helper(_Utils_Tuple0);\n}\n"
                                                    )
                                                    declarations
                                                )
                                        , \_ ->
                                            Expect.equal True
                                                (String.contains
                                                    "try {\n\tvar $author$project$Main$value = $author$project$Main$cyclic$value();\n\t$author$project$Main$cyclic$value = function () {\n\t\treturn $author$project$Main$value;\n\t};\n} catch ($) {\n\tthrow 'Some top-level definitions from `Main`"
                                                    declarations
                                                )
                                        ]
                                        ()
                       )
        , test "pure recursive value components remain rejected before JavaScript initialization" <|
            \_ ->
                let
                    helper =
                        ( "author/project", "Main", "helper" )

                    value =
                        ( "author/project", "Main", "value" )

                    nodes =
                        Dict.fromList
                            [ ( helper, { text = "", dependencies = [ value ], isKernel = False, eager = True } )
                            , ( value, { text = "", dependencies = [ helper ], isKernel = False, eager = True } )
                            ]
                in
                moduleScope "module Main exposing (..)\nvalue = helper\nhelper = value"
                    |> Result.andThen (\context -> CyclicDeclarations.allocate developmentMode context.environment nodes)
                    |> Expect.equal (Err (Errors.unsupported Location.Project RecursiveValueInitialization))
        , test "unsupported flags are rejected instead of using a unit decoder" <|
            \_ ->
                case classifyMain "module Main exposing (main)\nmain : Program ((), ()) () ()\nmain = ()" of
                    Err (UnsupportedFeature problem) ->
                        Expect.equal UnsupportedFlags problem.feature

                    _ ->
                        Expect.fail "Expected an unsupported flags capability"
        , test "Program initialization carries its decoder dependency and calling convention" <|
            \_ ->
                EntryPoint.compile developmentMode "0" MainType.ProgramWithUnitFlags ( "author/project", "Main", "main" )
                    |> Expect.equal
                        { initializer = "$author$project$Main$main(\n\t$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)"
                        , dependencies = [ ( "elm/json", "Json.Decode", "succeed" ) ]
                        }
        , test "command managers emit their hooks and leaf in initialization order" <|
            \_ ->
                moduleScope "effect module Main where { command = MyCmd } exposing (..)\ntype MyCmd msg = Perform msg"
                    |> Result.andThen (\context -> Effects.compile context.module_)
                    |> Expect.equal
                        (Ok
                            { text = "_Platform_effectManagers['Main'] = _Platform_createManager($author$project$Main$init, $author$project$Main$onEffects, $author$project$Main$onSelfMsg, $author$project$Main$cmdMap);\nvar $author$project$Main$command = _Platform_leaf('Main');\n"
                            , dependencies = [ ( "elm/core", "Elm.Kernel.Platform", "$" ), ( "author/project", "Main", "init" ), ( "author/project", "Main", "onEffects" ), ( "author/project", "Main", "onSelfMsg" ), ( "author/project", "Main", "cmdMap" ) ]
                            }
                        )
        , test "subscription managers reserve the command-map argument" <|
            \_ ->
                moduleScope "effect module Main where { subscription = MySub } exposing (..)\ntype MySub msg = Listen msg"
                    |> Result.andThen (\context -> Effects.compile context.module_)
                    |> Result.map .text
                    |> Expect.equal (Ok "_Platform_effectManagers['Main'] = _Platform_createManager($author$project$Main$init, $author$project$Main$onEffects, $author$project$Main$onSelfMsg, 0, $author$project$Main$subMap);\nvar $author$project$Main$subscription = _Platform_leaf('Main');\n")
        , test "combined managers share both generated leaves" <|
            \_ ->
                moduleScope "effect module Main where { command = MyCmd, subscription = MySub } exposing (..)\ntype MyCmd msg = Send msg\ntype MySub msg = Listen msg"
                    |> Result.andThen (\context -> Effects.compile context.module_)
                    |> Result.map .text
                    |> Expect.equal (Ok "_Platform_effectManagers['Main'] = _Platform_createManager($author$project$Main$init, $author$project$Main$onEffects, $author$project$Main$onSelfMsg, $author$project$Main$cmdMap, $author$project$Main$subMap);\nvar $author$project$Main$command = _Platform_leaf('Main');\nvar $author$project$Main$subscription = _Platform_leaf('Main');\n")
        , test "application effect modules remain explicitly unsupported" <|
            \_ ->
                moduleScope "effect module Main where { command = MyCmd } exposing (..)\ntype MyCmd msg = Send msg"
                    |> Result.andThen (\context -> Validate.moduleFeatures context.module_)
                    |> (\result ->
                            case result of
                                Err (UnsupportedFeature problem) ->
                                    Expect.equal EffectManagers problem.feature

                                _ ->
                                    Expect.fail "Expected unsupported application effect module"
                       )
        , test "ordered graph dependencies preserve effect-manager initialization order" <|
            \_ ->
                let
                    root =
                        ( "author/project", "Main", "$fx$" )

                    a =
                        ( "author/project", "Main", "a" )

                    z =
                        ( "author/project", "Main", "z" )

                    graph =
                        Dict.fromList
                            [ ( root, { text = "manager\n", dependencies = [ z, a ], isKernel = False, eager = False } )
                            , ( a, { text = "a\n", dependencies = [], isKernel = False, eager = False } )
                            , ( z, { text = "z\n", dependencies = [], isKernel = False, eager = False } )
                            ]
                in
                Graph.emit graph root |> Expect.equal (Ok ( "", "z\na\nmanager\n" ))
        , test "development kernel references do not retain the debugger graph" <|
            \_ ->
                Kernel.compile
                    { modules = Dict.empty, kernels = Dict.singleton ( "elm/browser", "Elm.Kernel.Debugger" ) "", dependencies = Dict.empty, exposedModules = Dict.empty }
                    "elm/browser"
                    "Elm.Kernel.Test"
                    "/*\nimport Elm.Kernel.Debugger exposing (element)\n*/\nvar __Debugger_element;\nvar active = __Debugger_element || 1;\n"
                    |> Expect.equal (Ok { text = "\nvar _Debugger_element;\nvar active = _Debugger_element || 1;\n", dependencies = Set.empty })
        , test "parser errors retain structured positions without decoding prose" <|
            \_ ->
                Parser.parseFileStructured "module Main exposing (main)\n\nmain =\n"
                    |> (\result ->
                            case result of
                                Err error ->
                                    Expect.equal { start = { row = 4, column = 1 }, end = { row = 4, column = 1 } } error.region

                                Ok _ ->
                                    Expect.fail "Expected a structured parser error"
                       )
        , test "independent diagnostics accumulate and preserve duplicates" <|
            \_ ->
                let
                    error =
                        Errors.problem Location.Project NoManifest
                in
                Errors.collect identity [ Err error, Ok (), Err error ]
                    |> Expect.equal (Err (CompilerErrors { location = Location.Project, kind = NoManifest } [ { location = Location.Project, kind = NoManifest } ]))
        , test "internal failures cannot be disguised by collected source errors" <|
            \_ ->
                let
                    error =
                        Errors.internal InvalidRequest "duplicate supplied package"
                in
                Errors.collect identity [ Err (Errors.problem Location.Project NoManifest), Err error ]
                    |> Expect.equal (Err error)
        , test "source regions reject reversed and zero-based positions" <|
            \_ ->
                [ Location.createRegion { line = 0, column = 1 } { line = 1, column = 1 }
                , Location.createRegion { line = 2, column = 5 } { line = 2, column = 4 }
                ]
                    |> List.all
                        (\result ->
                            case result of
                                Err _ ->
                                    True

                                Ok _ ->
                                    False
                        )
                    |> Expect.equal True
        , test "naming diagnostics retain each expression occurrence range" <|
            \_ ->
                moduleScope "module Main exposing (..)\nvalue = (missingOne, missingTwo)\n"
                    |> Result.andThen (\context -> Validate.modules context.environment context.module_)
                    |> (\result ->
                            case result of
                                Err (CompilerErrors first rest) ->
                                    (first :: rest)
                                        |> List.filterMap
                                            (\item ->
                                                case item.location of
                                                    Location.Source _ range ->
                                                        Just ( Location.start range, Location.end range )

                                                    _ ->
                                                        Nothing
                                            )
                                        |> Expect.equal
                                            [ ( { line = 2, column = 10 }, { line = 2, column = 20 } )
                                            , ( { line = 2, column = 22 }, { line = 2, column = 32 } )
                                            ]

                                _ ->
                                    Expect.fail "Expected both source diagnostics"
                       )
        , test "repeated missing imports retain their distinct source locations" <|
            \_ ->
                moduleScope "module Main exposing (..)\nimport Missing\nimport Missing\nvalue = 1\n"
                    |> Result.andThen (\context -> Validate.modules context.environment context.module_)
                    |> (\result ->
                            case result of
                                Err (CompilerErrors first rest) ->
                                    (first :: rest)
                                        |> List.filterMap
                                            (\item ->
                                                case item.location of
                                                    Location.Source _ range ->
                                                        Just (Location.start range).line

                                                    _ ->
                                                        Nothing
                                            )
                                        |> Expect.equal [ 2, 3 ]

                                _ ->
                                    Expect.fail "Expected a diagnostic for each missing import"
                       )
        , test "source syntax errors are not discarded" <|
            \_ ->
                case Canonicalize.parseModule "module Main exposing (main)\nmain =\n" of
                    Err _ ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Expected a syntax error"
        , test "literal compilation does not need unrelated modules or packages" <|
            \_ ->
                compile (StringLiteral "hello")
                    |> Expect.equal (Ok "'hello'")
        , test "ordinary source identifiers resolve in their module" <|
            \_ ->
                compile (Identifier [] "private")
                    |> Expect.equal (Ok "$author$project$Main$private")
        , test "unknown names fail rather than becoming undefined JavaScript variables" <|
            \_ ->
                case compile (Identifier [] "unknown") of
                    Err (CompilerErrors _ _) ->
                        Expect.pass

                    other ->
                        Expect.fail (Debug.toString other)
        , test "functions preserve local parameters" <|
            \_ ->
                compile (LambdaExpression [ VarPattern "value" ] (Identifier [] "value"))
                    |> Expect.equal (Ok "function (value) {\n\treturn value;\n}")
        , test "empty pattern conditions normalize to true" <|
            \_ ->
                ([ "true", "true" ]
                    |> List.filter ((/=) "true")
                    |> String.join " && "
                    |> (\text ->
                            if text == "" then
                                "true"

                            else
                                text
                       )
                )
                    |> Expect.equal "true"
        , test "single-constructor function patterns are irrefutable" <|
            \_ ->
                Canonicalize.parseModule "module Main exposing (..)\ntype Only = Only Int\nmain = 1"
                    |> Result.mapError (\( error, _ ) -> Errors.internal CompilerInvariant error.message)
                    |> Result.andThen
                        (\syntax ->
                            let
                                module_ =
                                    { package = "author/project", name = "Main", path = "Main.elm", syntax = syntax, origins = Dict.empty, file = Location.ApplicationFile [ "Main.elm" ], source = Nothing }

                                context =
                                    { module_ = module_
                                    , locals = Dict.empty
                                    , mode = developmentMode
                                    , origin = emptyOrigin
                                    , cyclicValues = Set.empty
                                    , environment =
                                        { modules = Dict.singleton ( "author/project", "Main" ) module_
                                        , kernels = Dict.empty
                                        , dependencies = Dict.empty
                                        , exposedModules = Dict.empty
                                        }
                                    }
                            in
                            JavaScript.expression context
                                (LambdaExpression [ NamedPattern { moduleName = [], name = "Only" } [ VarPattern "value" ] ] (Identifier [] "value"))
                                |> Result.map .text
                        )
                    |> Expect.equal (Ok "function (_v0) {\n\tvar value = _v0.a;\n\treturn value;\n}")
        , test "record fields are deterministically ordered" <|
            \_ ->
                compile (RecordExpr [ { fieldName = "z", value = IntegerLiteral 1 }, { fieldName = "a", value = IntegerLiteral 2 } ])
                    |> Expect.equal (Ok "{a: 2, z: 1}")
        , test "reserved local names are escaped" <|
            \_ ->
                [ "char", "class", "F2", "normal" ]
                    |> List.map Names.local
                    |> Expect.equal [ "_char", "_class", "_F2", "normal" ]
        , test "renderer groups nested infix expressions without altering associativity" <|
            \_ ->
                JS.Binary "-" (JS.Ref "x") (JS.Binary "-" (JS.Ref "y") (JS.Ref "z"))
                    |> JS.renderExpression
                    |> Expect.equal "x - (y - z)"
        , test "renderer expands parent calls when arguments are structural" <|
            \_ ->
                JS.Call (JS.Ref "outer") [ JS.Ref "f", JS.Call (JS.Ref "inner") [ JS.Literal "1" ] ]
                    |> JS.renderExpression
                    |> Expect.equal "outer(\n\tf,\n\tinner(1))"
        , test "constructors keep declaration field order and use arity helpers" <|
            \_ ->
                compileDefinition "module Main exposing (..)\ntype Value = Pair Int String" "Pair"
                    |> Expect.equal (Ok "var $author$project$Main$Pair = F2(\n\tfunction (a, b) {\n\t\treturn {$: 'Pair', a: a, b: b};\n\t});\n")
        , test "tail calls evaluate all replacement parameters before assigning" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nloop x y = if x == 0 then y else loop y x" "loop"
                    |> Expect.equal
                        (Ok
                            ("var $author$project$Main$loop = F2(\n\tfunction (x, y) {\n"
                                ++ "\t\tloop:\n\t\twhile (true) {\n\t\t\tif (!x) {\n\t\t\t\treturn y;\n\t\t\t} else {\n"
                                ++ "\t\t\t\tvar $temp$x = y,\n\t\t\t\t\t$temp$y = x;\n"
                                ++ "\t\t\t\tx = $temp$x;\n\t\t\t\ty = $temp$y;\n\t\t\t\tcontinue loop;\n"
                                ++ "\t\t\t}\n\t\t}\n\t});\n"
                            )
                        )
        , test "tail-call temporaries retain source names when parameters need JavaScript escaping" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nloop function y = if function == 0 then y else loop y function" "loop"
                    |> Expect.equal
                        (Ok
                            ("var $author$project$Main$loop = F2(\n\tfunction (_function, y) {\n"
                                ++ "\t\tloop:\n\t\twhile (true) {\n\t\t\tif (!_function) {\n\t\t\t\treturn y;\n\t\t\t} else {\n"
                                ++ "\t\t\t\tvar $temp$function = y,\n\t\t\t\t\t$temp$y = _function;\n"
                                ++ "\t\t\t\t_function = $temp$function;\n\t\t\t\ty = $temp$y;\n\t\t\t\tcontinue loop;\n"
                                ++ "\t\t\t}\n\t\t}\n\t});\n"
                            )
                        )
        , test "recursive patterned arguments retain destructuring and tail-call optimization" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nloop (x, y) _ = if x == 0 then y else loop (y, x) ()" "loop"
                    |> Result.map
                        (\text ->
                            String.contains "function (_v0, _v1)" text
                                && String.contains "var x = _v0.a;" text
                                && String.contains "$temp$_v0 = _Utils_Tuple2(y, x)" text
                                && String.contains "_v0 = $temp$_v0;" text
                                && String.contains "continue loop;" text
                        )
                    |> Expect.equal (Ok True)
        , test "exhaustive union decisions use the final constructor as default" <|
            \_ ->
                compileDefinition
                    "module Main exposing (..)\ntype Three = A | B | C\nchoose value = case value of\n    A -> 1\n    B -> 2\n    C -> 3"
                    "choose"
                    |> Expect.equal
                        (Ok
                            ("var $author$project$Main$choose = function (value) {\n"
                                ++ "\tswitch (value.$) {\n\t\tcase 'A':\n\t\t\treturn 1;\n"
                                ++ "\t\tcase 'B':\n\t\t\treturn 2;\n\t\tdefault:\n\t\t\treturn 3;\n\t}\n};\n"
                            )
                        )
        , test "list decisions reuse prior knowledge for singleton branches" <|
            \_ ->
                compileDefinition
                    "module Main exposing (..)\nchoose values = case values of\n    [] -> 0\n    [one] -> one\n    _ -> 2"
                    "choose"
                    |> Expect.equal
                        (Ok
                            ("var $author$project$Main$choose = function (values) {\n"
                                ++ "\tif (!values.b) {\n\t\treturn 0;\n\t} else {\n"
                                ++ "\t\tif (!values.b.b) {\n\t\t\tvar one = values.a;\n\t\t\treturn one;\n"
                                ++ "\t\t} else {\n\t\t\treturn 2;\n\t\t}\n\t}\n};\n"
                            )
                        )
        , test "recursive components emit their external dependencies once before members" <|
            \_ ->
                let
                    a =
                        ( "author/project", "Main", "a" )

                    b =
                        ( "author/project", "Main", "b" )

                    c =
                        ( "author/project", "Main", "c" )

                    graph =
                        Dict.fromList
                            [ ( a, { text = "a\n", dependencies = [ b, c ], isKernel = False, eager = False } )
                            , ( b, { text = "b\n", dependencies = [ a ], isKernel = False, eager = False } )
                            , ( c, { text = "c\n", dependencies = [], isKernel = False, eager = True } )
                            ]
                in
                Graph.emit graph b |> Expect.equal (Ok ( "", "c\na\nb\n" ))
        , test "strongly connected ordering keeps dependency components before recursive members" <|
            \_ ->
                Dict.fromList
                    [ ( "a", [ "b", "c" ] )
                    , ( "b", [ "a" ] )
                    , ( "c", [ "d" ] )
                    , ( "d", [] )
                    ]
                    |> StronglyConnected.dependencyOrder
                    |> Expect.equal [ [ "d" ], [ "c" ], [ "a", "b" ] ]
        , test "strongly connected ordering includes self-recursion and independent vertices" <|
            \_ ->
                Dict.fromList [ ( "a", [ "a", "external" ] ), ( "z", [] ) ]
                    |> StronglyConnected.dependencyOrder
                    |> Expect.equal [ [ "z" ], [ "a" ] ]
        , test "unused declarations are still checked for unknown names" <|
            \_ ->
                moduleScope "module Main exposing (..)\nmain = 1\nunused = missing"
                    |> Result.andThen (\context -> Validate.modules context.environment context.module_)
                    |> (\result ->
                            case result of
                                Err (CompilerErrors _ _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Expected unused declaration naming error"
                       )
        , test "duplicate function arguments are rejected" <|
            \_ ->
                moduleScope "module Main exposing (..)\nmain x x = x"
                    |> Result.andThen (\context -> Validate.modules context.environment context.module_)
                    |> (\result ->
                            case result of
                                Err (CompilerErrors _ _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Expected duplicate-binding error"
                       )
        , test "forward let references initialize dependencies first" <|
            \_ ->
                compileDefinition
                    "module Main exposing (..)\nvalue = let\n    first = second\n    second = \"ready\"\n  in first"
                    "value"
                    |> Expect.equal
                        (Ok
                            ("var $author$project$Main$value = function () {\n"
                                ++ "\tvar second = 'ready';\n\tvar first = second;\n\treturn first;\n}();\n"
                            )
                        )
        , test "independent let declarations have canonical key order" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = let\n    alpha = 1\n    zeta = 2\n  in (alpha, zeta)" "value"
                    |> Expect.equal
                        (Ok "var $author$project$Main$value = function () {\n\tvar zeta = 2;\n\tvar alpha = 1;\n\treturn _Utils_Tuple2(alpha, zeta);\n}();\n")
        , test "float exponent spelling survives abstract syntax normalization" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = 6.25e2" "value"
                    |> Expect.equal (Ok "var $author$project$Main$value = 6.25e2;\n")
        , test "source unicode escapes retain classic JavaScript encoding" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = \"\\u{03BB} and \\u{1F642}\"" "value"
                    |> Expect.equal (Ok "var $author$project$Main$value = '\\u03BB and \\uD83D\\uDE42';\n")
        , test "equivalent literal values retain their own source spellings" <|
            \_ ->
                compileDefinition ("module Main exposing (..)\nvalue = (\"" ++ String.fromChar (Char.fromCode 0x03BB) ++ "\", \"\\u{03BB}\")") "value"
                    |> Expect.equal (Ok ("var $author$project$Main$value = _Utils_Tuple2('" ++ String.fromChar (Char.fromCode 0x03BB) ++ "', '\\u03BB');\n"))
        , test "source origins follow let definitions after dependency reordering" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = let\n    alpha = \"\\u{03BB}\"\n    zeta = 6.25e2\n  in (alpha, zeta)" "value"
                    |> Expect.equal
                        (Ok "var $author$project$Main$value = function () {\n\tvar zeta = 6.25e2;\n\tvar alpha = '\\u03BB';\n\treturn _Utils_Tuple2(alpha, zeta);\n}();\n")
        , test "literal cases fan out with one switch rather than repeated tests" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nchoose number = case number of\n    1 -> \"one\"\n    2 -> \"two\"\n    _ -> \"other\"" "choose"
                    |> Expect.equal
                        (Ok "var $author$project$Main$choose = function (number) {\n\tswitch (number) {\n\t\tcase 1:\n\t\t\treturn 'one';\n\t\tcase 2:\n\t\t\treturn 'two';\n\t\tdefault:\n\t\t\treturn 'other';\n\t}\n};\n")
        , test "shared fallback branches use a labelled exit" <|
            \_ ->
                compileDefinition "module Main exposing (..)\ntype Flag = On | Off\ntype State = Yes | No\nchoose pair = case pair of\n    (On, Yes) -> 1\n    (Off, No) -> 2\n    _ -> 0" "choose"
                    |> Result.map (\text -> String.contains "break _v0$2;" text && String.contains "_v0$2:" text)
                    |> Expect.equal (Ok True)
        , test "nested character tests guard their parent list before calling valueOf" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nchoose chars = case chars of\n    'x' :: _ -> 1\n    _ -> 0" "choose"
                    |> Expect.equal
                        (Ok "var $author$project$Main$choose = function (chars) {\n\tif (chars.b && ('x' === chars.a.valueOf())) {\n\t\treturn 1;\n\t} else {\n\t\treturn 0;\n\t}\n};\n")
        , test "incomplete pattern decisions fail compilation rather than emitting a runtime placeholder" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nchoose number = case number of\n    1 -> \"one\"" "choose"
                    |> (\result ->
                            case result of
                                Err (UnsupportedFeature _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Expected an incomplete-pattern error"
                       )
        , test "recursive let values fail explicitly" <|
            \_ ->
                compileDefinition "module Main exposing (..)\nvalue = let\n    first = second\n    second = first\n  in first" "value"
                    |> (\result ->
                            case result of
                                Err (UnsupportedFeature _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Expected recursive-value error"
                       )
        , test "nested constructor decisions test source paths before destructuring temporaries" <|
            \_ ->
                compileDefinition
                    "module Main exposing (..)\ntype Value = A Int | B\ntype Wrapper = Wrap (Value, Int)\nchoose value = case value of\n    Wrap (A x, _) -> x\n    Wrap (B, _) -> 0"
                    "choose"
                    |> Result.map (String.contains "if (value.a.a.$ === 'A') {")
                    |> Expect.equal (Ok True)
        , test "recursive global values fail instead of reading uninitialized JavaScript variables" <|
            \_ ->
                let
                    key =
                        ( "author/project", "Main", "value" )

                    graph =
                        Dict.singleton key { text = "value\n", dependencies = [ key ], isKernel = False, eager = True }
                in
                case Graph.emit graph key of
                    Err (UnsupportedFeature _) ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected recursive-value error"
        , test "a missing manifest returns a structured error" <|
            \_ ->
                ElmMake.make request
                    |> Expect.equal (Err (Errors.problem Location.Project NoManifest))
        , test "debug processes source errors instead of rejecting its flavor" <|
            \_ ->
                ElmMake.make { request | flavor = Debug } |> Expect.equal (Err (Errors.problem Location.Project NoManifest))
        , test "optimized output processes source errors instead of rejecting its flavor" <|
            \_ ->
                ElmMake.make { request | flavor = Optimize } |> Expect.equal (Err (Errors.problem Location.Project NoManifest))
        , test "production field frequencies include dead declarations but not unrelated entry modules" <|
            \_ ->
                Result.map2 Tuple.pair
                    (moduleScope "module Main exposing (..)\nmain = { alpha = (), zebra = () }\ndead item = (item.zebra, item.zebra)")
                    (moduleScope "module Unrelated exposing (..)\nother item = (item.alpha, item.alpha, item.alpha, item.alpha)")
                    |> Result.map
                        (\( context, other ) ->
                            let
                                environment =
                                    context.environment

                                otherModule =
                                    other.module_

                                unrelated =
                                    { otherModule | name = "Unrelated" }
                            in
                            EmissionMode.prepare Optimize
                                { environment | modules = Dict.insert ( unrelated.package, unrelated.name ) unrelated environment.modules }
                                [ context.module_ ]
                                |> .fields
                        )
                    |> Expect.equal (Ok (Dict.fromList [ ( "zebra", "a" ), ( "alpha", "b" ) ]))
        , test "production enums use their numeric value without an object tag" <|
            \_ ->
                compileDefinitionWithFlavor Optimize "module Main exposing (..)\ntype Color = Red | Blue" "Blue"
                    |> Expect.equal (Ok "var $author$project$Main$Blue = 1;\n")
        , test "field frequencies use classic left-biased object-map union rather than cross-module addition" <|
            \_ ->
                Result.map2 Tuple.pair
                    (moduleScope "module Main exposing (..)\nmain = { alpha = (), zebra = () }\ndead item = item.zebra")
                    (moduleScope "module Zed exposing (..)\nother item = (item.alpha, item.alpha, item.alpha, item.alpha)")
                    |> Result.map
                        (\( context, other ) ->
                            let
                                environment =
                                    context.environment

                                parsed =
                                    other.module_

                                zed =
                                    { parsed | name = "Zed" }
                            in
                            EmissionMode.prepare Optimize
                                { environment | modules = Dict.insert ( zed.package, zed.name ) zed environment.modules }
                                [ context.module_, zed ]
                                |> .fields
                        )
                    |> Expect.equal (Ok (Dict.fromList [ ( "zebra", "a" ), ( "alpha", "b" ) ]))
        , test "production single-field constructor calls disappear" <|
            \_ ->
                compileDefinitionWithFlavor Optimize "module Main exposing (..)\ntype Box = Box Int\nvalue = Box 7" "value"
                    |> Expect.equal (Ok "var $author$project$Main$value = 7;\n")
        , test "artifact field precedence compares package author and project separately" <|
            \_ ->
                Result.map2 Tuple.pair
                    (moduleScope "module Main exposing (..)\nvalue item = (item.alpha, item.zebra, item.zebra)")
                    (moduleScope "module Main exposing (..)\nvalue item = (item.alpha, item.alpha, item.alpha, item.alpha)")
                    |> Result.map
                        (\( first, second ) ->
                            let
                                environment =
                                    first.environment

                                firstModule =
                                    first.module_

                                secondModule =
                                    second.module_

                                before =
                                    { firstModule | package = "a/z" }

                                after =
                                    { secondModule | package = "a-b/z" }
                            in
                            EmissionMode.prepare Optimize
                                { environment | modules = Dict.fromList [ ( ( before.package, before.name ), before ), ( ( after.package, after.name ), after ) ] }
                                []
                                |> .fields
                        )
                    |> Expect.equal (Ok (Dict.fromList [ ( "zebra", "a" ), ( "alpha", "b" ) ]))
        , test "production single-field constructor patterns bind the value directly" <|
            \_ ->
                compileDefinitionWithFlavor Optimize "module Main exposing (..)\ntype Box = Box Int\nunwrap (Box number) = number" "unwrap"
                    |> Result.map (\text -> not (String.contains ".a" text) && String.contains "return number;" text)
                    |> Expect.equal (Ok True)
        , test "production character patterns compare unboxed strings literal first" <|
            \_ ->
                compileDefinitionWithFlavor Optimize "module Main exposing (..)\nchoose character = case character of\n    'x' -> 1\n    _ -> 0" "choose"
                    |> Result.map (\text -> String.contains "'x' === character" text && not (String.contains ".valueOf" text))
                    |> Expect.equal (Ok True)
        , test "production Dict constructor tags retain their special negative values" <|
            \_ ->
                [ "RBNode_elm_builtin", "RBEmpty_elm_builtin" ]
                    |> List.indexedMap (\index name -> EmissionMode.tag { flavor = Optimize, fields = Dict.empty } ( "elm/core", "Dict", name ) index |> JS.renderExpression)
                    |> Expect.equal [ "-1", "-2" ]
        , test "production kernels use the same record field map as source expressions" <|
            \_ ->
                Kernel.compileWithMode
                    { flavor = Optimize, fields = Dict.singleton "message" "z" }
                    { modules = Dict.empty, kernels = Dict.empty, dependencies = Dict.empty, exposedModules = Dict.empty }
                    "elm/core"
                    "Elm.Kernel.Test"
                    "/*\n*/\nvar x__DEBUG = 1;\nvar y__PROD = __$message;\n"
                    |> Result.map .text
                    |> Expect.equal (Ok "\nvar x_UNUSED = 1;\nvar y = z;\n")
        , test "kernel debug branches retain development code and disable production code" <|
            \_ ->
                let
                    environment =
                        { modules = Dict.empty, kernels = Dict.empty, dependencies = Dict.empty, exposedModules = Dict.empty }
                in
                Kernel.compile environment "elm/core" "Elm.Kernel.Test" "/*\n*/\nvar x__DEBUG = __0_FIRST;\nvar y__PROD = __0_SECOND;\nvar obj = { __one: 1, __two: 2 };\n"
                    |> Result.map (\code -> ( code.text, code.dependencies ))
                    |> Expect.equal (Ok ( "\nvar x = 0;\nvar y_UNUSED = 1;\nvar obj = { a: 1, b: 2 };\n", Set.empty ))
        , test "kernel enumeration and field references are stable on repeated use" <|
            \_ ->
                Kernel.compile
                    { modules = Dict.empty, kernels = Dict.empty, dependencies = Dict.empty, exposedModules = Dict.empty }
                    "elm/core"
                    "Elm.Kernel.Test"
                    "/*\n*/\nvar a = __0_FIRST, b = __0_SECOND, c = __0_FIRST;\nvar d = __field, e = __other, f = __field;\n"
                    |> Result.map .text
                    |> Expect.equal (Ok "\nvar a = 0, b = 1, c = 0;\nvar d = a, e = b, f = a;\n")
        , test "kernel allocations remain stable after dictionary rotations" <|
            \_ ->
                let
                    tokens =
                        List.range 0 39 |> List.map (\index -> "__2_T" ++ String.fromInt index)

                    source =
                        "/*\n*/\n" ++ String.join "," (tokens ++ List.reverse tokens)

                    expected =
                        "\n" ++ String.join "," (List.map String.fromInt (List.range 0 39 ++ List.reverse (List.range 0 39)))
                in
                Kernel.compile
                    { modules = Dict.empty, kernels = Dict.empty, dependencies = Dict.empty, exposedModules = Dict.empty }
                    "elm/core"
                    "Elm.Kernel.Test"
                    source
                    |> Result.map .text
                    |> Expect.equal (Ok expected)
        , test "kernel enum families do not interfere with each other or fields" <|
            \_ ->
                Kernel.compile
                    { modules = Dict.empty, kernels = Dict.empty, dependencies = Dict.empty, exposedModules = Dict.empty }
                    "elm/core"
                    "Elm.Kernel.Test"
                    "/*\n*/\n__2_TEXT,__2_NODE,__2_KEYED_NODE,__2_CUSTOM,__2_TAGGER,__2_THUNK;\n__1_SUCCEED,__1_FAIL,__1_BINDING,__1_AND_THEN,__1_ON_ERROR,__1_RECEIVE;\n__3_ONE,__3_TWO,__foo,__bar,__3_ONE,__2_THUNK,__2_TEXT,__2_TAGGER,__1_SUCCEED;\n"
                    |> Result.map .text
                    |> Expect.equal (Ok "\n0,1,2,3,4,5;\n0,1,2,3,4,5;\n0,1,a,b,0,5,0,4,0;\n")
        ]
