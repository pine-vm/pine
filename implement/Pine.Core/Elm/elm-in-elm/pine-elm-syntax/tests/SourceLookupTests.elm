module SourceLookupTests exposing (suite)

import ElmSyntax.Concrete.File as File
import ElmSyntax.Concrete.Node as Node exposing (Node(..))
import ElmSyntax.Concrete.Parser.FromString
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SourceLookup as SourceLookup
import ElmSyntax.Path exposing (Path, Selection(..), Step(..))
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "structural paths and concrete source lookup"
        [ declarationSuite
        , expressionSuite
        , parenthesesSuite
        , recordSuite
        , patternSuite
        , typeAnnotationSuite
        , moduleAndImportSuite
        , documentationSuite
        , invalidPathSuite
        , lineEndingSuite
        , cursorSuite
        ]



-- Fixtures


sampleModule : String
sampleModule =
    """module Sample exposing (alfa, Beta(..), Gamma)

import Dict exposing (Dict)
import Set as SetAlias exposing (..)


type Beta
    = BetaOne Int
    | BetaTwo String Int


type alias Gamma =
    { first : Int
    , second : String
    }


alfa : Int -> Int
alfa delta =
    delta + 1
"""


expressionModule : String
expressionModule =
    """module Expr exposing (..)


run : Int -> Int
run arg =
    let
        helper first second =
            first + second

        other =
            [ 11, 22, 33 ]
    in
    case helper arg 3 of
        0 ->
            negate arg

        _ ->
            if arg > 0 then
                Dict.size other

            else
                (\\lambdaArg -> lambdaArg) arg
"""



-- Declarations


declarationSuite : Test
declarationSuite =
    Test.describe "declarations"
        [ Test.test "choice type declaration whole range" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 0 ]
                    SelectWhole
                    "type Beta\n    = BetaOne Int\n    | BetaTwo String Int"
        , Test.test "choice type declaration name" <|
            \_ ->
                expectSlice sampleModule [ StepDeclaration 0 ] SelectName "Beta"
        , Test.test "first constructor" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 0, StepConstructor 0 ]
                    SelectWhole
                    "BetaOne Int"
        , Test.test "second constructor name" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 0, StepConstructor 1 ]
                    SelectName
                    "BetaTwo"
        , Test.test "second constructor argument" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 0, StepConstructor 1, StepArgument 1 ]
                    SelectWhole
                    "Int"
        , Test.test "type alias name" <|
            \_ ->
                expectSlice sampleModule [ StepDeclaration 1 ] SelectName "Gamma"
        , Test.test "type alias type annotation" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 1, StepTypeAnnotation ]
                    SelectWhole
                    "{ first : Int\n    , second : String\n    }"
        , Test.test "function signature name" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 2, StepSignature ]
                    SelectName
                    "alfa"
        , Test.test "function signature type annotation" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 2, StepSignature, StepTypeAnnotation ]
                    SelectWhole
                    "Int -> Int"
        , Test.test "function implementation name" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 2, StepImplementation ]
                    SelectName
                    "alfa"
        , Test.test "function argument" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 2, StepImplementation, StepArgument 0 ]
                    SelectWhole
                    "delta"
        , Test.test "function body" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 2, StepImplementation, StepBody ]
                    SelectWhole
                    "delta + 1"
        , Test.test "declaration name of a function is the implementation name" <|
            \_ ->
                expectSlice sampleModule [ StepDeclaration 2 ] SelectName "alfa"
        , Test.test "port declaration signature" <|
            \_ ->
                expectSlice
                    "port module Ports exposing (..)\n\n\nport sendOut : String -> Cmd msg\n"
                    [ StepDeclaration 0, StepSignature ]
                    SelectName
                    "sendOut"
        , Test.test "infix declaration name" <|
            \_ ->
                expectSlice
                    "module Ops exposing (..)\n\n\ninfix right 5 (::) = cons\n"
                    [ StepDeclaration 0 ]
                    SelectName
                    "(::)"
        ]



-- Expressions


expressionSuite : Test
expressionSuite =
    Test.describe "expressions"
        [ Test.test "let declaration implementation name" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepLetDeclaration 0
                    , StepImplementation
                    ]
                    SelectName
                    "helper"
        , Test.test "let declaration argument" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepLetDeclaration 0
                    , StepImplementation
                    , StepArgument 1
                    ]
                    SelectWhole
                    "second"
        , Test.test "list item inside second let declaration" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepLetDeclaration 1
                    , StepImplementation
                    , StepBody
                    , StepChild 2
                    ]
                    SelectWhole
                    "33"
        , Test.test "case subject is child 0" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepBody
                    , StepChild 0
                    ]
                    SelectWhole
                    "helper arg 3"
        , Test.test "application function is child 0" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepBody
                    , StepChild 0
                    , StepChild 0
                    ]
                    SelectWhole
                    "helper"
        , Test.test "application second argument is child 2" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepBody
                    , StepChild 0
                    , StepChild 2
                    ]
                    SelectWhole
                    "3"
        , Test.test "case branch pattern" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepBody
                    , StepCaseBranch 0
                    , StepPattern
                    ]
                    SelectWhole
                    "0"
        , Test.test "case branch body" <|
            \_ ->
                expectSlice expressionModule
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepBody
                    , StepCaseBranch 0
                    , StepBody
                    ]
                    SelectWhole
                    "negate arg"
        , Test.test "if condition, then and else branches" <|
            \_ ->
                let
                    ifPath : Path
                    ifPath =
                        [ StepDeclaration 0
                        , StepImplementation
                        , StepBody
                        , StepBody
                        , StepCaseBranch 1
                        , StepBody
                        ]
                in
                Expect.equal
                    [ Just "arg > 0", Just "Dict.size other", Just "(\\lambdaArg -> lambdaArg) arg" ]
                    [ sliceAtPath expressionModule (ifPath ++ [ StepChild 0 ]) SelectWhole
                    , sliceAtPath expressionModule (ifPath ++ [ StepChild 1 ]) SelectWhole
                    , sliceAtPath expressionModule (ifPath ++ [ StepChild 2 ]) SelectWhole
                    ]
        , Test.test "qualified reference name and qualifier selections" <|
            \_ ->
                let
                    referencePath : Path
                    referencePath =
                        [ StepDeclaration 0
                        , StepImplementation
                        , StepBody
                        , StepBody
                        , StepCaseBranch 1
                        , StepBody
                        , StepChild 1
                        , StepChild 0
                        ]
                in
                Expect.equal
                    [ Just "Dict.size", Just "size", Just "Dict" ]
                    [ sliceAtPath expressionModule referencePath SelectWhole
                    , sliceAtPath expressionModule referencePath SelectName
                    , sliceAtPath expressionModule referencePath SelectQualifier
                    ]
        , Test.test "unqualified reference has no qualifier" <|
            \_ ->
                Expect.equal Nothing
                    (sliceAtPath expressionModule
                        [ StepDeclaration 0
                        , StepImplementation
                        , StepBody
                        , StepBody
                        , StepChild 0
                        , StepChild 0
                        ]
                        SelectQualifier
                    )
        , Test.test "lambda argument and body" <|
            \_ ->
                let
                    lambdaPath : Path
                    lambdaPath =
                        [ StepDeclaration 0
                        , StepImplementation
                        , StepBody
                        , StepBody
                        , StepCaseBranch 1
                        , StepBody
                        , StepChild 2
                        , StepChild 0
                        ]
                in
                Expect.equal
                    [ Just "lambdaArg", Just "lambdaArg" ]
                    [ sliceAtPath expressionModule (lambdaPath ++ [ StepArgument 0 ]) SelectWhole
                    , sliceAtPath expressionModule (lambdaPath ++ [ StepBody ]) SelectWhole
                    ]
        , Test.test "negation child" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv =\n    -someValue\n"
                    [ StepDeclaration 0, StepImplementation, StepBody, StepChild 0 ]
                    SelectWhole
                    "someValue"
        , Test.test "operator application operands" <|
            \_ ->
                let
                    operatorSource : String
                    operatorSource =
                        "module M exposing (..)\n\n\nv =\n    left ++ right\n"
                in
                Expect.equal
                    [ Just "left", Just "right" ]
                    [ sliceAtPath operatorSource
                        [ StepDeclaration 0, StepImplementation, StepBody, StepChild 0 ]
                        SelectWhole
                    , sliceAtPath operatorSource
                        [ StepDeclaration 0, StepImplementation, StepBody, StepChild 1 ]
                        SelectWhole
                    ]
        , Test.test "record access target" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv =\n    someRecord.field\n"
                    [ StepDeclaration 0, StepImplementation, StepBody, StepChild 0 ]
                    SelectWhole
                    "someRecord"
        , Test.test "tuple items" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv =\n    ( first, second, third )\n"
                    [ StepDeclaration 0, StepImplementation, StepBody, StepChild 1 ]
                    SelectWhole
                    "second"
        , Test.test "let destructuring pattern and body" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv =\n    let\n        ( left, right ) =\n            pair\n    in\n    left\n"

                    letDeclPath : Path
                    letDeclPath =
                        [ StepDeclaration 0, StepImplementation, StepBody, StepLetDeclaration 0 ]
                in
                Expect.equal
                    [ Just "( left, right )", Just "left", Just "pair" ]
                    [ sliceAtPath source (letDeclPath ++ [ StepPattern ]) SelectWhole
                    , sliceAtPath source (letDeclPath ++ [ StepPattern, StepChild 0 ]) SelectWhole
                    , sliceAtPath source (letDeclPath ++ [ StepBody ]) SelectWhole
                    ]
        ]



-- Parentheses


parenthesesSuite : Test
parenthesesSuite =
    Test.describe "concrete-only parentheses are transparent"
        [ Test.test "redundant parentheses are skipped and excluded from the range" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv =\n    (((inner)))\n"
                    [ StepDeclaration 0, StepImplementation, StepBody ]
                    SelectWhole
                    "inner"
        , Test.test "parenthesized argument resolves to the inner application" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv =\n    outer ((inner arg))\n"
                    [ StepDeclaration 0, StepImplementation, StepBody, StepChild 1 ]
                    SelectWhole
                    "inner arg"
        , Test.test "parenthesized pattern is skipped" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv arg =\n    case arg of\n        ((Just inner)) ->\n            inner\n\n        _ ->\n            arg\n"
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepCaseBranch 0
                    , StepPattern
                    ]
                    SelectWhole
                    "Just inner"
        ]



-- Records


recordSuite : Test
recordSuite =
    Test.describe "record fields keyed by name and occurrence"
        [ Test.test "fields are addressed by name regardless of source order" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv =\n    { zulu = 1, alfa = 2 }\n"

                    bodyPath : Path
                    bodyPath =
                        [ StepDeclaration 0, StepImplementation, StepBody ]
                in
                Expect.equal
                    [ Just "alfa = 2", Just "2", Just "zulu = 1" ]
                    [ sliceAtPath source (bodyPath ++ [ StepRecordField "alfa" 0 ]) SelectWhole
                    , sliceAtPath source (bodyPath ++ [ StepRecordField "alfa" 0, StepChild 0 ]) SelectWhole
                    , sliceAtPath source (bodyPath ++ [ StepRecordField "zulu" 0 ]) SelectWhole
                    ]
        , Test.test "duplicate field names are disambiguated by occurrence" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv =\n    { same = 1, same = 2 }\n"

                    bodyPath : Path
                    bodyPath =
                        [ StepDeclaration 0, StepImplementation, StepBody ]
                in
                Expect.equal
                    [ Just "1", Just "2", Nothing ]
                    [ sliceAtPath source (bodyPath ++ [ StepRecordField "same" 0, StepChild 0 ]) SelectWhole
                    , sliceAtPath source (bodyPath ++ [ StepRecordField "same" 1, StepChild 0 ]) SelectWhole
                    , sliceAtPath source (bodyPath ++ [ StepRecordField "same" 2, StepChild 0 ]) SelectWhole
                    ]
        , Test.test "record update record name and setters" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv =\n    { base | zulu = 1 }\n"

                    bodyPath : Path
                    bodyPath =
                        [ StepDeclaration 0, StepImplementation, StepBody ]
                in
                Expect.equal
                    [ Just "base", Just "1" ]
                    [ sliceAtPath source bodyPath SelectName
                    , sliceAtPath source (bodyPath ++ [ StepRecordField "zulu" 0, StepChild 0 ]) SelectWhole
                    ]
        , Test.test "record type field name and type" <|
            \_ ->
                Expect.equal
                    [ Just "second", Just "String" ]
                    [ sliceAtPath sampleModule
                        [ StepDeclaration 1, StepTypeAnnotation, StepRecordField "second" 0 ]
                        SelectName
                    , sliceAtPath sampleModule
                        [ StepDeclaration 1, StepTypeAnnotation, StepRecordField "second" 0, StepChild 0 ]
                        SelectWhole
                    ]
        ]



-- Patterns


patternSuite : Test
patternSuite =
    Test.describe "patterns"
        [ Test.test "named pattern name and qualifier" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv arg =\n    case arg of\n        Maybe.Just inner ->\n            inner\n\n        _ ->\n            arg\n"

                    patternPath : Path
                    patternPath =
                        [ StepDeclaration 0
                        , StepImplementation
                        , StepBody
                        , StepCaseBranch 0
                        , StepPattern
                        ]
                in
                Expect.equal
                    [ Just "Maybe.Just inner", Just "Just", Just "Maybe", Just "inner" ]
                    [ sliceAtPath source patternPath SelectWhole
                    , sliceAtPath source patternPath SelectName
                    , sliceAtPath source patternPath SelectQualifier
                    , sliceAtPath source (patternPath ++ [ StepChild 0 ]) SelectWhole
                    ]
        , Test.test "record pattern field names" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv { alfa, bravo } =\n    alfa\n"
                    [ StepDeclaration 0, StepImplementation, StepArgument 0, StepChild 1 ]
                    SelectName
                    "bravo"
        , Test.test "as pattern inner and alias name" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv ((alfa) as bravo) =\n    bravo\n"

                    patternPath : Path
                    patternPath =
                        [ StepDeclaration 0, StepImplementation, StepArgument 0 ]
                in
                Expect.equal
                    [ Just "bravo", Just "alfa" ]
                    [ sliceAtPath source patternPath SelectName
                    , sliceAtPath source (patternPath ++ [ StepChild 0 ]) SelectWhole
                    ]
        , Test.test "uncons pattern head and tail" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv arg =\n    case arg of\n        head :: tail ->\n            head\n\n        _ ->\n            arg\n"

                    patternPath : Path
                    patternPath =
                        [ StepDeclaration 0
                        , StepImplementation
                        , StepBody
                        , StepCaseBranch 0
                        , StepPattern
                        ]
                in
                Expect.equal
                    [ Just "head", Just "tail" ]
                    [ sliceAtPath source (patternPath ++ [ StepChild 0 ]) SelectWhole
                    , sliceAtPath source (patternPath ++ [ StepChild 1 ]) SelectWhole
                    ]
        , Test.test "list pattern items" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv arg =\n    case arg of\n        [ alfa, bravo ] ->\n            alfa\n\n        _ ->\n            arg\n"
                    [ StepDeclaration 0
                    , StepImplementation
                    , StepBody
                    , StepCaseBranch 0
                    , StepPattern
                    , StepChild 1
                    ]
                    SelectWhole
                    "bravo"
        ]



-- Type annotations


typeAnnotationSuite : Test
typeAnnotationSuite =
    Test.describe "type annotations"
        [ Test.test "function type annotation operands" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv : Dict.Dict String Int -> Maybe Int\nv d =\n    Nothing\n"

                    annotationPath : Path
                    annotationPath =
                        [ StepDeclaration 0, StepSignature, StepTypeAnnotation ]
                in
                Expect.equal
                    [ Just "Dict.Dict String Int", Just "Maybe Int" ]
                    [ sliceAtPath source (annotationPath ++ [ StepChild 0 ]) SelectWhole
                    , sliceAtPath source (annotationPath ++ [ StepChild 1 ]) SelectWhole
                    ]
        , Test.test "typed name, qualifier and arguments" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\nv : Dict.Dict String Int -> Maybe Int\nv d =\n    Nothing\n"

                    typedPath : Path
                    typedPath =
                        [ StepDeclaration 0, StepSignature, StepTypeAnnotation, StepChild 0 ]
                in
                Expect.equal
                    [ Just "Dict", Just "Dict", Just "String", Just "Int" ]
                    [ sliceAtPath source typedPath SelectName
                    , sliceAtPath source typedPath SelectQualifier
                    , sliceAtPath source (typedPath ++ [ StepChild 0 ]) SelectWhole
                    , sliceAtPath source (typedPath ++ [ StepChild 1 ]) SelectWhole
                    ]
        , Test.test "tupled type annotation items" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nv : ( Int, String )\nv =\n    ( 1, \"\" )\n"
                    [ StepDeclaration 0, StepSignature, StepTypeAnnotation, StepChild 1 ]
                    SelectWhole
                    "String"
        , Test.test "generic record type" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\ntype alias WithBase base =\n    { base | alfa : Int }\n"

                    annotationPath : Path
                    annotationPath =
                        [ StepDeclaration 0, StepTypeAnnotation ]
                in
                Expect.equal
                    [ Just "base", Just "Int" ]
                    [ sliceAtPath source annotationPath SelectName
                    , sliceAtPath source (annotationPath ++ [ StepRecordField "alfa" 0, StepChild 0 ]) SelectWhole
                    ]
        ]



-- Module and imports


moduleAndImportSuite : Test
moduleAndImportSuite =
    Test.describe "module definition and imports"
        [ Test.test "module name" <|
            \_ ->
                expectSlice sampleModule
                    [ StepModuleDefinition, StepModuleName ]
                    SelectWhole
                    "Sample"
        , Test.test "module exposing entries" <|
            \_ ->
                Expect.equal
                    [ Just "alfa", Just "Beta(..)", Just "Beta", Just "Gamma" ]
                    [ sliceAtPath sampleModule [ StepModuleDefinition, StepExposingEntry 0 ] SelectWhole
                    , sliceAtPath sampleModule [ StepModuleDefinition, StepExposingEntry 1 ] SelectWhole
                    , sliceAtPath sampleModule [ StepModuleDefinition, StepExposingEntry 1 ] SelectName
                    , sliceAtPath sampleModule [ StepModuleDefinition, StepExposingEntry 2 ] SelectWhole
                    ]
        , Test.test "import module name and exposing entry" <|
            \_ ->
                Expect.equal
                    [ Just "Dict", Just "Dict" ]
                    [ sliceAtPath sampleModule [ StepImport 0, StepModuleName ] SelectWhole
                    , sliceAtPath sampleModule [ StepImport 0, StepExposingEntry 0 ] SelectWhole
                    ]
        , Test.test "import alias" <|
            \_ ->
                Expect.equal
                    [ Just "Set", Just "SetAlias", Nothing ]
                    [ sliceAtPath sampleModule [ StepImport 1, StepModuleName ] SelectWhole
                    , sliceAtPath sampleModule [ StepImport 1, StepModuleAlias ] SelectWhole
                    , sliceAtPath sampleModule [ StepImport 1, StepExposingEntry 0 ] SelectWhole
                    ]
        , Test.test "import without alias has no alias node" <|
            \_ ->
                Expect.equal Nothing
                    (sliceAtPath sampleModule [ StepImport 0, StepModuleAlias ] SelectWhole)
        ]



-- Documentation


documentationSuite : Test
documentationSuite =
    Test.describe "documentation selections"
        [ Test.test "declaration without documentation is expanded to the line start" <|
            \_ ->
                expectSlice sampleModule
                    [ StepDeclaration 2 ]
                    SelectDeclarationWithoutDocumentation
                    "alfa : Int -> Int\nalfa delta =\n    delta + 1"
        , Test.test "documented declaration excludes its documentation" <|
            \_ ->
                let
                    source : String
                    source =
                        "module M exposing (..)\n\n\n{-| Doc comment.\n-}\nalfa : Int\nalfa =\n    1\n"
                in
                Expect.equal
                    [ Just "alfa : Int\nalfa =\n    1", Just "{-| Doc comment.\n-}" ]
                    [ sliceAtPath source [ StepDeclaration 0 ] SelectDeclarationWithoutDocumentation
                    , sliceAtPath source [ StepDeclaration 0 ] SelectDocumentation
                    ]
        , Test.test "declaration without documentation has no documentation range" <|
            \_ ->
                Expect.equal Nothing
                    (sliceAtPath sampleModule [ StepDeclaration 2 ] SelectDocumentation)
        ]



-- Invalid paths


invalidPathSuite : Test
invalidPathSuite =
    Test.describe "invalid paths resolve to Nothing"
        [ Test.test "declaration index out of bounds" <|
            \_ ->
                Expect.equal Nothing (sliceAtPath sampleModule [ StepDeclaration 99 ] SelectWhole)
        , Test.test "negative index" <|
            \_ ->
                Expect.equal Nothing (sliceAtPath sampleModule [ StepDeclaration -1 ] SelectWhole)
        , Test.test "step not applicable to node kind" <|
            \_ ->
                Expect.equal Nothing
                    (sliceAtPath sampleModule [ StepDeclaration 2, StepConstructor 0 ] SelectWhole)
        , Test.test "signature step on a declaration without signature" <|
            \_ ->
                Expect.equal Nothing
                    (sliceAtPath
                        "module M exposing (..)\n\n\nalfa =\n    1\n"
                        [ StepDeclaration 0, StepSignature ]
                        SelectWhole
                    )
        , Test.test "unknown record field name" <|
            \_ ->
                Expect.equal Nothing
                    (sliceAtPath sampleModule
                        [ StepDeclaration 1, StepTypeAnnotation, StepRecordField "missing" 0 ]
                        SelectWhole
                    )
        , Test.test "name selection is not available for every node" <|
            \_ ->
                Expect.equal Nothing
                    (sliceAtPath sampleModule
                        [ StepDeclaration 2, StepImplementation, StepBody ]
                        SelectName
                    )
        ]



-- Line endings and comments


lineEndingSuite : Test
lineEndingSuite =
    Test.describe "ranges with comments and shifted lines"
        [ Test.test "line comments before a declaration do not shift its range" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\n-- a leading comment\n-- another one\nalfa =\n    1\n"
                    [ StepDeclaration 0, StepImplementation ]
                    SelectName
                    "alfa"
        , Test.test "block comment inside an expression" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nalfa =\n    outer {- inline -} inner\n"
                    [ StepDeclaration 0, StepImplementation, StepBody, StepChild 1 ]
                    SelectWhole
                    "inner"
        , Test.test "multiline string literal range" <|
            \_ ->
                expectSlice
                    "module M exposing (..)\n\n\nalfa =\n    \"\"\"line one\nline two\"\"\"\n"
                    [ StepDeclaration 0, StepImplementation, StepBody ]
                    SelectWhole
                    "\"\"\"line one\nline two\"\"\""
        , Test.test "same declaration shifted by extra lines resolves to shifted range" <|
            \_ ->
                let
                    withoutShift : String
                    withoutShift =
                        "module M exposing (..)\n\n\nalfa =\n    1\n"

                    withShift : String
                    withShift =
                        "module M exposing (..)\n\n\n\n\n\nalfa =\n    1\n"

                    path : Path
                    path =
                        [ StepDeclaration 0, StepImplementation ]
                in
                Expect.equal
                    ( Just { start = { row = 4, column = 1 }, end = { row = 4, column = 5 } }
                    , Just { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } }
                    )
                    ( rangeAtPathIn withoutShift path SelectName
                    , rangeAtPathIn withShift path SelectName
                    )
        ]



-- Cursor lookup


cursorSuite : Test
cursorSuite =
    Test.describe "cursor to path"
        [ Test.test "location inside a reference selects that reference" <|
            \_ ->
                Expect.equal
                    (Just
                        [ StepDeclaration 2
                        , StepImplementation
                        , StepBody
                        , StepChild 0
                        ]
                    )
                    (pathAtLocationIn sampleModule { row = 20, column = 5 })
        , Test.test "cursor path and range agree" <|
            \_ ->
                let
                    location : Location
                    location =
                        { row = 20, column = 5 }
                in
                case pathAtLocationIn sampleModule location of
                    Nothing ->
                        Expect.fail "expected a path"

                    Just path ->
                        Expect.equal (Just "delta")
                            (sliceAtPath sampleModule path SelectWhole)
        , Test.test "location on a declaration name selects the implementation" <|
            \_ ->
                Expect.equal
                    (Just [ StepDeclaration 2, StepImplementation ])
                    (pathAtLocationIn sampleModule { row = 19, column = 2 })
        , Test.test "location in the module exposing list selects the entry" <|
            \_ ->
                Expect.equal
                    (Just [ StepModuleDefinition, StepExposingEntry 0 ])
                    (pathAtLocationIn sampleModule { row = 1, column = 25 })
        , Test.test "location outside any declaration selects the file" <|
            \_ ->
                Expect.equal (Just []) (pathAtLocationIn sampleModule { row = 2, column = 1 })
        ]



-- Helpers


parseSample : String -> Result String File.File
parseSample source =
    ElmSyntax.Concrete.Parser.FromString.parseFile source


rangeAtPathIn : String -> Path -> Selection -> Maybe Range
rangeAtPathIn source path selection =
    case parseSample source of
        Err _ ->
            Nothing

        Ok file ->
            SourceLookup.rangeAtPath path selection file


pathAtLocationIn : String -> Location -> Maybe Path
pathAtLocationIn source location =
    case parseSample source of
        Err _ ->
            Nothing

        Ok file ->
            Just (SourceLookup.pathAtLocation location file)


sliceAtPath : String -> Path -> Selection -> Maybe String
sliceAtPath source path selection =
    case parseSample source of
        Err _ ->
            Nothing

        Ok file ->
            case SourceLookup.rangeAtPath path selection file of
                Nothing ->
                    Nothing

                Just range ->
                    Just (sliceRange source range)


expectSlice : String -> Path -> Selection -> String -> Expect.Expectation
expectSlice source path selection expected =
    case parseSample source of
        Err error ->
            Expect.fail ("Failed to parse fixture: " ++ error)

        Ok file ->
            case SourceLookup.rangeAtPath path selection file of
                Nothing ->
                    Expect.fail ("No range for path, expected " ++ expected)

                Just range ->
                    Expect.equal expected (sliceRange source range)


sliceRange : String -> Range -> String
sliceRange source range =
    let
        lines : List String
        lines =
            String.split "\n" source

        selected : List String
        selected =
            lines
                |> List.indexedMap (\index line -> ( index + 1, line ))
                |> List.filter (\( row, _ ) -> row >= range.start.row && row <= range.end.row)
                |> List.map
                    (\( row, line ) ->
                        let
                            fromColumn : Int
                            fromColumn =
                                if row == range.start.row then
                                    range.start.column - 1

                                else
                                    0

                            toColumn : Int
                            toColumn =
                                if row == range.end.row then
                                    range.end.column - 1

                                else
                                    String.length line
                        in
                        String.slice fromColumn toColumn line
                    )
    in
    String.join "\n" selected
