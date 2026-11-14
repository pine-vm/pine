module ElmMake.Model exposing (..)

import Dict exposing (Dict)
import ElmMake.SourceLocation as Location
import ElmSyntax.Abstract.Expression exposing (Expression)
import ElmSyntax.Abstract.File exposing (File)
import ElmSyntax.Abstract.Pattern exposing (Pattern)
import ElmSyntax.Concrete.File as Concrete
import ElmSyntax.Concrete.Range as Range
import Set exposing (Set)


type OutputType
    = Html
    | JavaScript


type Flavor
    = Development
    | Debug
    | Optimize


type alias Mode =
    { flavor : Flavor, fields : Dict String String }


developmentMode : Mode
developmentMode =
    { flavor = Development, fields = Dict.empty }


type MakeError
    = CompilerErrors CompilerProblem (List CompilerProblem)
    | UnsupportedFeature UnsupportedProblem
    | InternalFailure InternalProblem


type alias CompilerProblem =
    { location : Location.Location
    , kind : ProblemKind
    }


type ProblemKind
    = SyntaxProblem String
    | NamingProblem NameProblem
    | ManifestProblem String
    | NoManifest
    | EntryPointNotFound (List String)
    | NoMain
    | CyclicTypeAlias String
    | DebugRemnants (List String)


type NameProblem
    = UnknownValue (List String) String
    | UnknownType (List String) String
    | MissingModule String
    | AmbiguousModule String
    | NotExposed String String
    | AmbiguousName String
    | DuplicateModule String
    | DuplicateDeclaration String
    | DuplicateBinding (List String)
    | ShadowedBinding String
    | WrongConstructorArity String Int Int
    | NotAConstructor String
    | InvalidExposure String String
    | RestrictedKernelImport


type alias UnsupportedProblem =
    { location : Location.Location
    , feature : UnsupportedCapability
    }


type UnsupportedCapability
    = Ports
    | EffectManagers
    | Glsl
    | CustomOperator String
    | RecursiveValueInitialization
    | RecursiveMainType String
    | PartialMainType String
    | MainTypeSubstitution
    | HigherOrderMain
    | UnsupportedMainType String
    | NonHtmlMain
    | UnsupportedFlags
    | RefutableArgumentPattern
    | RefutableLetPattern
    | ConstructorFieldCount Int
    | TupleArity Int
    | FloatPatterns
    | IncompleteCase


type alias InternalProblem =
    { kind : InternalProblemKind
    , context : String
    }


type InternalProblemKind
    = MissingPackageSources
    | InvalidPackageSources
    | InvalidRequest
    | InvalidSourceLocation
    | MissingGraphNode
    | CompilerInvariant


type alias SourceFile =
    { path : List String
    , content : String
    }


type alias Package =
    { name : String
    , version : String
    , files : List SourceFile
    }


type alias Request =
    { files : List SourceFile
    , manifestPath : List String
    , entryPoint : List String
    , outputType : OutputType
    , flavor : Flavor
    , packages : List Package
    }


type alias Module =
    { package : String
    , name : String
    , path : String
    , syntax : File
    , origins : Dict String SourceOrigin
    , file : Location.FileId
    , source : Maybe Concrete.File
    }


type alias Global =
    ( String, String, String )


type alias EmittedCode =
    { text : String, dependencies : Set Global }


type alias Environment =
    { modules : Dict ( String, String ) Module
    , kernels : Dict ( String, String ) String
    , dependencies : Dict String (List String)
    , exposedModules : Dict String (List String)
    }


type Definition
    = Function (List Pattern) Expression
    | Constructor Int
    | EffectLeaf


type alias Scope =
    { module_ : Module
    , locals : Dict String String
    , environment : Environment
    , origin : SourceOrigin
    , mode : Mode
    , cyclicValues : Set Global
    }


type SourceOrigin
    = SourceOrigin (Maybe String) (List SourceOrigin) (Maybe Range.Range)


emptyOrigin : SourceOrigin
emptyOrigin =
    SourceOrigin Nothing [] Nothing


childOrigin : Int -> SourceOrigin -> SourceOrigin
childOrigin index (SourceOrigin _ children _) =
    List.drop index children |> List.head |> Maybe.withDefault emptyOrigin


literalSpelling : SourceOrigin -> Maybe String
literalSpelling (SourceOrigin spelling _ _) =
    spelling


globalName : Global -> String
globalName ( package, moduleName, name ) =
    if String.startsWith "Elm.Kernel." moduleName then
        "_" ++ String.dropLeft 11 moduleName ++ "_" ++ name

    else
        "$" ++ String.replace "-" "_" (String.replace "/" "$" package) ++ "$" ++ String.replace "." "$" moduleName ++ "$" ++ name


cyclicName : Global -> String
cyclicName ( package, moduleName, name ) =
    globalName ( package, moduleName, "cyclic$" ++ name )


traverse : (a -> Result error b) -> List a -> Result error (List b)
traverse map values =
    List.foldr
        (\value rest -> Result.map2 (::) (map value) rest)
        (Ok [])
        values


foldResult : (a -> b -> Result error b) -> b -> List a -> Result error b
foldResult step initial items =
    case items of
        [] ->
            Ok initial

        first :: rest ->
            step first initial |> Result.andThen (\next -> foldResult step next rest)


originRange : SourceOrigin -> Maybe Range.Range
originRange (SourceOrigin _ _ range) =
    range
