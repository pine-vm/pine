using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Focused regression test that mirrors the failure of
/// <see cref="ElmLanguageServiceTests.References_request_finds_usage_across_modules"/>
/// (compiled-VM returns <c>TextDocumentReferencesResponse []</c> while the
/// interpreter path returns the expected non-empty list) while exercising
/// only the smallest possible Elm source.
/// <para>
/// <b>Reduction.</b> The original scenario uses two Elm modules and a
/// cross-module reference. The reduction below uses a <i>single</i> module
/// containing a self-recursive function. The compiled-VM still returns
/// <c>TextDocumentReferencesResponse []</c> for every queried position
/// (declaration site, recursive call site, type signature, exposing list);
/// the interpreter path of the same scenario returns a non-empty response.
/// </para>
/// <para>
/// <b>Where the divergence is.</b> The probe
/// <see cref="Probe_hoverItemsFromParsedModule_fromDeclarations"/> calls
/// <c>LanguageService.hoverItemsFromParsedModule</c> directly and prints
/// the first triple of <c>.fromDeclarations</c>. On the compiled-VM path
/// the triple comes back as
/// <code>
/// (Range [1,26] [1,32], &lt;pine_blob 28 bytes&gt;, [ ParseAndEval (Literal (Conditional ...)) ])
/// </code>
/// The Range (slot 1) is correct and identifies the <c>helper</c> token
/// in the <c>exposing (helper)</c> clause. Slots 2 and 3, however, are not
/// the expected <c>LocationInFile</c> and <c>String</c>. The 28-byte blob
/// in slot 2 is the seven Unicode code points of the string
/// <c>"Literal"</c> (Pine encodes each Unicode code point as a 4-byte
/// integer blob). Slot 3 is a one-element list containing an
/// <i>encoded Pine Expression</i> AST.
/// </para>
/// <para>
/// Together <c>(BlobValue("Literal"), ListValue([encodedExpr]))</c> is
/// exactly the Pine encoding of the Pine Expression node <c>Literal X</c>:
/// <c>EncodeExpressionAsValue(Literal X) == List [StringValue("Literal"), X]</c>.
/// In other words, the last two slots of the constructed triple
/// <c>(forNameInModuleRange, sourceLocation, hover)</c> in
/// <c>getForHoversForReferenceNode</c> are the unpacked two list-elements
/// of a single Pine-Expression-as-value. That is consistent with
/// emission producing the <i>function value</i> (or some encoded-expression
/// thunk) of <c>getHoverForFunctionOrName</c> in place of its applied
/// result, then having the surrounding tuple construction spread the
/// 2-element value across slots 2 and 3 of the 3-tuple.
/// </para>
/// <para>
/// Ruled out (verified by isolated probes during the bisection): record
/// destructure of <c>{ fromDeclarations }</c>, 3-tuple destructure with a
/// custom-type middle slot, the 3-level <c>LocationInFile / DeclarationRange
/// / Range</c> lambda destructure used by <c>provideDefinition</c>, the
/// <c>Just ( mb, loc, CompletionItem _ _ _ doc ) -> Just ( loc, doc )</c>
/// destructure pattern from <c>getHoverForFunctionOrName</c> on synthetic
/// data, optimization rounds (still fails with
/// <c>maxOptimizationRounds: 0</c>), inlining (already disabled), and the
/// <c>.fieldName</c> record-accessor leading-dot bug which was fixed
/// separately by commit <c>d534d42a</c>.
/// </para>
/// </summary>
public class LanguageServiceReferencesEmptyResponseRegressionTests
{
    private const string TestModuleText =
        """"
        module Probe exposing (..)

        import Common
        import Dict
        import Frontend.MonacoEditor
        import LanguageService
        import LanguageServiceInterface


        initState : LanguageService.LanguageServiceState
        initState =
            LanguageService.initLanguageServiceState []


        addWorkspaceFile :
            String
            -> String
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        addWorkspaceFile filePath fileText state =
            case LanguageService.handleRequestInCurrentWorkspace
                (LanguageServiceInterface.AddWorkspaceFileRequest
                    filePath
                    { asBase64 = ""
                    , asText = Just fileText
                    }
                )
                state
            of
                ( Ok response, newState ) ->
                    ( response, newState )

                ( Err err, newState ) ->
                    ( LanguageServiceInterface.ProvideHoverResponse [ "Error: " ++ err ], newState )


        textDocumentReferences :
            String
            -> Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        textDocumentReferences filePath lineNumber column state =
            case LanguageService.handleRequestInCurrentWorkspace
                (LanguageServiceInterface.TextDocumentReferencesRequest
                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation filePath
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state
            of
                ( Ok response, newState ) ->
                    ( response, newState )

                ( Err err, newState ) ->
                    ( LanguageServiceInterface.ProvideHoverResponse [ "Error: " ++ err ], newState )


        provideDefinition :
            String
            -> Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        provideDefinition filePath lineNumber column state =
            case LanguageService.handleRequestInCurrentWorkspace
                (LanguageServiceInterface.ProvideDefinitionRequest
                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation filePath
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state
            of
                ( Ok response, newState ) ->
                    ( response, newState )

                ( Err err, newState ) ->
                    ( LanguageServiceInterface.ProvideHoverResponse [ "Error: " ++ err ], newState )


        renderFromDeclarations :
            String
            -> LanguageService.LanguageServiceState
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        renderFromDeclarations fileUri state =
            case Dict.get fileUri state.documentCache of
                Nothing ->
                    []

                Just cacheItem ->
                    case cacheItem.parsedFileLastSuccess of
                        Nothing ->
                            []

                        Just parsed ->
                            let
                                items =
                                    LanguageService.hoverItemsFromParsedModule
                                        ( parsed.syntax
                                        , parsed.completionItems
                                        , LanguageServiceInterface.WorkspaceFileLocation fileUri
                                        )
                                        state
                            in
                            items.fromDeclarations


        probe_synthetic_triple_identity :
            Int
            -> ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_synthetic_triple_identity _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )
            in
            ( r, sl, "doc-identity" )


        probe_synthetic_triple_via_maybe_destructure :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_synthetic_triple_via_maybe_destructure _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                m =
                    Just ( sl, "doc-maybe" )
            in
            case m of
                Nothing ->
                    []

                Just ( srcLoc, hov ) ->
                    [ ( r, srcLoc, hov ) ]


        probe_synthetic_triple_via_listMapFind :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_synthetic_triple_via_listMapFind _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                wrap : Int -> Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                wrap n =
                    if n == 1 then
                        Just ( sl, "doc-listMapFind" )

                    else
                        Nothing

                lookup : Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                lookup =
                    Common.listMapFind wrap [ 0, 1, 2 ]
            in
            case lookup of
                Nothing ->
                    []

                Just ( srcLoc, hov ) ->
                    [ ( r, srcLoc, hov ) ]


        probe_synthetic_via_assocListGet_with_completionItem_destructure :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_synthetic_via_assocListGet_with_completionItem_destructure _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                ci =
                    LanguageService.CompletionItem "labelL" "insertI" Frontend.MonacoEditor.FunctionCompletionItemKind "doc-via-assoc"

                items :
                    List
                        ( String
                        , ( Maybe LanguageService.Range
                          , LanguageService.LocationInFile LanguageService.DeclarationRange
                          , LanguageService.CompletionItem
                          )
                        )
                items =
                    [ ( "helper"
                      , ( Nothing
                        , sl
                        , ci
                        )
                      )
                    ]

                getHoverForFunctionOrName : String -> Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getHoverForFunctionOrName name =
                    case Common.assocListGet name items of
                        Nothing ->
                            Nothing

                        Just ( maybeFilterRange, locationUnderFilePath, LanguageService.CompletionItem _ _ _ completionItemDocumentation ) ->
                            case maybeFilterRange of
                                Nothing ->
                                    Just ( locationUnderFilePath, completionItemDocumentation )

                                Just _ ->
                                    Just ( locationUnderFilePath, completionItemDocumentation )

                getForHoversForReferenceNode : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getForHoversForReferenceNode name =
                    case getHoverForFunctionOrName name of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( r, sourceLocation, hover ) ]
            in
            List.concatMap getForHoversForReferenceNode [ "helper" ]


        probe_no_assocListGet_no_completionItem :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_no_assocListGet_no_completionItem _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                getHoverForFunctionOrName : String -> Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getHoverForFunctionOrName _ =
                    Just ( sl, "doc-no-assoc" )

                getForHoversForReferenceNode : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getForHoversForReferenceNode name =
                    case getHoverForFunctionOrName name of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( r, sourceLocation, hover ) ]
            in
            List.concatMap getForHoversForReferenceNode [ "helper" ]


        probe_assocListGet_no_completionItem_destructure :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_assocListGet_no_completionItem_destructure _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                items : List ( String, ( LanguageService.LocationInFile LanguageService.DeclarationRange, String ) )
                items =
                    [ ( "helper", ( sl, "doc-assoc-2tuple" ) ) ]

                getHoverForFunctionOrName : String -> Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getHoverForFunctionOrName name =
                    Common.assocListGet name items

                getForHoversForReferenceNode : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getForHoversForReferenceNode name =
                    case getHoverForFunctionOrName name of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( r, sourceLocation, hover ) ]
            in
            List.concatMap getForHoversForReferenceNode [ "helper" ]


        probe_no_assocListGet_with_completionItem_destructure :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_no_assocListGet_with_completionItem_destructure _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                ci =
                    LanguageService.CompletionItem "labelL" "insertI" Frontend.MonacoEditor.FunctionCompletionItemKind "doc-no-assoc-with-ci"

                getHoverForFunctionOrName : String -> Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getHoverForFunctionOrName _ =
                    let
                        threeTuple =
                            ( Nothing, sl, ci )
                    in
                    case threeTuple of
                        ( _, locationUnderFilePath, LanguageService.CompletionItem _ _ _ completionItemDocumentation ) ->
                            Just ( locationUnderFilePath, completionItemDocumentation )

                getForHoversForReferenceNode : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getForHoversForReferenceNode name =
                    case getHoverForFunctionOrName name of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( r, sourceLocation, hover ) ]
            in
            List.concatMap getForHoversForReferenceNode [ "helper" ]


        probe_letFn_direct_call_no_concatMap :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_letFn_direct_call_no_concatMap _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                getHoverForFunctionOrName : String -> Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getHoverForFunctionOrName _ =
                    Just ( sl, "doc-direct" )

                getForHoversForReferenceNode : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getForHoversForReferenceNode name =
                    case getHoverForFunctionOrName name of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( r, sourceLocation, hover ) ]
            in
            getForHoversForReferenceNode "helper"


        probe_letFn_concatMap_no_closure :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_letFn_concatMap_no_closure _ =
            let
                getForHoversForReferenceNode : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                getForHoversForReferenceNode _ =
                    let
                        fl =
                            LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                        dr =
                            LanguageService.DeclarationRange
                                (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                                []

                        sl =
                            LanguageService.LocationInFile fl dr

                        r =
                            LanguageService.Range ( 7, 7 ) ( 7, 13 )

                        m =
                            Just ( sl, "doc-no-closure" )
                    in
                    case m of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( r, sourceLocation, hover ) ]
            in
            List.concatMap getForHoversForReferenceNode [ "helper" ]


        probe_topLevelFn_concatMap_with_args :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_topLevelFn_concatMap_with_args _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )
            in
            List.concatMap (topLevelGetHovers sl r) [ "helper" ]


        probe_letFn_concatMap_with_closure_minimal :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_letFn_concatMap_with_closure_minimal _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                helperFn : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                helperFn _ =
                    [ ( r, sl, "doc-min-closure" ) ]
            in
            List.concatMap helperFn [ "helper" ]


        topLevelGetHovers :
            LanguageService.LocationInFile LanguageService.DeclarationRange
            -> LanguageService.Range
            -> String
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        topLevelGetHovers sl r _ =
            [ ( r, sl, "doc-toplevel" ) ]


        probe_single_letFn_with_internal_maybe :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_single_letFn_with_internal_maybe _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                fn : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                fn _ =
                    let
                        m =
                            Just ( sl, "doc-single-with-maybe" )
                    in
                    case m of
                        Nothing ->
                            []

                        Just ( s, h ) ->
                            [ ( r, s, h ) ]
            in
            fn "x"


        probe_two_letFns_inner_returns_plain :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_two_letFns_inner_returns_plain _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                inner : String -> LanguageService.LocationInFile LanguageService.DeclarationRange
                inner _ =
                    sl

                outer : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                outer name =
                    [ ( r, inner name, "doc-two-plain" ) ]
            in
            outer "x"


        probe_two_letFns_inner_returns_just_pair :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_two_letFns_inner_returns_just_pair _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                inner : String -> Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                inner _ =
                    Just ( sl, "doc-two-just-pair" )

                outer : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                outer name =
                    case inner name of
                        Nothing ->
                            []

                        Just ( s, h ) ->
                            [ ( r, s, h ) ]
            in
            outer "x"


        probe_two_letFns_inner_returns_just_pair_int :
            Int
            -> List ( LanguageService.Range, Int, String )
        probe_two_letFns_inner_returns_just_pair_int _ =
            let
                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                inner : String -> Maybe ( Int, String )
                inner _ =
                    Just ( 42, "doc-int-pair" )

                outer : String -> List ( LanguageService.Range, Int, String )
                outer name =
                    case inner name of
                        Nothing ->
                            []

                        Just ( s, h ) ->
                            [ ( r, s, h ) ]
            in
            outer "x"


        probe_two_letFns_inner_no_args :
            Int
            -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
        probe_two_letFns_inner_no_args _ =
            let
                fl =
                    LanguageServiceInterface.WorkspaceFileLocation "src/X.elm"

                dr =
                    LanguageService.DeclarationRange
                        (LanguageService.Range ( 1, 1 ) ( 1, 5 ))
                        []

                sl =
                    LanguageService.LocationInFile fl dr

                r =
                    LanguageService.Range ( 7, 7 ) ( 7, 13 )

                inner : Maybe ( LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                inner =
                    Just ( sl, "doc-no-args" )

                outer : String -> List ( LanguageService.Range, LanguageService.LocationInFile LanguageService.DeclarationRange, String )
                outer _ =
                    case inner of
                        Nothing ->
                            []

                        Just ( s, h ) ->
                            [ ( r, s, h ) ]
            in
            outer "x"
        """";

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(() => BuildEnv());

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildEnv()
    {
        var bundledTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            bundledTree.GetNodeAtPath(["elm-kernel-modules"])
            ?? throw new Exception("Did not find elm-kernel-modules");

        var elmSyntaxSrcTree =
            bundledTree.GetNodeAtPath(["elm-syntax", "src"])
            ?? throw new Exception("Did not find elm-syntax/src");

        var elmInElmSrcTree =
            bundledTree.GetNodeAtPath(["src"])
            ?? throw new Exception("Did not find src");

        var otherLibraryModulesTree =
            bundledTree.GetNodeAtPath(["other-library-modules"]);

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));

        foreach (var (path, file) in elmInElmSrcTree.EnumerateFilesTransitive())
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));

        if (otherLibraryModulesTree is not null)
        {
            foreach (var (path, file) in otherLibraryModulesTree.EnumerateFilesTransitive())
                mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["Probe.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(b => b.path[^1].Equals("Probe.elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => (IReadOnlyList<string>)b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithTest,
                rootFilePaths: rootFilePaths,
                disableInlining: true,
                maxOptimizationRounds: 1)
            .Map(r => r.compiledEnvValue)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        return
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing: " + err));
    }

    private static PineValue GetFunc(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "Probe")
        .moduleContent.FunctionDeclarations[name];

    private static PineValue Apply(string name, PineValue[] args)
    {
        var fr =
            FunctionRecord.ParseFunctionRecordTagged(GetFunc(name), new PineVMParseCache())
            .Extract(err => throw new Exception(err.ToString()));

        var ev =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(fr, args)
            .Extract(err => throw new Exception(err.ToString()));

        return
            s_vm.EvaluateExpressionOnCustomStack(
                ev.expression,
                ev.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(err => throw new Exception("Failed eval for '" + name + "': " + err))
            .ReturnValue.Evaluate();
    }

    private static PineValue Eval0Arg(string name) =>
        Apply(name, []);

    private static PineValue StateAfterAddingHelperModule()
    {
        var initState = Eval0Arg("initState");

        // Single module with a self-recursive function. Smaller than the
        // two-module original scenario; still triggers the same defect.
        const string moduleText =
            """
            module ModuleA exposing (helper)

            helper : Int -> Int
            helper x =
                helper (x + 1)

            """;

        return
            ((PineValue.ListValue)Apply("addWorkspaceFile",
                [
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("src/ModuleA.elm")),
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(moduleText)),
                    initState,
                ])).Items.Span[1];
    }

    /// <summary>
    /// Currently FAILS on the compiled-VM path: the references response is
    /// the empty <c>TextDocumentReferencesResponse []</c> regardless of which
    /// position inside the helper declaration is queried (the recursive call
    /// site at line 5 / column 5 is the easiest to argue about — it is a
    /// plain in-expression reference to the same top-level binding and any
    /// reasonable references implementation must return at least one
    /// occurrence).
    /// <para>
    /// The interpreter path
    /// (<see cref="ElmLanguageServiceTests.References_request_finds_usage_across_modules_via_interpreter"/>)
    /// runs the same Elm source and returns a non-empty response for the
    /// equivalent position. The divergence is therefore in the
    /// compile-to-PineVM stage, not in the language-service Elm code.
    /// </para>
    /// </summary>
    [Fact]
    public void References_request_for_recursive_call_site_returns_non_empty()
    {
        var stateAfter = StateAfterAddingHelperModule();

        var refsResult =
            Apply(
                "textDocumentReferences",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(5)),
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(5)),
                stateAfter,
                ]);

        var responsePine =
            ((PineValue.ListValue)refsResult).Items.Span[0];

        var rendered =
            ElmValue.RenderAsElmExpression(
                ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
                .Extract(err => throw new Exception(err)))
            .expressionString;

        rendered.Should().NotBe("TextDocumentReferencesResponse []");
    }

    /// <summary>
    /// Sharper diagnostic test: a <c>ProvideDefinitionRequest</c> on the same
    /// recursive-call-site position currently returns one item, but the
    /// item's payload is corrupted — every integer field of <c>range</c>
    /// renders as <c>&lt;empty-blob&gt;</c> and <c>fileLocation</c> renders
    /// as a single-character blob (<c>' '</c>). When the defect is fixed the
    /// response should render as a <c>ProvideDefinitionResponse</c> with a
    /// well-formed <c>WorkspaceFileLocation</c> and integer line/column
    /// numbers — in particular it must NOT contain the substring
    /// <c>&lt;empty-blob&gt;</c>.
    /// </summary>
    [Fact]
    public void ProvideDefinition_response_is_well_formed()
    {
        var stateAfter = StateAfterAddingHelperModule();

        var defResult =
            Apply(
                "provideDefinition",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(5)),
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(5)),
                stateAfter,
                ]);

        var responsePine =
            ((PineValue.ListValue)defResult).Items.Span[0];

        var rendered =
            ElmValue.RenderAsElmExpression(
                ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
                .Extract(err => throw new Exception(err)))
            .expressionString;

        rendered.Should().NotContain("<empty-blob>");
        rendered.Should().Contain("WorkspaceFileLocation");
    }

    [Fact]
    public void Probe_hoverItemsFromParsedModule_fromDeclarations()
    {
        var stateAfter = StateAfterAddingHelperModule();

        var result =
            Apply(
                "renderFromDeclarations",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("src/ModuleA.elm")),
                stateAfter,
                ]);

        var rendered =
            ElmValue.RenderAsElmExpression(
                ElmValueEncoding.PineValueAsElmValue(result, null, null)
                .Extract(err => throw new Exception(err)))
            .expressionString;

        // Each entry is (Range, LocationInFile DeclarationRange, String).
        // Before the lambda-lifting fix, slot 2 of the first entry came back
        // as "<pine_blob 28 bytes>" (i.e. the bytes of the string "Literal" —
        // the constructor name of Pine.Expression.Literal) and slot 3 as a
        // single-element list containing an encoded Pine Expression value
        // (rendered as "ParseAndEval (Literal …)"). Both indicate that the
        // entry was constructed from the WRONG values — the result of
        // calling a lifted function with the user's argument as its first
        // captured parameter instead of the captured value.
        rendered.Should().NotContain("<pine_blob 28 bytes>");
        rendered.Should().NotContain("ParseAndEval");
        rendered.Should().Contain("LocationInFile (WorkspaceFileLocation \"src/ModuleA.elm\")");
    }

    /// <summary>
    /// Sanity probe: building a 3-tuple of (Range, LocationInFile, String)
    /// directly from let-bound values must produce the literal triple back
    /// unchanged. Always passed; included as a control to localize future
    /// regressions.
    /// </summary>
    [Fact]
    public void Probe_synthetic_triple_identity()
    {
        AssertProbeRendersAs(
            "probe_synthetic_triple_identity",
            "[ Range [ 7, 7 ] [ 7, 13 ], LocationInFile (WorkspaceFileLocation \"src/X.elm\") (DeclarationRange (Range [ 1, 1 ] [ 1, 5 ]) []), \"doc-identity\" ]");
    }

    /// <summary>
    /// Building the triple via a single-let-binding <c>case Just (sl, "doc")
    /// of Just (s, h) -> [(r, s, h)]</c> destructure-and-reconstruct
    /// pattern. Always passed; control case for the bisection.
    /// </summary>
    [Fact]
    public void Probe_synthetic_triple_via_maybe_destructure()
    {
        AssertProbeRendersAs(
            "probe_synthetic_triple_via_maybe_destructure",
            "[ [ Range [ 7, 7 ] [ 7, 13 ], LocationInFile (WorkspaceFileLocation \"src/X.elm\") (DeclarationRange (Range [ 1, 1 ] [ 1, 5 ]) []), \"doc-maybe\" ] ]");
    }

    /// <summary>
    /// Building the triple via <c>Common.listMapFind</c>, mirroring the
    /// <c>moduleCompletionItems.fromTopLevel |&gt; Common.listMapFind</c>
    /// pattern in <c>getHoverForFunctionOrName</c>. Always passed; control.
    /// </summary>
    [Fact]
    public void Probe_synthetic_triple_via_listMapFind()
    {
        AssertProbeRendersAs(
            "probe_synthetic_triple_via_listMapFind",
            "[ [ Range [ 7, 7 ] [ 7, 13 ], LocationInFile (WorkspaceFileLocation \"src/X.elm\") (DeclarationRange (Range [ 1, 1 ] [ 1, 5 ]) []), \"doc-listMapFind\" ] ]");
    }

    /// <summary>
    /// Mirrors the structure of <c>getHoverForFunctionOrName</c> /
    /// <c>getForHoversForReferenceNode</c> in <c>LanguageService.elm</c>:
    /// <c>assocListGet</c> on a list of <c>(label, (filterRange, locationInFile,
    /// CompletionItem))</c>, destructured via <c>Just (mb, loc, CompletionItem
    /// _ _ _ doc)</c>, and rebuilt into a <c>(Range, LocationInFile, String)</c>
    /// triple. <b>Was the smallest reproducer of the original
    /// "TextDocumentReferencesResponse []" bug</b>; FAILED before the lambda-lifting
    /// fix and passes after.
    /// </summary>
    [Fact]
    public void Probe_synthetic_via_assocListGet_with_completionItem_destructure()
    {
        AssertProbePreservesPayload(
            "probe_synthetic_via_assocListGet_with_completionItem_destructure",
            "doc-via-assoc");
    }

    /// <summary>
    /// As <see cref="Probe_synthetic_via_assocListGet_with_completionItem_destructure"/>
    /// but with both <c>assocListGet</c> and the <c>CompletionItem _ _ _ doc</c>
    /// pattern stripped out. The corruption persisted; this isolates the bug
    /// from those two suspects. FAILED before the fix; passes after.
    /// </summary>
    [Fact]
    public void Probe_no_assocListGet_no_completionItem()
    {
        AssertProbePreservesPayload("probe_no_assocListGet_no_completionItem", "doc-no-assoc");
    }

    /// <summary>
    /// <c>assocListGet</c> on a list of 2-tuples (no <c>CompletionItem</c>
    /// destructure). Bisection step. FAILED before the fix; passes after.
    /// </summary>
    [Fact]
    public void Probe_assocListGet_no_completionItem_destructure()
    {
        AssertProbePreservesPayload(
            "probe_assocListGet_no_completionItem_destructure",
            "doc-assoc-2tuple");
    }

    /// <summary>
    /// No <c>assocListGet</c>; <c>CompletionItem _ _ _ doc</c> destructure
    /// retained. Bisection step. FAILED before the fix; passes after.
    /// </summary>
    [Fact]
    public void Probe_no_assocListGet_with_completionItem_destructure()
    {
        AssertProbePreservesPayload(
            "probe_no_assocListGet_with_completionItem_destructure",
            "doc-no-assoc-with-ci");
    }

    /// <summary>
    /// Two let-bound functions (inner returns <c>Just (sl, "doc")</c>;
    /// outer destructures and rebuilds), called <i>without</i>
    /// <c>List.concatMap</c>. Confirms the corruption is independent of
    /// <c>List.concatMap</c>. FAILED before the fix; passes after.
    /// </summary>
    [Fact]
    public void Probe_letFn_direct_call_no_concatMap()
    {
        AssertProbePreservesPayload("probe_letFn_direct_call_no_concatMap", "doc-direct");
    }

    /// <summary>
    /// One let-bound function called via <c>List.concatMap</c>, but the
    /// function does not close over any outer let-binding. Always passed;
    /// proves <c>List.concatMap</c> alone does not trigger the bug.
    /// </summary>
    [Fact]
    public void Probe_letFn_concatMap_no_closure()
    {
        AssertProbePreservesPayload("probe_letFn_concatMap_no_closure", "doc-no-closure");
    }

    /// <summary>
    /// Top-level (non-let-bound) function passed to <c>List.concatMap</c>
    /// with explicit captured arguments. Always passed; proves the bug
    /// is specific to <i>let-bound</i> functions.
    /// </summary>
    [Fact]
    public void Probe_topLevelFn_concatMap_with_args()
    {
        AssertProbePreservesPayload("probe_topLevelFn_concatMap_with_args", "doc-toplevel");
    }

    /// <summary>
    /// <i>One</i> let-bound function (no sibling) called via
    /// <c>List.concatMap</c>, capturing outer values. Always passed; proves
    /// the bug requires <i>two</i> let-bound functions.
    /// </summary>
    [Fact]
    public void Probe_letFn_concatMap_with_closure_minimal()
    {
        AssertProbePreservesPayload(
            "probe_letFn_concatMap_with_closure_minimal",
            "doc-min-closure");
    }

    /// <summary>
    /// Single let-bound function whose body destructures a locally-built
    /// <c>Just (sl, "doc")</c>. Always passed; proves the bug requires
    /// <i>two</i> let-bound functions where one calls the other.
    /// </summary>
    [Fact]
    public void Probe_single_letFn_with_internal_maybe()
    {
        AssertProbePreservesPayload(
            "probe_single_letFn_with_internal_maybe",
            "doc-single-with-maybe");
    }

    /// <summary>
    /// Two let-bound functions; inner returns the closed-over
    /// <c>LocationInFile</c> directly (not wrapped in <c>Just</c>), outer
    /// builds a 3-tuple with it. FAILED before the fix; passes after.
    /// </summary>
    [Fact]
    public void Probe_two_letFns_inner_returns_plain()
    {
        AssertProbePreservesPayload("probe_two_letFns_inner_returns_plain", "doc-two-plain");
    }

    /// <summary>
    /// Two let-bound functions; inner returns <c>Just (sl, "doc")</c> where
    /// <c>sl</c> is closed over from the outer <c>let</c>. <b>This is the
    /// minimal reduction of the original bug</b> — same shape as
    /// <c>getHoverForFunctionOrName</c> + <c>getForHoversForReferenceNode</c>.
    /// FAILED before the fix; passes after.
    /// </summary>
    [Fact]
    public void Probe_two_letFns_inner_returns_just_pair()
    {
        AssertProbePreservesPayload(
            "probe_two_letFns_inner_returns_just_pair",
            "doc-two-just-pair");
    }

    /// <summary>
    /// Same shape as <see cref="Probe_two_letFns_inner_returns_just_pair"/>
    /// but with the closed-over <c>LocationInFile</c> replaced by an
    /// integer literal. Always passed (even before the fix) because the
    /// inner function then has no external captures, so the bad
    /// substitution path is not taken. Demonstrates that the bug requires
    /// the inner function to <i>capture</i> something complex.
    /// </summary>
    [Fact]
    public void Probe_two_letFns_inner_returns_just_pair_int()
    {
        AssertProbePreservesPayload(
            "probe_two_letFns_inner_returns_just_pair_int",
            "doc-int-pair");
    }

    /// <summary>
    /// Two let bindings where the "inner" one is a 0-arg <i>value</i> (not
    /// a function) and "outer" is a function that destructures it. Always
    /// passed; proves the bug requires the inner declaration to be a
    /// function (i.e. take ≥1 argument), which is what triggers lambda
    /// lifting on it.
    /// </summary>
    [Fact]
    public void Probe_two_letFns_inner_no_args()
    {
        AssertProbePreservesPayload("probe_two_letFns_inner_no_args", "doc-no-args");
    }

    /// <summary>
    /// Asserts that the named probe in <c>Probe.elm</c>, when applied to
    /// the integer 0, produces a result whose rendered Elm form equals
    /// <paramref name="expectedRendered"/> exactly.
    /// </summary>
    private static void AssertProbeRendersAs(string probeName, string expectedRendered)
    {
        var result =
            Apply(
                probeName,
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(0))]);

        var rendered =
            ElmValue.RenderAsElmExpression(
                ElmValueEncoding.PineValueAsElmValue(result, null, null)
                .Extract(err => throw new Exception(err)))
            .expressionString;

        rendered.Should().Be(expectedRendered);
    }

    /// <summary>
    /// Asserts that the named probe's rendered Elm form contains the
    /// expected payload string and does NOT contain any of the markers
    /// that indicate the lambda-lifting sibling-capture corruption: a
    /// 28-byte blob (the bytes of "Literal"), the substring
    /// <c>ParseAndEval</c> (a Pine.Expression constructor name that should
    /// never appear in well-formed user data), and the substring
    /// <c>EncodeExpressionAsValue</c>.
    /// </summary>
    private static void AssertProbePreservesPayload(string probeName, string expectedPayload)
    {
        var result =
            Apply(
                probeName,
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(0))]);

        var rendered =
            ElmValue.RenderAsElmExpression(
                ElmValueEncoding.PineValueAsElmValue(result, null, null)
                .Extract(err => throw new Exception(err)))
            .expressionString;

        rendered.Should().Contain("\"" + expectedPayload + "\"");
        rendered.Should().NotContain("<pine_blob 28 bytes>");
        rendered.Should().NotContain("ParseAndEval");
    }
}
