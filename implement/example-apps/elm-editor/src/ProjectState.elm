module ProjectState exposing (..)

import Bytes
import Bytes.Encode
import Bytes.Extra
import Dict
import List
import Pine
import ProjectState_2021_01
import SHA256
import Set
import Tuple


type alias ProjectState_2020_12 =
    FileTreeNode


type FileTreeNode
    = BlobNode Bytes.Bytes
    | TreeNode TreeNodeStructure


type alias TreeNodeStructure =
    List TreeNodeEntryStructure


type alias TreeNodeEntryStructure =
    ( String, FileTreeNode )


compositionPineValueFromFileTreeNode : FileTreeNode -> Pine.Value
compositionPineValueFromFileTreeNode treeNode =
    case treeNode of
        BlobNode bytes ->
            Pine.BlobValue (Bytes.Extra.toByteValues bytes)

        TreeNode entries ->
            entries
                |> List.map
                    (\( entryName, entryValue ) ->
                        Pine.ListValue [ Pine.valueFromString entryName, compositionPineValueFromFileTreeNode entryValue ]
                    )
                |> Pine.ListValue


fileTreeNodeFromPineValue : Pine.Value -> Result String FileTreeNode
fileTreeNodeFromPineValue pineValue =
    case pineValue of
        Pine.BlobValue blobValue ->
            Ok (BlobNode (Bytes.Extra.fromByteValues blobValue))

        Pine.ListValue listElements ->
            let
                namedFileTreeNodeFromListElement listElement =
                    case listElement of
                        Pine.ListValue listElementList ->
                            case listElementList of
                                [ nameValue, childNode ] ->
                                    case Pine.stringFromValue nameValue of
                                        Err error ->
                                            Err ("Failed to translate name entry to string: " ++ error)

                                        Ok nodeName ->
                                            case fileTreeNodeFromPineValue childNode of
                                                Err error ->
                                                    Err ("Failed to translate child node to file tree node: " ++ error)

                                                Ok childFileTreeNode ->
                                                    Ok ( nodeName, childFileTreeNode )

                                _ ->
                                    Err ("Unexpected number of elements in list element: " ++ String.fromInt (List.length listElementList))

                        Pine.BlobValue _ ->
                            Err "Unexpected type of value in list element: BlobValue"
            in
            listElements
                |> List.map namedFileTreeNodeFromListElement
                |> List.foldr
                    (\elementResult ->
                        Result.andThen
                            (\listOk -> elementResult |> Result.map (\elementSuccess -> elementSuccess :: listOk))
                    )
                    (Ok [])
                |> Result.map TreeNode


compositionHashFromFileTreeNode : FileTreeNode -> SHA256.Digest
compositionHashFromFileTreeNode =
    compositionPineValueFromFileTreeNode >> Pine.hashDigestFromValue


flatListOfBlobsFromFileTreeNode : FileTreeNode -> List ( List String, Bytes.Bytes )
flatListOfBlobsFromFileTreeNode treeNode =
    case treeNode of
        BlobNode blob ->
            [ ( [], blob ) ]

        TreeNode treeEntries ->
            treeEntries
                |> List.concatMap
                    (\( childName, childContent ) ->
                        childContent
                            |> flatListOfBlobsFromFileTreeNode
                            |> List.map (Tuple.mapFirst ((::) childName))
                    )


sortedFileTreeFromListOfBlobs : List ( List String, Bytes.Bytes ) -> FileTreeNode
sortedFileTreeFromListOfBlobs =
    List.foldl setBlobAtPathInSortedFileTree (TreeNode [])


getBlobAtPathFromFileTree : List String -> FileTreeNode -> Maybe Bytes.Bytes
getBlobAtPathFromFileTree path treeNode =
    case getNodeAtPathFromFileTree path treeNode of
        Just (BlobNode blob) ->
            Just blob

        _ ->
            Nothing


getNodeAtPathFromFileTree : List String -> FileTreeNode -> Maybe FileTreeNode
getNodeAtPathFromFileTree path treeNode =
    case path of
        [] ->
            Just treeNode

        pathFirstElement :: pathRest ->
            case treeNode of
                BlobNode _ ->
                    Nothing

                TreeNode treeElements ->
                    case treeElements |> List.filter (Tuple.first >> (==) pathFirstElement) |> List.head of
                        Nothing ->
                            Nothing

                        Just ( _, subNode ) ->
                            getNodeAtPathFromFileTree pathRest subNode


setNodeAtPathInSortedFileTree : ( List String, FileTreeNode ) -> FileTreeNode -> FileTreeNode
setNodeAtPathInSortedFileTree ( path, nodeAtPath ) nodeBefore =
    case path of
        [] ->
            nodeAtPath

        pathFirstElement :: pathRest ->
            let
                childNodeBefore =
                    getNodeAtPathFromFileTree [ pathFirstElement ] nodeBefore

                childNode =
                    childNodeBefore
                        |> Maybe.withDefault (BlobNode (Bytes.Encode.encode (Bytes.Encode.string "")))
                        |> setNodeAtPathInSortedFileTree ( pathRest, nodeAtPath )

                nodeBeforeTreeEntries =
                    case nodeBefore of
                        BlobNode _ ->
                            []

                        TreeNode treeBeforeEntries ->
                            treeBeforeEntries

                treeEntries =
                    nodeBeforeTreeEntries
                        |> Dict.fromList
                        |> Dict.insert pathFirstElement childNode
                        |> Dict.toList
            in
            TreeNode treeEntries


setBlobAtPathInSortedFileTree : ( List String, Bytes.Bytes ) -> FileTreeNode -> FileTreeNode
setBlobAtPathInSortedFileTree ( path, blobContent ) =
    setNodeAtPathInSortedFileTree ( path, BlobNode blobContent )


removeNodeAtPath : List String -> FileTreeNode -> Maybe FileTreeNode
removeNodeAtPath path nodeBefore =
    case path of
        [] ->
            Nothing

        nextPathElement :: remainingPathElements ->
            case nodeBefore of
                BlobNode _ ->
                    Just nodeBefore

                TreeNode treeBefore ->
                    case
                        treeBefore
                            |> List.filterMap
                                (\( branchName, branchValue ) ->
                                    if branchName == nextPathElement then
                                        removeNodeAtPath remainingPathElements branchValue
                                            |> Maybe.map (Tuple.pair branchName)

                                    else
                                        Just ( branchName, branchValue )
                                )
                    of
                        [] ->
                            Nothing

                        remainingBranches ->
                            Just (TreeNode remainingBranches)


applyProjectStateDifference_2021_01 : ProjectState_2021_01.ProjectStateDifference -> FileTreeNode -> Result String FileTreeNode
applyProjectStateDifference_2021_01 differenceFromBase baseComposition =
    let
        compositionAfterRemovals =
            differenceFromBase.removeNodes
                |> List.foldl
                    (\removeNode -> removeNodeAtPath removeNode >> Maybe.withDefault (TreeNode []))
                    baseComposition

        projectStateAfterChangeBlobs =
            differenceFromBase.changeBlobs
                |> List.foldl
                    (\( blobPath, changesFromBaseBlob ) stateBefore ->
                        let
                            blobValueBefore =
                                compositionAfterRemovals
                                    |> getBlobAtPathFromFileTree blobPath
                                    |> Maybe.withDefault Bytes.Extra.empty

                            changedBlobValue =
                                ProjectState_2021_01.applyBlobChanges changesFromBaseBlob blobValueBefore
                        in
                        setBlobAtPathInSortedFileTree ( blobPath, changedBlobValue ) stateBefore
                    )
                    compositionAfterRemovals
    in
    Ok projectStateAfterChangeBlobs


searchProjectStateDifference_2021_01 : FileTreeNode -> { baseComposition : FileTreeNode } -> Result String ProjectState_2021_01.ProjectStateDifference
searchProjectStateDifference_2021_01 projectState { baseComposition } =
    let
        baseCompositionBlobs =
            baseComposition |> flatListOfBlobsFromFileTreeNode

        baseCompositionBlobsDict =
            baseCompositionBlobs |> Dict.fromList

        baseCompositionBlobsPaths =
            baseCompositionBlobs
                |> List.map Tuple.first
                |> Set.fromList

        projectStateBlobs =
            projectState |> flatListOfBlobsFromFileTreeNode

        projectStateBlobsPaths =
            projectStateBlobs
                |> List.map Tuple.first
                |> Set.fromList

        removeNodes =
            Set.diff baseCompositionBlobsPaths projectStateBlobsPaths
                |> Set.toList

        areBytesEqual bytesA bytesB =
            {- Avoid bug in Elm core library as reported at https://github.com/elm/bytes/issues/15 :
               Convert to other representation before comparing.
            -}
            Bytes.Extra.toByteValues bytesA == Bytes.Extra.toByteValues bytesB

        addedOrChangedBlobs =
            projectStateBlobs
                |> List.filter
                    (\( blobPath, newBlobContent ) ->
                        baseCompositionBlobsDict
                            |> Dict.get blobPath
                            |> Maybe.map (areBytesEqual newBlobContent)
                            |> Maybe.withDefault False
                            |> not
                    )

        addedOrChangedBlobsWithHash =
            addedOrChangedBlobs
                |> List.map
                    (\( blobPath, blobValue ) ->
                        let
                            baseCompositionBlobValue =
                                baseCompositionBlobsDict
                                    |> Dict.get blobPath
                                    |> Maybe.withDefault Bytes.Extra.empty

                            changesFromBaseBlob =
                                ProjectState_2021_01.findBlobChanges baseCompositionBlobValue blobValue
                        in
                        { blobPath = blobPath
                        , blobValue = blobValue
                        , blobHash =
                            blobValue
                                |> BlobNode
                                |> compositionPineValueFromFileTreeNode
                                |> Pine.hashDigestFromValue
                                |> SHA256.toHex
                        , changesFromBaseBlob = changesFromBaseBlob
                        }
                    )
    in
    Ok
        { removeNodes = removeNodes
        , changeBlobs =
            addedOrChangedBlobsWithHash
                |> List.map (\blob -> ( blob.blobPath, blob.changesFromBaseBlob ))
        }


isBlobNode : FileTreeNode -> Bool
isBlobNode node =
    case node of
        BlobNode _ ->
            True

        TreeNode _ ->
            False
