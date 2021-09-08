module FileTreeInWorkspace exposing (..)

{-| This module contains file tree representations specialized in reducing runtime expenses in frontend scenarios, where we see frequent edits.
-}

import Base64
import Bytes
import Bytes.Extra
import Dict
import FileTree exposing (FileTreeNode(..))
import List
import Pine
import ProjectState_2021_01
import SHA256
import Set
import Tuple


type alias FileTreeNode =
    FileTree.FileTreeNode BlobNodeWithCache


type alias BlobNodeWithCache =
    { asBytes : Bytes.Bytes
    , asBase64 : String
    }


compositionPineValueFromFileTreeNode : FileTree.FileTreeNode Bytes.Bytes -> Pine.Value
compositionPineValueFromFileTreeNode treeNode =
    case treeNode of
        BlobNode asBytes ->
            Pine.BlobValue (Bytes.Extra.toByteValues asBytes)

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
            Ok (blobNodeFromBytes (Bytes.Extra.fromByteValues blobValue))

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
    mapBlobsToBytes >> compositionHashFromFileTreeNodeBytes


compositionHashFromFileTreeNodeBytes : FileTree.FileTreeNode Bytes.Bytes -> SHA256.Digest
compositionHashFromFileTreeNodeBytes =
    compositionPineValueFromFileTreeNode >> Pine.hashDigestFromValue


sortedFileTreeFromListOfBlobsAsBytes : List ( List String, Bytes.Bytes ) -> FileTreeNode
sortedFileTreeFromListOfBlobsAsBytes =
    List.foldl setBlobAtPathInSortedFileTreeFromBytes (TreeNode [])


setBlobAtPathInSortedFileTreeFromBytes : ( List String, Bytes.Bytes ) -> FileTreeNode -> FileTreeNode
setBlobAtPathInSortedFileTreeFromBytes ( path, blobContent ) =
    FileTree.setNodeAtPathInSortedFileTree ( path, blobNodeFromBytes blobContent )


applyProjectStateDifference_2021_01 : ProjectState_2021_01.ProjectStateDifference -> FileTree.FileTreeNode Bytes.Bytes -> Result String (FileTree.FileTreeNode Bytes.Bytes)
applyProjectStateDifference_2021_01 differenceFromBase baseComposition =
    let
        compositionAfterRemovals =
            differenceFromBase.removeNodes
                |> List.foldl
                    (\removeNode -> FileTree.removeNodeAtPath removeNode >> Maybe.withDefault (TreeNode []))
                    baseComposition

        projectStateAfterChangeBlobs =
            differenceFromBase.changeBlobs
                |> List.foldl
                    (\( blobPath, changesFromBaseBlob ) ->
                        let
                            blobValueBefore =
                                compositionAfterRemovals
                                    |> FileTree.getBlobAtPathFromFileTree blobPath
                                    |> Maybe.withDefault Bytes.Extra.empty

                            changedBlobValue =
                                ProjectState_2021_01.applyBlobChanges changesFromBaseBlob blobValueBefore
                        in
                        FileTree.setNodeAtPathInSortedFileTree ( blobPath, BlobNode changedBlobValue )
                    )
                    compositionAfterRemovals
    in
    Ok projectStateAfterChangeBlobs


searchProjectStateDifference_2021_01 : FileTreeNode -> { baseComposition : FileTreeNode } -> Result String ProjectState_2021_01.ProjectStateDifference
searchProjectStateDifference_2021_01 projectState { baseComposition } =
    let
        baseCompositionBlobs =
            baseComposition |> FileTree.flatListOfBlobsFromFileTreeNode

        baseCompositionBlobsDict =
            baseCompositionBlobs |> Dict.fromList

        baseCompositionBlobsPaths =
            baseCompositionBlobs
                |> List.map Tuple.first
                |> Set.fromList

        projectStateBlobs =
            projectState |> FileTree.flatListOfBlobsFromFileTreeNode

        projectStateBlobsPaths =
            projectStateBlobs
                |> List.map Tuple.first
                |> Set.fromList

        removeNodes =
            Set.diff baseCompositionBlobsPaths projectStateBlobsPaths
                |> Set.toList

        addedOrChangedBlobs =
            projectStateBlobs
                |> List.filter
                    (\( blobPath, newBlobContent ) ->
                        baseCompositionBlobsDict
                            |> Dict.get blobPath
                            |> Maybe.map (.asBase64 >> (==) newBlobContent.asBase64)
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
                                    |> Maybe.map .asBytes
                                    |> Maybe.withDefault Bytes.Extra.empty

                            changesFromBaseBlob =
                                ProjectState_2021_01.findBlobChanges baseCompositionBlobValue blobValue.asBytes
                        in
                        { blobPath = blobPath
                        , blobValue = blobValue
                        , blobHash =
                            blobValue
                                |> BlobNode
                                |> compositionHashFromFileTreeNode
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


mapBlobsToBytes : FileTreeNode -> FileTree.FileTreeNode Bytes.Bytes
mapBlobsToBytes =
    FileTree.mapBlobs .asBytes


mapBlobsFromBytes : FileTree.FileTreeNode Bytes.Bytes -> FileTreeNode
mapBlobsFromBytes =
    FileTree.mapBlobs blobValueFromBytes


blobNodeFromBytes : Bytes.Bytes -> FileTreeNode
blobNodeFromBytes asBytes =
    BlobNode (blobValueFromBytes asBytes)


blobValueFromBytes : Bytes.Bytes -> BlobNodeWithCache
blobValueFromBytes asBytes =
    { asBytes = asBytes
    , asBase64 =
        asBytes
            |> Base64.fromBytes
            |> Maybe.withDefault "Error encoding in base64"
    }
