module ProjectState exposing (..)

import Bytes
import Bytes.Encode
import Dict
import List


type alias ProjectState =
    ProjectState_2020_12


type alias ProjectState_2020_12 =
    FileTreeNode


type FileTreeNode
    = BlobNode Bytes.Bytes
    | TreeNode TreeNodeStructure


type alias TreeNodeStructure =
    List TreeNodeEntryStructure


type alias TreeNodeEntryStructure =
    ( String, FileTreeNode )


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


setBlobAtPathInSortedFileTree : ( List String, Bytes.Bytes ) -> FileTreeNode -> FileTreeNode
setBlobAtPathInSortedFileTree ( path, blobContent ) stateBefore =
    case path of
        [] ->
            BlobNode blobContent

        pathFirstElement :: pathRest ->
            let
                nodeBefore =
                    getNodeAtPathFromFileTree [ pathFirstElement ] stateBefore

                node =
                    nodeBefore
                        |> Maybe.withDefault (BlobNode (Bytes.Encode.encode (Bytes.Encode.string "")))
                        |> setBlobAtPathInSortedFileTree ( pathRest, blobContent )

                treeEntriesBefore =
                    case stateBefore of
                        BlobNode _ ->
                            []

                        TreeNode treeBeforeEntries ->
                            treeBeforeEntries

                treeEntries =
                    treeEntriesBefore
                        |> Dict.fromList
                        |> Dict.insert pathFirstElement node
                        |> Dict.toList
            in
            TreeNode treeEntries
