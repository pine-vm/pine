module FileTree exposing (..)

import Dict
import List
import Tuple


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (TreeNodeStructure blobStructure)


type alias TreeNodeStructure blobStructure =
    List (TreeNodeEntryStructure blobStructure)


type alias TreeNodeEntryStructure blobStructure =
    ( String, FileTreeNode blobStructure )


flatListOfBlobsFromFileTreeNode : FileTreeNode b -> List ( List String, b )
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


getBlobAtPathFromFileTree : List String -> FileTreeNode b -> Maybe b
getBlobAtPathFromFileTree path treeNode =
    case getNodeAtPathFromFileTree path treeNode of
        Just (BlobNode blob) ->
            Just blob

        _ ->
            Nothing


getNodeAtPathFromFileTree : List String -> FileTreeNode b -> Maybe (FileTreeNode b)
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


setNodeAtPathInSortedFileTree : ( List String, FileTreeNode b ) -> FileTreeNode b -> FileTreeNode b
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
                        |> Maybe.withDefault nodeAtPath
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


removeNodeAtPath : List String -> FileTreeNode b -> Maybe (FileTreeNode b)
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


mapBlobs : (a -> b) -> FileTreeNode a -> FileTreeNode b
mapBlobs mapBlob node =
    case node of
        TreeNode tree ->
            TreeNode (tree |> List.map (Tuple.mapSecond (mapBlobs mapBlob)))

        BlobNode blob ->
            BlobNode (mapBlob blob)


mapBlobsWithPath : (( List String, a ) -> b) -> FileTreeNode a -> FileTreeNode b
mapBlobsWithPath =
    mapBlobsWithPathWithPrefix []


mapBlobsWithPathWithPrefix : List String -> (( List String, a ) -> b) -> FileTreeNode a -> FileTreeNode b
mapBlobsWithPathWithPrefix pathPrefix mapBlobWithPath node =
    case node of
        TreeNode tree ->
            TreeNode
                (tree
                    |> List.map
                        (\( nodeName, nodeValue ) ->
                            ( nodeName, mapBlobsWithPathWithPrefix (pathPrefix ++ [ nodeName ]) mapBlobWithPath nodeValue )
                        )
                )

        BlobNode blob ->
            BlobNode (mapBlobWithPath ( pathPrefix, blob ))


isBlobNode : FileTreeNode a -> Bool
isBlobNode node =
    case node of
        BlobNode _ ->
            True

        TreeNode _ ->
            False
