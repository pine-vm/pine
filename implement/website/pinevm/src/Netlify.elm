module Netlify exposing (..)

import Base64
import Build
import Bytes
import Bytes.Encode
import CompilationInterface.SourceFiles
import FileTree


fileTree : () -> FileTree.FileTreeNode { base64 : String }
fileTree thunkArg =
    Build.fileTree thunkArg
        |> FileTree.setBlobsFromTreeInSortedFileTree
            (Build.mapFileTreeNodeFromSource CompilationInterface.SourceFiles.file_tree____netlify_publish)


blobMain : () -> FileTree.FileTreeNode Bytes.Bytes
blobMain thunkArg =
    fileTree thunkArg
        |> FileTree.mapBlobs
            (\blob ->
                case Base64.toBytes blob.base64 of
                    Just bytes ->
                        bytes

                    Nothing ->
                        Bytes.Encode.encode
                            (Bytes.Encode.string
                                ("Failed to decode as base64: " ++ blob.base64)
                            )
            )
