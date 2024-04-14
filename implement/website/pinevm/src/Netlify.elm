module Netlify exposing (..)

import Base64
import Build
import Bytes
import Bytes.Encode
import CompilationInterface.SourceFiles
import FileTree
import Time
import Zip
import Zip.Entry


fileTree : FileTree.FileTreeNode { base64 : String }
fileTree =
    Build.fileTree
        |> FileTree.setBlobsFromTreeInSortedFileTree
            (Build.mapFileTreeNodeFromSource CompilationInterface.SourceFiles.file_tree____netlify_publish)


blobMain : Bytes.Bytes
blobMain =
    let
        files =
            FileTree.flatListOfBlobsFromFileTreeNode fileTree
    in
    files
        |> List.foldl
            (\( filePath, file ) aggregateArchive ->
                let
                    fileBytes =
                        Base64.toBytes file.base64
                            |> Maybe.withDefault
                                ("Error: Failed to encode base64 to bytes"
                                    |> Bytes.Encode.string
                                    |> Bytes.Encode.encode
                                )

                    entry =
                        Zip.Entry.store
                            { path = String.join "/" filePath
                            , lastModified = ( Time.utc, Time.millisToPosix 0 )
                            , comment = Nothing
                            }
                            fileBytes
                in
                Zip.insert entry aggregateArchive
            )
            Zip.empty
        |> Zip.toBytes
