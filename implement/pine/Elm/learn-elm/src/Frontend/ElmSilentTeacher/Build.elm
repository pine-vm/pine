module Frontend.ElmSilentTeacher.Build exposing (..)

import Base64
import Bytes
import Bytes.Encode
import CompilationInterface.ElmMake
import FileTree
import Time
import Zip
import Zip.Entry


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


fileTree : FileTree.FileTreeNode { base64 : String }
fileTree =
    FileTree.TreeNode []
        |> FileTree.setNodeAtPathInSortedFileTree
            ( [ "index.html" ]
            , FileTree.BlobNode
                { base64 =
                    frontendHtmlDocument
                        |> Base64.fromString
                        |> Maybe.withDefault "Failed to encode as base64"
                }
            )


frontendHtmlDocument : String
frontendHtmlDocument =
    String.trimLeft
        """
<!DOCTYPE HTML>
<html>

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Elm Silent Teacher</title>
  <script type="text/javascript">
"""
        ++ CompilationInterface.ElmMake.elm_make____src_Frontend_ElmSilentTeacher_elm.javascript.utf8
        ++ """
</script>
</head>

<body>
    <div id="elm-app-container"></div>
</body>

<script type="text/javascript">

var app = Elm.Frontend.ElmSilentTeacher.init({
    node: document.getElementById('elm-app-container')
});

</script>

</html>
"""
