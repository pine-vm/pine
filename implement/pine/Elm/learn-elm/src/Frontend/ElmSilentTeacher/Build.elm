module Frontend.ElmSilentTeacher.Build exposing (..)

import Base64
import Bytes
import Bytes.Encode
import CompilationInterface.ElmMake
import FileTree


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


fileTree : () -> FileTree.FileTreeNode { base64 : String }
fileTree _ =
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
