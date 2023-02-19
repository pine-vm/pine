module Build exposing (..)

import Base64
import CompilationInterface.ElmMake
import CompilationInterface.SourceFiles
import FileTree


fileTree : FileTree.FileTreeNode { base64 : String }
fileTree =
    CompilationInterface.SourceFiles.file_tree____static_content
        |> mapFileTreeNodeFromSource
        |> FileTree.setNodeAtPathInSortedFileTree
            ( [ elmMadeScriptFileNameDefault ]
            , FileTree.BlobNode { base64 = CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.javascript.base64 }
            )
        |> FileTree.setNodeAtPathInSortedFileTree
            ( [ "index.html" ]
            , FileTree.BlobNode
                { base64 =
                    frontendHtmlDocument { debug = False }
                        |> Base64.fromString
                        |> Maybe.withDefault "Failed to encode as base64"
                }
            )


mapFileTreeNodeFromSource : CompilationInterface.SourceFiles.FileTreeNode a -> FileTree.FileTreeNode a
mapFileTreeNodeFromSource node =
    case node of
        CompilationInterface.SourceFiles.BlobNode blob ->
            FileTree.BlobNode blob

        CompilationInterface.SourceFiles.TreeNode tree ->
            tree |> List.map (Tuple.mapSecond mapFileTreeNodeFromSource) |> FileTree.TreeNode


frontendHtmlDocument : { debug : Bool } -> String
frontendHtmlDocument { debug } =
    let
        elmMadeScriptFileName =
            elmMadeScriptFileNameDefault
    in
    """
<!DOCTYPE HTML>
<html>

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Elm-Time</title>
  <link rel="icon" type="image/x-icon" href="/favicon.ico">
  <script type="text/javascript" src="""
        ++ elmMadeScriptFileName
        ++ """></script>
</head>

<body>
    <div id="elm-app-container"></div>
</body>

<script type="text/javascript">

var app = Elm.Frontend.Main.init({
    node: document.getElementById('elm-app-container')
});

</script>

</html>
"""


elmMadeScriptFileNameDefault : String
elmMadeScriptFileNameDefault =
    "elm.js"
