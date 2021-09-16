module CompilationInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file_tree____static_content : FileTreeNode Bytes.Bytes
file_tree____static_content =
    "The compiler replaces this declaration."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> BlobNode


file__utf8____readme_md : String
file__utf8____readme_md =
    "The compiler replaces this value."
