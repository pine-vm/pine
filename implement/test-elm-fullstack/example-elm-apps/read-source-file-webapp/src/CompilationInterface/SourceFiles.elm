module CompilationInterface.SourceFiles exposing (..)


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file_tree____static_content : FileTreeNode { base64 : String }
file_tree____static_content =
    TreeNode []


file__utf8____readme_md : String
file__utf8____readme_md =
    "The compiler replaces this value."
