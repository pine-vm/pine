module CompilationInterface.SourceFiles exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacesourcefiles-elm-module>
-}


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file_tree____static_content : FileTreeNode { base64 : String }
file_tree____static_content =
    TreeNode []


file_tree____netlify_publish : FileTreeNode { base64 : String }
file_tree____netlify_publish =
    TreeNode []
