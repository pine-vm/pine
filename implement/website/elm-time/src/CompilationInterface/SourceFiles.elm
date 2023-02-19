module CompilationInterface.SourceFiles exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacesourcefiles-elm-module>
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
