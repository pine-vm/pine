module CompilationInterface.SourceFiles exposing (..)


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file__base64____src_monarch_js : String
file__base64____src_monarch_js =
    "The compiler replaces this value."


file__base64____static_favicon_svg : String
file__base64____static_favicon_svg =
    "The compiler replaces this value."


file__utf8____src_Backend_VolatileProcess_csx : String
file__utf8____src_Backend_VolatileProcess_csx =
    "The compiler replaces this value."


file_tree____elm_core_modules : FileTreeNode { utf8 : String }
file_tree____elm_core_modules =
    TreeNode []
