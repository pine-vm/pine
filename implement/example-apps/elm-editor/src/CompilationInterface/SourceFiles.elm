module CompilationInterface.SourceFiles exposing (..)


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file____src_monarch_js : { base64 : String }
file____src_monarch_js =
    { base64 = "The compiler replaces this value." }


file____static_favicon_svg : { base64 : String }
file____static_favicon_svg =
    { base64 = "The compiler replaces this value." }


file____src_Backend_VolatileProcess_csx : { utf8 : String }
file____src_Backend_VolatileProcess_csx =
    { utf8 = "The compiler replaces this value." }


file_tree____elm_core_modules_implicit_import : FileTreeNode { utf8 : String }
file_tree____elm_core_modules_implicit_import =
    BlobNode { utf8 = "The compiler replaces this value." }


file_tree____elm_core_modules_explicit_import : FileTreeNode { utf8 : String }
file_tree____elm_core_modules_explicit_import =
    BlobNode { utf8 = "The compiler replaces this value." }
