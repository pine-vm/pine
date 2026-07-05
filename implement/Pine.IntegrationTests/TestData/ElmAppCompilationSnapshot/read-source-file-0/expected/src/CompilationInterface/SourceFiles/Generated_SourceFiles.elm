module CompilationInterface.SourceFiles.Generated_SourceFiles exposing (..)


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file__a_file_txt =
    { utf8 = "Text file content ✔️" }

file__directory_file_alpha_txt =
    { utf8 = "Text file content 0 ✔️"
    , base64 = "VGV4dCBmaWxlIGNvbnRlbnQgMCDinJTvuI8=" }