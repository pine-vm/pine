{- For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfacesourcefiles-elm-module> -}


module CompilationInterface.SourceFiles exposing (..)
import CompilerGenerated.EncodeBytes as EncodeBytes
import CompilationInterface.SourceFiles.Generated_SourceFiles


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file_tree____static : FileTreeNode { base64 : String }
file_tree____static =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_static
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { base64 = blobValue.base64
    })


file____src_Backend_VolatileProcess_csx : { utf8 : String }
file____src_Backend_VolatileProcess_csx =
    { utf8 = CompilationInterface.SourceFiles.Generated_SourceFiles.file__src_Backend_VolatileProcess_csx.utf8
    }


file_tree____elm_core_modules_implicit_import : FileTreeNode { utf8 : String }
file_tree____elm_core_modules_implicit_import =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_core_modules_implicit_import
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
    })


file_tree____elm_core_modules_explicit_import : FileTreeNode { utf8 : String }
file_tree____elm_core_modules_explicit_import =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_core_modules_explicit_import
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
    })


file_tree____elm_kernel_modules_json_src : FileTreeNode { utf8 : String }
file_tree____elm_kernel_modules_json_src =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_kernel_modules_json_src
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
    })


file_tree____elm_kernel_modules_http_src : FileTreeNode { utf8 : String }
file_tree____elm_kernel_modules_http_src =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_kernel_modules_http_src
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
    })


file_tree____elm_kernel_modules_time_src : FileTreeNode { utf8 : String }
file_tree____elm_kernel_modules_time_src =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_kernel_modules_time_src
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
    })


file_tree____elm_kernel_modules_html_src : FileTreeNode { utf8 : String }
file_tree____elm_kernel_modules_html_src =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_kernel_modules_html_src
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
    })


file_tree____elm_kernel_modules_browser_src : FileTreeNode { utf8 : String }
file_tree____elm_kernel_modules_browser_src =
    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_kernel_modules_browser_src
    |>mapFileTreeNodeFromGenerated
    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
    })




mapFileTreeNodeFromGenerated : CompilationInterface.SourceFiles.Generated_SourceFiles.FileTreeNode a -> FileTreeNode a
mapFileTreeNodeFromGenerated node =
    case node of
        CompilationInterface.SourceFiles.Generated_SourceFiles.BlobNode blob ->
            BlobNode blob

        CompilationInterface.SourceFiles.Generated_SourceFiles.TreeNode tree ->
            tree |> List.map (Tuple.mapSecond mapFileTreeNodeFromGenerated) |> TreeNode




mapBlobs : (a -> b) -> FileTreeNode a -> FileTreeNode b
mapBlobs mapBlob node =
    case node of
        TreeNode tree ->
            TreeNode (tree |> List.map (Tuple.mapSecond (mapBlobs mapBlob)))

        BlobNode blob ->
            BlobNode (mapBlob blob)
