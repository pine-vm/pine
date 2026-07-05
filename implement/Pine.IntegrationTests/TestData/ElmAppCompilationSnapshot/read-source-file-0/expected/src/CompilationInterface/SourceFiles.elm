module CompilationInterface.SourceFiles exposing (..)
import CompilerGenerated.EncodeBytes as EncodeBytes
import CompilationInterface.SourceFiles.Generated_SourceFiles

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacesourcefiles-elm-module>
-}


type alias Base64AndUtf8 =
    { utf8 : String, base64 : String }


file____a_file_txt : { utf8 : String }
file____a_file_txt =
    { utf8 = CompilationInterface.SourceFiles.Generated_SourceFiles.file__a_file_txt.utf8
    }


file____directory_file_alpha_txt : Base64AndUtf8
file____directory_file_alpha_txt =
    { utf8 = CompilationInterface.SourceFiles.Generated_SourceFiles.file__directory_file_alpha_txt.utf8
    , base64 = CompilationInterface.SourceFiles.Generated_SourceFiles.file__directory_file_alpha_txt.base64
    }
