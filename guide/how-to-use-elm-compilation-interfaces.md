# How to Use Elm Compilation Interfaces

The compilation interfaces enable simple customization and extension of the Elm compilation process.
There are dedicated Elm compilation interfaces for the following use cases:

+ Integrate and read source files of any type.
+ Generate functions to encode Elm values to JSON or decode JSON to Elm values.
+ Invoke an `elm make` command to generate JavaScript or HTML documents.
+ Integrate files from other sources into the compilation and build process.

### `CompilationInterface.GenerateJsonConverters` Elm Module

This module provides automatically generated JSON encoders and decoders for Elm types.

By adding a declaration in this module, we instruct the compiler to generate a JSON encoder or decoder. A common use case for this automation is types used at the interface between the front-end and the back-end.

In this module, we can freely choose the names for functions, as we only need type annotations to tell the compiler what we want to have generated. To encode to JSON, add a function which takes this type and returns a `Json.Encode.Value`:

```Elm
jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The compiler replaces this declaration.")
```

To get a JSON decoder, declare a name for an instance of `Json.Decode.Decoder`:

```Elm
jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The compiler replaces this declaration."
```


### `CompilationInterface.SourceFiles` Elm Module

The `SourceFiles` module provides access to the source files, regardless of their type or other usage.

In addition to individual files, this module also supports accessing the contents of whole directories.

By adding a declaration to this module, we can pick a source file and read its contents. The compilation step for this module happens before the one for the front-end. Therefore the source files are available to both front-end and back-end apps.

The [app 'Elm Editor' uses this interface](https://github.com/pine-vm/pine/blob/67658db8f7e2ed50a9dd2a3ffcfaba2e20c7615d/implement/example-apps/elm-editor/src/CompilationInterface/SourceFiles.elm) to get the contents of various files in the app code directory. The app uses some of these files in the front-end and some in the back-end.

```Elm
module CompilationInterface.SourceFiles exposing (..)


type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))


file_tree____static : FileTreeNode { base64 : String }
file_tree____static =
    TreeNode []


file____src_Backend_VolatileProcess_csx : { utf8 : String }
file____src_Backend_VolatileProcess_csx =
    { utf8 = "The compiler replaces this declaration." }


file_tree____elm_core_modules_implicit_import : FileTreeNode { utf8 : String }
file_tree____elm_core_modules_implicit_import =
    BlobNode { utf8 = "The compiler replaces this declaration." }


file_tree____elm_core_modules_explicit_import : FileTreeNode { utf8 : String }
file_tree____elm_core_modules_explicit_import =
    BlobNode { utf8 = "The compiler replaces this declaration." }
```

To map the source file path to a name in this module, replace any non-alphanumeric character with an underscore. The directory separator (a slash or backslash on many operating systems) also becomes an underscore. Here are some examples:

| file path                         | Name in the Elm module                    |
| --------------------------------  | --------------------------                |
| `README.md`                       | `file____README_md`                       |
| `static/README.md`                | `file____static_README_md`                |
| `static/chat.message-added.0.mp3` | `file____static_chat_message_added_0_mp3` |

The compilation will fail if this module contains a name that matches more than one or none of the source files.

Using the record type on a function declaration, we can choose from the encodings `bytes`, `base64` and `utf8`.


### `CompilationInterface.ElmMake` Elm Module

The `ElmMake` module provides an interface to run the `elm make` command and use the output file value in our Elm app.
For each function declaration in this module, the compiler replaces the declaration with the output(s) from `elm  make`.

Using the name of the declaration, we specify the source file name.
Using a type signature on the function declaration, we select the flags for elm make and the encoding of the output file. This signature must always be a record type or an alias of a record type declared in the same module. Using the record field names, we select:

+ Flags for elm make: `debug` or none.
+ Output type: `javascript`, `html` or none for HTML.
+ Encoding: Either `bytes` or `base64` or `utf8`.

Here is an example that compiles a source file located at path `src/Frontend/Main.elm`:

```Elm
module CompilationInterface.ElmMake exposing (..)

import Bytes
import Bytes.Encode


elm_make____src_Frontend_Main_elm : { bytes : Bytes.Bytes }
elm_make____src_Frontend_Main_elm =
    { bytes =
        "The compiler replaces this value."
            |> Bytes.Encode.string
            |> Bytes.Encode.encode
    }

```

We can also get the output encoded as a base64 string instead of `Bytes.Bytes`, by using the field name `base64`:

```Elm
elm_make____src_Frontend_Main_elm : { base64 : String }
elm_make____src_Frontend_Main_elm =
    { base64 = "The compiler replaces this value." }
```

We use nested record types to combine multiple of those names. For example, this declaration gets us two compilation variants of the same file, one without flags and one compiled the `--debug` flag:

```Elm
elm_make____src_Frontend_Main_elm : { debug : { javascript : { base64 : String } }, javascript : { base64 : String } }
elm_make____src_Frontend_Main_elm =
    { javascript = { base64 = "The compiler replaces this value." }
    , debug = { javascript = { base64 = "The compiler replaces this value." } }
    }
```

The tree we modeled with this record type has two leaves:

+ `debug.javascript.base64 : String`
+ `javascript.base64 : String`

Backend apps often use the output from `elm make` send the frontend to web browsers with HTTP responses. We can also see this in the [example app](https://github.com/pine-vm/pine/blob/67658db8f7e2ed50a9dd2a3ffcfaba2e20c7615d/implement/example-apps/docker-image-default-app/src/Backend/Main.elm#L43-L55) mentioned earlier:

```Elm
    httpResponse =
        if
            httpRequestEvent.request.uri
                |> Url.fromString
                |> Maybe.map urlLeadsToFrontendHtmlDocument
                |> Maybe.withDefault False
        then
            { statusCode = 200
            , bodyAsBase64 = Just CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.debug.base64
            , headersToAdd =
                [ { name = "Content-Type", values = [ "text/html" ] }
                ]
            }

        else
```
