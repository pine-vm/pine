# Customizing Elm App Builds with Compilation Interfaces

The compilation interfaces enable customization and extension of how an Elm app is built and composed.

There are dedicated Elm compilation interfaces for the following use cases:

+ Generate JSON encoders/decoders to/from Elm values.
+ Integrate and read source files of any type.
+ Invoke an `elm make` command to generate JavaScript or HTML documents.
+ Integrate files from other sources into the compilation and build process.

### `CompilationInterface.GenerateJsonConverters` Module

The `GenerateJsonConverters` module provides automatically generated JSON encoders and decoders for Elm types.

Adding a declaration in this module instructs the compiler to generate a JSON encoder or decoder.

This automation is often used when building full-stack applications in Elm: JSON offers a way to serialize Elm values to enable exchanging them between the backend and the frontend. Using this compilation interface means we can delegate the writing of the JSON encoders and decoders for these messages to the machine.

In this module, we can freely choose the names for declarations, as we only need type annotations to tell the compiler what we want to have generated.

To encode to JSON, add a function which takes the Elm type to serialize as parameter and returns a `Json.Encode.Value`:

```Elm
jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The compiler replaces this declaration.")
```

To get a JSON decoder, add a declaration for an instance of `Json.Decode.Decoder`:

```Elm
jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The compiler replaces this declaration."
```

As the examples above show, we keep a placeholder value in each declaration with a matching type. Using this form in the source code means the interface Elm module syntax remains compatible with type-checking tools. The insertion of the actual implementation happens automatically under the hood every time we build the app.

In the example above, we use a type declared in another module.
We are free to distribute the type declarations over any number of modules. The parser follows imports to recursively collect the graph of type declarations until it reaches atomic Elm types at the leaves.

### `CompilationInterface.SourceFiles` Module

The `SourceFiles` module provides access to the source files, regardless of file type or other usage.

In addition to individual files, this module also supports reading whole directories.

Adding a declaration to this module lets us pick a source file and read its contents. The compilation step for this module happens before the one for the `ElmMake` module. Therefore, the values of embedded source files are available for both frontend and backend apps.

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

For some examples of typical usages, see <https://github.com/pine-vm/pine/blob/c764a804d90f1fa1002e1690b04487f7c06f765e/implement/example-apps/elm-editor/src/CompilationInterface/SourceFiles.elm>

In the example module linked above, we use this interface to get the contents of various files in the app code directory. Some of these files are used in the front end, and some are used in the back end.

### `CompilationInterface.ElmMake` Module

The `ElmMake` module provides an interface to run the `elm make` command and use the output file value in our Elm app.
For each function declaration in this module, the compiler replaces the declaration with the output(s) from `elm  make`.

Using the name of the declaration, we specify the source file name.
Using a type signature on the function declaration, we select the flags for elm make and the encoding of the output file. This signature must always be a record type or an alias of a record type declared in the same module. Using the record field names, we select:

+ Flags for `elm  make`: `debug` or `optimize` or none.
+ Output type: `javascript`, `html` or none. If none is specified, the output defaults to HTML.
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

In the example above, the tree structure of the declaration type has two leaves:

+ `debug.javascript.base64 : String`
+ `javascript.base64 : String`

Since the compilation flags differ between the two paths, the build process will invoke the `elm  make` command once for each of the flags to build the complete record value for that declaration.

Backend apps often use the output from `elm  make` send the frontend to web browsers with HTTP responses. We can also see this in the [example app](https://github.com/pine-vm/pine/blob/3a5c9d0052ab344984bafa5094d2debc3ad1ecb7/implement/example-apps/docker-image-default-app/src/Backend/Main.elm#L46-L62) mentioned earlier:

```Elm
    httpResponse =
        if
            httpRequestEvent.request.uri
                |> Url.fromString
                |> Maybe.map urlLeadsToFrontendHtmlDocument
                |> Maybe.withDefault False
        then
            { statusCode = 200
            , body =
                CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.debug.base64
                    |> Base64.toBytes
            , headersToAdd =
                [ { name = "Content-Type", values = [ "text/html" ] }
                ]
            }

        else
```
