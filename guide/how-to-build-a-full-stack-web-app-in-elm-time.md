# How to Build a Full-Stack Web App in Elm-Time

Elm-Time makes building full-stack web apps easier by shielding app developers from incidental complexity.
By automating the work to integrate frontend and backend implementations, Elm-Time lets us focus on business logic.

In summary, our approach here is to compile frontend and backend in a single build process from a single code base and use compilation interfaces to automate program code generation.

For the development of frontends using Elm, there are already a lot of resources, including books and online courses. This guide assumes you are familiar with developing web frontend apps using Elm.
This guide focuses on integrating frontend Elm apps with a backend Elm app. For the development of backend apps specifically, there is a dedicated guide at [How to Build a Backend App in Elm-Time](./how-to-build-a-backend-app-in-elm-time.md).


## How to Deliver the Frontend to a Browser

To make a web browser load the frontend app, we deliver it as an HTML file in response to an HTTP request. We might also include other files, but in any case, we need at least one main HTML document.

So we subscribe to HTTP requests in the backend app and then send an HTTP response containing the HTML document.
But how do we get that HTML into the backend program? Our code repository contains the Elm modules making up the frontend app. We invoke the Elm compiler as part of the backend build to get the corresponding HTML or JavaScript. We do this by adding a declaration to the `CompilationInterface.ElmMake` Elm module. For each declaration in that module, Elm-Time invokes the `make` command and then replaces the declaration in `CompilationInterface.ElmMake` to hold the resulting file.

The `make` command allows us to build the frontend into a single HTML file embedding the necessary JavaScript code. We don't have to deliver a separate JavaScript file to the web browser when we use this option.

Since the file produced by `elm make` is now a value in the backend Elm program, we can apply any post-processing before handing it to the client. For example, we can add a `<script>` tag to the HTML document to load a JavaScript file from a CDN or change the `<title>` tag.


## How to Enable Type-Checking between Frontend and Backend

To enable type-checking between frontend and backend, we declare the types describing the exchanged messages in a shared Elm module.
This enables the Elm compiler to check for type mismatches and generate error messages when necessary.


## How to Exchange Elm Values between Frontend and Backend

So we have delivered the frontend app to client web browsers and made sure the update functions for communication between them use matching types.
But how do we let them exchange Elm values? Any software that wants to communicate over the network must serialize and deserialize these messages. Since the code implementing these functions depends entirely on the message type, we let Elm-Time generate this code automatically at build time.

The Elm module `CompilationInterface.GenerateJsonConverters` provides automatically generated JSON encoders and decoders for Elm types of your choice.

By adding a declaration in this module, we instruct the compiler to generate a JSON encoder or decoder. The compiler replaces the declaration with the generated code. The compiler also checks that the type is serializable and deserializable.

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

In the example above, we use a type declared in another module.
We are free to distribute the type declarations over any number of modules. The parser follows imports to recursively collect the graph of type declarations until it reaches atomic Elm types at the leaves.

> Note that, at the moment, the serialization of functions is not implemented.
> 
> ```Elm
> jsonEncodeMyRecordType : { field_with_function_type : Int -> Int } -> Json.Encode.Value
> jsonEncodeMyRecordType =
>     always (Json.Encode.string "The compiler replaces this declaration.")
> ```
> 
> When the type we pick in the type annotation contains a function type somewhere, the compiler will show an error message like this:
> 
> ```
> Compilation failed with 1 error:
> 
> in file src/CompilationInterface/GenerateJsonConverters.elm: Failed to prepare mapping 'jsonEncodeMyRecordType': Failed to parse type annotation: Failed to parse annotation of field 'field_with_function_type': FunctionTypeAnnotation not implemented
> ```

Now that we have taken care of the serialization, we can use the generic HTTP APIs in the frontend and backend to send and receive them.

Since we have automated the changing parts, our code to exchange messages between the frontend and backend stays the same, no matter the message type.


## Flexibility in Integrating and Compiling Multiple Frontends

The approach introduced above allows for the flexible composition of frontends:

+ We can integrate any number of frontends into the backend build.
+ We can compile separate frontends with separate compiler flags. For example, some full-stack apps enable the `--debug` flag on the `make` command to integrate the Elm frontend debugger with some of the frontends.
+ When using multiple frontend apps, we can choose whether to use the same message type for multiple frontends or whether to use different message types.


## Background and Design Goals of Elm-Time

The design of Elm-Time and its boundaries follows from observations of how incidental complexity in software development leads to wasted effort. When looking at what developers spend time on, we notice some code rarely needs to be changed, while other parts must be adapted frequently. Elm-Time takes over where incidental complexity would otherwise cause frequent rework.

+ Code that needs to be frequently adapted is generated automatically in the background and invisible by default.
+ Code only written once and never needs to change remains part of our application code, checked into version control.
