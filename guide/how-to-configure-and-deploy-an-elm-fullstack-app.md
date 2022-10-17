# How to Configure and Deploy an Elm Fullstack App

## Installing and Registering the `elm-fs` Command

In this guide, I use the `elm-fs` command-line interface (CLI) program. You can find all downloads in the releases section at https://github.com/elm-fullstack/elm-fullstack/releases

Here are direct links to the downloads, containing the `elm-fs` executable file in a zip-archive:

+ Windows: https://github.com/elm-fullstack/elm-fullstack/releases/download/v2022-10-17/elm-fullstack-bin-1fd86d6f4e45b25c270057203effd5c5b2747bff-win10-x64.zip
+ Linux: https://github.com/elm-fullstack/elm-fullstack/releases/download/v2022-10-17/elm-fullstack-bin-1fd86d6f4e45b25c270057203effd5c5b2747bff-linux-x64.zip

To register the elm-fs executable on your system, run the `elm-fs  install` command. If you use Linux or PowerShell on Windows, you can achieve this by running the following command after navigating to the directory containing the executable file extracted from the downloaded archive:

```text
./elm-fs  install
```

In Windows, you will get a confirmation like this:

```text
I added the path 'C:\Users\John\Downloads\elm-fullstack-bin-1fd86d6f4e45b25c270057203effd5c5b2747bff-win10-x64' to the 'PATH' environment variable for the current user account. You will be able to use the 'elm-fs' command in newer instances of the Command Prompt.
```

On Linux, the confirmation of the installation looks like this:

```text
I copied the executable file to '/bin/elm-fs'. You will be able to use the 'elm-fs' command in newer terminal instances.
```

## Running a Server and Deploying an App

As part of a deployment, Elm Fullstack compiles the app program code.
The compiler requires the program code to contain the entry point for a back-end app. In addition, it offers various functions we can use independent of each other as needed. It supports projects without a front-end or with multiple front-ends apps.

Here is an example app containing back-end and front-end: https://github.com/elm-fullstack/elm-fullstack/tree/8ce169c776160db7a035a8d3a7845341af11f871/implement/example-apps/docker-image-default-app

We can use this command to run a server and deploy this app:

```cmd
elm-fs  run-server  --public-urls="http://*:5000"  --deploy=https://github.com/elm-fullstack/elm-fullstack/tree/8ce169c776160db7a035a8d3a7845341af11f871/implement/example-apps/docker-image-default-app
```

When running this command, we get an output like this:

```txt
I got no path to a persistent store for the process. This process will not be persisted!
Loading app config to deploy...
This path looks like a URL into a remote git repository. Trying to load from there...
This path points to commit 8ce169c776160db7a035a8d3a7845341af11f871
The first parent commit with same tree is https://github.com/elm-fullstack/elm-fullstack/tree/cd427019349f3d9d2f5b4c594b34d4e6c3e193cf/implement/example-apps/docker-image-default-app
Loaded source composition fe9b6872328a845ef50c7953bf969e9df773b8fd4b6abe24ba032f72501d021f from 'https://github.com/elm-fullstack/elm-fullstack/tree/8ce169c776160db7a035a8d3a7845341af11f871/implement/example-apps/docker-image-default-app'.
Starting the web server with the admin interface...
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Begin to build the process live representation.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Begin to restore the process state.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Found 1 composition log records to use for restore.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Restored the process state in 4 seconds.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Completed building the process live representation.
info: ElmFullstack.WebHost.StartupPublicApp[0]
      I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Started the public app at 'http://*:5000'.
Completed starting the web server with the admin interface at 'http://*:4000'.
```

When this server has completed starting, we can see the deployed app at http://localhost:5000/

## App Code Structure Conventions

This section covers the conventions for structuring the app code so that we can deploy it to an Elm Fullstack process. The [example apps](https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps) follow these conventions, but not every example app uses all available options, so the listing below is a more concise reference.

### `Backend.Main` Elm Module

The [main Elm module of the backend](/implement/example-apps/minimal-backend-hello-world/src/Backend/Main.elm) configures the backend with the declaration of `backendMain`:

```Elm
backendMain : ElmFullstack.BackendConfig ()
backendMain =
[...]
```

As we can see in the example apps, we compose the backend from an `init` value and an `subscriptions` function:

```Elm
backendMain : ElmFullstack.BackendConfig ()
backendMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }
```

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

Backend apps often use the output from `elm make` send the frontend to web browsers with HTTP responses. We can also see this in the [example app](https://github.com/elm-fullstack/elm-fullstack/blob/30f16796f49d9c86b2975b145b188c5abfd4f7ca/implement/example-apps/docker-image-default-app/src/Backend/Main.elm#L43-L55) mentioned earlier:

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
            , headersToAdd = []
            }

        else
```

### `CompilationInterface.GenerateJsonCoders` Elm Module

This module provides automatically generated JSON encoders and decoders for Elm types.

By adding a declaration in this module, we instruct the compiler to generate a JSON encoder or decoder. A common use case for this automation is types used at the interface between the front-end and the back-end.

In this module, we can freely choose the names for functions, as we only need type annotations to tell the compiler what we want to have generated. To encode to JSON, add a function which takes this type and returns a `Json.Encode.Value`:

```Elm
jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The compiler replaces this value.")
```

To get a JSON decoder, declare a name for an instance of `Json.Decode.Decoder`:

```Elm
jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The compiler replaces this value."
```

### `CompilationInterface.SourceFiles` Elm Module

The `SourceFiles` module provides access to the app source code files.

By adding a declaration to this module, we can pick a source file and read its contents. The compilation step for this module happens before the one for the front-end. Therefore the source files are available to both front-end and back-end apps.

The [app 'Elm Editor' uses this interface](https://github.com/elm-fullstack/elm-fullstack/blob/94c2551500ab48cc5c0743adf8dac0c386604203/implement/example-apps/elm-editor/src/CompilationInterface/SourceFiles.elm) to get the contents of various files in the app code directory. The app uses some of these files in the front-end and some in the back-end.

```Elm
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


file_tree____elm_core_modules : FileTreeNode { utf8 : String }
file_tree____elm_core_modules =
    TreeNode []
```

To map the source file path to a name in this module, replace any non-alphanumeric character with an underscore. The directory separator (a slash or backslash on many operating systems) also becomes an underscore. Here are some examples:

| file path                         | Name in the Elm module                    |
| --------------------------------  | --------------------------                |
| `readme.md`                       | `file____readme_md`                       |
| `static/readme.md`                | `file____static_readme_md`                |
| `static/chat.message-added.0.mp3` | `file____static_chat_message_added_0_mp3` |

The compilation will fail if this module contains a name that matches more than one or none of the source files.

Using the record type on a function declaration, we can choose from the encodings `bytes`, `base64` and `utf8`.

### `Backend.MigrateState` Elm Module

We need to add the `Backend.MigrateState` module when we choose to migrate the back-end state during an app's deployment. We encode the migration in a function named `migrate` with types matching previous app and new app accordingly.

In the simplest case, we did not change the back-end state model since the last deployment to the process. In this case, both input type and return type are the same. Then we can implement the whole module as follows:

```Elm
module Backend.MigrateState exposing (migrate)

import Backend.Main
import ElmFullstack


migrate : Backend.Main.State -> ( Backend.Main.State, ElmFullstack.BackendCmds Backend.Main.State )
migrate state =
    ( state, [] )
```

We don't have to return the same value here. We can also use the migration to make a custom atomic update to our back-end apps state.

Here is another example, almost as simple, with the back-end state just a primitive type, migrating from an `Int` to a `String`: https://github.com/elm-fullstack/elm-fullstack/blob/30f16796f49d9c86b2975b145b188c5abfd4f7ca/implement/test-elm-fullstack/example-elm-apps/migrate-from-int-to-string-builder-web-app/src/Backend/MigrateState.elm

### `elm-fullstack.json`

The `elm-fullstack.json` file is where you can configure the acquisition of SSL certificates and rate-limiting of HTTP requests to the backend app.
Since these features are optional to use, in the simplest case, this file is not present at all.

## Running a Server With an Elm-Fullstack Process

At the beginning of this guide, we ran a server and deployed an app in a single command. But combining these two operations is not necessary. Deployments are part of the process history, which means the last deployment follows from the state of the process store. (To learn more about the persistence, see [persistence-in-elm-fullstack.md](./persistence-in-elm-fullstack.md))

When running a server, we want to configure two aspects: The location where to persist the process state, and the password to access the admin interface.
On startup, the server restores the state of the process from the given store location. During operation, it appends to the history in the same store. Currently, the only supported kind of store location is a directory on the file system.

Here is a complete command to run a server that maintains the persistence of the Elm Fullstack process:

```cmd
elm-fs  run-server  --process-store=./process-store  --admin-password=test  --admin-urls="http://*:4000"  --public-urls="http://*:5000"
```

When running this command, you will get an output like this:

```text
Starting the web server with the admin interface...
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Begin to build the process live representation.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Begin to restore the process state.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Found no composition record, default to initial state.
info: ElmFullstack.WebHost.StartupAdminInterface[0]
      Completed building the process live representation.
Completed starting the web server with the admin interface at 'http://*:4000'.
```

In case the process store contained a process in which an app was deployed, the output will also contain this message:

```text
Started the public app at 'http://*:5000'
```

This server continues running until you shut it down. It will output additional log messages for various events, for example, HTTP requests.

When you navigate to http://localhost:4000/ using a web browser, you find a prompt to authenticate. You can use the password `test` that we specified in the command above. You don't need a username in that prompt.

When you log in at http://localhost:4000/, you will get this message:

```
Welcome to the Elm Fullstack admin interface version 2022-10-17.
```

But we don't need a web browser to interact with the admin interface. The command-line interface offers a range of commands to operate a running server, for example, to deploy a new version of an app.

## Deploy an App to an Existing Server

Use the `elm-fs  deploy` command to deploy an app to a running server in an atomic operation.

With this command, we need to specify the path to the app to deploy and the destination site to deploy to.
Here is an example that matches the admin interface configured with the `run-server` command above:

```cmd
elm-fs  deploy  --init-app-state  https://github.com/elm-fullstack/elm-fullstack/tree/cd427019349f3d9d2f5b4c594b34d4e6c3e193cf/implement/example-apps/docker-image-default-app  http://localhost:4000
```

The `--init-app-state` option means we do not migrate the previous backend state but reset it to the value from the init function.

Since the server requires us to authenticate for deployment, we will get this prompt when running the command from above:

```text
The server at 'http://localhost:4000/api/deploy-and-init-app-state' is asking for authentication. Please enter the password we should use to authenticate there:
>
```

We enter the same password we gave with the `--admin-password` option on the command to run the server.

The `elm-fs  deploy` command also writes a report of the deployment attempt into a file under the current directory. It points out the exact path to the report file in a log message:
```text
Saved report to file 'C:\Users\John\elm-fullstack-tool\report\2021-08-20T06-36-09_deploy.json'.
```
In this report, we can see if the deployment was successful and how much time it took. If a migration fails, we also find a description of the problem in this report.

## Configure the Admin Password via Environment Variable

If you do not use the `--admin-password` option with the `run-server` command, the program will get the password from the environment variable `APPSETTING_adminPassword`.
Configuring the password using the environment variable makes it easier to reuse the standard Docker image:

```cmd
docker  run  -p 80:80  -p 4000:4000  --env "APPSETTING_adminPassword=test"  elmfullstack/elm-fullstack
```

## Manage the Process Store

The process store contains not only the latest state of the app but also the event log.
In the Docker image `elmfullstack/elm-fullstack`, the process store is located in the directory `/elm-fullstack/process-store`.
You can copy this directory to backup the process store or copy it to another container.

Alternatively, use a [docker volume](https://docs.docker.com/storage/volumes/) to map this directory to another location:
```powershell
docker run --mount source=your-docker-volume-name,destination=/elm-fullstack/process-store -p 80:80 -p 4000:4000 elmfullstack/elm-fullstack
```

## Support HTTPS

The Elm-fullstack web host supports HTTPS. Thanks to the [`FluffySpoon.AspNet.LetsEncrypt`](https://github.com/ffMathy/FluffySpoon.AspNet.LetsEncrypt) project, it can automatically get an SSL certificate from [Let's Encrypt](https://letsencrypt.org/). To configure this, add a `letsEncryptOptions` property to the `elm-fullstack.json` file as follows:
```json
{
    "letsEncryptOptions": {
        "Domains": [
            "your-domain.com"
        ],
        "Email": "youremailaddress@gmail.com",
        "CertificateSigningRequest": {
            "CountryName": "Germany",
            "State": "DE",
            "Locality": "DE",
            "Organization": "Organization",
            "OrganizationUnit": "Organization Unit"
        },
        "UseStaging": true
    }
}
```

When you have started a container like this, the application emits log entries indicating the progress with getting the SSL certificate:
```
FluffySpoon.AspNet.LetsEncrypt.ILetsEncryptRenewalService[0]
Ordering LetsEncrypt certificate for domains your-domain.com.
FluffySpoon.AspNet.LetsEncrypt.ILetsEncryptRenewalService[0]
Validating all pending order authorizations.
[...]
Certificate persisted for later use.
```
In case you restart the app, you can see a log entry like this:
```
A persisted non-expired LetsEncrypt certificate was found and will be used.
```

As long as the `UseStaging` property is set to `true`, the app gets the SSL certificate from the [Let's Encrypt Staging Environment](https://letsencrypt.org/docs/staging-environment/). This way you can experiment without the risk of running into the stricter production rate-limits of Let's Encrypt. You can test with a web-browser that the SSL certificate successfully arrives on the client side. When this works, switch from staging to production SSL certificates by setting the `UseStaging` property to `false`.

