# How to Configure and Deploy an Elm-Fullstack App

In this guide, I use the elm-fullstack command-line interface (CLI) program. You can find all downloads in the releases section at https://github.com/elm-fullstack/elm-fullstack/releases

Here are direct links to the downloads, containing the `elm-fullstack` executable file in a zip-archive:

+ Windows: https://github.com/elm-fullstack/elm-fullstack/releases/download/v2021-01-16/elm-fullstack-bin-c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33-win10-x64.zip
+ Linux: https://github.com/elm-fullstack/elm-fullstack/releases/download/v2021-01-16/elm-fullstack-bin-c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33-linux-x64.zip

To register the elm-fullstack executable on your systems PATH environment variable, run the `elm-fullstack  install-command` command.

## Running a Server and Deploying an App

To deploy an Elm-fullstack app, we place a front-end and back-end app in a single elm project, sharing an `elm.json` file. As long as we put the apps entry points in the right Elm modules, the Elm-fullstack tooling can deploy these together.

Here is an example app containing back-end and front-end: https://github.com/elm-fullstack/elm-fullstack/tree/c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33/implement/example-apps/docker-image-default-app

We can use this command to run a server and deploy an app:

```cmd
elm-fullstack  run-server  --public-urls="http://*:5000"  --deploy-app-from=https://github.com/elm-fullstack/elm-fullstack/tree/c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33/implement/example-apps/docker-image-default-app
```

When running this command, we get an output like this:

```txt
I got no path to a persistent store for the process. This process will not be persisted!
Loading app config to deploy...
Loaded source composition 0a748dfed56d1f88c51e604d35dd8fdde95bc4accfb9557e11359c60316f9413 from 'https://github.com/elm-fullstack/elm-fullstack/tree/c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33/implement/example-apps/docker-image-default-app'.
Starting the web server with the admin interface...
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Begin to build the process volatile representation.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Begin to restore the process state.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Found 1 composition log records to use for restore.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Restored the process state in 4 seconds.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Completed building the process volatile representation.
info: Kalmit.PersistentProcess.WebHost.StartupPublicApp[0]
      I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Started the public app at 'http://*:5000'.
Completed starting the web server with the admin interface at 'http://*:4000'.
```

When this server has completed starting, we can see the deployed app at http://localhost:5000/

## App Code Structure Conventions

This section covers the conventions for structuring the app code so that we can deploy it to an Elm-fullstack process. The [example apps](https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps) follow these conventions, but not every example app uses all available options, so the listing below is a more concise reference.

### `Backend.Main` Elm Module

The [main Elm module of the backend](/implement/example-apps/docker-image-default-app/src/Backend/Main.elm) contains the following functions which are called by the engine:

+ `interfaceToHost_initState : State`
+ `interfaceToHost_processEvent : String -> State -> ( State, String )`

As we can see in the examples, the `interfaceToHost_processEvent` takes care of deserializing the event coming from the host (the `String` parameter) and serializing the response for this specific event to the host (the `String` in the returned tuple). It delegates the rest of the processing to a function working with the types resulting from this (de)serialization:

```Elm
processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
```

Analogous to the update function in a front-end Elm app, this function returns the new state of the back-end app as the first element in the tuple.

### `ElmFullstackCompilerInterface.ElmMake` Elm Module

The `ElmMake` module provides an interface to run the `elm make` command and use the resulting file value in our Elm app.
For each name in this module, the full-stack compiler replaces the value with the output from `elm  make`.

Using the name in the Elm module, we specify the source file name and optional flags for `elm  make`. By default, the value we get is of type `Bytes.Bytes`.
Here is an example that depends on the source file located at path `src/FrontendWeb/Main.elm`:

```Elm
module ElmFullstackCompilerInterface.ElmMake exposing (..)


elm_make____src_FrontendWeb_Main_elm : Bytes.Bytes
elm_make____src_FrontendWeb_Main_elm =
    "The Elm-fullstack compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


elm_make__debug____src_FrontendWeb_Main_elm : Bytes.Bytes
elm_make__debug____src_FrontendWeb_Main_elm =
    "The Elm-fullstack compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
```

We can also get the value encoded as a base64 string instead of `Bytes.Bytes`, by adding the `base64` flag:

```Elm
elm_make__base64____src_FrontendWeb_Main_elm : String
elm_make__base64____src_FrontendWeb_Main_elm =
    "The Elm-fullstack compiler replaces this value."
```

Backend apps often use the output from `elm make` send the frontend to web browsers with HTTP responses. We can also see this in the [example app](https://github.com/elm-fullstack/elm-fullstack/blob/c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33/implement/example-apps/docker-image-default-app/src/Backend/Main.elm#L38-L50) mentioned earlier:

```Elm
    httpResponse =
        if
            httpRequestEvent.request.uri
                |> Url.fromString
                |> Maybe.map urlLeadsToFrontendHtmlDocument
                |> Maybe.withDefault False
        then
            { statusCode = 200
            , bodyAsBase64 = Just ElmFullstackCompilerInterface.ElmMake.elm_make__debug__base64____src_FrontendWeb_Main_elm
            , headersToAdd = []
            }

        else
```

### `ElmFullstackCompilerInterface.GenerateJsonCoders` Elm Module

This module provides automatically generated JSON encoders and decoders for Elm types.

By adding a declaration in this module, we instruct the compiler to generate a JSON encoder or decoder. A common use case for this automation is types used at the interface between the front-end and the back-end.

In this module, we can freely choose the names for functions, as we only need type annotations to tell the compiler what we want to have generated. To encode to JSON, add a function which takes this type and returns a `Json.Encode.Value`:

```Elm
jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")
```

To get a JSON decoder, declare a name for an instance of `Json.Decode.Decoder`:

```Elm
jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."
```

### `ElmFullstackCompilerInterface.SourceFiles` Elm Module

This module provides access to the app source code files.

By adding a name to this module, we can pick a source file and read its contents. The lowering step for this module happens before the one for the front-end. Therefore the source files are available to both front-end and back-end apps.

The [`rich-chat-room` example app uses this interface](https://github.com/elm-fullstack/elm-fullstack/blob/c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33/implement/example-apps/rich-chat-room/src/ElmFullstackCompilerInterface/SourceFiles.elm) to get the contents of the `readme.md` file in the app code directory and display it in the frontend:

```Elm
file____readme_md : Bytes.Bytes
file____readme_md =
    "The Elm-fullstack compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
```

To map the source file path to a name in this module, replace any non-alphanumeric character with an underscore. The directory separator (a slash or backslash on many operating systems) also becomes an underscore. Here are some examples:

| file path                        | Name in the Elm module                   |
| -------------------------------- | --------------------------               |
| `readme.md`                      | `file____readme_md`                      |
| `static/readme.md`               | `file____static_readme_md`               |
| `static/command-from-player.mp3` | `file____static_command_from_player_mp3` |

The compilation will fail if this module contains a name that matches more than one or none of the source files.

### `MigrateBackendState` Elm Module

We need to add the `MigrateBackendState` module when we choose to migrate the back-end state during an app's deployment. We encode the migration in a function named `migrate` with types matching previous app and new app accordingly.

In the simplest case, we did not change the back-end state model since the last deployment to the process. In this case, both input type and return type are the same. Then we can implement the whole module as follows:

```Elm
module MigrateBackendState exposing (migrate)

import Backend.Main


migrate : Backend.Main.State -> Backend.Main.State
migrate backendState =
    backendState
```

We don't have to return the same value here. We can also use the migration to make a custom atomic update to our back-end apps state.

Here is another example, almost as simple, with the back-end state just a primitive type, migrating from an `Int` to a `String`: https://github.com/elm-fullstack/elm-fullstack/blob/c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33/implement/test-elm-fullstack/example-elm-apps/migrate-from-int-to-string-builder-web-app/src/MigrateBackendState.elm

### `elm-fullstack.json`

The `elm-fullstack.json` file is where you can configure the acquisition of SSL certificates and rate-limiting of HTTP requests to the backend app.
Since these features are optional to use, in the simplest case, this file is not present at all.

## Running a Server With an Elm-Fullstack Process

At the beginning of this guide, we ran a server and deployed an app in a single command. But combining these two operations is not necessary. Deployments are part of the process history, which means the last deployment follows from the state of the process store. (To learn more about the persistence, see [persistence-in-elm-fullstack.md](./persistence-in-elm-fullstack.md))

When running a server, we want to configure two aspects: The location where to persist the process state, and the password to access the admin interface.
On startup, the server restores the state of the process from the given store location. During operation, it appends to the history in the same store. Currently, the only supported kind of store location is a directory on the file system.

Here is a complete command to run a server that maintains the persistence of the Elm-fullstack process:

```cmd
elm-fullstack  run-server  --process-store-directory-path=./process-store  --admin-password=secret  --admin-urls="http://*:4000"  --public-urls="http://*:5000"
```

When running this command, you will get an output like this:

```text
Starting the web server with the admin interface...
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Begin to build the process volatile representation.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Begin to restore the process state.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Found no composition record, default to initial state.
info: Kalmit.PersistentProcess.WebHost.StartupAdminInterface[0]
      Completed building the process volatile representation.
Completed starting the web server with the admin interface at 'http://*:4000'.
```

In case the process store contained a process in which an app was deployed, the output will also contain this message:

```text
Started the public app at 'http://*:5000'
```

This server continues running until you shut it down. It will output additional log messages for various events, for example, HTTP requests.

When you navigate to http://localhost:4000/ using a web browser, you find a prompt to authenticate. You can use the username `root` and the password `secret` that we specified in the command above.

When you log in at http://localhost:4000/, you will get this message:

```
Welcome to Elm-fullstack version 2020-10-17.
```

But we don't need a web browser to interact with the admin interface. The command-line interface offers a range of commands to operate a running server, for example, to deploy a new version of an app.

## Deploy an App to an Existing Server

Use the `elm-fullstack  deploy-app` command to deploy an app to a running server in an atomic operation.

With this command, we need to specify the path to the app to deploy and the destination site to deploy to.
Here is an example that matches the admin interface configured with the `run-server` command above:

```cmd
elm-fullstack  deploy-app  --site=http://localhost:4000  --from=https://github.com/elm-fullstack/elm-fullstack/tree/c66a7ec746fa6eb4b769f2914c53f3c7b3fb4a33/implement/example-apps/docker-image-default-app  --init-elm-app-state
```

The `--init-elm-app-state` option means we do not migrate the previous backend state but initialize the backend state from the init function.

Since the server requires us to authenticate for deployment, we will get this prompt when running the command from above:

```text
The server at 'http://localhost:4000/api/deploy-app-config-and-init-elm-app-state' is asking for authentication. Please enter the password we should use to authenticate there:
>
```

We enter the same password we gave with the `--admin-password` option on the command to run the server.

The `deploy-app` command also writes a report of the deployment attempt into a file under the current directory. It points out the exact path to the report file in a log message:
```text
Saved report to file 'C:\Users\John\elm-fullstack-tool\report\2020-05-26T09-26-34_deploy-app.json'.
```
In this report, we can see if the deployment was successful and how much time it took. If a migration fails, we also find a description of the problem in this report.

## Configure the Admin Password via Environment Variable

If you do not use the `--admin-password` option with the `run-server` command, the program will get the password from the environment variable `APPSETTING_adminPassword`.
Configuring the password using the environment variable makes it easier to reuse the standard Docker image:

```cmd
docker run -p 80:80 -p 4000:4000 --env "APPSETTING_adminPassword=secret" elmfullstack/elm-fullstack
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

