# How to Configure and Deploy Your Elm Full-Stack App

In this guide, I use the elm-fullstack command-line interface (CLI) to run an Elm-fullstack web app. You can download the CLI program from the releases section at https://github.com/elm-fullstack/elm-fullstack/releases

To register the elm-fullstack executable on your systems PATH environment variable, run the `elm-fullstack install-command` command.

## Running a Server With an Elm-Fullstack Process

First step is to run a server using the `elm-fullstack run-server` command. We need to configure two aspects of this server: The location where the process state should be persisted, and the password to access the admin interface.

Here is a complete command to run a server:

```cmd
elm-fullstack  run-server  --process-store-directory-path=./process-store  --admin-root-password=secret  --admin-interface-http-port=4000
```

(There are more options available when running a server, you can see these options using the command `elm-fullstack run-server --help`.)

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

This server continues running until you shut it down. It will output additional log messages for various events, for example, HTTP requests.

When you navigate to http://localhost:4000/ using a web browser, you find a prompt to authenticate. You can use the username `root` and the password `secret` that we specified in the command above.

When you log in at http://localhost:4000/, you will get this message:

```
Welcome to Elm-fullstack version 2020-05-18.
```

But we don't need a web browser to interact with the admin interface. The command-line interface offers a range of commands to operate a running server, for example, to deploy a new version of an app.

## Deploying an App

To deploy an Elm-fullstack app, we place a front-end and back-end app in a single elm project, sharing an `elm.json` file. As long as we put the apps entry points in the right Elm modules, the Elm-fullstack tooling can deploy these together.

Here is an example app containing back-end and front-end: https://github.com/elm-fullstack/elm-fullstack/tree/46c6172fd3bf3827dfa2de47297d1a46b51d1cf2/implement/PersistentProcess/example-elm-apps/default-full-stack-app

One way to deploy an app is to do it at the same time as starting a new server. We can use the `--deploy-app-config` option with the `run-server` command. You can try this by running this command from the directory https://github.com/elm-fullstack/elm-fullstack/tree/46c6172fd3bf3827dfa2de47297d1a46b51d1cf2/implement/PersistentProcess/example-elm-apps/default-full-stack-app (This is the directory containing the `elm-fullstack.json` file)

```cmd
elm-fullstack  run-server  --process-store-directory-path=./process-store  --public-urls="http://*:5000"  --deploy-app-config  --delete-previous-process
```

When this server has completed starting, we can see the app at http://localhost:5000/

## App File and Module Name Conventions

This section covers the conventions for structuring the app code so that we can deploy it to an Elm-fullstack process.

### `elm-app/src/Backend/Main.elm`

The [main Elm module of the backend](/implement/PersistentProcess/example-elm-apps/default-full-stack-app/elm-app/src/Backend/Main.elm) contains the following functions which are called by the engine:

+ `interfaceToHost_initState : State`
+ `interfaceToHost_processEvent : String -> State -> ( State, String )`

As we can see in the examples, the `interfaceToHost_processEvent` takes care of deserializing the event coming from the host (the `String` parameter) and serializing the response for this specific event to the host (the `String` in the returned tuple). It delegates the rest of the processing to a function working with the types resulting from this (de)serialization:

```Elm
processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
```

Analogous to the update function in a front-end Elm app, this function returns the new state of the back-end app as the first element in the tuple.

### `elm-app/src/MigrateBackendState.elm`

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

Here is another example, almost as simple, with the back-end state just a primitive type, migrating from an `Int` to a `String`: https://github.com/elm-fullstack/elm-fullstack/blob/46c6172fd3bf3827dfa2de47297d1a46b51d1cf2/implement/PersistentProcess/example-elm-apps/migrate-from-int-to-string-builder-web-app/src/MigrateBackendState.elm

### `elm-app/src/FrontendWeb/Main.elm`

This file is optional. If it exists in your app, the build process compiles it to an HTML file and adds it to the static files as `FrontendWeb.html`.

### `elm-fullstack.json`

The `elm-fullstack.json` file is where you can configure the acquisition of SSL certificates, rate-limiting, and other features.
Since all of these features are optional to use, in the simplest case, this file is not present at all.

If your app includes a front-end, you need to decide on which paths the server should serve the HTML document containing the front-end.

Below is an example which directs HTTP requests to the static file of the front-end if the path does not start with `/api/`:
```JSON
{
    "mapsFromRequestUrlToStaticFileName": [
        {
            "matchingRegexPattern": "^.*//[^/]+(|/(?!(api/)).*)$",
            "resultString": "FrontendWeb.html"
        }
    ]
}
```

## Configure the Admin Password via Environment Variable

If you do not use the `--admin-root-password` option with the `run-server` command, the program will get the password from the environment variable `APPSETTING_adminRootPassword`.
Configuring the password using the environment variable makes it easier to reuse the standard Docker image:

```cmd
docker run -p 80:80 -p 4000:4000 --env "APPSETTING_adminRootPassword=secret" elmfullstack/elm-fullstack
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

