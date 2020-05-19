# How to Configure and Deploy Your Elm Full-Stack App

In this guide, I use the elm-fullstack command-line interface (CLI) to run an Elm-fullstack web app. You can download the CLI program from the releases section at https://github.com/elm-fullstack/elm-fullstack/releases

To register the elm-fullstack executable on your systems PATH environment variable, run the `elm-fullstack install-command` command.

## Running a Server With an Elm-Fullstack Process

First step is to run a server using the `elm-fullstack run-server` command. We need to configure two aspects of this server: The location where the process state should be persisted, and the password to access the admin interface.

Here is a complete command to run a server:

```cmd
elm-fullstack run-server --process-store-directory-path=./process-store  --admin-root-password=secret  --admin-interface-http-port=4000
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
To learn about this admin interface, see http://elm-fullstack.org/
```

But we don't need a web browser to interact with the admin interface. To deploy an app, we will use the command-line interface in a new terminal window.

## Full Stack App File Structure

### `elm-app/src/Backend/Main.elm`

The [main Elm module of the backend](/implement/PersistentProcess/example-elm-apps/default-full-stack-app/elm-app/src/Backend/Main.elm) contains the following functions which are called by the engine:

+ `interfaceToHost_initState : State`
+ `interfaceToHost_processEvent : String -> State -> ( State, String )`

As we can see in the examples, the `interfaceToHost_processEvent` takes care of deserializing the event coming from the host (the `String` parameter) and serializing the response for this specific event to the host (the `String` in the returned tuple). It delegates the rest of the processing to a function working with the types resulting from this (de)serialization:

```Elm
processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
```

Analogous to the update function in a client Elm app, this function returns the new state of your app as the first element in the tuple. The web server takes care of saving this state and automatically restores it in case the server restarts. When you stop and restart the docker container, you will find the server still has the state which resulted from processing the last event.

### `elm-app/src/FrontendWeb/Main.elm`

This file is optional. If it exists in your app, the build process compiles it to an HTML file and adds it to the static files as `FrontendWeb.html`.

### `elm-fullstack.json`

The `elm-fullstack.json` file is where you can configure the acquisition of SSL certificates, rate-limiting, and other features.
Since all of these features are optional to use, in the simplest case, this file is not present at all.

If your app includes a frontend, you need to decide on which paths the server should serve the HTML document containing the frontend.

Below is an example which directs HTTP requests to the static file of the frontend if the path does not start with `/api/` or `/elm-fullstack-admin/`:
```JSON
{
    "mapsFromRequestUrlToStaticFileName": [
        {
            "matchingRegexPattern": "^.*//[^/]+(|/(?!(api/|elm-fullstack-admin/)).*)$",
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

