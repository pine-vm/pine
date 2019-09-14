# How to Configure and Deploy Your Kalmit Web App

Following is the easiest way to build and deploy your full-stack Elm app:

+ Make sure you have [Docker](https://www.docker.com) installed.
+ Clone this repository.
+ Run the script at [`/implement/PersistentProcess/start-server.ps1`](/implement/PersistentProcess/start-server.ps1). This script takes the example app from [/implement/PersistentProcess/example-elm-apps/default-full-stack-app](/implement/PersistentProcess/example-elm-apps/default-full-stack-app) and builds a docker image running this app.
This script also contains a `docker run` command to start the app. Docker will then forward you logs like these:
```shell
info: Kalmit.PersistentProcess.WebHost.Startup[0]
      Loaded configuration 4C632674652AC57C514DA1FD57FE57F1906037D47147E896DC85F02361AFBA9F
info: Kalmit.PersistentProcess.WebHost.Startup[0]
      I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.
info: Kalmit.PersistentProcess.WebHost.Startup[0]
      Begin to build the persistent process for Elm app 5BD744FCCBF4A6C1F727DBA62816189ADB7FC18745FD5426DAAA894E4B345330
info: Kalmit.PersistentProcess.WebHost.Startup[0]
      Begin to restore the process state using the storeReader.
info: Kalmit.PersistentProcess.WebHost.Startup[0]
      Found no composition record, default to initial state.
info: Kalmit.PersistentProcess.WebHost.Startup[0]
      Completed building the persistent process.
Hosting environment: Production
Content root path: /kalmit
Now listening on: http://[::]:80
Now listening on: https://[::]:443
Application started. Press Ctrl+C to shut down.
```
+ When you open this site in a web browser, you will find a SPA which connects to the backend using HTTP requests. So this example app contains a backend (entry point in [elm-app/src/Main.elm](/implement/PersistentProcess/example-elm-apps/default-full-stack-app/elm-app/src/Main.elm) and a frontend (entry point in [elm-app/src/FrontendWeb/Main.elm](/implement/PersistentProcess/example-elm-apps/default-full-stack-app/elm-app/src/FrontendWeb/Main.elm)).

## Full Stack App File Structure

### Backend `Main.elm`

The [main Elm module of the backend](/implement/PersistentProcess/example-elm-apps/default-full-stack-app/elm-app/src/Main.elm) contains the following functions which are called by the engine:

+ `interfaceToHost_processEvent : String -> State -> ( State, String )`
+ `interfaceToHost_initState : State`
+ `interfaceToHost_serializeState : State -> String`
+ `interfaceToHost_deserializeState : String -> State`

### `map.json`

The `map.json` file is where you can add configuration for Let's Encrypt, rate-limiting, and other features.
Since all of these features are optional to use, in the simplest case, this file is not present at all.

## Support HTTPS

The Kalmit web host supports HTTPS. Thanks to the [`FluffySpoon.AspNet.LetsEncrypt`](https://github.com/ffMathy/FluffySpoon.AspNet.LetsEncrypt) project, it can automatically get an SSL certificate from Let's Encrypt. To configure this, add a `letsEncryptOptions` property to the `map.json` file as follows:
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

## Set a Password for the Admin Interface

In case you want to use the admin interface, you can set it using the environment variable `APPSETTING_adminRootPassword`. The username is `root`.
You can use these credentials for example at https://your-domain.com/kalmit-admin/api/process/state to inspect or set the process state.
When using Docker, you can set the environment variable when creating a container as follows:
```cmd
docker run -p 80:80 --env "APPSETTING_adminRootPassword=secret" kalmit/kalmit-web-app
```
For more details about environment variables in docker, see https://docs.docker.com/engine/reference/run/#env-environment-variables

## Manage the Process Store

The process store contains not only the latest state of the app but also the event log.
In the Docker image `kalmit/kalmit-web-app`, the process store is located in the directory `/kalmit/process-store`.
You can copy this directory to backup the process store or copy it to another container.

Alternatively, use a [docker volume](https://docs.docker.com/storage/volumes/) to map this directory to another location:
```powershell
docker run --mount source=your-docker-volume-name,destination=/kalmit/process-store -p 80:80 kalmit/kalmit-web-app
```
