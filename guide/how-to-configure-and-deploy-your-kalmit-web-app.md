# How to Configure and Deploy Your Kalmit Web App

The configuration of Kalmit web apps is done in the `web-app-config.zip` file. This ZIP archive contains the Elm app, configuration for Let's Encrypt, rate limiting, static files, and other features.

## `web-app-config.zip`

An example is the default app in the project repository at [/implementation/PersistentProcess/default-config/web-app/web-app-config.zip](https://github.com/Viir/Kalmit/blob/8a28424f5d550c0583293002b26568c5ea128ba8/implementation/PersistentProcess/default-config/web-app/web-app-config.zip)

The general structure of `web-app-config.zip` is as follows:

```
web-app-config.zip
+-- map.json
+-- elm-app.zip
    +-- elm-app.map.json
    +-- elm-app
        +-- elm.json
        +-- ElmAppInKalmitProcess.elm
        +-- Your Elm Files
+-- (Optional) static-files
```

### `map.json`

The `map.json` file is where you can add configuration for Let's Encrypt, rate-limiting and other features.
Since all of these features are optional to use, in the simplest case, it only contains an empty json object:
```json
{
}
```

### `elm-app.zip`

The Elm app is contained in the `elm-app` directory in the `elm-app.zip` archive. At the root level, the archive contains the `elm-app.map.json` file to tell the framework which functions in your elm app to use. It looks like this:
```json
{
    "WithCustomSerialization": {
        "pathToFileWithElmEntryPoint": "DefaultWebApp.elm",
        "pathToInitialStateFunction": "DefaultWebApp.initState",
        "pathToSerializedEventFunction": "DefaultWebApp.processSerializedEvent",
        "pathToSerializeStateFunction": "DefaultWebApp.serializeState",
        "pathToDeserializeStateFunction": "DefaultWebApp.deserializeState"
    }
}
```

## Deploy Using Docker

The easiest way to deploy your Kalmit web app is using Docker. The instructions below depend on familiarity with Docker, at least knowing the relation between image and container is a precondition. The base image [`kalmit/kalmit-web-app`](https://hub.docker.com/r/kalmit/kalmit-web-app) contains everything you need to start a web app:
```powershell
docker run -p 80:80 kalmit/kalmit-web-app
```
Docker will then forward you logs like these:
```powershell
info: Kalmit.PersistentProcess.WebHost.Startup[0]
      I did not find letsEncryptOptions.
Hosting environment: Production
Content root path: /kalmit
Now listening on: http://[::]:80
Now listening on: https://[::]:443
Application started. Press Ctrl+C to shut down.
```
To deploy your own web app, add your `web-app-config.zip` file to that Docker image. You can do this by building a dockerfile as follows:
```dockerfile
FROM kalmit/kalmit-web-app

COPY ./web-app-config.zip /kalmit
```

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

To ensure HTTPS requests reach the app, also map port `443` of the docker container:
```powershell
docker run -p 80:80 -p 443:443 your-docker-image
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
