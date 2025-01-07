# How to Configure and Deploy an Elm Backend App

This guide covers running a backend app programmed in Elm, including configuration, deployment, and migrations.
The backend includes a web server and a database persisting the application state and automating state migrations.

## Installing and Registering the `pine` Command

In this guide, I use the `pine` command-line interface (CLI) program. You can find all downloads at <https://pine-vm.org/downloads> and <https://github.com/pine-vm/pine/releases>

To register the pine executable on your system, run the `pine  install` command. If you use Linux or PowerShell on Windows, you can achieve this by running the following command after navigating to the directory containing the executable file extracted from the downloaded archive:

```text
./pine  install
```

In Windows, you will get a confirmation like this:

```text
I added the path 'C:\Users\John\Downloads\pine-bin-v0.3.0-win-x64' to the 'PATH' environment variable for the current user account. You will be able to use the 'pine' command in newer instances of the Command Prompt.
```

On Linux, the confirmation of the installation looks like this:

```text
I copied the executable file to '/bin/pine'. You will be able to use the 'pine' command in newer terminal instances.
```

## Running a Server and Deploying an App

As part of a deployment, Pine compiles the Elm app program code.
The compiler requires the program code to contain the entry point for a web server app. In addition, it offers various functions we can use independent of each other as needed. It supports projects without a front-end or with multiple front-ends apps.

Here is an example app containing backend and frontend: <https://github.com/pine-vm/pine/tree/67658db8f7e2ed50a9dd2a3ffcfaba2e20c7615d/implement/example-apps/docker-image-default-app>

We can use this command to run a server and deploy this app:

```cmd
pine  run-server  --public-urls="http://*:5000"  --deploy=https://github.com/pine-vm/pine/tree/67658db8f7e2ed50a9dd2a3ffcfaba2e20c7615d/implement/example-apps/docker-image-default-app
```

When running this command, we get an output like this:

```txt
I got no path to a persistent store for the process. This process will not be persisted!
Loading app config to deploy...
This path looks like a URL into a remote git repository. Trying to load from there...
This path points to commit 67658db8f7e2ed50a9dd2a3ffcfaba2e20c7615d
The first parent commit with same tree is https://github.com/pine-vm/pine/tree/5007bb0929fbd14e6bf701c97d048573e07fb672/implement/example-apps/docker-image-default-app
Loaded source composition 52e5acafc9ec2087e1b6700a2b4f0d9f8d199e15944f7bb51da8cc5545b6f58e from 'https://github.com/pine-vm/pine/tree/67658db8f7e2ed50a9dd2a3ffcfaba2e20c7615d/implement/example-apps/docker-image-default-app'.
Starting web server with admin interface (using engine JavaScript_V8 { })...
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      letsEncryptRenewalServiceCertificateCompleted: False
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Begin to build the process live representation.
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Begin to restore the process state.
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Found 1 composition log records to use for restore.
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Restored the process state in 0 seconds.
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Completed building the process live representation.
info: ElmTime.Platform.WebService.PublicAppState[0]
      disableLetsEncrypt: null
info: ElmTime.Platform.WebService.PublicAppState[0]
      I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: http://[::]:5000
info: Microsoft.Hosting.Lifetime[0]
      Application started. Press Ctrl+C to shut down.
info: Microsoft.Hosting.Lifetime[0]
      Hosting environment: Production
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Started the public app at 'http://[::]:5000'.
Completed starting the web server with the admin interface at 'http://*:4000'.
```

When this server has completed starting, we can see the deployed app at http://localhost:5000/

## App Code Structure Conventions

This section covers the conventions for structuring the app code so that we can deploy it using Pine. The [example apps](https://github.com/pine-vm/pine/tree/main/implement/example-apps) follow these conventions, but not every example app uses all available options, so the listing below is a more concise reference.

### `Backend.Main` Elm Module

The [main Elm module of the backend](/implement/example-apps/minimal-backend-hello-world/src/Backend/Main.elm) configures the backend with the declaration of `webServiceMain`:

```Elm
webServiceMain : Platform.WebService.WebServiceConfig ()
webServiceMain =
[...]
```

As we can see in the example apps, we compose the backend from an `init` value and an `subscriptions` function:

```Elm
webServiceMain : Platform.WebService.WebServiceConfig ()
webServiceMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }
```

### `Backend.MigrateState` Elm Module

We need to add the `Backend.MigrateState` module when we choose to migrate the back-end state during an app's deployment. We encode the migration in a function named `migrate` with types matching previous app and new app accordingly.

In the simplest case, we did not change the back-end state model since the last deployment to the process. In this case, both input type and return type are the same. Then we can implement the whole module as follows:

```Elm
module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebService


migrate : Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
migrate state =
    ( state, [] )
```

We don't have to return the same value here. We can also use the migration to make a custom atomic update to our back-end apps state.

Here is another example, almost as simple, with the back-end state just a primitive type, migrating from an `Int` to a `String`: <https://github.com/pine-vm/pine/blob/ba36482db83001b3ede203a92e56d31a30356b16/implement/test-elm-time/test-elm-apps/migrate-from-int-to-string-builder-web-app/src/Backend/MigrateState.elm>

### `web-service.json`

The `web-service.json` file is where we can configure the acquisition of SSL certificates and rate-limiting of HTTP requests to the backend app.
Since these features are optional to use, in the simplest case, this file is not present at all.

## Running a Server With an Pine Database

At the beginning of this guide, we ran a server and deployed an app in a single command. But combining these two operations is not necessary. Deployments are part of the process history, which means the last deployment follows from the state of the process store. (To learn more about the persistence, see [persistence-of-application-state-in-pine.md](./persistence-of-application-state-in-pine.md))

When running a server, we want to configure two aspects: The location where to persist the process state, and the password to access the admin interface.
On startup, the server restores the state of the process from the given store location. During operation, it appends to the history in the same store. Currently, the only supported kind of store location is a directory on the file system.

Here is a complete command to run a server that maintains the persistence of the Elm web service state:

```cmd
pine  run-server  --process-store=./process-store  --admin-password=test  --admin-urls="http://*:4000"  --public-urls="http://*:5000"
```

When running this command, we get an output like this:

```text
Starting web server with admin interface (using engine JavaScript_V8 { })...
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Begin to build the process live representation.
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Begin to restore the process state.
info: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Found no composition record, default to initial state.
fail: ElmTime.Platform.WebService.StartupAdminInterface[0]
      Found no composition record, default to initial state.
Completed starting the web server with the admin interface at 'http://*:4000'.
```

In case the process store contained a process in which an app was deployed, the output will also contain this message:

```text
Started the public app at 'http://*:5000'
```

This server continues running until we shut it down. It will output additional log messages for various events, for example, HTTP requests.

When we navigate to http://localhost:4000/ using a web browser, we find a prompt to authenticate. We can use the password `test` that we specified in the command above. We don't need a username in that prompt.

When we log in at http://localhost:4000/, we get this message:

```
Welcome to the Pine admin interface version 0.3.1.
```

But we don't need a web browser to interact with the admin interface. The command-line interface offers a range of commands to operate a running server, for example, to deploy a new version of an app.

## Deploy an App to an Existing Server

Use the `pine  deploy` command to deploy an app to a running server in an atomic operation.

With this command, we need to specify the path to the app to deploy and the destination site to deploy to.
Here is an example that matches the admin interface configured with the `run-server` command above:

```cmd
pine  deploy  --init-app-state  https://github.com/pine-vm/pine/tree/67658db8f7e2ed50a9dd2a3ffcfaba2e20c7615d/implement/example-apps/docker-image-default-app  http://localhost:4000
```

The `--init-app-state` option means we do not migrate the previous backend state but reset it to the value from the init function.

Since the server requires us to authenticate for deployment, we will get this prompt when running the command from above:

```text
The server at 'http://localhost:4000/api/deploy-and-init-app-state' is asking for authentication. Please enter the password we should use to authenticate there:
>
```

We enter the same password we gave with the `--admin-password` option on the command to run the server.

The `pine  deploy` command also writes a report of the deployment attempt into a file under the current directory. It points out the exact path to the report file in a log message:
```text
Saved report to file 'C:\Users\John\pine-tool\report\2023-02-17T15-28-25_deploy.json'.
```

In this report, we can see if the deployment was successful and how much time it took. If a migration fails, we also find a description of the problem in this report.

## Configure the Admin Password via Environment Variable

If you do not use the `--admin-password` option with the `run-server` command, the program will get the password from the environment variable `APPSETTING_adminPassword`.
Configuring the password using the environment variable makes it easier to reuse the standard Docker image:

```cmd
docker  run  -p 5000:80  -p 4000:4000  --env "APPSETTING_adminPassword=test"  ghcr.io/pine-vm/pine
```

## Manage the Process Store

The process store contains not only the latest state of the app but also the event log.
In the Docker image `pine-vm/pine`, the process store is located in the directory `/pine/process-store`.
You can copy this directory to backup the process store or copy it to another container.

Alternatively, use a [docker volume](https://docs.docker.com/storage/volumes/) to map this directory to another location:
```powershell
docker  run  --mount source=your-docker-volume-name,destination=/pine/process-store  -p 80:80  -p 4000:4000  ghcr.io/pine-vm/pine
```

## Support HTTPS

The Pine web host supports HTTPS. Thanks to the [`FluffySpoon.AspNet.LetsEncrypt`](https://github.com/ffMathy/FluffySpoon.AspNet.LetsEncrypt) project, it can automatically get an SSL certificate from [Let's Encrypt](https://letsencrypt.org/). To configure this, add a `letsEncryptOptions` property to the `web-service.json` file as follows:
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

