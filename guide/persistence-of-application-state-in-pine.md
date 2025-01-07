# Persistence of application state in Pine

This guide assumes you already know how state changes work in [Elm](https://elm-lang.org). To learn about the role of the `update` function as used here, have a look at [The Elm Architecture (TEA) animation](https://medium.com/@l.mugnaini/the-elm-architecture-tea-animation-3efc555e8faf) by lucamug. Note that his illustration contains 'view' and 'DOM' parts which are specific to client-side applications and not used here. The persistence functionality only applies to the backend.

## Persistence for Development and Operation

Immediate automatic persistence of the application state is one of the main features of Pine. Every update is an atomic transaction in the database.

What does 'immediate automatic persistence' mean exactly? Let's break this down and define these terms.

+ Why *persistence*? After the server application has confirmed or reported a transaction to an external party, it should remember the resulting state. The framework records every change in the application state to support consistent continuation even after a sudden shutdown of the hosting machine. Since server applications run for months and years, such an interruption is bound to happen sooner or later. When the system starts up again, Pine restores the application state automatically.
+ *Immediately* means before effects from an update become observable. Elm update functions can return commands, and Pine starts these commands after writing the state update to storage. For example, when a client receives an HTTP response from your Elm web service, you know the new state of the app is already persisted.
+ *automatic* means implementing this persistence requires no attention from application developers, as Pine takes care of it. Since the app state is persistent, considering a database in the application code is unnecessary.

## Automatic Migration on Deployment

Since Pine integrates the database management system and application runtime, it also handles deployments and migrations. When we deploy a new app version, Pine automatically migrates the application state in an atomic transaction. It detects type mismatches in migrations and rejects invalid deployments accordingly. Like any other event in the Elm app, a database migration is an atomic operation. If the deployment or migration fails, the app continues running as if we had never attempted a deployment.

## Implementing Migrations

We add the `Backend.MigrateState` module to our app source code files to implement a migration. In this module, we encode the migration in a function named `migrate` with types matching the previous app and the new app accordingly.

In the simplest case, we did not change the back-end state model since the last deployment to the process. In this case, both input type and return type are the same. Then, we can implement the whole module as follows:

```Elm
module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebService


migrate : Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
migrate state =
    ( state, [] )
```

We don't have to return the same value here. We can also use the migration to make a custom atomic update to our database.

> Note: If we only want to perform a custom update on our database without deploying new program code, it's easier to use the dedicated database functions feature: <https://michaelrätzel.com/blog/database-functions-in-elm-time-easy-database-updates-in-production>

Note: If we did not change the state type of our app, the runtime also accepts deployments without a migration function. The runtime will then accept a deployment even if the module `Backend.MigrateState` does not exist.

Here is another example, almost as simple, with the back-end state just a primitive type, migrating from an `Int` to a `String`: <https://github.com/pine-vm/pine/blob/ba36482db83001b3ede203a92e56d31a30356b16/implement/test-elm-time/test-elm-apps/migrate-from-int-to-string-builder-web-app/src/Backend/MigrateState.elm>

The migration runs automatically as part of a deployment of our app, which we can start via the `pine  deploy` command:

```cmd
pine  deploy  ./my-elm-web-service-source/  website.com
```

Of course, we can also test our migration on a local copy before deploying it to production.

For more information on app deployment, see the guide on [How to Configure and Deploy an Elm Backend App](./how-to-configure-and-deploy-an-elm-backend-app.md)
