# State Migrations In Web Service Applications

Since Pine integrates a database management system and application runtime, it also handles deployments and migrations. When we deploy a new version of a web service app, Pine automatically migrates the application state. Like any other event in the Elm app, a database migration is an atomic transaction. If the deployment or migration fails, the app continues running as if we had never attempted a deployment.

Migrations happen as part of deployments, so we do not trigger them explicitly. By default, every deployment implies a migration. But sometimes, we prefer to throw away the current backend state instead of coding a migration. For these cases, the API offers an option to take the app state from an init function instead of looking for a migration. In the command-line interface, this option surfaces as `--init-app-state` on the `deploy` command.

## Implementing Migrations

When deploying a new app version, we describe how to map values from the previous state using a migration function.
This migration function looks like a normal Elm function.
The only difference is that it is located in a module named `Backend.MigrateState` and has to have the name `migrate`. Here is a simple example of a migration function:

```Elm
module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebService
import PrevBackendStateType


migrate : PrevBackendStateType.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
migrate state =
    ( state, [] )

```

The `PrevBackendStateType.State` of the first parameter represents the state type of the app before the migration. We are free to choose the name of this type, and we can also place it in a separate module.

The return value of the migration function is a tuple with the new state and a list of commands to execute after the migration. The Elm runtime will execute these commands after the migration is complete.

In the simplest case, we did not change the state model since the last deployment to the process. In this case, both input type and return type are the same. Then, we can implement the whole module as follows:

```Elm
module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebService


migrate : Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
migrate state =
    ( state, [] )
```

**Note**: If we did not change the state type of our app, as in the last example above, the runtime also accepts deployments without a migration function. The runtime will then accept a deployment even if the module `Backend.MigrateState` does not exist.

> Note: If we only want to perform a custom update on our database without deploying new program code, it's easier to use the dedicated database functions feature: <https://michaelrätzel.com/blog/database-functions-in-elm-time-easy-database-updates-in-production>

Here is another example, almost as simple, with the back-end state just a primitive type, migrating from an `Int` to a `String`: <https://github.com/pine-vm/pine/blob/ba36482db83001b3ede203a92e56d31a30356b16/implement/test-elm-time/test-elm-apps/migrate-from-int-to-string-builder-web-app/src/Backend/MigrateState.elm>

The migration runs as part of a deployment of our app, which we can start via the `pine  deploy` command:

```cmd
pine  deploy  ./my-elm-web-service-source/  website.com
```

Of course, we can also test our migration on a local copy before deploying it to production.

## Relaxation of Type Checking in State Migrations

Compared to the Elm compiler, Pine's migration system is more lenient regarding type-checking. Instead of looking at the type declaration of the previous application program code, the migration system uses the concrete record field names and tag names in the database to check they correspond to the type referenced by the migration function.

This relaxation of type checking means we need to be more careful when writing the type representing the previous state given to a migration function. For example, if a list is empty, the migration system will accept any type for the list items.

The following constraints remain for type-checking in migrations:

+ For record types, the type can only have fields also present in the current record value. The migration will fail if a record type has additional fields not present in the current record value.
+ For choice types (union types), the type needs to have all the tag names found in the concrete values in the database. The migration will fail if one of these tags is missing in the type declaration.

In practice, a simple way to get total strictness in type-checking is to (automatically) copy the state type declaration tree into a dedicated module before making any changes and then use this module for the migration function.

For more information on app deployment, see the guide on [How to Configure and Deploy an Elm Backend App](./how-to-configure-and-deploy-an-elm-backend-app.md)

