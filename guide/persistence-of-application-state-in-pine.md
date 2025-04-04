# Persistence of Application State in Pine

This guide assumes you already know how state changes work in [Elm](https://elm-lang.org). To learn about the role of the `update` function as used here, have a look at [The Elm Architecture (TEA) animation](https://medium.com/@l.mugnaini/the-elm-architecture-tea-animation-3efc555e8faf) by lucamug. Note that his illustration contains 'view' and 'DOM' parts which are specific to client-side applications and not used here. The persistence functionality only applies to the backend.

## Persistence for Development and Operation

Immediate automatic persistence of the application state is one of the main features of Pine. Every update is an atomic transaction in the database.

What does 'immediate automatic persistence' mean exactly? Let's break this down and define these terms.

+ Why *persistence*? After the server application has confirmed or reported a transaction to an external party, it should remember the resulting state. The framework records every change in the application state to support consistent continuation even after a sudden shutdown of the hosting machine. Since server applications run for months and years, such an interruption is bound to happen sooner or later. When the system starts up again, Pine restores the application state automatically.
+ *Immediately* means before effects from an update become observable. Elm update functions can return commands, and Pine starts these commands after writing the state update to storage. For example, when a client receives an HTTP response from your Elm web service, you know the new state of the app is already persisted.
+ *automatic* means implementing this persistence requires no attention from application developers, as Pine takes care of it. Since the app state is persistent, considering a database in the application code is unnecessary.

## Automatic Migration on Deployment

Since Pine integrates the database management system and application runtime, it also handles deployments and migrations. When we deploy a new app version, Pine automatically migrates the application state in an atomic transaction.

To learn more about state migrations, see the dedicated guide: [State Migrations in Web Service Applications](./state-migrations-in-web-service-applications.md)
