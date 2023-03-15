# Persistence of application state in Elm-Time

This guide assumes you already know how state changes work in [Elm](https://elm-lang.org). To learn about the role of the `update` function as used here, have a look at [The Elm Architecture (TEA) animation](https://medium.com/@l.mugnaini/the-elm-architecture-tea-animation-3efc555e8faf) by lucamug. Note that his illustration contains 'view' and 'DOM' parts which are specific to client-side applications and not used here. The persistence functionality only applies to the backend.

## Persistence for Development and Operation

Immediate automatic persistence of the application state is one of the main features of Elm-Time. Every update is an atomic transaction in the database.

What does 'immediate automatic persistence' mean exactly? Let's break this down and define these terms.

+ Why *persistence*? After the server application has confirmed or reported a transaction to an external party, it should remember the resulting state. The framework records every change in the application state to support consistent continuation even after a sudden shutdown of the hosting machine. Since server applications run for months and years, such an interruption is bound to happen sooner or later. When the system starts up again, Elm-Time restores the application state automatically.
+ *Immediately* means before effects from an update become observable. Elm update functions can return commands, and Elm-Time starts these commands after writing the state update to storage. For example, when a client receives an HTTP response from your server, you know the new state of the app is already persisted.
+ *automatic* means implementing this persistence requires no attention from application developers, as Elm-Time takes care of it. Since the app state is persistent, considering a database in the application code is unnecessary.

## Automatic migration on deployment

Since Elm-Time integrates the database management system and application runtime, it also handles deployments and migrations. When we deploy a new app version, Elm-Time automatically migrates the application state in an atomic transaction. It detects type mismatches in migrations and rejects invalid deployments accordingly.
See the dedicated guide to learn more about the deployment and migrations of Elm applications.
