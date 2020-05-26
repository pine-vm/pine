# Persistence in Elm-fullstack

This guide assumes you already know how state changes work in [Elm](https://elm-lang.org) applications. To learn about the role of the `update` function as used here, have a look at [The Elm Architecture (TEA) animation](https://medium.com/@l.mugnaini/the-elm-architecture-tea-animation-3efc555e8faf) by lucamug. Note that his illustration contains 'view' and 'DOM' parts which are specific to client-side applications and not used here. The persistence functionality only applies to the backend.

## Persistence for Development and Operation

Immediate automatic persistence is one of the main features of the Elm-fullstack web host. Every [`update`](http://toreto.re/tea/) of the application state is immediately persisted.

What does 'immediate automatic persistence' mean exactly? Let's break this down and define these terms.
+ Why *persistence*? After the server application has confirmed or reported a transaction to an external party, it should remember the resulting state. The framework records every change of the application state, to support consistent continuation even after sudden shutdown of the hosting machine. Since server applications run for months and years, such an interruption is bound to happen sooner or later. When the system starts up again, the application state is restored automatically.
+ *Immediately* means before other effects from an update become observable. The commands emitted by the update function are only applied after the state from the same update is saved. For example, when a client receives an HTTP response from your server, you know the new state of the server is already persisted.
+ I use the attribute *automatic* to clarify that the implementation of this persistence requires no attention by the app developer, as the framework takes care of it. Since the app state itself is persistent, considering a database in the application code is not necessary.

## Events in the Elm-fullstack Process

The sequence of events that entered an Elm-fullstack process defines its complete state. The process state includes the current code of the backend Elm-app, which determines the reaction to clients' requests on the public interface. Deployment events are the way to transition from one backend Elm app to another one. In the history of an Elm-fullstack process, we find these kinds of events:

+ Update for an event in the Elm app: This kind usually accounts for more than 99.9% of the events, handling clients HTTP requests and task results. In the guide on the Elm-architecture, this event is often called a 'message'.
+ Set the state of the Elm app: Used for manual intervention, for example, when migrating the state of an app for which no copy of the process store is available.
+ Deploy app config and migrate Elm app state: Deploy a new configuration, including an Elm app for the backend. Uses the migrate function in the Elm code to migrate the state from the previously active Elm app.
+ Deploy app config and init Elm app state: Deploy a new configuration, including an Elm app for the backend. In contrast to the deployment with migration, this one uses the init function in the Elm code to initialize the state of the backend.

To learn how to do deployments and migrations, see the guide on [How to Configure and Deploy an Elm-Fullstack App](guide/how-to-configure-and-deploy-an-elm-fullstack-app.md).

## Under the Hood - How It Works

After clarifying observable parts and guarantees from the runtime, the remainder of this guide illustrates what happens under the hood to make the persistence possible and performant.

### Recovering After Unexpected Shutdown

During operation of the process, the system records the sequence of events entering the process. Events are appended to a composition log, contained in composition records. Each composition record also contains the SHA256 hash of the previous composition record. These references from one record to another form a chain enabling complete traceability. A reader can determine the original order of the processed events using the hash references, even if the records have been shuffled around in the meantime.

On startup, the Elm-fullstack web host uses the composition log to restore the process state. It reads the sequence of events and replays the updates to compose the process state again.

For an automated test covering the restoration of the process state, see the [method `Restore_process_state_over_compositions` in the implementation](https://github.com/elm-fullstack/elm-fullstack/blob/512bea42674f6d214745c73af8d20c52bca096f6/implement/PersistentProcess/PersistentProcess.Test/TestPersistentProcess.cs#L111-L153).

### Optimizing for Faster Replication

For real-world applications, it is not practical to rely on replaying the whole history every time we want to replicate a process state. Even more so, since replication is not only used for restoring in production but also for experimentation and performance profiling.
Besides the time it takes for replaying the composition, a growing composition chain takes up more and more storage space.

To solve both the time and storage space problems, Elm-fullstack also stores reductions of the composition chain at regular intervals. The reduced value equals the process state derived from the reduced composition chain. When replicating the process state, the composition chain is not anymore read back to the beginning of history, but only up to the newest available reduction record. Since the earlier part of the composition log is not needed anymore for replication, we can truncate it, freeing up storage space.

The automated test [`Restore_process_state_from_combination_of_reduction_and_compositions` in the implementation](https://github.com/elm-fullstack/elm-fullstack/blob/512bea42674f6d214745c73af8d20c52bca096f6/implement/PersistentProcess/PersistentProcess.Test/TestPersistentProcess.cs#L155-L220) demonstrates the recovery of process states from truncated composition logs, with the help of reductions.
