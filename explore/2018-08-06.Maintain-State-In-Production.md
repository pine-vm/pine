This document reported on the state in August 2018, before work on the open source implementation started.

# Maintain State In Production

## Observations

Observations from video game development projects:

Games are implemented to run in popular web browsers, game mechanics are modeled using the Elm language. Eventually, games support people playing together, over a central server. Both server and client implementations integrate the game mechanics. On the server, the state should be maintained over longer time spans and needs to be recovered in case of unexpected shutdown of the host.
Now the challenge is to model the game mechanics on the server so they can be executed in a persistent environment.

Asking around, it looks like there is no public solution for Elm:
https://discourse.elm-lang.org/t/how-to-maintain-state-in-production-and-recover-from-unexpected-shutdown/1063

## Alternative Approaches

Some options to solve the problem of modeling the game mechanics and maintaining state in production:
+ On the server, use a language for which the necessary persistency framework already exists. Manually maintain the symmetry of the implementations between client and server. Problem with this approach is that cost of changes rises as humans take over work from the compiler.
+ Use a language for which a tool exists to compile down to both the frameworks used on client and server. Problem with this approach is finding such a tool that does not require sacrificing the efficiency of the current modeling approach.
+ Maintain functions to serialize and deserialize the app state in the Elm codebase. Then run the app in a framework which takes care of invoking the serializing and deserializing functions to save and restore the app state.

## Current State

The last described approach went live. The framework invokes the serializing function after each processing of an event. On startup, the last state obtained this way is given to the Elm app to continue from. The Elm app is compiled to Javascript and runs on a Javascript engine. Some implementation tasks resulting from this approach:
+ Enable the host to invoke the interfacing functions in the Elm app: The current solution changes the Javascript file obtained from the Elm compiler to make functions visible on the global scope.
+ State handling: The state is kept in a global variable in the Javascript engine, so it does not need to be restored each time an event is processed. To support recovery after an unexpected interruption, the serialized state is obtained from the Javascript engine after integrating an event.
+ Maintaining the implementation for serialization and deserialization: So far, this is done manually in Elm code. In contrast to the implementation tasks listed above, this needs to be done every time the app state model changes. Therefore it seems reasonable to automate the derivation of these functions from the Elm type definitions.

## Current Goals

+ Reduce the cost of creating and maintaining integrating apps by making the functionality for persistence available in a way that is easier to reuse.
+ Reduce costs by automating more parts of the implementation work.

## Future

Besides this primary focus, other features will become interesting eventually:
To support debugging, understanding how the app arrived in a state can be interesting. To support this, a history needs to be recorded.
As the size of state and frequency of events rise, runtime expenses will also increase in two ways:
+ Processing time needed to create the full snapshots of the application state.
+ Storage space needed to keep the history.

These costs can be reduced by making full snapshots less often. To maintain the ability to restore despite not making a full snapshot after each event, states are restored by replaying events. This way, the cost of persisting can be traded off against the cost of restoring by choosing the frequency of full snapshots.
