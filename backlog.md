## Backlog

### Integration Development

+ Automate boring tasks: Support deriving the functions for serializing and deserializing from the app state Elm type. This depends on parsing imports, types and aliases from modules.
+ Simplify modeling of tests: Support modeling Elm app using a string for the main (and only) module contents, using the default `elm.json`.
+ Support [function-level dead code elimination](https://elm-lang.org/blog/small-assets-without-the-headache): Generate the Elm code needed to inform the Elm compiler about our entry points.
+ Illustrate how the web-app-config is composed.
+ Support hosted app choosing frequency when subscribing to time. (Could be modeled for example with Response of type `Subscriptions`, (Posix time cyclic with distance in ms, Posix time once)).

### Integration Operation

+ Support caching per URL for requests using the "GET" method. Probably the URL patterns here should be modeled with the same model as the one used to redirect to static files.
+ Support integrating app to configure tradeoff between the cost of persisting and cost of restoring.
+ Reduce load on storage: Provide automation to remove reductions which are not needed anymore from the store.
+ Consolidate Hash Representation: E.g.: Why is the `ParentHash` in the composition log mapped to a different representation than the file names for reductions?
+ Expand support for modeling appended events in composition: Also support event description via hash, reflect the literal quality of the existing properties in their names (`literalAsString`?). Is there a reason to have different models for event and reduction? If we cannot find such a reason, consolidate them.
+ Add information in the admin section on the web host at `/kalmit-admin`. This seems a good place to inform about available options. Illustrate how to set the process state.

### Collaboration

Ensure people can easily understand for a given change how well it would fit into the project:

+ Increase test coverage: The process store can reliably model values as offered on the interface. (E.g. line-breaks (or similar UTF sequences) in the serialized event do not damage the composition store)
