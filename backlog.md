## Backlog

### Application Operation

+ Support application to configure tradeoff between the cost of persisting and cost of restoring.
+ Reduce load on storage: Provide automation to remove reductions which are not needed anymore from the store.

### Collaboration on Elm-Fullstack

+ Simplify modeling of tests: Support modeling Elm app using a string for the main (and only) module contents, using the default `elm.json`.

Ensure people can easily understand for a given change how well it would fit into the project:

+ Increase test coverage: The process store can reliably model values as offered on the interface. (E.g. line-breaks (or similar UTF sequences) in the serialized event do not damage the composition store)
+ Add an automated test for using the CLI to build a web app configuration from local files.
