# Kalmit

Simple Game Development

## Maintain State In Production

Current focus is the project to support Elm as a language to model game mechanics while at the same time offering a framework to maintain state in production. See the project description at [projects/Maintain State In Production.md](projects/Maintain%20State%20In%20Production.md)

### Backlog

#### Reduce Development Expenses

+ Support web browsers as clients: Implement a framework to map HTTP requests to events and responses to HTTP responses.
+ Automate boring tasks: Support deriving the functions for serializing and deserializing from the app state Elm type.
+ Simplify modeling of tests: Support modeling Elm app using a string for the main (and only) module contents, using the default `elm.json`.
+ Support [function-level dead code elimination](https://elm-lang.org/blog/small-assets-without-the-headache): Generate the Elm code needed to inform the Elm compiler about our entry points.

#### Reduce Operating Expenses

+ Support integrating app to configure tradeoff between the cost of persisting and cost of restoring.
+ Reduce load on storage: Provide automation to remove reductions which are not needed anymore from the store.
+ Make it easier to truncate history: Support dividing composition log into multiple files by time. (e.g. continue to log on new file each day)
