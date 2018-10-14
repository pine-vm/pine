# Kalmit

Simple Game Development

## Maintain State In Production

Current focus is the project to support Elm as a language to model game mechanics while at the same time offering a framework to maintain state in production. See the project description at [projects/Maintain State In Production.md](projects/Maintain%20State%20In%20Production.md)

### Backlog

#### Reduce Development Expenses

+ Automate boring tasks: Support deriving the functions for serializing and deserializing from the app state Elm type.
+ Simplify modeling of tests: Support modeling Elm app using a string for the main (and only) module contents, using the default `elm.json`.
+ Support [function-level dead code elimination](https://elm-lang.org/blog/small-assets-without-the-headache): Generate the Elm code needed to inform the Elm compiler about our entry points.

#### Reduce Operating Expenses

+ Support restoring state from a combination of snapshot and events.
+ Support integrating app to configure tradeoff between the cost of persisting and cost of restoring.

