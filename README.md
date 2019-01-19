# Kalmit

*Simple Persistent Processes*

Kalmit supports modeling a persistent process using the Elm programming language, by providing:
+ Framework to automatically persist and restore the process state.
+ Web server to host a persistent process as a web service:
  + HTTP requests and responses are mapped to Elm types.
  + Additional common functionality like serving static files, rate-limiting, CORS configuration.
  + Admin interface to read and set the process state.

## Deployment

+ For deploying on Azure app service, see [guide/operate-persistent-process.md](./guide/operate-persistent-process.md)

## Support

Any questions? Reach out via [GitHub issues](https://github.com/Viir/Kalmit/issues), [twitter](https://twitter.com/michael_raetzel) or [email](mailto:Michael@michaelrätzel.com).

### Backlog

#### Web App Robustness

+ Support caching per URL for requests using the "GET" method. Probably the URL patterns here should be modeled with the same model as the one used to redirect to static files.

#### Reduce Development Expenses

+ Automate boring tasks: Support deriving the functions for serializing and deserializing from the app state Elm type.
+ Simplify modeling of tests: Support modeling Elm app using a string for the main (and only) module contents, using the default `elm.json`.
+ Support [function-level dead code elimination](https://elm-lang.org/blog/small-assets-without-the-headache): Generate the Elm code needed to inform the Elm compiler about our entry points.
+ Support hosted app choosing frequency when subscribing to time. (Could be modeled for example with Response of type `Subscriptions`, (Posix time cyclic with distance in ms, Posix time once)).

#### Reduce Operating Expenses

+ Support integrating app to configure tradeoff between the cost of persisting and cost of restoring.
+ Reduce load on storage: Provide automation to remove reductions which are not needed anymore from the store.
