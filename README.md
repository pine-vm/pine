# Elm Full-Stack

In this repository, I share what I learn about building full-stack apps using the [Elm programming language](https://elm-lang.org). This approach emerged out of the development of online multiplayer video games like [DRTS](https://drtsgame.com).

In these applications, backend and frontend share an understanding of game mechanics and the game world. (Changes in these shared functionalities need to be synchronized between backend and frontend implementation.) Frontend and backend implementations use the same Elm modules for the common parts which need to be kept consistent. The tests run by elm-test also integrate backend and frontend for automated integration tests.

This repository also contains the implementation of the Elm-fullstack web host, tooling, and the command-line interface. These software components implement common non-app specific functionality, such as:

+ Persistence: The framework automatically persists the state of your app, including events in the backend Elm app, deployments of new app configurations, and migrations. To learn more about this functionality, see the [guide on persistence in Elm-fullstack](./guide/persistence-in-elm-fullstack.md).
+ Mapping HTTP requests to Elm types in the backend so that we can work with a strongly typed interface.
+ Generating the functions to serialize and deserialize the messages exchanged between frontend and backend.
+ Atomic backend state migrations: Model your backend state migrations with an Elm function that maps from the previous backend state type to the new one. The framework uses this function to migrate the backend state during deployment.
+ [HTTPS support](./guide/how-to-configure-and-deploy-an-elm-fullstack-app.md#support-https): Choose a list of domains, and the server automatically acquires and renews an SSL certificate from [Let's Encrypt](https://letsencrypt.org/).
+ Rate-limit HTTP requests to the backend app.

## Getting Started

See the guide [How to Configure and Deploy an Elm-Fullstack App](guide/how-to-configure-and-deploy-an-elm-fullstack-app.md).

## Example Projects

### Rich Chat Room

The [rich chat room example app](https://github.com/elm-fullstack/elm-fullstack/tree/master/implement/example-apps/rich-chat-room) demonstrates features typically found in a chat app, such as user names, message rate-limiting, sound effects, etc.
