# Elm Full-Stack

In this repository, I share what I learn about building full-stack apps using the [Elm programming language](https://elm-lang.org). This approach emerged out of the development of online multiplayer video games like [DRTS](https://drtsgame.com).

In these applications, backend and frontend share an understanding of game mechanics and the game world. (Changes in these shared functionalities need to be synchronized between backend and frontend implementation.) Frontend and backend implementations use the same Elm modules for the common parts which need to be kept consistent. The tests run by elm-test also integrate backend and frontend for automated integration tests.

Common, non-application specific functionality is implemented in the Elm-fullstack framework. This includes:

+ Persisting each `update` in the backend and automatically restore the app state when necessary. To learn more about this functionality, see the [guide on persistence in Elm-fullstack](./guide/persistence-in-elm-fullstack.md).
+ HTTP requests and responses are mapped to Elm types so that we can work with a strongly typed interface.
+ Admin interface to read and set the app state, in case you want to manually intervene or monitor.
+ [HTTPS support](./guide/how-to-configure-and-deploy-your-elm-full-stack-app.md#support-https): The web host automatically gets and renews SSL certificates from Let's Encrypt.
+ Rate-limit client HTTP requests which result in updates of the app.
+ Serve static files on selected paths.

For how to build from Elm code and configure optional features, see the guide on [how to configure and deploy your Elm full-stack app](guide/how-to-configure-and-deploy-your-elm-full-stack-app.md).

## CI Build Status

Platform | Status
--- | ---
| vs2017-win2016 | [![Build Status Windows](https://dev.azure.com/kalmit/kalmit/_apis/build/status/Kalmit%20Windows?branchName=master)](https://dev.azure.com/kalmit/kalmit/_build/latest?definitionId=2?branchName=master) |
| ubuntu-16.04 | [![Build Status Linux](https://dev.azure.com/kalmit/kalmit/_apis/build/status/Kalmit%20Linux?branchName=master)](https://dev.azure.com/kalmit/kalmit/_build/latest?definitionId=3?branchName=master) |
