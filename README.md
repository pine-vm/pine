# Kalmit

*Simple Persistent Processes*

Kalmit supports modeling a persistent process using the Elm programming language, by providing:
+ A framework to automatically persist the process state and restore the app state when the server restarts.
+ A web server to host a persistent process as a web service:
  + HTTP requests and responses are mapped to Elm types.
  + Admin interface to read and set the process state.
  + Additional common functionality like Let's Encrypt, static files, rate-limiting, CORS.

## Configure & Deploy

See the guide on [how to configure and deploy your web app](guide/how-to-configure-and-deploy-your-kalmit-web-app.md).

## Support

Any questions? Reach out via [GitHub issues](https://github.com/Viir/Kalmit/issues), [twitter](https://twitter.com/michael_raetzel) or [email](mailto:Michael@michaelrätzel.com).

## Roadmap

+ State Migrations
+ Inspection
+ Measuring Runtime Performance
+ Increasing Runtime Performance
+ World Domination

For a more details about planned work, see the [backlog](./backlog.md).

## CI Build Status

Platform | Status
--- | ---
| vs2017-win2016 | [![Build Status Windows](https://dev.azure.com/kalmit/kalmit/_apis/build/status/Kalmit%20Windows?branchName=master)](https://dev.azure.com/kalmit/kalmit/_build/latest?definitionId=2?branchName=master) |
| ubuntu-16.04 | [![Build Status Linux](https://dev.azure.com/kalmit/kalmit/_apis/build/status/Kalmit%20Linux?branchName=master)](https://dev.azure.com/kalmit/kalmit/_build/latest?definitionId=3?branchName=master) |
