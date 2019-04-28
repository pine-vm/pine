# Kalmit

*Simple development and operation of web services*

Implement your web service the simple way, using the [Elm programming language](https://elm-lang.org):

+ Don't worry about connecting to a database or losing application state: The runtime persists each `update` so the app state can be restored. To learn more about this feature, see the [guide on persistence in Kalmit](./guide/persistence-in-kalmit.md)
+ HTTP requests and responses are mapped to Elm types so that you can work with a strongly typed interface.

To help you with the operation side, the Kalmit web host supports configuring common functionality:

+ Admin interface to read and set the app state, in case you want to manually intervene or monitor.
+ Automatically get and renew SSL certificates from Let's Encrypt.
+ Rate-limit client HTTP requests which result in updates of your app.
+ Serve static files on selected paths.

## Configure & Deploy

See the guide on [how to configure and deploy your web app](guide/how-to-configure-and-deploy-your-kalmit-web-app.md).

## Support

Any questions? Reach out via [GitHub issues](https://github.com/Viir/Kalmit/issues), [twitter](https://twitter.com/michael_raetzel) or [email](mailto:michael@michaelrätzel.com).

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
