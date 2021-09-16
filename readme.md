# Elm Full-Stack

Elm Fullstack is a tool for developing web services and full-stack web applications. It leverages the [Elm programming language](https://elm-lang.org) and its ecosystem of tools and libraries.
As it evolves, Elm Fullstack automates more and more activities in software development that humans had performed in the past.

Elm Fullstack currently offers:

+ Persisting and restoring the state of backend Elm apps.
+ Migrating backend state when deploying a new program version.
+ Generating functions to serialize and deserialize Elm values based on type declarations from Elm programs.
+ Ports to integrate and interoperate with custom components written in any programming language.
+ A single executable file (per operating system) integrating all tools with a command-line interface.

## Getting Started

Use this command to run a server and deploy an example app:

```cmd
elm-fs  run-server  --public-urls="http://*:5000"  --deploy=https://github.com/elm-fullstack/elm-fullstack/tree/c2c44f466fab0e57ca8a4fa85c2d299a379e0d45/implement/example-apps/docker-image-default-app
```

For a guide on installation and configuration options, see [How to Configure and Deploy an Elm Fullstack App](guide/how-to-configure-and-deploy-an-elm-fullstack-app.md).

## Releases / Artifacts

### elm-fs Executable File

The `elm-fs` executable file contains all the functionality to build apps and operate backend processes. You can download the files from the [releases section](https://github.com/elm-fullstack/elm-fullstack/releases) on GitHub.

### Docker Image

If you prefer deploying using docker, use the `elmfullstack/elm-fullstack` image from [docker hub](https://hub.docker.com/r/elmfullstack/elm-fullstack/tags). The tags are aligned with the version IDs in the CLI executable file.

```cmd
docker  run  -p 5000:80  -p 4000:4000  --env "APPSETTING_adminPassword=test" elmfullstack/elm-fullstack
```

To learn more about these artifacts' interfaces, see [How to Configure and Deploy an Elm Fullstack App](guide/how-to-configure-and-deploy-an-elm-fullstack-app.md).

## Building from Source

The easiest way to test a change in the source code is to trigger the GitHub action workflow in [`.github/workflows/test-and-publish.yml`](./.github/workflows/test-and-publish.yml). This workflow gets you the results of automated tests and your version of the executable files. Push to your fork on GitHub to trigger this action.

As we can see in the [workflow file](./.github/workflows/test-and-publish.yml), the project uses .NET 6 to build the executable file and run tests. You can download the `dotnet` tool from https://dotnet.microsoft.com/download/dotnet to build and test locally.

## Example Apps

### Rich Chat Room

The [rich chat room example app](https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps/rich-chat-room) demonstrates features typically found in a chat app, such as user names, message rate-limiting, sound effects, etc.
For a detailed description of this app, see the readme file at https://github.com/elm-fullstack/elm-fullstack/blob/main/implement/example-apps/rich-chat-room/readme.md

### Elm Editor

[Elm Editor](https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps/elm-editor) is an editor for developing Elm programs. It also provides tooling specifically for web frontend-apps to compile and view an app inline.

<a href="https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps/elm-editor/readme.md">
<img src="./guide/image/2021-03-17-elm-editor-user-interface.png" width="500" />
</a>

To see Elm Editor in action, check out the public instance at https://elm-editor.com

To learn more about how it works, see https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps/elm-editor/readme.md
