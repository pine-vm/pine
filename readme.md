# Elm-Time

Elm-Time is an open-source, cross-platform runtime environment for the Elm programming language.

Elm-Time integrates a web server and a database management system, automating the persistence and maintenance of application state and database migrations.
To learn more about the database management system and persistence, see the dedicated guide [Persistence in Elm-Time](./guide/persistence-in-elm-time.md)

The Elm-Time compiler offers various interfaces supporting the automatic generation of Elm code at build time. This automation frees applications from boilerplate and glue code and allows us to focus on business logic.

## Getting Started

Use this command to run a server and deploy an example app:

```cmd
elm-time  run-server  --public-urls="http://*:5000"  --deploy=https://github.com/elm-time/elm-time/tree/56e788f7fbcf723443714c9263f4af9ece9c0933/implement/example-apps/docker-image-default-app
```

For a guide on installation and configuration options, see [How to Configure and Deploy an Elm Backend App](guide/how-to-configure-and-deploy-an-elm-backend-app.md).

## Releases / Artifacts

### elm-time Executable File

The `elm-time` executable file contains all the functionality to build apps and operate backend processes. You can download the files from the [releases section](https://github.com/elm-time/elm-time/releases) on GitHub.

### Docker Image

To deploy in a docker container, use the `elm-time/elm-time` image from the [GitHub Container registry](https://github.com/elm-time/elm-time/pkgs/container/elm-time) (`ghcr.io/elm-time/elm-time`). The tags are aligned with the version IDs in the CLI executable file.

```cmd
docker  run  -p 5000:80  -p 4000:4000  --env "APPSETTING_adminPassword=test"  ghcr.io/elm-time/elm-time
```

To learn more about these artifacts' interfaces, see [How to Configure and Deploy an Elm Backend App](guide/how-to-configure-and-deploy-an-elm-backend-app.md).

## Building from Source

The easiest way to test a change in the source code is to trigger the GitHub action workflow in [`.github/workflows/test-and-publish.yml`](./.github/workflows/test-and-publish.yml). This workflow gets you the results of automated tests and your version of the executable files. Push to your fork on GitHub to trigger this action.

As we can see in the [workflow file](./.github/workflows/test-and-publish.yml), the project uses .NET 7 to build the executable file and run tests. You can download the `dotnet` tool from https://dotnet.microsoft.com/download/dotnet to build and test locally.

## Example Apps

### Rich Chat Room

The [rich chat room example app](https://github.com/elm-time/elm-time/tree/main/implement/example-apps/rich-chat-room) demonstrates features typically found in a chat app, such as user names, message rate-limiting, sound effects, etc.
For a detailed description of this app, see the readme file at https://github.com/elm-time/elm-time/blob/main/implement/example-apps/rich-chat-room/readme.md

### Elm Editor

[Elm Editor](https://github.com/elm-time/elm-time/tree/main/implement/example-apps/elm-editor) is a web app for developing Elm programs.

As an integrated development environment, it assists us in reading, writing, and testing Elm programs and in collaborating with other developers.

<a href="https://github.com/elm-time/elm-time/tree/main/implement/example-apps/elm-editor/readme.md">
<img src="./guide/image/2021-03-17-elm-editor-user-interface.png" width="500" />
</a>

To see Elm Editor in action, check out the public instance at https://elm-editor.com

To learn more about Elm Editor, see https://github.com/elm-time/elm-time/tree/main/implement/example-apps/elm-editor/readme.md
