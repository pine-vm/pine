# Elm-Time

Elm-Time is an open-source, cross-platform runtime environment for the Elm programming language.

Elm-Time integrates a web server and a database management system, automating the persistence and maintenance of application state and database migrations.
To learn more about the database management system and persistence, see the dedicated guide on [persistence of application state](./guide/persistence-of-application-state-in-elm-time.md)

The Elm-Time compiler offers various interfaces supporting the automatic generation of Elm code at build time. This automation frees applications from boilerplate and glue code and allows us to focus on business logic.


## Getting Started

Download the pre-built Elm-Time binary for your platform at <https://elm-time.org/download>, or on the [releases page](https://github.com/elm-time/elm-time/releases) on GitHub.

The `elm-time` executable file integrates all functionality to build apps and operate web services.

The command below runs a server and deploys a full-stack web app:

```txt
elm-time  run-server  --public-urls="http://*:5000"  --deploy=https://github.com/elm-time/elm-time/tree/8dbd5c91853fbcef3b645d95bccc01a886ccd7e2/implement/example-apps/docker-image-default-app
```


## Docker Image

To deploy in a docker container, use the `elm-time/elm-time` image from the [GitHub Container registry](https://github.com/elm-time/elm-time/pkgs/container/elm-time) (`ghcr.io/elm-time/elm-time`). The tags are aligned with the version IDs in the CLI executable file.

```txt
docker  run  -p 5000:80  -p 4000:4000  --env "APPSETTING_adminPassword=test"  ghcr.io/elm-time/elm-time
```


## Guides

A selection of guides on the most popular topics:

+ Building full-stack web apps: [./guide/how-to-build-a-full-stack-web-app-in-elm-time.md](./guide/how-to-build-a-full-stack-web-app-in-elm-time.md)

+ Building a backend or web service: [./guide/how-to-build-a-backend-app-in-elm-time.md](./guide/how-to-build-a-backend-app-in-elm-time.md)

For an overview of all guides and documentation, see the [`guide` directory](./guide/).

## Example Apps

### Rich Chat Room

The [rich chat room example app](https://github.com/elm-time/elm-time/tree/main/implement/example-apps/rich-chat-room) demonstrates features typically found in a chat app, such as user names, message rate-limiting, sound effects, etc.
For a detailed description of this app, see the readme file at <https://github.com/elm-time/elm-time/blob/main/implement/example-apps/rich-chat-room/README.md>

### Elm Editor

[Elm Editor](https://github.com/elm-time/elm-time/tree/main/implement/example-apps/elm-editor) is a web app for developing Elm programs.

As an integrated development environment, it assists us in reading, writing, and testing Elm programs and in collaborating with other developers.

<a href="https://github.com/elm-time/elm-time/tree/main/implement/example-apps/elm-editor/README.md">
<img src="./guide/image/2021-03-17-elm-editor-user-interface.png" width="500" />
</a>

To see Elm Editor in action, check out the public instance at https://elm-editor.com

To learn more about Elm Editor, see <https://github.com/elm-time/elm-time/tree/main/implement/example-apps/elm-editor/README.md>

### More Examples

For more example apps, see the [`example-apps` directory](./implement/example-apps/)
