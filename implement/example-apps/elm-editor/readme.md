# Elm Editor

The [Elm Editor](https://github.com/elm-fullstack/elm-fullstack/tree/master/implement/example-apps/elm-editor) is an integrated development environment for building Elm apps.

![working on a game using the Elm Editor](./../../../guide/image/2020-12-31-elm-editor-with-project-freemake.png)

To see the editor in action, you can test the public instance at https://editor.elm-fullstack.org

This project evolved to help newcomers get started with minimal friction and is optimized for trainers and students.

The user interface also supports:

+ Viewing and editing all Elm module files and the `elm.json` file.
+ Check for Elm compiler errors using the chosen Elm module as the entry point.
+ Saving and sharing the current state of a project, including all code files.
+ Importing complete projects from public git repositories, also from subdirectories.
+ For frontend apps, viewing and testing the compiled web app in an iframe.

The Elm Editor frontend integrates the [Monaco Editor](https://microsoft.github.io/monaco-editor/) to provide functionality around code editing and features like syntax highlighting.

