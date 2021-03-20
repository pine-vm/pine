# Elm Editor

The [Elm Editor](https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps/elm-editor) is an integrated development environment for building Elm apps.

This project minimizes the friction for newcomers to get started with programming.
Since the frontend is entirely web-based, it requires practically no installation. Any modern web browser is sufficient to write code, see code diagnostics, and live test apps.

To see the Elm Editor in action, test the public instance at https://elm-editor.com

![working on a game using the Elm Editor](./../../../guide/image/2021-03-17-elm-editor-user-interface.png)

## Overview of Features

+ Viewing and editing all Elm module files and the `elm.json` file.
+ Check for Elm compiler errors using the chosen Elm module as the entry point.
+ Saving and sharing the current state of a project, including all code files.
+ Importing complete projects from public git repositories, also from subdirectories.
+ For frontend apps, viewing and testing the compiled web app in an iframe.


## Saving and Sharing Projects

The 'Save or Share Project' dialog helps to persist or share the whole state of a project, including all files. This user interface encodes the project state in a URL for easy sharing in mediums like chat rooms and websites.

### Anatomy of the Project Link URL

The project link URL we get from the UI contains three components:

+ A description of the tree structure containing the files. This description can have different shapes, as detailed in the 'Project State Models' section below.
+ A hash of the file tree structure. The app uses this redundant information to check for defects in the file tree and warn the user if necessary.
+ The path of the file that should is currently opened in the code editor. When a user enters the project using the link, the editor opens this file again.

### Project State Models

The model describing the files in a project is optimized for typical training scenarios. Users often enter a project with a state as already modeled in a git repository in a subdirectory. Using an URL to a git tree in hosting services like GitHub or GitLab is sufficient to describe the project state. The editor then contacts the corresponding git hosting service to load the git repository contents. While loading is in progress, the app displays a message informing about the loading operation.

An example of such an URL to a git tree is https://github.com/onlinegamemaker/making-online-games/tree/fd35d23d89a50014097e64d362f1a991a8af206f/games-program-codes/simple-snake

The corresponding URL into the editor looks like this:
https://elm-editor.com/?project-state=https%3A%2F%2Fgithub.com%2Fonlinegamemaker%2Fmaking-online-games%2Ftree%2Ffd35d23d89a50014097e64d362f1a991a8af206f%2Fgames-program-codes%2Fsimple-snake

When a user started with a state from a git tree and made some changes, generating a link will encode the project state as the difference relative to that git tree. This encoding often leads to much smaller URLs. Like in the case of a pure git URL, the editor loads the base from the third-party service. When the loading from git is complete, the app applies the changes encoded with the URL on top to compute the final file tree.

![Saving a project state based on difference to git tree](./../../../guide/image/2021-01-16-elm-editor-save-project-diff-based.png)

### Compression of the Project State Model

To make the links into projects even smaller, the interface to save a project compresses the project state model using the deflate algorithm. This compressed representation appears in the `project-state-deflate-base64` query parameter in the final link URL.


## Code Editor

The code editor is a central part of an IDE. Elm Editor integrates the [Monaco Editor](https://microsoft.github.io/monaco-editor/) to provide a range of standard IDE features, including the following:

+ Text search with options for case sensitivity, regular expressions, and replacing instances.
+ Visual markers in the code to quickly find locations of problems.
+ Command palette helps discover new functionality and keyboard shortcuts for commands they already use.
+ Minimap for improved navigation of large documents.
