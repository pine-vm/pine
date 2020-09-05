The `rich-chat-room` example implements a full-featured chat room.

## Features

+ Plays a sound when any user adds a new message.
+ Offers the user to set or change their name to display along with their messages.
+ List of people online: Displays a list of users who recently viewed the chat room.
+ Avoids a single user demanding too much attention from others: The backend applies rate-limiting to messages. When a user runs into the limit while trying to add a message, they see an error message informing them about the limit.

## Running and Hacking on the Rich Chat Room App

You can find the source code for this example at [https://github.com/elm-fullstack/elm-fullstack/tree/master/implement/example-apps/rich-chat-room](https://github.com/elm-fullstack/elm-fullstack/tree/master/implement/example-apps/rich-chat-room)

If you have copied (and modified) the code on your local file system, you can run the app with this command:

```cmd
elm-fullstack  run-server  --deploy-app-from=.
```

To run the app with the default source code:

```cmd
elm-fullstack  run-server  --deploy-app-from=https://github.com/elm-fullstack/elm-fullstack/tree/master/implement/example-apps/rich-chat-room
```
