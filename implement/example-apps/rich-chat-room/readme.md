The `rich-chat-room` example implements a full-featured chat room.

## Features

+ Plays a sound when any user adds a new message.
+ Offers the user to set or change their name to display along with their messages.
+ List of people online: Displays a list of users who recently viewed the chat room.
+ Avoids a single user demanding too much attention from others: The backend applies rate-limiting to messages. When a user runs into the limit while trying to add a message, they see an error message informing them about the limit.
+ Fast and efficient broadcast of updates via HTTP: This example uses long-polling to support the immediate broadcast of new messages without WebSockets' complexities or high-frequency polling. For an explanation of long-polling, see the dedicated section below.

## Running and Hacking on the Rich Chat Room App

You can find the source code for this example at <https://github.com/elm-time/elm-time/tree/main/implement/example-apps/rich-chat-room>

To run the app with the default source code, use the `run-server` command as follows:

```cmd
elm-time  run-server  --deploy=https://github.com/elm-time/elm-time/tree/main/implement/example-apps/rich-chat-room
```

If you have copied (and modified) the code on your local file system, you can run the app with this command:

```cmd
elm-time  run-server  --deploy=.
```

## Fast Distribution of Messages in the Chat Room Using Long-Polling

In the 'rich chat room' example app, new messages sent by one user appear almost immediately for other users. But this example uses HTTP for communication between backend and frontends, a protocol that does not support pushing a message to a client. How do messages arrive so quickly despite the use of HTTP?

The trick we use here is called 'long-polling'. It means the backend does not always respond to the HTTP request immediately but waits until there is a change in the room that we should broadcast. As soon as a change happens in the room, the backend completes the HTTP response. The wait time is limited, so the backend will eventually respond with the same state if nothing changes in the chat room. You can see this when you open the network inspection in the web browser's 'devtools'. Here you will find that the backend often takes a long time to answer an HTTP request. When a user sends a new message into the chat room, this HTTP response time becomes shorter to ensure the immediate broadcast.

To implement long-polling, we expand the HTTP request from the client and the state on the backend. The client tells the backend about the last message it has already seen, so the backend knows if the room's current state is new to this particular client. If the client is already up-to-date, the backend needs to remember the pending HTTP request. When any client sends a new message, the backend uses this list of pending requests to send the corresponding HTTP responses.
