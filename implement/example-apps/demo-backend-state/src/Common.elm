module Common exposing (describeApp)


describeApp : String
describeApp =
    "This example app demonstrates how Elm values are serialized and deserialized automatically. Have a look at '/api/elm-app-state' on the admin interface port to see the canonical representation of the current backend state. You can also change the backend state by sending a 'POST' HTTP request."
