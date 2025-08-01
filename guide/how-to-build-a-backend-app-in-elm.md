# How to Build a Backend App in Elm

To support building Elm backend apps, Pine brings the following components:

+ Web service platform as a type of Elm application. Enables handling HTTP requests from clients.
+ Database management system: Guarantees state changes in the backend Elm app are [ACID](https://en.wikipedia.org/wiki/ACID), and deployments are type-safe.
+ Volatile processes: A generic interface for hosting third-party components in containers and integrating them with the Elm app, similar to the 'ports' in frontend Elm apps.

Pine integrates and manages these aspects automatically to avoid distractions and lets us focus on business logic.

To implement backend applications in Elm, use the application type provided by the `WebService` platform.
A `WebService` application subscribes to HTTP requests, can subscribe to time, and can open ports to integrate foreign software components such as binaries.
To implement a web service, create an Elm module exposing a `webServiceMain` declaration like this:

```Elm
module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes
import Bytes.Encode
import Platform.WebService


type alias State =
    ()


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = init
    , subscriptions = subscriptions
    }


init : ( State, Platform.WebService.Commands State )
init =
    ( (), [] )


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent :
    Platform.WebService.HttpRequestEventStruct
    -> State
    -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent state =
    let
        httpResponseBody : Bytes.Bytes
        httpResponseBody =
            "Hello, World!"
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        httpResponse : Platform.WebService.HttpResponse
        httpResponse =
            { statusCode = 200
            , body = Just httpResponseBody
            , headersToAdd = []
            }

        httpResponseCommand : Platform.WebService.RespondToHttpRequestStruct
        httpResponseCommand =
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
    in
    ( state
    , [ Platform.WebService.RespondToHttpRequest httpResponseCommand
      ]
    )

```

The example above shows a minimal web service implementation that only handles HTTP requests, without using other interfaces.

In the web service platform, we use the following event type for the arrival of an HTTP request:

```Elm

type alias HttpRequestEventStruct =
    { httpRequestId : String
    , posixTimeMilli : Int
    , requestContext : HttpRequestContext
    , request : HttpRequestProperties
    }


type alias HttpRequestContext =
    { clientAddress : Maybe String
    }


type alias HttpRequestProperties =
    { method : String
    , uri : String
    , body : Maybe Bytes.Bytes
    , headers : List HttpHeader
    }


type alias HttpHeader =
    { name : String
    , values : List String
    }

```

To respond to an HTTP request, we hand the runtime a command with this type:

```Elm

type alias RespondToHttpRequestStruct =
    { httpRequestId : String
    , response : HttpResponse
    }


type alias HttpResponse =
    { statusCode : Int
    , body : Maybe Bytes.Bytes
    , headersToAdd : List HttpHeader
    }

```

When we use long-polling or call a third-party service, the data we need to compose may not be available at the time we receive a client's HTTP request. In this case, we store the `httpRequestId` in the Elm application state until the necessary data for the HTTP response is available.


To learn about web-service-specific configuration, deployments, and migrations, see the guide at [how-to-configure-and-deploy-an-elm-backend-app.md](./how-to-configure-and-deploy-an-elm-backend-app.md)

