effect module Http where { command = MyCmd, subscription = MySub } exposing
  ( get, post, request
  , Header, header
  , Body, emptyBody, stringBody, jsonBody, fileBody, bytesBody
  , multipartBody, Part, stringPart, filePart, bytesPart
  , Expect, expectString, expectJson, expectBytes, expectWhatever, Error(..)
  , track, Progress(..), fractionSent, fractionReceived
  , cancel
  , riskyRequest
  , expectStringResponse, expectBytesResponse, Response(..), Metadata
  , task, Resolver, stringResolver, bytesResolver, riskyTask
  )

{-| Send HTTP requests.

# Requests
@docs get, post, request

# Header
@docs Header, header

# Body
@docs Body, emptyBody, stringBody, jsonBody, fileBody, bytesBody

# Body Parts
@docs multipartBody, Part, stringPart, filePart, bytesPart

# Expect
@docs Expect, expectString, expectJson, expectBytes, expectWhatever, Error

# Progress
@docs track, Progress, fractionSent, fractionReceived

# Cancel
@docs cancel

# Risky Requests
@docs riskyRequest

# Elaborate Expectations
@docs expectStringResponse, expectBytesResponse, Response, Metadata

# Tasks
@docs task, Resolver, stringResolver, bytesResolver, riskyTask
-}


import Bytes exposing (Bytes)
import Bytes.Decode as Bytes
import Dict exposing (Dict)
import Elm.Kernel.Http
import File exposing (File)
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task exposing (Task)



-- REQUESTS


{-| Create a `GET` request.

    import Http

    type Msg
      = GotText (Result Http.Error String)

    getPublicOpinion : Cmd Msg
    getPublicOpinion =
      Http.get
        { url = "https://elm-lang.org/assets/public-opinion.txt"
        , expect = Http.expectString GotText
        }

You can use functions like [`expectString`](#expectString) and
[`expectJson`](#expectJson) to interpret the response in different ways. In
this example, we are expecting the response body to be a `String` containing
the full text of _Public Opinion_ by Walter Lippmann.

**Note:** Use [`elm/url`](/packages/elm/url/latest) to build reliable URLs.
-}
get
  : { url : String
    , expect : Expect msg
    }
  -> Cmd msg
get r =
  request
    { method = "GET"
    , headers = []
    , url = r.url
    , body = emptyBody
    , expect = r.expect
    , timeout = Nothing
    , tracker = Nothing
    }


{-| Create a `POST` request. So imagine we want to send a POST request for
some JSON data. It might look like this:

    import Http
    import Json.Decode exposing (list, string)

    type Msg
      = GotBooks (Result Http.Error (List String))

    postBooks : Cmd Msg
    postBooks =
      Http.post
        { url = "https://example.com/books"
        , body = Http.emptyBody
        , expect = Http.expectJson GotBooks (list string)
        }

Notice that we are using [`expectJson`](#expectJson) to interpret the response
as JSON. You can learn more about how JSON decoders work [here][] in the guide.

We did not put anything in the body of our request, but you can use functions
like [`stringBody`](#stringBody) and [`jsonBody`](#jsonBody) if you need to
send information to the server.

[here]: https://guide.elm-lang.org/interop/json.html
-}
post
  : { url : String
    , body : Body
    , expect : Expect msg
    }
  -> Cmd msg
post r =
  request
    { method = "POST"
    , headers = []
    , url = r.url
    , body = r.body
    , expect = r.expect
    , timeout = Nothing
    , tracker = Nothing
    }


{-| Create a custom request. For example, a PUT for files might look like this:

    import File
    import Http

    type Msg = Uploaded (Result Http.Error ())

    upload : File.File -> Cmd Msg
    upload file =
      Http.request
        { method = "PUT"
        , headers = []
        , url = "https://example.com/publish"
        , body = Http.fileBody file
        , expect = Http.expectWhatever Uploaded
        , timeout = Nothing
        , tracker = Nothing
        }

It lets you set custom `headers` as needed. The `timeout` is the number of
milliseconds you are willing to wait before giving up. The `tracker` lets you
[`cancel`](#cancel) and [`track`](#track) requests.
-}
request
  : { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
  -> Cmd msg
request r =
  command <| Request <|
    { method = r.method
    , headers = r.headers
    , url = r.url
    , body = r.body
    , expect = r.expect
    , timeout = r.timeout
    , tracker = r.tracker
    , allowCookiesFromOtherDomains = False
    }



-- HEADERS


{-| An HTTP header for configuring requests. See a bunch of common headers
[here](https://en.wikipedia.org/wiki/List_of_HTTP_header_fields).
-}
type Header = Header String String


{-| Create a `Header`.

    header "If-Modified-Since" "Sat 29 Oct 1994 19:43:31 GMT"
    header "Max-Forwards" "10"
    header "X-Requested-With" "XMLHttpRequest"
-}
header : String -> String -> Header
header =
  Header



-- BODY


{-| Represents the body of a `Request`.
-}
type Body = Body


{-| Create an empty body for your `Request`. This is useful for GET requests
and POST requests where you are not sending any data.
-}
emptyBody : Body
emptyBody =
  Elm.Kernel.Http.emptyBody


{-| Put some JSON value in the body of your `Request`.

Maybe you want to search for 10 books relevant to a certain topic:

    import Http
    import Json.Encode as E

    searchForBooks : String -> Cmd Msg
    searchForBooks topic =
      Http.post
        { url = "https://api.example.com/books"
        , body =
            Http.jsonBody <|
              E.object
                [ ( "topic", E.string topic )
                , ( "limit", E.int 10 )
                ]
        , expect =
            Http.expectJson GotBooks booksDecoder
        }

**Note:** This will automatically add the `Content-Type: application/json` header.
-}
jsonBody : Encode.Value -> Body
jsonBody value =
  Elm.Kernel.Http.pair "application/json" (Encode.encode 0 value)


{-| Put some string in the body of your `Request`. Defining `jsonBody` looks
like this:

    import Json.Encode as Encode

    jsonBody : Encode.Value -> Body
    jsonBody value =
      stringBody "application/json" (Encode.encode 0 value)

The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type)
of the body. Some servers are strict about this!
-}
stringBody : String -> String -> Body
stringBody =
  Elm.Kernel.Http.pair


{-| Put some `Bytes` in the body of your `Request`. This allows you to use
[`elm/bytes`](/packages/elm/bytes/latest) to have full control over the binary
representation of the data you are sending. For example, you could create an
`archive.zip` file and send it along like this:

    import Bytes exposing (Bytes)

    zipBody : Bytes -> Body
    zipBody bytes =
      bytesBody "application/zip" bytes

The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type)
of the body. In other scenarios you may want to use MIME types like `image/png`
or `image/jpeg` instead.

**Note:** Use [`track`](#track) to track upload progress.
-}
bytesBody : String -> Bytes -> Body
bytesBody =
  Elm.Kernel.Http.pair


{-| Use a file as the body of your `Request`. When someone uploads an image
into the browser with [`elm/file`](/packages/elm/file/latest) you can forward
it to a server.

This will automatically set the `Content-Type` to the MIME type of the file.

**Note:** Use [`track`](#track) to track upload progress.
-}
fileBody : File -> Body
fileBody =
  Elm.Kernel.Http.pair ""



-- PARTS


{-| When someone clicks submit on the `<form>`, browsers send a special HTTP
request with all the form data. Something like this:

```
POST /test.html HTTP/1.1
Host: example.org
Content-Type: multipart/form-data;boundary="7MA4YWxkTrZu0gW"

--7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="title"

Trip to London
--7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="album[]"; filename="parliment.jpg"

...RAW...IMAGE...BITS...
--7MA4YWxkTrZu0gW--
```

This was the only way to send files for a long time, so many servers expect
data in this format. **The `multipartBody` function lets you create these
requests.** For example, to upload a photo album all at once, you could create
a body like this:

    multipartBody
      [ stringPart "title" "Trip to London"
      , filePart "album[]" file1
      , filePart "album[]" file2
      , filePart "album[]" file3
      ]

All of the body parts need to have a name. Names can be repeated. Adding the
`[]` on repeated names is a convention from PHP. It seems weird, but I see it
enough to mention it. You do not have to do it that way, especially if your
server uses some other convention!

The `Content-Type: multipart/form-data` header is automatically set when
creating a body this way.

**Note:** Use [`track`](#track) to track upload progress.
-}
multipartBody : List Part -> Body
multipartBody parts =
  Elm.Kernel.Http.pair "" (Elm.Kernel.Http.toFormData parts)


{-| One part of a `multipartBody`.
-}
type Part = Part


{-| A part that contains `String` data.

    multipartBody
      [ stringPart "title" "Tom"
      , filePart "photo" tomPng
      ]
-}
stringPart : String -> String -> Part
stringPart =
  Elm.Kernel.Http.pair


{-| A part that contains a file. You can use
[`elm/file`](/packages/elm/file/latest) to get files loaded into the
browser. From there, you can send it along to a server like this:

    multipartBody
      [ stringPart "product" "Ikea Bekant"
      , stringPart "description" "Great desk for home office."
      , filePart "image[]" file1
      , filePart "image[]" file2
      , filePart "image[]" file3
      ]
-}
filePart : String -> File -> Part
filePart =
  Elm.Kernel.Http.pair


{-| A part that contains bytes, allowing you to use
[`elm/bytes`](/packages/elm/bytes/latest) to encode data exactly in the format
you need.

    multipartBody
      [ stringPart "title" "Tom"
      , bytesPart "photo" "image/png" bytes
      ]

**Note:** You must provide a MIME type so that the receiver has clues about
how to interpret the bytes.
-}
bytesPart : String -> String -> Bytes -> Part
bytesPart key mime bytes =
  Elm.Kernel.Http.pair key (Elm.Kernel.Http.bytesToBlob mime bytes)



-- EXPECT


{-| Logic for interpreting a response body.
-}
type Expect msg = Expect


{-| Expect the response body to be a `String`. Like when getting the full text
of a book:

    import Http

    type Msg
      = GotText (Result Http.Error String)

    getPublicOpinion : Cmd Msg
    getPublicOpinion =
      Http.get
        { url = "https://elm-lang.org/assets/public-opinion.txt"
        , expect = Http.expectString GotText
        }

The response body is always some sequence of bytes, but in this case, we
expect it to be UTF-8 encoded text that can be turned into a `String`.
-}
expectString : (Result Error String -> msg) -> Expect msg
expectString toMsg =
  expectStringResponse toMsg (resolve Ok)


{-| Expect the response body to be JSON. Like if you want to get a random cat
GIF you might say:

    import Http
    import Json.Decode exposing (Decoder, field, string)

    type Msg
      = GotGif (Result Http.Error String)

    getRandomCatGif : Cmd Msg
    getRandomCatGif =
      Http.get
        { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
        , expect = Http.expectJson GotGif gifDecoder
        }

    gifDecoder : Decoder String
    gifDecoder =
      field "data" (field "image_url" string)

The official guide goes through this particular example [here][]. That page
also introduces [`elm/json`][json] to help you get started turning JSON into
Elm values in other situations.

[here]: https://guide.elm-lang.org/interop/json.html
[json]: /packages/elm/json/latest/

If the JSON decoder fails, you get a `BadBody` error that tries to explain
what went wrong.
-}
expectJson : (Result Error a -> msg) -> Decode.Decoder a -> Expect msg
expectJson toMsg decoder =
  expectStringResponse toMsg <| resolve <|
    \string ->
      Result.mapError Decode.errorToString (Decode.decodeString decoder string)


{-| Expect the response body to be binary data. For example, maybe you are
talking to an endpoint that gives back ProtoBuf data:

    import Bytes.Decode as Bytes
    import Http

    type Msg
      = GotData (Result Http.Error Data)

    getData : Cmd Msg
    getData =
      Http.get
        { url = "/data"
        , expect = Http.expectBytes GotData dataDecoder
        }

    -- dataDecoder : Bytes.Decoder Data

You would use [`elm/bytes`](/packages/elm/bytes/latest/) to decode the binary
data according to a proto definition file like `example.proto`.

If the decoder fails, you get a `BadBody` error that just indicates that
_something_ went wrong. It probably makes sense to debug by peeking at the
bytes you are getting in the browser developer tools or something.
-}
expectBytes : (Result Error a -> msg) -> Bytes.Decoder a -> Expect msg
expectBytes toMsg decoder =
  expectBytesResponse toMsg <| resolve <|
    \bytes ->
      Result.fromMaybe "unexpected bytes" (Bytes.decode decoder bytes)


{-| Expect the response body to be whatever. It does not matter. Ignore it!
For example, you might want this when uploading files:

    import Http

    type Msg
      = Uploaded (Result Http.Error ())

    upload : File -> Cmd Msg
    upload file =
      Http.post
        { url = "/upload"
        , body = Http.fileBody file
        , expect = Http.expectWhatever Uploaded
        }

The server may be giving back a response body, but we do not care about it.
-}
expectWhatever : (Result Error () -> msg) -> Expect msg
expectWhatever toMsg =
  expectBytesResponse toMsg (resolve (\_ -> Ok ()))


resolve : (body -> Result String a) -> Response body -> Result Error a
resolve toResult response =
  case response of
    BadUrl_ url -> Err (BadUrl url)
    Timeout_ -> Err Timeout
    NetworkError_ -> Err NetworkError
    BadStatus_ metadata _ -> Err (BadStatus metadata.statusCode)
    GoodStatus_ _ body -> Result.mapError BadBody (toResult body)


{-| A `Request` can fail in a couple ways:

- `BadUrl` means you did not provide a valid URL.
- `Timeout` means it took too long to get a response.
- `NetworkError` means the user turned off their wifi, went in a cave, etc.
- `BadStatus` means you got a response back, but the status code indicates failure.
- `BadBody` means you got a response back with a nice status code, but the body
of the response was something unexpected. The `String` in this case is a
debugging message that explains what went wrong with your JSON decoder or
whatever.

**Note:** You can use [`expectStringResponse`](#expectStringResponse) and
[`expectBytesResponse`](#expectBytesResponse) to get more flexibility on this.
-}
type Error
  = BadUrl String
  | Timeout
  | NetworkError
  | BadStatus Int
  | BadBody String



-- ELABORATE EXPECTATIONS


{-| Expect a [`Response`](#Response) with a `String` body. So you could define
your own [`expectJson`](#expectJson) like this:

    import Http
    import Json.Decode as D

    expectJson : (Result Http.Error a -> msg) -> D.Decoder a -> Expect msg
    expectJson toMsg decoder =
      expectStringResponse toMsg <|
        \response ->
          case response of
            Http.BadUrl_ url ->
              Err (Http.BadUrl url)

            Http.Timeout_ ->
              Err Http.Timeout

            Http.NetworkError_ ->
              Err Http.NetworkError

            Http.BadStatus_ metadata body ->
              Err (Http.BadStatus metadata.statusCode)

            Http.GoodStatus_ metadata body ->
              case D.decodeString decoder body of
                Ok value ->
                  Ok value

                Err err ->
                  Err (Http.BadBody (D.errorToString err))

This function is great for fancier error handling and getting response headers.
For example, maybe when your sever gives a 404 status code (not found) it also
provides a helpful JSON message in the response body. This function lets you
add logic to the `BadStatus_` branch so you can parse that JSON and give users
a more helpful message! Or make your own custom error type for your particular
application!
-}
expectStringResponse : (Result x a -> msg) -> (Response String -> Result x a) -> Expect msg
expectStringResponse toMsg toResult =
  Elm.Kernel.Http.expect "" identity (toResult >> toMsg)


{-| Expect a [`Response`](#Response) with a `Bytes` body.

It works just like [`expectStringResponse`](#expectStringResponse), giving you
more access to headers and more leeway in defining your own errors.
-}
expectBytesResponse : (Result x a -> msg) -> (Response Bytes -> Result x a) -> Expect msg
expectBytesResponse toMsg toResult =
  Elm.Kernel.Http.expect "arraybuffer" Elm.Kernel.Http.toDataView (toResult >> toMsg)


{-| A `Response` can come back a couple different ways:

- `BadUrl_` &mdash; you did not provide a valid URL.
- `Timeout_` &mdash; it took too long to get a response.
- `NetworkError_` &mdash; the user turned off their wifi, went in a cave, etc.
- `BadStatus_` &mdash; a response arrived, but the status code indicates failure.
- `GoodStatus_` &mdash; a response arrived with a nice status code!

The type of the `body` depends on whether you use
[`expectStringResponse`](#expectStringResponse)
or [`expectBytesResponse`](#expectBytesResponse).
-}
type Response body
  = BadUrl_ String
  | Timeout_
  | NetworkError_
  | BadStatus_ Metadata body
  | GoodStatus_ Metadata body


{-| Extra information about the response:

- `url` of the server that actually responded (so you can detect redirects)
- `statusCode` like `200` or `404`
- `statusText` describing what the `statusCode` means a little
- `headers` like `Content-Length` and `Expires`

**Note:** It is possible for a response to have the same header multiple times.
In that case, all the values end up in a single entry in the `headers`
dictionary. The values are separated by commas, following the rules outlined
[here](https://stackoverflow.com/questions/4371328/are-duplicate-http-response-headers-acceptable).
-}
type alias Metadata =
  { url : String
  , statusCode : Int
  , statusText : String
  , headers : Dict String String
  }



-- CANCEL


{-| Try to cancel an ongoing request based on a `tracker`.
-}
cancel : String -> Cmd msg
cancel tracker =
  command (Cancel tracker)



-- PROGRESS


{-| Track the progress of a request. Create a [`request`](#request) where
`tracker = Just "form.pdf"` and you can track it with a subscription like
`track "form.pdf" GotProgress`.
-}
track : String -> (Progress -> msg) -> Sub msg
track tracker toMsg =
  subscription (MySub tracker toMsg)


{-| There are two phases to HTTP requests. First you **send** a bunch of data,
then you **receive** a bunch of data. For example, say you use `fileBody` to
upload a file of 262144 bytes. From there, progress will go like this:

```
Sending   { sent =      0, size = 262144 }  -- 0.0
Sending   { sent =  65536, size = 262144 }  -- 0.25
Sending   { sent = 131072, size = 262144 }  -- 0.5
Sending   { sent = 196608, size = 262144 }  -- 0.75
Sending   { sent = 262144, size = 262144 }  -- 1.0
Receiving { received =  0, size = Just 16 } -- 0.0
Receiving { received = 16, size = Just 16 } -- 1.0
```

With file uploads, the **send** phase is expensive. That is what we saw in our
example. But with file downloads, the **receive** phase is expensive.

Use [`fractionSent`](#fractionSent) and [`fractionReceived`](#fractionReceived)
to turn this progress information into specific fractions!

**Note:** The `size` of the response is based on the [`Content-Length`][cl]
header, and in rare and annoying cases, a server may not include that header.
That is why the `size` is a `Maybe Int` in `Receiving`.

[cl]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Length
-}
type Progress
  = Sending { sent : Int, size : Int }
  | Receiving { received : Int, size : Maybe Int }


{-| Turn `Sending` progress into a useful fraction.

    fractionSent { sent =   0, size = 1024 } == 0.0
    fractionSent { sent = 256, size = 1024 } == 0.25
    fractionSent { sent = 512, size = 1024 } == 0.5

    -- fractionSent { sent = 0, size = 0 } == 1.0

The result is always between `0.0` and `1.0`, ensuring that any progress bar
animations never go out of bounds.

And notice that `size` can be zero. That means you are sending a request with
an empty body. Very common! When `size` is zero, the result is always `1.0`.

**Note:** If you create your own function to compute this fraction, watch out
for divide-by-zero errors!
-}
fractionSent : { sent : Int, size : Int } -> Float
fractionSent p =
  if p.size == 0 then
    1
  else
    clamp 0 1 (toFloat p.sent / toFloat p.size)


{-| Turn `Receiving` progress into a useful fraction for progress bars.

    fractionReceived { received =   0, size = Just 1024 } == 0.0
    fractionReceived { received = 256, size = Just 1024 } == 0.25
    fractionReceived { received = 512, size = Just 1024 } == 0.5

    -- fractionReceived { received =   0, size = Nothing } == 0.0
    -- fractionReceived { received = 256, size = Nothing } == 0.0
    -- fractionReceived { received = 512, size = Nothing } == 0.0

The `size` here is based on the [`Content-Length`][cl] header which may be
missing in some cases. A server may be misconfigured or it may be streaming
data and not actually know the final size. Whatever the case, this function
will always give `0.0` when the final size is unknown.

Furthermore, the `Content-Length` header may be incorrect! The implementation
clamps the fraction between `0.0` and `1.0`, so you will just get `1.0` if
you ever receive more bytes than promised.

**Note:** If you are streaming something, you can write a custom version of
this function that just tracks bytes received. Maybe you show that 22kb or 83kb
have been downloaded, without a specific fraction. If you do this, be wary of
divide-by-zero errors because `size` can always be zero!

[cl]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Length
-}
fractionReceived : { received : Int, size : Maybe Int } -> Float
fractionReceived p =
  case p.size of
    Nothing ->
      0

    Just n ->
      if n == 0 then
        1
      else
        clamp 0 1 (toFloat p.received / toFloat n)



-- CUSTOM REQUESTS


{-| Create a request with a risky security policy. Things like:

- Allow responses from other domains to set cookies.
- Include cookies in requests to other domains.

This is called [`withCredentials`][wc] in JavaScript, and it allows a couple
other risky things as well. It can be useful if `www.example.com` needs to
talk to `uploads.example.com`, but it should be used very carefully!

For example, every HTTP request includes a `Origin` header revealing the domain,
so any request to `facebook.com` reveals the website that sent it. From there,
cookies can be used to correlate browsing habits with specific users. “Oh, it
looks like they visited `example.com`. Maybe they want ads about examples!”
This is why you can get shoe ads for months without saying anything about it
on any social networks. **This risk exists even for people who do not have an
account.** Servers can set a new cookie to uniquely identify the browser and
build a profile around that. Same kind of tricks for logged out users.

**Context:** A significantly worse version of this can happen when trying to
add integrations with Google, Facebook, Pinterest, Twitter, etc. “Add our share
button. It is super easy. Just add this `<script>` tag!” But the goal here is
to get _arbitrary_ access to the executing context. Now they can track clicks,
read page content, use time zones to approximate location, etc. As of this
writing, suggesting that developers just embed `<script>` tags is the default
for Google Analytics, Facebook Like Buttons, Twitter Follow Buttons, Pinterest
Save Buttons, and Instagram Embeds.

[ah]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization
[wc]: https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials
-}
riskyRequest
  : { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
  -> Cmd msg
riskyRequest r =
  command <| Request <|
    { method = r.method
    , headers = r.headers
    , url = r.url
    , body = r.body
    , expect = r.expect
    , timeout = r.timeout
    , tracker = r.tracker
    , allowCookiesFromOtherDomains = True
    }



-- TASKS


{-| Just like [`request`](#request), but it creates a `Task`. This makes it
possible to pair your HTTP request with `Time.now` if you need timestamps for
some reason. **This should be quite rare.**
-}
task
  : { method : String
    , headers : List Header
    , url : String
    , body : Body
    , resolver : Resolver x a
    , timeout : Maybe Float
    }
  -> Task x a
task r =
  Elm.Kernel.Http.toTask () resultToTask
    { method = r.method
    , headers = r.headers
    , url = r.url
    , body = r.body
    , expect = r.resolver
    , timeout = r.timeout
    , tracker = Nothing
    , allowCookiesFromOtherDomains = False
    }


{-| Describes how to resolve an HTTP task. You can create a resolver with
[`stringResolver`](#stringResolver) and [`bytesResolver`](#bytesResolver).
-}
type Resolver x a = Resolver


{-| Turn a response with a `String` body into a result.
Similar to [`expectStringResponse`](#expectStringResponse).
-}
stringResolver : (Response String -> Result x a) -> Resolver x a
stringResolver =
  Elm.Kernel.Http.expect "" identity


{-| Turn a response with a `Bytes` body into a result.
Similar to [`expectBytesResponse`](#expectBytesResponse).
-}
bytesResolver : (Response Bytes -> Result x a) -> Resolver x a
bytesResolver =
  Elm.Kernel.Http.expect "arraybuffer" Elm.Kernel.Http.toDataView


{-| Just like [`riskyRequest`](#riskyRequest), but it creates a `Task`. **Use
with caution!** This has all the same security concerns as `riskyRequest`.
-}
riskyTask
  : { method : String
    , headers : List Header
    , url : String
    , body : Body
    , resolver : Resolver x a
    , timeout : Maybe Float
    }
  -> Task x a
riskyTask r =
  Elm.Kernel.Http.toTask () resultToTask
    { method = r.method
    , headers = r.headers
    , url = r.url
    , body = r.body
    , expect = r.resolver
    , timeout = r.timeout
    , tracker = Nothing
    , allowCookiesFromOtherDomains = True
    }


resultToTask : Result x a -> Task x a
resultToTask result =
  case result of
    Ok a -> Task.succeed a
    Err x -> Task.fail x



-- COMMANDS and SUBSCRIPTIONS


type MyCmd msg
  = Cancel String
  | Request
      { method : String
      , headers : List Header
      , url : String
      , body : Body
      , expect : Expect msg
      , timeout : Maybe Float
      , tracker : Maybe String
      , allowCookiesFromOtherDomains : Bool
      }


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd =
  case cmd of
    Cancel tracker ->
      Cancel tracker

    Request r ->
      Request
        { method = r.method
        , headers = r.headers
        , url = r.url
        , body = r.body
        , expect = Elm.Kernel.Http.mapExpect func r.expect
        , timeout = r.timeout
        , tracker = r.tracker
        , allowCookiesFromOtherDomains = r.allowCookiesFromOtherDomains
        }


type MySub msg =
  MySub String (Progress -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub tracker toMsg) =
  MySub tracker (toMsg >> func)



-- EFFECT MANAGER


type alias State msg =
  { reqs : Dict String Process.Id
  , subs : List (MySub msg)
  }


init : Task Never (State msg)
init =
  Task.succeed (State Dict.empty [])


type alias MyRouter msg =
  Platform.Router msg SelfMsg



-- APP MESSAGES


onEffects : MyRouter msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs state =
  updateReqs router cmds state.reqs
    |> Task.andThen (\reqs -> Task.succeed (State reqs subs))


updateReqs : MyRouter msg -> List (MyCmd msg) -> Dict String Process.Id -> Task x (Dict String Process.Id)
updateReqs router cmds reqs =
  case cmds of
    [] ->
      Task.succeed reqs

    cmd :: otherCmds ->
      case cmd of
        Cancel tracker ->
          case Dict.get tracker reqs of
            Nothing ->
              updateReqs router otherCmds reqs

            Just pid ->
              Process.kill pid
                |> Task.andThen (\_ -> updateReqs router otherCmds (Dict.remove tracker reqs))

        Request req ->
          Process.spawn (Elm.Kernel.Http.toTask router (Platform.sendToApp router) req)
            |> Task.andThen (\pid ->
                  case req.tracker of
                    Nothing ->
                      updateReqs router otherCmds reqs

                    Just tracker ->
                      updateReqs router otherCmds (Dict.insert tracker pid reqs)
              )



-- SELF MESSAGES


type alias SelfMsg =
  (String, Progress)


onSelfMsg : MyRouter msg -> SelfMsg -> State msg -> Task Never (State msg)
onSelfMsg router (tracker, progress) state =
  Task.sequence (List.filterMap (maybeSend router tracker progress) state.subs)
    |> Task.andThen (\_ -> Task.succeed state)


maybeSend : MyRouter msg -> String -> Progress -> MySub msg -> Maybe (Task x ())
maybeSend router desiredTracker progress (MySub actualTracker toMsg) =
  if desiredTracker == actualTracker then
    Just (Platform.sendToApp router (toMsg progress))
  else
    Nothing
