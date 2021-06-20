module Playground exposing
    ( picture, animation, game
    , Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon
    , words
    , image
    , move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY
    , scale, rotate, fade
    , group
    , Time, spin, wave, zigzag
    , Computer, Mouse, Screen, Keyboard, toX, toY, toXY
    , Color, rgb, red, orange, yellow, green, blue, purple, brown
    , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
    , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
    , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
    , lightGray, gray, darkGray
    , Number
    , ShapeInteractivity(..), reactOnClick, render, toScreen
    )

{-|


# Playgrounds

@docs picture, animation, game


# Shapes

@docs Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon


# Words

@docs words


# Images

@docs image


# Move Shapes

@docs move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY


# Customize Shapes

@docs scale, rotate, fade


# Groups

@docs group


# Time

@docs Time, spin, wave, zigzag


# Computer

@docs Computer, Mouse, Screen, Keyboard, toX, toY, toXY


# Colors

@docs Color, rgb, red, orange, yellow, green, blue, purple, brown


### Light Colors

@docs lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown


### Dark Colors

@docs darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown


### Shades of Grey

@docs white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black


### Alternate Spellings of Gray

@docs lightGray, gray, darkGray


### Numbers

@docs Number

-}

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html
import Html.Attributes as H
import Json.Decode as D
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


type alias RenderConfig state event =
    { attributeForShapeInteractivity : Maybe (ShapeInteractivity state -> Svg.Attribute event)
    }


renderConfigWithoutInteractivity : RenderConfig state event
renderConfigWithoutInteractivity =
    { attributeForShapeInteractivity = Nothing
    }


{-| Make a picture! Here is a picture of a triangle with an eyeball:

    import Playground exposing (..)

    main =
        picture
            [ triangle green 150
            , circle white 40
            , circle black 10
            ]

-}
picture : List (Shape state) -> Program () Screen ( Int, Int )
picture shapes =
    let
        init () =
            ( toScreen 600 600, Cmd.none )

        view screen =
            { title = "Playground"
            , body = [ render renderConfigWithoutInteractivity screen shapes ]
            }

        update ( width, height ) _ =
            ( toScreen (toFloat width) (toFloat height)
            , Cmd.none
            )

        subscriptions _ =
            E.onResize Tuple.pair
    in
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| When writing a [`game`](#game), you can look up all sorts of information
about your computer:

  - [`Mouse`](#Mouse) - Where is the mouse right now?
  - [`Keyboard`](#Keyboard) - Are the arrow keys down?
  - [`Screen`](#Screen) - How wide is the screen?
  - [`Time`](#Time) - What time is it right now?

So you can use expressions like `computer.mouse.x` and `computer.keyboard.enter`
in games where you want some mouse or keyboard interaction.

-}
type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }


{-| Figure out what is going on with the mouse.

You could draw a circle around the mouse with a program like this:

    import Playground exposing (..)

    main =
        game view update 0

    view computer memory =
        [ circle yellow 40
            |> moveX computer.mouse.x
            |> moveY computer.mouse.y
        ]

    update computer memory =
        memory

You could also use `computer.mouse.down` to change the color of the circle
while the mouse button is down.

-}
type alias Mouse =
    { x : Number
    , y : Number
    , down : Bool
    , click : Bool
    }


{-| A number like `1` or `3.14` or `-120`.
-}
type alias Number =
    Float


{-| Figure out what is going on with the keyboard.

If someone is pressing the UP and RIGHT arrows, you will see a value like this:

    { up = True
    , down = False
    , left = False
    , right = True
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.fromList [ "ArrowUp", "ArrowRight" ]
    }

So if you want to move a character based on arrows, you could write an update
like this:

    update computer y =
        if computer.keyboard.up then
            y + 1

        else
            y

Check out [`toX`](#toX) and [`toY`](#toY) which make this even easier!

**Note:** The `keys` set will be filled with the name of all keys which are
down right now. So you will see things like `"a"`, `"b"`, `"c"`, `"1"`, `"2"`,
`"Space"`, and `"Control"` in there. Check out [this list][list] to see the
names used for all the different special keys! From there, you can use
[`Set.member`][member] to check for whichever key you want. E.g.
`Set.member "Control" computer.keyboard.keys`.

[list]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
[member]: /packages/elm/core/latest/Set#member

-}
type alias Keyboard =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    , enter : Bool
    , shift : Bool
    , backspace : Bool
    , keys : Set.Set String
    }


{-| Turn the LEFT and RIGHT arrows into a number.

    toX { left = False, right = False, ... } == 0
    toX { left = True , right = False, ... } == -1
    toX { left = False, right = True , ... } == 1
    toX { left = True , right = True , ... } == 0

So to make a square move left and right based on the arrow keys, we could say:

    import Playground exposing (..)

    main =
        game view update 0

    view computer x =
        [ square green 40
            |> moveX x
        ]

    update computer x =
        x + toX computer.keyboard

-}
toX : Keyboard -> Number
toX keyboard =
    (if keyboard.right then
        1

     else
        0
    )
        - (if keyboard.left then
            1

           else
            0
          )


{-| Turn the UP and DOWN arrows into a number.

    toY { up = False, down = False, ... } == 0
    toY { up = True , down = False, ... } == 1
    toY { up = False, down = True , ... } == -1
    toY { up = True , down = True , ... } == 0

This can be used to move characters around in games just like [`toX`](#toX):

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square blue 40
            |> move x y
        ]

    update computer ( x, y ) =
        ( x + toX computer.keyboard
        , y + toY computer.keyboard
        )

-}
toY : Keyboard -> Number
toY keyboard =
    (if keyboard.up then
        1

     else
        0
    )
        - (if keyboard.down then
            1

           else
            0
          )


{-| If you just use `toX` and `toY`, you will move diagonal too fast. You will go
right at 1 pixel per update, but you will go up/right at 1.41421 pixels per
update.

So `toXY` turns the arrow keys into an `(x,y)` pair such that the distance is
normalized:

    toXY { up = True , down = False, left = False, right = False, ... } == (1, 0)
    toXY { up = True , down = False, left = False, right = True , ... } == (0.707, 0.707)
    toXY { up = False, down = False, left = False, right = True , ... } == (0, 1)

Now when you go up/right, you are still going 1 pixel per update.

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square green 40
            |> move x y
        ]

    update computer ( x, y ) =
        let
            ( dx, dy ) =
                toXY computer.keyboard
        in
        ( x + dx, y + dy )

-}
toXY : Keyboard -> ( Number, Number )
toXY keyboard =
    let
        x =
            toX keyboard

        y =
            toY keyboard
    in
    if x /= 0 && y /= 0 then
        ( x / squareRootOfTwo, y / squareRootOfTwo )

    else
        ( x, y )


squareRootOfTwo : Number
squareRootOfTwo =
    sqrt 2


{-| Get the dimensions of the screen. If the screen is 800 by 600, you will see
a value like this:

    { width = 800
    , height = 600
    , top = 300
    , left = -400
    , right = 400
    , bottom = -300
    }

This can be nice when used with [`moveY`](#moveY) if you want to put something
on the bottom of the screen, no matter the dimensions.

-}
type alias Screen =
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
    }


{-| The current time.

Helpful when making an [`animation`](#animation) with functions like
[`spin`](#spin), [`wave`](#wave), and [`zigzag`](#zigzag).

-}
type Time
    = Time Time.Posix


{-| Create an angle that cycles from 0 to 360 degrees over time.

Here is an [`animation`](#animation) with a spinning triangle:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ triangle orange 50
            |> rotate (spin 8 time)
        ]

It will do a full rotation once every eight seconds. Try changing the `8` to
a `2` to make it do a full rotation every two seconds. It moves a lot faster!

-}
spin : Number -> Time -> Number
spin period time =
    360 * toFrac period time


{-| Smoothly wave between two numbers.

Here is an [`animation`](#animation) with a circle that resizes:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ circle lightBlue (wave 50 90 7 time)
        ]

The radius of the circle will cycles between 50 and 90 every seven seconds.
It kind of looks like it is breathing.

-}
wave : Number -> Number -> Number -> Time -> Number
wave lo hi period time =
    lo + (hi - lo) * (1 + cos (turns (toFrac period time))) / 2


{-| Zig zag between two numbers.

Here is an [`animation`](#animation) with a rectangle that tips back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ rectangle lightGreen 20 100
            |> rotate (zigzag -20 20 4 time)
        ]

It gets rotated by an angle. The angle cycles from -20 degrees to 20 degrees
every four seconds.

-}
zigzag : Number -> Number -> Number -> Time -> Number
zigzag lo hi period time =
    lo + (hi - lo) * abs (2 * toFrac period time - 1)


toFrac : Float -> Time -> Float
toFrac period (Time posix) =
    let
        ms =
            Time.posixToMillis posix

        p =
            period * 1000
    in
    toFloat (modBy (round p) ms) / p


{-| Create an animation!

Once you get comfortable using [`picture`](#picture) to layout shapes, you can
try out an `animation`. Here is square that zigzags back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square blue 40
            |> moveX (zigzag -100 100 2 time)
        ]

We need to define a `view` to make our animation work.

Within `view` we can use functions like [`spin`](#spin), [`wave`](#wave),
and [`zigzag`](#zigzag) to move and rotate our shapes.

-}
animation : (Time -> List (Shape e)) -> Program () Animation Msg
animation viewFrame =
    let
        init () =
            ( Animation E.Visible (toScreen 600 600) (Time (Time.millisToPosix 0))
            , Task.perform GotViewport Dom.getViewport
            )

        view (Animation _ screen time) =
            { title = "Playground"
            , body = [ render renderConfigWithoutInteractivity screen (viewFrame time) ]
            }

        update event state =
            ( animationUpdate event state
            , Cmd.none
            )

        subscriptions (Animation visibility _ _) =
            case visibility of
                E.Hidden ->
                    E.onVisibilityChange VisibilityChanged

                E.Visible ->
                    animationSubscriptions
    in
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Animation
    = Animation E.Visibility Screen Time


animationSubscriptions : Sub Msg
animationSubscriptions =
    Sub.batch
        [ E.onResize Resized
        , E.onAnimationFrame Tick
        , E.onVisibilityChange VisibilityChanged
        ]


animationUpdate : Msg -> Animation -> Animation
animationUpdate event ((Animation v s t) as state) =
    case event of
        Tick posix ->
            Animation v s (Time posix)

        VisibilityChanged vis ->
            Animation vis s t

        GotViewport { viewport } ->
            Animation v (toScreen viewport.width viewport.height) t

        Resized w h ->
            Animation v (toScreen (toFloat w) (toFloat h)) t

        KeyChanged _ _ ->
            state

        MouseMove _ _ ->
            state

        MouseClick ->
            state

        MouseButton _ ->
            state


{-| Create a game!

Once you get comfortable with [`animation`](#animation), you can try making a
game with the keyboard and mouse. Here is an example of a green square that
just moves to the right:

    import Playground exposing (..)

    main =
        game view update 0

    view computer offset =
        [ square green 40
            |> moveRight offset
        ]

    update computer offset =
        offset + 0.03

This shows the three important parts of a game:

1.  `memory` - makes it possible to store information. So with our green square,
    we save the `offset` in memory. It starts out at `0`.
2.  `view` - lets us say which shapes to put on screen. So here we move our
    square right by the `offset` saved in memory.
3.  `update` - lets us update the memory. We are incrementing the `offset` by
    a tiny amount on each frame.

The `update` function is called about 60 times per second, so our little
changes to `offset` start to add up pretty quickly!

This game is not very fun though! Making a `game` also gives you access to the
[`Computer`](#Computer), so you can use information about the [`Mouse`](#Mouse)
and [`Keyboard`](#Keyboard) to make it interactive! So here is a red square that
moves based on the arrow keys:

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square red 40
            |> move x y
        ]

    update computer ( x, y ) =
        ( x + toX computer.keyboard
        , y + toY computer.keyboard
        )

Notice that in the `update` we use information from the keyboard to update the
`x` and `y` values. These building blocks let you make pretty fancy games!

-}
game : (Computer -> memory -> List (Shape e)) -> (Computer -> memory -> memory) -> memory -> Program () (Game memory) Msg
game viewMemory updateMemory initialMemory =
    let
        init () =
            ( Game E.Visible initialMemory initialComputer
            , Task.perform GotViewport Dom.getViewport
            )

        view (Game _ memory computer) =
            { title = "Playground"
            , body = [ render renderConfigWithoutInteractivity computer.screen (viewMemory computer memory) ]
            }

        update event state =
            ( gameUpdate updateMemory event state
            , Cmd.none
            )

        subscriptions (Game visibility _ _) =
            case visibility of
                E.Hidden ->
                    E.onVisibilityChange VisibilityChanged

                E.Visible ->
                    gameSubscriptions
    in
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard = emptyKeyboard
    , screen = toScreen 600 600
    , time = Time (Time.millisToPosix 0)
    }


gameSubscriptions : Sub Msg
gameSubscriptions =
    Sub.batch
        [ E.onResize Resized
        , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , E.onAnimationFrame Tick
        , E.onVisibilityChange VisibilityChanged
        , E.onClick (D.succeed MouseClick)
        , E.onMouseDown (D.succeed (MouseButton True))
        , E.onMouseUp (D.succeed (MouseButton False))
        , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
        ]


type Game memory
    = Game E.Visibility memory Computer


type Msg
    = KeyChanged Bool String
    | Tick Time.Posix
    | GotViewport Dom.Viewport
    | Resized Int Int
    | VisibilityChanged E.Visibility
    | MouseMove Float Float
    | MouseClick
    | MouseButton Bool


gameUpdate : (Computer -> memory -> memory) -> Msg -> Game memory -> Game memory
gameUpdate updateMemory event (Game vis memory computer) =
    case event of
        Tick time ->
            Game vis (updateMemory computer memory) <|
                if computer.mouse.click then
                    { computer | time = Time time, mouse = mouseClick False computer.mouse }

                else
                    { computer | time = Time time }

        GotViewport { viewport } ->
            Game vis memory { computer | screen = toScreen viewport.width viewport.height }

        Resized w h ->
            Game vis memory { computer | screen = toScreen (toFloat w) (toFloat h) }

        KeyChanged isDown key ->
            Game vis memory { computer | keyboard = updateKeyboard isDown key computer.keyboard }

        MouseMove pageX pageY ->
            let
                x =
                    computer.screen.left + pageX

                y =
                    computer.screen.top - pageY
            in
            Game vis memory { computer | mouse = mouseMove x y computer.mouse }

        MouseClick ->
            Game vis memory { computer | mouse = mouseClick True computer.mouse }

        MouseButton isDown ->
            Game vis memory { computer | mouse = mouseDown isDown computer.mouse }

        VisibilityChanged visibility ->
            Game visibility
                memory
                { computer
                    | keyboard = emptyKeyboard
                    , mouse = Mouse computer.mouse.x computer.mouse.y False False
                }


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }


mouseClick : Bool -> Mouse -> Mouse
mouseClick bool mouse =
    { mouse | click = bool }


mouseDown : Bool -> Mouse -> Mouse
mouseDown bool mouse =
    { mouse | down = bool }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
    { mouse | x = x, y = y }


emptyKeyboard : Keyboard
emptyKeyboard =
    { up = False
    , down = False
    , left = False
    , right = False
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.empty
    }


updateKeyboard : Bool -> String -> Keyboard -> Keyboard
updateKeyboard isDown key keyboard =
    let
        keys =
            if isDown then
                Set.insert key keyboard.keys

            else
                Set.remove key keyboard.keys
    in
    case key of
        " " ->
            { keyboard | keys = keys, space = isDown }

        "Enter" ->
            { keyboard | keys = keys, enter = isDown }

        "Shift" ->
            { keyboard | keys = keys, shift = isDown }

        "Backspace" ->
            { keyboard | keys = keys, backspace = isDown }

        "ArrowUp" ->
            { keyboard | keys = keys, up = isDown }

        "ArrowDown" ->
            { keyboard | keys = keys, down = isDown }

        "ArrowLeft" ->
            { keyboard | keys = keys, left = isDown }

        "ArrowRight" ->
            { keyboard | keys = keys, right = isDown }

        _ ->
            { keyboard | keys = keys }


{-| Shapes help you make a `picture`, `animation`, or `game`.

Read on to see examples of [`circle`](#circle), [`rectangle`](#rectangle),
[`words`](#words), [`image`](#image), and many more!

-}
type Shape state
    = Shape
        Number
        -- x
        Number
        -- y
        Number
        -- angle
        Number
        -- scale
        Number
        -- alpha
        (Form state)
        (List (ShapeInteractivity state))


type Form state
    = Circle Color Number
    | Oval Color Number Number
    | Rectangle Color Number Number
    | Ngon Color Int Number
    | Polygon Color (List ( Number, Number ))
    | Image Number Number String
    | Words Color String
    | Group (List (Shape state))


type ShapeInteractivity state
    = ReactOnClick (state -> state)


{-| Make circles:

    dot =
        circle red 10

    sun =
        circle yellow 300

You give a color and then the radius. So the higher the number, the larger
the circle.

-}
circle : Color -> Number -> Shape state
circle color radius =
    Shape 0 0 0 1 1 (Circle color radius) []


{-| Make ovals:

    football =
        oval brown 200 100

You give the color, and then the width and height. So our `football` example
is 200 pixels wide and 100 pixels tall.

-}
oval : Color -> Number -> Number -> Shape state
oval color width height =
    Shape 0 0 0 1 1 (Oval color width height) []


{-| Make squares. Here are two squares combined to look like an empty box:

    import Playground exposing (..)

    main =
        picture
            [ square purple 80
            , square white 60
            ]

The number you give is the dimension of each side. So that purple square would
be 80 pixels by 80 pixels.

-}
square : Color -> Number -> Shape state
square color n =
    Shape 0 0 0 1 1 (Rectangle color n n) []


{-| Make rectangles. This example makes a red cross:

    import Playground exposing (..)

    main =
        picture
            [ rectangle red 20 60
            , rectangle red 60 20
            ]

You give the color, width, and then height. So the first shape is vertical
part of the cross, the thinner and taller part.

-}
rectangle : Color -> Number -> Number -> Shape state
rectangle color width height =
    Shape 0 0 0 1 1 (Rectangle color width height) []


{-| Make triangles. So if you wanted to draw the Egyptian pyramids, you could
do a simple version like this:

    import Playground exposing (..)

    main =
        picture
            [ triangle darkYellow 200
            ]

The number is the "radius", so the distance from the center to each point of
the pyramid is `200`. Pretty big!

-}
triangle : Color -> Number -> Shape state
triangle color radius =
    Shape 0 0 0 1 1 (Ngon color 3 radius) []


{-| Make pentagons:

    import Playground exposing (..)

    main =
        picture
            [ pentagon darkGrey 100
            ]

You give the color and then the radius. So the distance from the center to each
of the five points is 100 pixels.

-}
pentagon : Color -> Number -> Shape state
pentagon color radius =
    Shape 0 0 0 1 1 (Ngon color 5 radius) []


{-| Make hexagons:

    import Playground exposing (..)

    main =
        picture
            [ hexagon lightYellow 50
            ]

The number is the radius, the distance from the center to each point.

If you made more hexagons, you could [`move`](#move) them around to make a
honeycomb pattern!

-}
hexagon : Color -> Number -> Shape state
hexagon color radius =
    Shape 0 0 0 1 1 (Ngon color 6 radius) []


{-| Make octogons:

    import Playground exposing (..)

    main =
        picture
            [ octagon red 100
            ]

You give the color and radius, so each point of this stop sign is 100 pixels
from the center.

-}
octagon : Color -> Number -> Shape state
octagon color radius =
    Shape 0 0 0 1 1 (Ngon color 8 radius) []


{-| Make any shape you want! Here is a very thin triangle:

    import Playground exposing (..)

    main =
        picture
            [ polygon black [ ( -10, -20 ), ( 0, 100 ), ( 10, -20 ) ]
            ]

**Note:** If you [`rotate`](#rotate) a polygon, it will always rotate around
`(0,0)`. So it is best to build your shapes around that point, and then use
[`move`](#move) or [`group`](#group) so that rotation makes more sense.

-}
polygon : Color -> List ( Number, Number ) -> Shape state
polygon color points =
    Shape 0 0 0 1 1 (Polygon color points) []


{-| Add some image from the internet:

    import Playground exposing (..)

    main =
        picture
            [ image 96 96 "https://elm-lang.org/images/turtle.gif"
            ]

You provide the width, height, and then the URL of the image you want to show.

-}
image : Number -> Number -> String -> Shape state
image w h src =
    Shape 0 0 0 1 1 (Image w h src) []


{-| Show some words!

    import Playground exposing (..)

    main =
        picture
            [ words black "Hello! How are you?"
            ]

You can use [`scale`](#scale) to make the words bigger or smaller.

-}
words : Color -> String -> Shape state
words color string =
    Shape 0 0 0 1 1 (Words color string) []


{-| Put shapes together so you can [`move`](#move) and [`rotate`](#rotate)
them as a group. Maybe you want to put a bunch of stars in the sky:

    import Playground exposing (..)

    main =
        picture
            [ star
                |> move 100 100
                |> rotate 5
            , star
                |> move -120 40
                |> rotate 20
            , star
                |> move 80 -150
                |> rotate 32
            , star
                |> move -90 -30
                |> rotate -16
            ]

    star =
        group
            [ triangle yellow 20
            , triangle yellow 20
                |> rotate 180
            ]

-}
group : List (Shape state) -> Shape state
group shapes =
    Shape 0 0 0 1 1 (Group shapes) []



-- TRANSFORMS


{-| Move a shape by some number of pixels:

    import Playground exposing (..)

    main =
        picture
            [ square red 100
                |> move -60 60
            , square yellow 100
                |> move 60 60
            , square green 100
                |> move 60 -60
            , square blue 100
                |> move -60 -60
            ]

-}
move : Number -> Number -> Shape state -> Shape state
move dx dy (Shape x y a s o f interactivity) =
    Shape (x + dx) (y + dy) a s o f interactivity


{-| Move a shape up by some number of pixels. So if you wanted to make a tree
you could move the leaves up above the trunk:

    import Playground exposing (..)

    main =
        picture
            [ rectangle brown 40 200
            , circle green 100
                |> moveUp 180
            ]

-}
moveUp : Number -> Shape state -> Shape state
moveUp =
    moveY


{-| Move a shape down by some number of pixels. So if you wanted to put the sky
above the ground, you could move the sky up and the ground down:

    import Playground exposing (..)

    main =
        picture
            [ rectangle lightBlue 200 100
                |> moveUp 50
            , rectangle lightGreen 200 100
                |> moveDown 50
            ]

-}
moveDown : Number -> Shape state -> Shape state
moveDown dy (Shape x y a s o f interactivity) =
    Shape x (y - dy) a s o f interactivity


{-| Move shapes to the left.

    import Playground exposing (..)

    main =
        picture
            [ circle yellow 10
                |> moveLeft 80
                |> moveUp 30
            ]

-}
moveLeft : Number -> Shape state -> Shape state
moveLeft dx (Shape x y a s o f interactivity) =
    Shape (x - dx) y a s o f interactivity


{-| Move shapes to the right.

    import Playground exposing (..)

    main =
        picture
            [ square purple 20
                |> moveRight 80
                |> moveDown 100
            ]

-}
moveRight : Number -> Shape state -> Shape state
moveRight =
    moveX


{-| Move the `x` coordinate of a shape by some amount. Here is a square that
moves back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square purple 20
            |> moveX (wave 4 -200 200 time)
        ]

Using `moveX` feels a bit nicer here because the movement may be positive or negative.

-}
moveX : Number -> Shape state -> Shape state
moveX dx (Shape x y a s o f interactivity) =
    Shape (x + dx) y a s o f interactivity


{-| Move the `y` coordinate of a shape by some amount. Maybe you want to make
grass along the bottom of the screen:

    import Playground exposing (..)

    main =
        game view update 0

    update computer memory =
        memory

    view computer count =
        [ rectangle green computer.screen.width 100
            |> moveY computer.screen.bottom
        ]

Using `moveY` feels a bit nicer when setting things relative to the bottom or
top of the screen, since the values are negative sometimes.

-}
moveY : Number -> Shape state -> Shape state
moveY dy (Shape x y a s o f interactivity) =
    Shape x (y + dy) a s o f interactivity


{-| Make a shape bigger or smaller. So if you wanted some [`words`](#words) to
be larger, you could say:

    import Playground exposing (..)

    main =
        picture
            [ words black "Hello, nice to see you!"
                |> scale 3
            ]

-}
scale : Number -> Shape state -> Shape state
scale ns (Shape x y a s o f interactivity) =
    Shape x y a (s * ns) o f interactivity


{-| Rotate shapes in degrees.

    import Playground exposing (..)

    main =
        picture
            [ words black "These words are tilted!"
                |> rotate 10
            ]

The degrees go **counter-clockwise** to match the direction of the
[unit circle](https://en.wikipedia.org/wiki/Unit_circle).

-}
rotate : Number -> Shape state -> Shape state
rotate da (Shape x y a s o f interactivity) =
    Shape x y (a + da) s o f interactivity


{-| Fade a shape. This lets you make shapes see-through or even completely
invisible. Here is a shape that fades in and out:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square orange 30
        , square blue 200
            |> fade (zigzag 0 1 3 time)
        ]

The number has to be between `0` and `1`, where `0` is totally transparent
and `1` is completely solid.

-}
fade : Number -> Shape state -> Shape state
fade o (Shape x y a s _ f interactivity) =
    Shape x y a s o f interactivity


{-| Represents a color.

The colors below, like `red` and `green`, come from the [Tango palette][tango].
It provides a bunch of aesthetically reasonable colors. Each color comes with a
light and dark version, so you always get a set like `lightYellow`, `yellow`,
and `darkYellow`.

[tango]: https://en.wikipedia.org/wiki/Tango_Desktop_Project

-}
type Color
    = Hex String
    | Rgb Int Int Int


{-| -}
lightYellow : Color
lightYellow =
    Hex "#fce94f"


{-| -}
yellow : Color
yellow =
    Hex "#edd400"


{-| -}
darkYellow : Color
darkYellow =
    Hex "#c4a000"


{-| -}
lightOrange : Color
lightOrange =
    Hex "#fcaf3e"


{-| -}
orange : Color
orange =
    Hex "#f57900"


{-| -}
darkOrange : Color
darkOrange =
    Hex "#ce5c00"


{-| -}
lightBrown : Color
lightBrown =
    Hex "#e9b96e"


{-| -}
brown : Color
brown =
    Hex "#c17d11"


{-| -}
darkBrown : Color
darkBrown =
    Hex "#8f5902"


{-| -}
lightGreen : Color
lightGreen =
    Hex "#8ae234"


{-| -}
green : Color
green =
    Hex "#73d216"


{-| -}
darkGreen : Color
darkGreen =
    Hex "#4e9a06"


{-| -}
lightBlue : Color
lightBlue =
    Hex "#729fcf"


{-| -}
blue : Color
blue =
    Hex "#3465a4"


{-| -}
darkBlue : Color
darkBlue =
    Hex "#204a87"


{-| -}
lightPurple : Color
lightPurple =
    Hex "#ad7fa8"


{-| -}
purple : Color
purple =
    Hex "#75507b"


{-| -}
darkPurple : Color
darkPurple =
    Hex "#5c3566"


{-| -}
lightRed : Color
lightRed =
    Hex "#ef2929"


{-| -}
red : Color
red =
    Hex "#cc0000"


{-| -}
darkRed : Color
darkRed =
    Hex "#a40000"


{-| -}
lightGrey : Color
lightGrey =
    Hex "#eeeeec"


{-| -}
grey : Color
grey =
    Hex "#d3d7cf"


{-| -}
darkGrey : Color
darkGrey =
    Hex "#babdb6"


{-| -}
lightCharcoal : Color
lightCharcoal =
    Hex "#888a85"


{-| -}
charcoal : Color
charcoal =
    Hex "#555753"


{-| -}
darkCharcoal : Color
darkCharcoal =
    Hex "#2e3436"


{-| -}
white : Color
white =
    Hex "#FFFFFF"


{-| -}
black : Color
black =
    Hex "#000000"



-- ALTERNATE SPELLING GREYS


{-| -}
lightGray : Color
lightGray =
    Hex "#eeeeec"


{-| -}
gray : Color
gray =
    Hex "#d3d7cf"


{-| -}
darkGray : Color
darkGray =
    Hex "#babdb6"



-- CUSTOM COLORS


{-| RGB stands for Red-Green-Blue. With these three parts, you can create any
color you want. For example:

    brightBlue =
        rgb 18 147 216

    brightGreen =
        rgb 119 244 8

    brightPurple =
        rgb 94 28 221

Each number needs to be between 0 and 255.

It can be hard to figure out what numbers to pick, so try using a color picker
like [paletton] to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.

[paletton]: http://paletton.com/

-}
rgb : Number -> Number -> Number -> Color
rgb r g b =
    Rgb (colorClamp r) (colorClamp g) (colorClamp b)


colorClamp : Number -> Int
colorClamp number =
    clamp 0 255 (round number)


render : RenderConfig state event -> Screen -> List (Shape state) -> Html.Html event
render config screen shapes =
    let
        w =
            String.fromFloat screen.width

        h =
            String.fromFloat screen.height

        x =
            String.fromFloat screen.left

        y =
            String.fromFloat screen.bottom
    in
    svg
        [ viewBox (x ++ " " ++ y ++ " " ++ w ++ " " ++ h)
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , width "100%"
        , height "100%"
        ]
        (List.map (renderShape config) shapes)


renderShape : RenderConfig state event -> Shape state -> Svg event
renderShape config (Shape x y angle s alpha form interactivity) =
    let
        shapeWithoutInteractivity =
            case form of
                Circle color radius ->
                    renderCircle color radius x y angle s alpha

                Oval color width height ->
                    renderOval color width height x y angle s alpha

                Rectangle color width height ->
                    renderRectangle color width height x y angle s alpha

                Ngon color n radius ->
                    renderNgon color n radius x y angle s alpha

                Polygon color points ->
                    renderPolygon color points x y angle s alpha

                Image width height src ->
                    renderImage width height src x y angle s alpha

                Words color string ->
                    renderWords color string x y angle s alpha

                Group shapes ->
                    g (transform (renderTransform x y angle s) :: renderAlpha alpha)
                        (List.map (renderShape config) shapes)
    in
    case config.attributeForShapeInteractivity of
        Nothing ->
            shapeWithoutInteractivity

        Just attributeForShapeInteractivity ->
            if interactivity == [] then
                shapeWithoutInteractivity

            else
                [ shapeWithoutInteractivity ]
                    |> g (interactivity |> List.map attributeForShapeInteractivity)


renderCircle : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Svg event
renderCircle color radius x y angle s alpha =
    Svg.circle
        (r (String.fromFloat radius)
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
        )
        []


renderOval : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg event
renderOval color width height x y angle s alpha =
    ellipse
        (rx (String.fromFloat (width / 2))
            :: ry (String.fromFloat (height / 2))
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
        )
        []


renderRectangle : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg event
renderRectangle color w h x y angle s alpha =
    rect
        (width (String.fromFloat w)
            :: height (String.fromFloat h)
            :: fill (renderColor color)
            :: transform (renderRectTransform w h x y angle s)
            :: renderAlpha alpha
        )
        []


renderRectTransform : Number -> Number -> Number -> Number -> Number -> Number -> String
renderRectTransform width height x y angle s =
    renderTransform x y angle s
        ++ " translate("
        ++ String.fromFloat (-width / 2)
        ++ ","
        ++ String.fromFloat (-height / 2)
        ++ ")"


renderImage : Number -> Number -> String -> Number -> Number -> Number -> Number -> Number -> Svg event
renderImage w h src x y angle s alpha =
    Svg.image
        (xlinkHref src
            :: width (String.fromFloat w)
            :: height (String.fromFloat h)
            :: transform (renderRectTransform w h x y angle s)
            :: renderAlpha alpha
        )
        []


renderNgon : Color -> Int -> Number -> Number -> Number -> Number -> Number -> Number -> Svg event
renderNgon color n radius x y angle s alpha =
    Svg.polygon
        (points (toNgonPoints 0 n radius "")
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
        )
        []


toNgonPoints : Int -> Int -> Float -> String -> String
toNgonPoints i n radius string =
    if i == n then
        string

    else
        let
            a =
                turns (toFloat i / toFloat n - 0.25)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        toNgonPoints (i + 1) n radius (string ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")


renderPolygon : Color -> List ( Number, Number ) -> Number -> Number -> Number -> Number -> Number -> Svg event
renderPolygon color coordinates x y angle s alpha =
    Svg.polygon
        (points (List.foldl addPoint "" coordinates)
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
        )
        []


addPoint : ( Float, Float ) -> String -> String
addPoint ( x, y ) str =
    str ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ " "


renderWords : Color -> String -> Number -> Number -> Number -> Number -> Number -> Svg event
renderWords color string x y angle s alpha =
    text_
        (textAnchor "middle"
            :: dominantBaseline "central"
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
        )
        [ text string
        ]


renderColor : Color -> String
renderColor color =
    case color of
        Hex str ->
            str

        Rgb r g b ->
            "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"


renderAlpha : Number -> List (Svg.Attribute event)
renderAlpha alpha =
    if alpha == 1 then
        []

    else
        [ opacity (String.fromFloat (clamp 0 1 alpha)) ]


renderTransform : Number -> Number -> Number -> Number -> String
renderTransform x y a s =
    if a == 0 then
        if s == 1 then
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ")"

        else
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") scale(" ++ String.fromFloat s ++ ")"

    else if s == 1 then
        "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromFloat -a ++ ")"

    else
        "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromFloat -a ++ ") scale(" ++ String.fromFloat s ++ ")"


reactOnClick : (state -> state) -> Shape state -> Shape state
reactOnClick updateState (Shape x y a s o f interactivity) =
    Shape x y a s o f (ReactOnClick updateState :: interactivity)
