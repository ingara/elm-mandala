module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Canvas exposing (Commands)
import CanvasColor as Color exposing (Color)
import Frame2d exposing (Frame2d)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Point2d exposing (Point2d)


color : Color
color =
    Color.black


numSections : Int
numSections =
    12



---- MODEL ----


type alias PointerData =
    { midPoint : Point2d
    , point : Point2d
    }


type alias Model =
    { width : Int
    , height : Int
    , buffer : Commands
    , toDraw : Commands
    , pointer : Maybe PointerData
    , frames : List Frame2d
    }


init : ( Model, Cmd Msg )
init =
    let
        width =
            800

        height =
            800

        origin =
            Point2d.fromCoordinates ( width / 2, height / 2 )
    in
    ( { width = width
      , height = height
      , buffer =
            Canvas.empty
                |> Canvas.strokeStyle Color.black
                |> Canvas.lineCap Canvas.RoundCap
                |> Canvas.lineJoin Canvas.RoundJoin
                |> Canvas.lineWidth 4
                |> Canvas.fillStyle (Color.rgb 255 255 255)
                |> Canvas.fillRect 0 0 width height
      , toDraw = Canvas.empty
      , pointer = Nothing
      , frames =
            List.range 0 (numSections - 1)
                |> List.map
                    (\i ->
                        (toFloat i / toFloat numSections)
                            * 360
                            |> degrees
                    )
                |> List.map
                    (\rotation ->
                        Frame2d.atOrigin
                            |> Frame2d.rotateAround origin rotation
                    )
      }
        |> pendingToBuffer
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AnimationFrame Float
    | StartAt Point2d
    | MoveAt Point2d
    | EndAt Point2d


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta AnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        AnimationFrame _ ->
            model |> pendingToBuffer

        StartAt coords ->
            model
                |> startDrawing coords

        MoveAt coords ->
            case model.pointer of
                Just pointer ->
                    model
                        |> drawPoint coords pointer

                Nothing ->
                    model

        EndAt coords ->
            case model.pointer of
                Just pointer ->
                    model
                        |> drawFinalPoint coords pointer

                Nothing ->
                    model
    , Cmd.none
    )


pendingToBuffer : Model -> Model
pendingToBuffer model =
    { model
        | buffer = Canvas.empty
        , toDraw = model.buffer
    }


startDrawing : Point2d -> Model -> Model
startDrawing point model =
    { model
        | pointer = Just { point = point, midPoint = point }
    }


drawPoint : Point2d -> PointerData -> Model -> Model
drawPoint newPoint { point, midPoint } ({ frames, buffer } as model) =
    let
        newMidPoint =
            Point2d.midpoint point newPoint
    in
    { model
        | pointer = Just { point = newPoint, midPoint = newMidPoint }
        , buffer =
            frames
                |> List.foldl
                    (\frame buff ->
                        buff
                            |> drawLine
                                { start =
                                    midPoint
                                        |> Point2d.placeIn frame
                                , control =
                                    point
                                        |> Point2d.placeIn frame
                                , end =
                                    newMidPoint
                                        |> Point2d.placeIn frame
                                }
                    )
                    buffer
    }


drawLine : { start : Point2d, control : Point2d, end : Point2d } -> Commands -> Commands
drawLine { start, control, end } buffer =
    let
        ( startX, startY ) =
            Point2d.coordinates start

        ( controlX, controlY ) =
            Point2d.coordinates control

        ( endX, endY ) =
            Point2d.coordinates end
    in
    buffer
        |> Canvas.beginPath
        |> Canvas.moveTo startX startY
        |> Canvas.quadraticCurveTo controlX controlY endX endY
        |> Canvas.stroke


drawFinalPoint : Point2d -> PointerData -> Model -> Model
drawFinalPoint newPoint { point, midPoint } ({ buffer } as model) =
    { model
        | pointer = Nothing
        , buffer =
            buffer
                |> drawLine
                    { start = midPoint
                    , control = point
                    , end = newPoint
                    }
    }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewCanvas model
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        ( width, height ) =
            ( toFloat model.width, toFloat model.height )
    in
    Canvas.element
        model.width
        model.height
        [ Mouse.onDown (.offsetPos >> Point2d.fromCoordinates >> StartAt)
        , Mouse.onMove (.offsetPos >> Point2d.fromCoordinates >> MoveAt)
        , Mouse.onUp (.offsetPos >> Point2d.fromCoordinates >> EndAt)
        ]
        model.toDraw


tupleMapBoth : (a -> b -> c) -> ( a, b ) -> c
tupleMapBoth f ( a, b ) =
    f a b



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
