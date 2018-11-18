port module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Browser
import Browser.Events
import Canvas exposing (Commands)
import CanvasColor
import Color exposing (Color)
import ColorPicker
import Frame2d exposing (Frame2d)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Point2d exposing (Point2d)


port toJs : { msg : String, payload : String } -> Cmd msg


port fromJs : ({ msg : String, payload : String } -> msg) -> Sub msg



---- MODEL ----


type alias PointerData =
    { midPoint : Point2d
    , point : Point2d
    }


type alias Flags =
    { width : Float, height : Float }


type alias Model =
    { width : Float
    , height : Float
    , numSections : Int
    , lineWidth : Float
    , brushColor : Color
    , colorPicker : ColorPicker.State
    , allowDrawingOutside : Bool
    , buffer : Commands
    , toDraw : Commands
    , pointer : Maybe PointerData
    , frames : List Frame2d
    , history : List String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        size =
            Basics.min flags.width flags.height
    in
    ( { width = size
      , height = size
      , numSections = 5
      , lineWidth = 4
      , brushColor = Color.black
      , colorPicker = ColorPicker.empty
      , allowDrawingOutside = False
      , buffer =
            Canvas.empty
      , toDraw = Canvas.empty
      , pointer = Nothing
      , frames = []
      , history = []
      }
        |> resetCanvas
    , Cmd.none
    )


toCanvasColor : Color -> CanvasColor.Color
toCanvasColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    CanvasColor.rgba
        (round <| 255 * red)
        (round <| 255 * green)
        (round <| 255 * blue)
        alpha


resetCanvas : Model -> Model
resetCanvas =
    setFrames >> initCanvas >> pendingToBuffer


initCanvas : Model -> Model
initCanvas ({ width, height } as model) =
    { model
        | buffer =
            Canvas.empty
                |> Canvas.strokeStyle (toCanvasColor model.brushColor)
                |> Canvas.lineCap Canvas.RoundCap
                |> Canvas.lineJoin Canvas.RoundJoin
                |> Canvas.lineWidth model.lineWidth
                |> Canvas.fillStyle CanvasColor.white
                |> Canvas.fillRect 0 0 width height
    }


setFrames : Model -> Model
setFrames ({ numSections, width, height } as model) =
    let
        origin =
            Point2d.fromCoordinates ( width / 2, height / 2 )

        frames =
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
    in
    { model | frames = frames }



---- UPDATE ----


type Msg
    = NoOp
    | AnimationFrame Float
    | StartAt Point2d
    | MoveAt Point2d
    | EndAt Point2d
    | ClearClicked
    | NumSectionsInput String
    | LineWidthInput String
    | ColorPickerMsg ColorPicker.Msg
    | AllowDrawingOutsideToggle Bool
    | JsMsg { msg : String, payload : String }
    | HistoryElementChosen String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationFrame
        , fromJs JsMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AnimationFrame _ ->
            ( model |> pendingToBuffer, Cmd.none )

        StartAt coords ->
            ( model
                |> startDrawing coords
            , Cmd.none
            )

        MoveAt coords ->
            case model.pointer of
                Just pointer ->
                    ( model
                        |> drawPoint coords pointer
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EndAt coords ->
            case model.pointer of
                Just pointer ->
                    ( model
                        |> drawFinalPoint coords pointer
                    , toJs { msg = "SAVE", payload = "" }
                    )

                Nothing ->
                    ( model, Cmd.none )

        ClearClicked ->
            ( model
                |> pendingToBuffer
                |> initCanvas
            , Cmd.none
            )

        NumSectionsInput num ->
            case String.toInt num of
                Just v ->
                    ( { model | numSections = v }
                        |> setFrames
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        LineWidthInput num ->
            case String.toFloat num of
                Just v ->
                    ( { model | lineWidth = v }
                        |> setLineWidth
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ColorPickerMsg subMsg ->
            let
                ( colorPicker, color ) =
                    ColorPicker.update
                        subMsg
                        model.brushColor
                        model.colorPicker
            in
            ( { model
                | colorPicker = colorPicker
                , brushColor = Maybe.withDefault model.brushColor color
              }
                |> setBrushColor
            , Cmd.none
            )

        AllowDrawingOutsideToggle val ->
            ( { model | allowDrawingOutside = val }, Cmd.none )

        JsMsg data ->
            case data.msg of
                "SAVED" ->
                    ( { model
                        | history = data.payload :: model.history
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        HistoryElementChosen str ->
            ( model, toJs { msg = "LOAD", payload = str } )


setLineWidth : Model -> Model
setLineWidth ({ lineWidth, buffer } as model) =
    { model
        | buffer =
            buffer
                |> Canvas.lineWidth lineWidth
    }


setBrushColor : Model -> Model
setBrushColor ({ brushColor, buffer } as model) =
    { model
        | buffer =
            buffer
                |> Canvas.strokeStyle (toCanvasColor brushColor)
    }


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
    }
        |> drawInFrames { start = midPoint, control = point, end = newMidPoint }


drawInFrames : { start : Point2d, control : Point2d, end : Point2d } -> Model -> Model
drawInFrames { start, control, end } ({ frames, buffer } as model) =
    { model
        | buffer =
            frames
                |> List.foldl
                    (\frame buff ->
                        buff
                            |> drawLine
                                { start =
                                    start
                                        |> Point2d.placeIn frame
                                , control =
                                    control
                                        |> Point2d.placeIn frame
                                , end =
                                    end
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
    }
        |> drawInFrames
            { start = midPoint
            , control = point
            , end = newPoint
            }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "cont" ]
        [ viewCanvas model
        , viewControls model
        , viewHistory model
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        ( width, height ) =
            ( floor model.width, floor model.height )
    in
    Canvas.element
        width
        height
        [ id "canvas"
        , Mouse.onDown (.offsetPos >> Point2d.fromCoordinates >> StartAt)
        , Mouse.onMove (.offsetPos >> Point2d.fromCoordinates >> MoveAt)
        , Mouse.onUp (.offsetPos >> Point2d.fromCoordinates >> EndAt)
        , Mouse.onLeave
            (.offsetPos
                >> Point2d.fromCoordinates
                >> (\point ->
                        if model.allowDrawingOutside then
                            NoOp

                        else
                            EndAt point
                   )
            )
        ]
        model.toDraw


viewControls : Model -> Html Msg
viewControls model =
    Form.formInline []
        [ Form.group []
            [ Form.label [] [ text "Number of sections" ]
            , Input.number [ Input.onInput NumSectionsInput, Input.placeholder <| String.fromInt model.numSections ]
            ]
        , Form.group []
            [ Form.label [] [ text "Brush size" ]
            , Input.number [ Input.onInput LineWidthInput, Input.placeholder <| String.fromFloat model.lineWidth ]
            ]
        , Form.group []
            [ Form.label [] [ text "Brush color" ]
            , Html.map ColorPickerMsg <|
                ColorPicker.view model.brushColor model.colorPicker
            ]
        , Form.group []
            [ Checkbox.checkbox
                [ Checkbox.checked model.allowDrawingOutside
                , Checkbox.onCheck AllowDrawingOutsideToggle
                ]
                "Allow drawing outside"
            ]
        , Button.button
            [ Button.warning
            , Button.onClick ClearClicked
            ]
            [ text "Clear" ]
        ]


viewHistory : Model -> Html Msg
viewHistory { history } =
    div [ class "history" ]
        [ h2 [] [ text "HISTORY" ]
        , ul [ class "history" ]
            (history
                |> List.reverse
                |> List.indexedMap
                    (\i h ->
                        li [ onClick <| HistoryElementChosen h ]
                            [ text <| String.fromInt i ]
                    )
            )
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
