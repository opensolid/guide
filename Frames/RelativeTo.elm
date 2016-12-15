module Frames.RelativeTo exposing (..)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Axis2d as Axis2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Svg as Svg
import Common exposing (..)
import Mouse
import Json.Decode as Decode
import Formatting exposing (Format, (<>))


type DraggedObject
    = Point
    | OriginPoint
    | XDirection
    | YDirection


type alias DragInProgress =
    { draggedObject : DraggedObject
    , startPosition : Mouse.Position
    , endPosition : Mouse.Position
    }


type alias Model =
    { point : Point2d
    , frame : Frame2d
    , dragInProgress : Maybe DragInProgress
    }


type Msg
    = DragStart DraggedObject Mouse.Position
    | DragTo Mouse.Position
    | DragEnd Mouse.Position


init : ( Model, Cmd Msg )
init =
    let
        initialPoint =
            Point2d ( 1.5, 0.5 )

        initialFrame =
            Frame2d.at (Point2d ( 0.75, 0.75 ))
                |> Frame2d.rotateBy (degrees 30)

        initialModel =
            { point = initialPoint
            , frame = initialFrame
            , dragInProgress = Nothing
            }
    in
        ( initialModel, Cmd.none )


displaced : Point2d -> Mouse.Position -> Mouse.Position -> Point2d
displaced basePoint dragStart dragEnd =
    let
        ( x, y ) =
            Point2d.coordinates basePoint

        dx =
            toFloat (dragEnd.x - dragStart.x) / pixelsPerUnit

        dy =
            toFloat (dragStart.y - dragEnd.y) / pixelsPerUnit
    in
        Point2d ( x + dx, y + dy )


actualPoint : Model -> Point2d
actualPoint model =
    case model.dragInProgress of
        Just dragInProgress ->
            case dragInProgress.draggedObject of
                Point ->
                    displaced model.point
                        dragInProgress.startPosition
                        dragInProgress.endPosition

                _ ->
                    model.point

        Nothing ->
            model.point


rotatedFrame : Frame2d -> Point2d -> Point2d -> Frame2d
rotatedFrame frame dragStartPoint dragEndPoint =
    let
        originPoint =
            Frame2d.originPoint frame

        startDirection =
            Vector2d.direction (Point2d.vectorFrom originPoint dragStartPoint)

        endDirection =
            Vector2d.direction (Point2d.vectorFrom originPoint dragEndPoint)

        rotationAngle =
            Maybe.map2 Direction2d.angleFrom startDirection endDirection
    in
        case rotationAngle of
            Just angle ->
                Frame2d.rotateBy angle frame

            Nothing ->
                frame


actualFrame : Model -> Frame2d
actualFrame model =
    case model.dragInProgress of
        Just dragInProgress ->
            case dragInProgress.draggedObject of
                OriginPoint ->
                    let
                        displacedOrigin =
                            displaced (Frame2d.originPoint model.frame)
                                dragInProgress.startPosition
                                dragInProgress.endPosition
                    in
                        Frame2d
                            { originPoint = displacedOrigin
                            , xDirection = Frame2d.xDirection model.frame
                            , yDirection = Frame2d.yDirection model.frame
                            }

                XDirection ->
                    let
                        tipPoint =
                            Point2d.placeIn model.frame (Point2d ( 1, 0 ))
                    in
                        rotatedFrame model.frame
                            tipPoint
                            (displaced tipPoint
                                dragInProgress.startPosition
                                dragInProgress.endPosition
                            )

                YDirection ->
                    let
                        tipPoint =
                            Point2d.placeIn model.frame (Point2d ( 0, 1 ))
                    in
                        rotatedFrame model.frame
                            tipPoint
                            (displaced tipPoint
                                dragInProgress.startPosition
                                dragInProgress.endPosition
                            )

                Point ->
                    model.frame

        Nothing ->
            model.frame


formatFloat : Float -> String
formatFloat value =
    let
        printResult =
            Formatting.print (Formatting.roundTo 2) value
    in
        if value < 0.0 && (not (String.startsWith "-" printResult)) then
            "-" ++ printResult
        else
            printResult


coordinatesString : Point2d -> String
coordinatesString point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        "(" ++ formatFloat x ++ ", " ++ formatFloat y ++ ")"


noSelectAttribute : Svg.Attribute msg
noSelectAttribute =
    Svg.Attributes.style
        (String.join "; "
            [ "-webkit-user-select: none"
            , "-moz-user-select: none"
            , "-ms-user-select: none"
            , "user-select: none"
            ]
        )


view : Model -> Html Msg
view model =
    let
        boundingBox =
            BoundingBox2d
                { minX = -1
                , minY = -1
                , maxX = 3
                , maxY = 3
                }

        currentPoint =
            actualPoint model

        currentFrame =
            actualFrame model

        relativePoint =
            Point2d.relativeTo currentFrame currentPoint

        originPoint =
            Frame2d.originPoint currentFrame

        xDirection =
            Frame2d.xDirection currentFrame

        yDirection =
            Frame2d.yDirection currentFrame

        ( globalX, globalY ) =
            Point2d.coordinates currentPoint

        ( localX, localY ) =
            Point2d.coordinates relativePoint

        coordinateLabel point color =
            let
                ( x, y ) =
                    Point2d.coordinates point

                offset =
                    0.1

                ( anchorX, anchorType ) =
                    if x <= BoundingBox2d.midX boundingBox then
                        ( x + offset, "start" )
                    else
                        ( x - offset, "end" )

                mirrorAxis =
                    Axis2d { originPoint = point, direction = Direction2d.x }
            in
                Svg.mirrorAcross mirrorAxis
                    (Svg.text_
                        [ Svg.Attributes.x (toString anchorX)
                        , Svg.Attributes.textAnchor anchorType
                        , Svg.Attributes.y (toString y)
                        , Svg.Attributes.fontSize "0.2"
                        , Svg.Attributes.fill (colorString color)
                        , noSelectAttribute
                        ]
                        [ Svg.text (coordinatesString point) ]
                    )
    in
        Html.div []
            [ scene2d boundingBox
                [ frame2d Black Frame2d.xy
                , Svg.g [ Svg.Attributes.cursor "move" ]
                    [ Svg.g [ onMouseDown (DragStart XDirection) ]
                        [ direction2d Blue originPoint xDirection ]
                    , Svg.g [ onMouseDown (DragStart YDirection) ]
                        [ direction2d Blue originPoint yDirection ]
                    , Svg.g [ onMouseDown (DragStart OriginPoint) ]
                        [ originPoint2d Blue originPoint ]
                    , Svg.g [ onMouseDown (DragStart Point) ]
                        [ point2d Orange currentPoint ]
                    ]
                , coordinateLabel currentPoint Black
                ]
            , scene2d boundingBox
                [ frame2d Blue Frame2d.xy
                , point2d Orange relativePoint
                , coordinateLabel relativePoint Blue
                ]
            ]


onMouseDown : (Mouse.Position -> Msg) -> Svg.Attribute Msg
onMouseDown tag =
    Svg.Events.on "mousedown" (Decode.map tag Mouse.position)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart draggedObject position ->
            let
                newDrag =
                    { draggedObject = draggedObject
                    , startPosition = position
                    , endPosition = position
                    }
            in
                ( { model | dragInProgress = Just newDrag }, Cmd.none )

        DragTo position ->
            case model.dragInProgress of
                Just currentDrag ->
                    let
                        updatedDrag =
                            { currentDrag | endPosition = position }
                    in
                        ( { model | dragInProgress = Just updatedDrag }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        DragEnd position ->
            ( { model
                | point = actualPoint model
                , frame = actualFrame model
                , dragInProgress = Nothing
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragInProgress of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragTo, Mouse.ups DragEnd ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
