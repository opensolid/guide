module Frames.PlaceIn exposing (..)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Axis2d as Axis2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Svg as Svg
import Common exposing (..)
import Frames.Common exposing (..)
import Mouse


type DraggedObject
    = Point
    | OriginPoint
    | XDirection
    | YDirection


type alias DragInProgress =
    ( DraggedObject, Mouse.Position, Mouse.Position )


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
            Point2d ( 0.5, -0.75 )

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


transformedPoint : Model -> Point2d
transformedPoint model =
    case model.dragInProgress of
        Just ( Point, startPosition, endPosition ) ->
            Point2d.translateBy
                (dragDisplacement startPosition endPosition)
                model.point

        _ ->
            model.point


rotatedFrame : Frame2d -> Point2d -> Vector2d -> Frame2d
rotatedFrame frame tipPoint tipDisplacement =
    let
        rotationAngle =
            sweptAngleAround (Frame2d.originPoint frame)
                tipPoint
                (Point2d.translateBy tipDisplacement tipPoint)
    in
        case rotationAngle of
            Just angle ->
                Frame2d.rotateBy angle frame

            Nothing ->
                frame


transformedFrame : Model -> Frame2d
transformedFrame model =
    case model.dragInProgress of
        Just ( draggedObject, startPosition, endPosition ) ->
            let
                displacement =
                    dragDisplacement startPosition endPosition
            in
                case draggedObject of
                    OriginPoint ->
                        Frame2d.translateBy displacement model.frame

                    XDirection ->
                        rotatedFrame model.frame (xTip model.frame) displacement

                    YDirection ->
                        rotatedFrame model.frame (yTip model.frame) displacement

                    Point ->
                        model.frame

        Nothing ->
            model.frame


view : Model -> Html Msg
view model =
    let
        viewBox =
            BoundingBox2d
                { minX = -0.5
                , minY = -1
                , maxX = 2.5
                , maxY = 3
                }

        currentPoint =
            transformedPoint model

        currentFrame =
            transformedFrame model

        placedPoint =
            Point2d.placeIn currentFrame currentPoint

        originPoint =
            Frame2d.originPoint currentFrame

        xTipPoint =
            xTip currentFrame

        yTipPoint =
            yTip currentFrame

        xAxis =
            Frame2d.xAxis currentFrame
    in
        Html.div []
            [ scene2d viewBox
                [ coordinateLines currentPoint Axis2d.x Blue
                , frame2d Blue Frame2d.xy
                , point2d Orange currentPoint
                , coordinateLabel viewBox Blue currentPoint
                , Svg.g [ Svg.Attributes.cursor "move" ]
                    [ dragCircle (DragStart Point) currentPoint ]
                ]
            , scene2d viewBox
                [ coordinateLines placedPoint xAxis Blue
                , coordinateLines placedPoint Axis2d.x Black
                , frame2d Black Frame2d.xy
                , frame2d Blue currentFrame
                , point2d Orange placedPoint
                , coordinateLabel viewBox Black placedPoint
                , Svg.g [ Svg.Attributes.cursor "move" ]
                    [ dragCircle (DragStart XDirection) xTipPoint
                    , dragCircle (DragStart YDirection) yTipPoint
                    , dragCircle (DragStart OriginPoint) originPoint
                    ]
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart draggedObject position ->
            let
                newDrag =
                    ( draggedObject, position, position )
            in
                ( { model | dragInProgress = Just newDrag }, Cmd.none )

        DragTo position ->
            case model.dragInProgress of
                Just ( draggedObject, startPosition, endPosition ) ->
                    let
                        updatedDrag =
                            ( draggedObject, startPosition, position )
                    in
                        ( { model | dragInProgress = Just updatedDrag }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        DragEnd position ->
            ( { model
                | point = transformedPoint model
                , frame = transformedFrame model
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
