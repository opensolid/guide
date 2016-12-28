module Frames.RelativeTo exposing (..)

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


actualPoint : Model -> Point2d
actualPoint model =
    case model.dragInProgress of
        Just ( Point, startPosition, endPosition ) ->
            let
                displacement =
                    dragDisplacement startPosition endPosition
            in
                Point2d.translateBy displacement
                    model.point

        _ ->
            model.point


rotatedFrame : Frame2d -> Point2d -> Point2d -> Frame2d
rotatedFrame frame dragStartPoint dragEndPoint =
    let
        rotationAngle =
            sweptAngleAround (Frame2d.originPoint frame)
                dragStartPoint
                dragEndPoint
    in
        case rotationAngle of
            Just angle ->
                Frame2d.rotateBy angle frame

            Nothing ->
                frame


actualFrame : Model -> Frame2d
actualFrame model =
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
                        let
                            tipPoint =
                                Point2d.placeIn model.frame (Point2d ( 1, 0 ))

                            displacedTipPoint =
                                Point2d.translateBy displacement tipPoint
                        in
                            rotatedFrame model.frame tipPoint displacedTipPoint

                    YDirection ->
                        let
                            tipPoint =
                                Point2d.placeIn model.frame (Point2d ( 0, 1 ))

                            displacedTipPoint =
                                Point2d.translateBy displacement tipPoint
                        in
                            rotatedFrame model.frame tipPoint displacedTipPoint

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

        xTip =
            Point2d.placeIn currentFrame (Point2d ( 1, 0 ))

        yTip =
            Point2d.placeIn currentFrame (Point2d ( 0, 1 ))

        ( globalX, globalY ) =
            Point2d.coordinates currentPoint

        ( localX, localY ) =
            Point2d.coordinates relativePoint
    in
        Html.div []
            [ scene2d viewBox
                [ coordinateLines currentPoint (Frame2d.xAxis currentFrame) Blue
                , coordinateLines currentPoint Axis2d.x Black
                , frame2d Black Frame2d.xy
                , frame2d Blue currentFrame
                , point2d Orange currentPoint
                , coordinateLabel viewBox Black currentPoint
                , Svg.g [ Svg.Attributes.cursor "move" ]
                    [ dragCircle (DragStart XDirection) xTip
                    , dragCircle (DragStart YDirection) yTip
                    , dragCircle (DragStart OriginPoint) originPoint
                    , dragCircle (DragStart Point) currentPoint
                    ]
                ]
            , scene2d viewBox
                [ coordinateLines relativePoint Axis2d.x Blue
                , frame2d Blue Frame2d.xy
                , point2d Orange relativePoint
                , coordinateLabel viewBox Blue relativePoint
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
