module Frames.Common
    exposing
        ( coordinateLabel
        , coordinateLines
        , dragCircle
        , dragDisplacement
        , sweptAngleAround
        , xTip
        , yTip
        )

import Common exposing (..)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Svg as Svg
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Json.Decode as Decode
import Mouse
import Formatting exposing (Format)


coordinateLines : Point2d -> Axis2d -> Color -> Svg msg
coordinateLines point referenceAxis color =
    let
        axisOrigin =
            Axis2d.originPoint referenceAxis

        projectedPoint =
            Point2d.projectOnto referenceAxis point

        parallelSegment =
            LineSegment2d ( axisOrigin, projectedPoint )

        perpendicularSegment =
            LineSegment2d ( projectedPoint, point )
    in
        Svg.g
            [ Svg.Attributes.stroke (colorString color)
            , Svg.Attributes.strokeWidth "0.01"
            , Svg.Attributes.strokeDasharray "0.03 0.03"
            ]
            [ Svg.lineSegment2d [] parallelSegment
            , Svg.lineSegment2d [] perpendicularSegment
            ]


formatFloat : Float -> String
formatFloat =
    Formatting.print (Formatting.roundTo 2)


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


coordinateLabel : BoundingBox2d -> Color -> Point2d -> Svg msg
coordinateLabel viewBox color point =
    let
        ( x, y ) =
            Point2d.coordinates point

        offset =
            0.1

        ( anchorX, anchorType ) =
            if x <= BoundingBox2d.midX viewBox then
                ( x + offset, "start" )
            else
                ( x - offset, "end" )

        mirrorAxis =
            Axis2d { originPoint = point, direction = Direction2d.x }

        coordinatesString =
            "(" ++ formatFloat x ++ ", " ++ formatFloat y ++ ")"
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
                [ Svg.text coordinatesString ]
            )


onMouseDown : (Mouse.Position -> msg) -> Svg.Attribute msg
onMouseDown tag =
    Svg.Events.on "mousedown" (Decode.map tag Mouse.position)


dragCircle : (Mouse.Position -> msg) -> Point2d -> Svg msg
dragCircle tag point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        Svg.circle
            [ Svg.Attributes.cx (toString x)
            , Svg.Attributes.cy (toString y)
            , Svg.Attributes.r "0.15"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.fill "rgba(0, 0, 0, 0)"
            , onMouseDown tag
            ]
            []


dragDisplacement : Mouse.Position -> Mouse.Position -> Vector2d
dragDisplacement dragStart dragEnd =
    Vector2d
        ( toFloat (dragEnd.x - dragStart.x) / pixelsPerUnit
        , toFloat (dragStart.y - dragEnd.y) / pixelsPerUnit
        )


sweptAngleAround : Point2d -> Point2d -> Point2d -> Maybe Float
sweptAngleAround centerPoint startPoint endPoint =
    let
        startDirection =
            Vector2d.direction (Point2d.vectorFrom centerPoint startPoint)

        endDirection =
            Vector2d.direction (Point2d.vectorFrom centerPoint endPoint)
    in
        Maybe.map2 Direction2d.angleFrom startDirection endDirection


xTip : Frame2d -> Point2d
xTip frame =
    Point2d.placeIn frame (Point2d ( 1, 0 ))


yTip : Frame2d -> Point2d
yTip frame =
    Point2d.placeIn frame (Point2d ( 0, 1 ))
