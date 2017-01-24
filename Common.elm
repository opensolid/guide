module Common
    exposing
        ( pixelsPerUnit
        , Color(..)
        , colorString
        , scene2d
        , point2d
        , centerPoint2d
        , originPoint2d
        , direction2d
        , frame2d
        , lineSegment2d
        , triangle2d
        )

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Svg as Svg


config =
    { scale = 100.0
    , stroke =
        { width = 0.02
        , thinWidth = 0.01
        }
    , point =
        { radius = 0.05
        }
    , originPoint =
        { radius = 0.05
        }
    , centerPoint =
        { radius = 0.03
        , crossRadius = 0.1
        }
    , direction =
        { length = 1
        , tipLength = 0.15
        , tipWidth = 0.1
        }
    }


pixelsPerUnit : Float
pixelsPerUnit =
    config.scale


type Color
    = Black
    | Blue
    | Orange
    | Teal


colorString : Color -> String
colorString color =
    case color of
        Black ->
            "black"

        Blue ->
            "rgb(0, 109, 219)"

        Orange ->
            "rgb(219, 109, 0)"

        Teal ->
            "rgb(0, 146, 146)"


scene2d : BoundingBox2d -> List (Svg msg) -> Html msg
scene2d boundingBox elements =
    let
        minX =
            BoundingBox2d.minX boundingBox

        maxX =
            BoundingBox2d.maxX boundingBox

        minY =
            BoundingBox2d.minY boundingBox

        maxY =
            BoundingBox2d.maxY boundingBox

        width =
            toString (config.scale * (maxX - minX))

        height =
            toString (config.scale * (maxY - minY))

        topLeftFrame =
            Frame2d
                { originPoint = Point2d ( minX, maxY )
                , xDirection = Direction2d.x
                , yDirection = Direction2d.flip Direction2d.y
                }

        globalAttributes =
            [ Attributes.strokeWidth (toString config.stroke.width) ]
    in
        Svg.svg [ Attributes.width width, Attributes.height height ]
            [ Svg.g globalAttributes elements
                |> Svg.relativeTo topLeftFrame
                |> Svg.scaleAbout Point2d.origin config.scale
            ]


point2d : Color -> Point2d -> Svg msg
point2d color point =
    Svg.circle2d [ Attributes.fill (colorString color) ]
        (Circle2d { centerPoint = point, radius = config.point.radius })


centerPoint2d : Color -> Point2d -> Svg msg
centerPoint2d color point =
    let
        horizontalLine =
            LineSegment2d
                ( Point2d ( -config.centerPoint.crossRadius, 0 )
                , Point2d ( config.centerPoint.crossRadius, 0 )
                )

        verticalLine =
            LineSegment2d
                ( Point2d ( 0, -config.centerPoint.crossRadius )
                , Point2d ( 0, config.centerPoint.crossRadius )
                )

        circle =
            Circle2d
                { centerPoint = Point2d.origin
                , radius = config.centerPoint.radius
                }
    in
        Svg.g []
            [ Svg.circle2d [ Attributes.fill (colorString color) ] circle
            , Svg.g
                [ Attributes.stroke (colorString color)
                , Attributes.strokeWidth (toString config.stroke.thinWidth)
                ]
                [ Svg.lineSegment2d [] verticalLine
                , Svg.lineSegment2d [] horizontalLine
                ]
            ]
            |> Svg.placeIn (Frame2d.at point)


originPoint2d : Color -> Point2d -> Svg msg
originPoint2d color point =
    let
        attributes =
            [ Attributes.fill "white"
            , Attributes.stroke (colorString color)
            , Attributes.strokeWidth (toString config.stroke.thinWidth)
            ]

        circle =
            Circle2d { centerPoint = point, radius = config.originPoint.radius }
    in
        Svg.circle2d attributes circle


direction2d : Color -> Point2d -> Direction2d -> Svg msg
direction2d color basePoint direction =
    let
        frame =
            Frame2d
                { originPoint = basePoint
                , xDirection = direction
                , yDirection = Direction2d.perpendicularTo direction
                }

        tipPoint =
            Point2d ( config.direction.length, 0 )

        stemLength =
            config.direction.length - config.direction.tipLength

        stemPoint =
            Point2d ( stemLength, 0 )

        leftPoint =
            Point2d ( stemLength, config.direction.tipWidth / 2 )

        rightPoint =
            Point2d ( stemLength, -config.direction.tipWidth / 2 )

        stem =
            LineSegment2d ( Point2d.origin, stemPoint )

        tip =
            Triangle2d ( tipPoint, leftPoint, rightPoint )

        attributes =
            [ Attributes.stroke (colorString color)
            , Attributes.fill "white"
            , Attributes.strokeWidth (toString config.stroke.thinWidth)
            ]
    in
        Svg.g attributes [ Svg.lineSegment2d [] stem, Svg.triangle2d [] tip ]
            |> Svg.placeIn frame


frame2d : Color -> Frame2d -> Svg msg
frame2d color frame =
    let
        originPoint =
            Frame2d.originPoint frame

        xDirection =
            Frame2d.xDirection frame

        yDirection =
            Frame2d.yDirection frame
    in
        Svg.g []
            [ direction2d color originPoint xDirection
            , direction2d color originPoint yDirection
            , originPoint2d color originPoint
            ]


lineSegment2d : Color -> LineSegment2d -> Svg msg
lineSegment2d color lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment

        lineSegmentAttributes =
            [ Attributes.stroke (colorString color)
            , Attributes.strokeWidth (toString config.stroke.width)
            ]
    in
        Svg.g []
            [ Svg.lineSegment2d lineSegmentAttributes lineSegment
            , point2d color p1
            , point2d color p2
            ]


triangle2d : Color -> Triangle2d -> Svg msg
triangle2d color triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle

        triangleAttributes =
            [ Attributes.stroke (colorString color)
            , Attributes.strokeWidth (toString config.stroke.width)
            , Attributes.fill "none"
            ]
    in
        Svg.g []
            [ Svg.triangle2d triangleAttributes triangle
            , point2d color p1
            , point2d color p2
            , point2d color p3
            ]
