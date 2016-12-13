module SimpleRotation exposing (..)

import Html exposing (Html)
import Html.Attributes as Html
import Common exposing (..)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Point2d as Point2d
import Kintail.InputWidget as InputWidget


view : Float -> Html Float
view angle =
    let
        boundingBox =
            BoundingBox2d
                { minX = -0.5
                , minY = -0.5
                , maxX = 2.5
                , maxY = 3.5
                }

        triangle =
            Triangle2d
                ( Point2d ( 1, 0.5 )
                , Point2d ( 2, 0.5 )
                , Point2d ( 2, 2 )
                )

        rotatedTriangle =
            Triangle2d.rotateAround Point2d.origin (degrees angle) triangle

        scene =
            scene2d boundingBox
                [ frame2d Black Frame2d.xy
                , triangle2d Blue triangle
                , triangle2d Orange rotatedTriangle
                ]

        label =
            Html.text "Angle:"

        slider =
            InputWidget.slider [ Html.style [ ( "flex-grow", "1" ) ] ] { min = 0, max = 45, step = 1 } angle
    in
        Html.div [ Html.style [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "width", "250px" ), ( "align-items", "center" ) ] ]
            [ scene
            , Html.div [ Html.style [ ( "display", "flex" ), ( "flex-direction", "row" ), ( "align-self", "stretch" ) ] ]
                [ Html.div [ Html.style [ ( "height", "24px" ), ( "line-height", "24px" ), ( "vertical-align", "middle" ) ] ] [ label ]
                , Html.div [ Html.style [ ( "height", "24px" ), ( "flex-grow", "1" ), ( "display", "flex" ) ] ] [ slider ]
                ]
            ]


main : Program Never Float Float
main =
    Html.beginnerProgram
        { model = 15
        , view = view
        , update = always
        }
