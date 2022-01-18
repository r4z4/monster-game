module TestSvg exposing (..)

import Html exposing (..)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)


showTestSVG : Html msg
showTestSVG =
    svg
        [ viewBox "0 0 400 400"
        , Svg.Attributes.width "200"
        , Svg.Attributes.height "200"
        ]
        [ circle
            [ cx "50"
            , cy "50"
            , r "40"
            , fill "green"
            , stroke "black"
            , strokeWidth "3"
            ]
            []
        , rect
            [ x "100"
            , y "10"
            , Svg.Attributes.width "40"
            , Svg.Attributes.height "40"
            , fill "orange"
            , stroke "black"
            , strokeWidth "2"
            ]
            []
        , line
            [ x1 "20"
            , y1 "200"
            , x2 "200"
            , y2 "20"
            , stroke "pink"
            , strokeWidth "10"
            , strokeLinecap "round"
            ]
            []
        , polyline
            [ points "200,40 240,40 240,80 280,80 280,120 320,120 320,160"
            , fill "none"
            , stroke "red"
            , strokeWidth "4"
            , strokeDasharray "20,2"
            ]
            []
        , text_
            [ x "130"
            , y "130"
            , fill "yellow"
            , textAnchor "middle"
            , dominantBaseline "central"
            , transform "rotate(-45 130,130)"
            ]
            [ Svg.text "Welcome to Shapes Club"
            ]
        ]
