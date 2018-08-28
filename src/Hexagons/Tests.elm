module Hexagons.Tests exposing (hex, layout, map)

import Dict exposing (Dict)
import ElmTest exposing (..)
import Hexagons.Hex exposing ((!==), (***), (+++), (=--), (===), Direction(..), Hex(..))
import Hexagons.Layout exposing (Layout, Orientation, Point)
import Hexagons.Map exposing (Map)
import String


hex : Test
hex =
    suite "Test Hexagons.Hex module"
        [ test "Generate Hex from Int pair" <|
            assert <|
                Hexagons.Hex.intFactory ( 2, 3 )
                    === IntCubeHex ( 2, 3, -5 )
        , test "Generate Hex from Floor pair" <|
            assert <|
                Hexagons.Hex.floatFactory ( 4.0, 5.0 )
                    === FloatCubeHex ( 4.0, 5.0, -9.0 )
        , test "Compare equal Hexes" <|
            assert <|
                IntCubeHex ( 2, 3, -5 )
                    === IntCubeHex ( 2, 3, -5 )
        , test "Compare not equal Hexes" <|
            assert <|
                FloatCubeHex ( 2.0, 3.0, -5.0 )
                    !== FloatCubeHex ( 2.1, 3.1, -5.2 )
        , test "Convert between FloatCubeHex and IntCubeHex" <|
            assert <|
                Hexagons.Hex.toIntHex (FloatCubeHex ( 4.7, 5.8, -10.5 ))
                    === IntCubeHex ( 4, 6, -10 )
        , test "Convert between IntCubeHex and FloatCubeHex" <|
            assert <|
                Hexagons.Hex.toFloatHex (IntCubeHex ( 2, 3, -5 ))
                    === FloatCubeHex ( 2.0, 3.0, -5.0 )
        , test "Get q Float coordinate from Hex" <|
            assert <|
                (Hexagons.Hex.q << Hexagons.Hex.intFactory) ( 2, 3 )
                    == 2.0
        , test "Get r Float coordinate from Hex" <|
            assert <|
                (Hexagons.Hex.r << Hexagons.Hex.intFactory) ( 2, 3 )
                    == 3.0
        , test "Get s Float coordinate from Hex" <|
            assert <|
                (Hexagons.Hex.s << Hexagons.Hex.intFactory) ( 2, 3 )
                    == -5.0
        , test "Get q Int coordinate from Hex" <|
            assert <|
                (Hexagons.Hex.intQ << Hexagons.Hex.intFactory) ( 2, 3 )
                    == 2
        , test "Get r Int coordinate from Hex" <|
            assert <|
                (Hexagons.Hex.intR << Hexagons.Hex.intFactory) ( 2, 3 )
                    == 3
        , test "Get s Int coordinate from Hex" <|
            assert <|
                (Hexagons.Hex.intS << Hexagons.Hex.intFactory) ( 2, 3 )
                    == -5
        , test "Get s Int coordinate from Hex" <|
            assert <|
                (Hexagons.Hex.intS << Hexagons.Hex.intFactory) ( 2, 3 )
                    == -5
        , test "Addition of Hexes" <|
            assert <|
                IntCubeHex ( 2, 3, -5 )
                    +++ IntCubeHex ( 1, 2, -3 )
                    === IntCubeHex ( 3, 5, -8 )
        , test "Subtraction of Hexes" <|
            assert <|
                IntCubeHex ( 2, 3, -5 )
                    =-- IntCubeHex ( 1, 2, -3 )
                    === IntCubeHex ( 1, 1, -2 )
        , test "Multiplication of Hexes" <|
            assert <|
                5
                    *** IntCubeHex ( 2, 3, -5 )
                    === IntCubeHex ( 10, 15, -25 )
        , test "Length of Int Hex" <|
            assert <|
                Hexagons.Hex.length (IntCubeHex ( 2, 3, -5 ))
                    == 5
        , test "Length of Float Hex" <|
            assert <|
                Hexagons.Hex.length (FloatCubeHex ( 2.2, 3.3, -5.5 ))
                    == 5
        , test "Distance between Hexes" <|
            assert <|
                Hexagons.Hex.distance (IntCubeHex ( 2, 3, -5 )) (FloatCubeHex ( 3.2, 4.3, -7.5 ))
                    == 2
        , test "Get neighboring Hex by Direction" <|
            assert <|
                Hexagons.Hex.neighbor (IntCubeHex ( 2, 3, -5 )) NW
                    === IntCubeHex ( 2, 4, -6 )
        ]


layout : Test
layout =
    let
        testLayout =
            { orientation = Hexagons.Layout.orientationLayoutPointy
            , size = ( 10.0, 10.0 )
            , origin = ( 0.0, 0.0 )
            }
    in
    suite "Test Hexagons.Layout module"
        [ test "Turn Hex coordinates into a Point location on a Layout" <|
            assert <|
                Hexagons.Layout.hexToPoint testLayout (IntCubeHex ( 2, 3, -5 ))
                    == ( 60.62, 45.0 )
        , test "Turn Point location on a Layout into a Hex coordinates" <|
            assert <|
                (Hexagons.Hex.toIntHex <| Hexagons.Layout.pointToHex testLayout ( 60.62, 45.0 ))
                    === IntCubeHex ( 2, 3, -5 )
        , test "Turn offset cordinates to a Hex coordinates" <|
            assert <|
                Hexagons.Layout.offsetToHex ( 2, 2 )
                    === IntCubeHex ( 1, 2, -3 )
        , test "Turn Hex cordinates to an offset ones " <|
            assert <|
                Hexagons.Layout.hexToOffset (IntCubeHex ( 2, 2, -4 ))
                    == ( 3, 2 )
        , test "Calculate polygon corner positions for a Hex location on a Layout" <|
            assert <|
                Hexagons.Layout.polygonCorners testLayout (IntCubeHex ( 2, 3, -5 ))
                    == [ ( 69.28, 50 ), ( 60.62, 55 ), ( 51.96, 50 ), ( 51.96, 40 ), ( 60.62, 35 ), ( 69.28, 40 ) ]
        , test "Draw a line between 2 Hexes returning a list of Hex connections" <|
            assert <|
                Hexagons.Layout.drawLine (IntCubeHex ( 2, 3, -5 )) (IntCubeHex ( 3, 3, -5 ))
                    == [ IntCubeHex ( 3, 3, -6 ) ]
        , test "Draw a circle with the given radius around the Hex" <|
            assert <|
                (List.length <| Hexagons.Layout.drawCircle (IntCubeHex ( 2, 3, -5 )) 2)
                    == 19
        ]


map : Test
map =
    suite "Test Hexagons.Map module"
        [ test "Generation of rectangular map with pointy hexagons" <|
            assert <|
                Dict.isEmpty <|
                    Dict.diff (Hexagons.Map.rectangularPointyTopMap 2 2)
                        (Dict.fromList
                            [ ( ( -1, 2, -1 ), IntCubeHex ( -1, 2, -1 ) )
                            , ( ( 0, 0, 0 ), IntCubeHex ( 0, 0, 0 ) )
                            , ( ( 0, 1, -1 ), IntCubeHex ( 0, 1, -1 ) )
                            , ( ( 0, 2, -2 ), IntCubeHex ( 0, 2, -2 ) )
                            , ( ( 1, 0, -1 ), IntCubeHex ( 1, 0, -1 ) )
                            , ( ( 1, 1, -2 ), IntCubeHex ( 1, 1, -2 ) )
                            , ( ( 1, 2, -3 ), IntCubeHex ( 1, 2, -3 ) )
                            , ( ( 2, 0, -2 ), IntCubeHex ( 2, 0, -2 ) )
                            , ( ( 2, 1, -3 ), IntCubeHex ( 2, 1, -3 ) )
                            ]
                        )
        ]
