module LayoutTest exposing (layout)

import Dict exposing (Dict)
import Expect
import Hexagons.Hex exposing (Direction(..), Hex(..), eq)
import Hexagons.Layout exposing (Layout, Orientation, Point)
import Hexagons.Map exposing (Map)
import String
import Test exposing (..)


layout : Test
layout =
    let
        testLayoutPointy =
            { orientation = Hexagons.Layout.orientationLayoutPointy
            , size = ( 10.0, 10.0 )
            , origin = ( 0.0, 0.0 )
            }
        testLayoutFlat =
            { orientation = Hexagons.Layout.orientationLayoutFlat
            , size = ( 10.0, 10.0 )
            , origin = ( 0.0, 0.0 )
            }
        isEq arg1 arg2 =
            Expect.true "Expect that Hexes are equal" <|
                eq arg1 arg2
    in
    describe "Test Hexagons.Layout module"
        [ test "Turn Hex coordinates into a Point location on a pointy Layout" <|
            \_ ->
                Hexagons.Layout.hexToPoint testLayoutPointy (IntCubeHex ( 2, 3, -5 ))
                    |> Expect.equal ( 60.62, 45.0 )
        , test "Turn Point location on a pointy Layout into a Hex coordinates" <|
            \_ ->
                (Hexagons.Hex.toIntHex <| Hexagons.Layout.pointToHex testLayoutPointy ( 60.62, 45.0 ))
                    |> isEq (IntCubeHex ( 2, 3, -5 ))
        , test "Turn Hex coordinates into a Point location on a flat Layout" <|
            \_ ->
                Hexagons.Layout.hexToPoint testLayoutFlat (IntCubeHex ( 2, 3, -5 ))
                    |> Expect.equal ( 30.0, 69.28 )
        , test "Turn Point location on a flat Layout into a Hex coordinates" <|
            \_ ->
                (Hexagons.Hex.toIntHex <| Hexagons.Layout.pointToHex testLayoutFlat ( 30.0, 69.28 ))
                    |> isEq (IntCubeHex ( 2, 3, -5 ))
        , test "Turn offset cordinates to a Hex coordinates" <|
            \_ ->
                Hexagons.Layout.offsetToHex ( 2, 2 )
                    |> isEq (IntCubeHex ( 1, 2, -3 ))
        , test "Turn Hex cordinates to an offset ones " <|
            \_ ->
                Hexagons.Layout.hexToOffset (IntCubeHex ( 2, 2, -4 ))
                    |> Expect.equal ( 3, 2 )
        , test "Calculate polygon corner positions for a Hex location on a pointy Layout" <|
            \_ ->
                Hexagons.Layout.polygonCorners testLayoutPointy (IntCubeHex ( 2, 3, -5 ))
                    |> Expect.equal [ ( 69.28, 50 ), ( 60.62, 55 ), ( 51.96, 50 ), ( 51.96, 40 ), ( 60.62, 35 ), ( 69.28, 40 ) ]
        , test "Calculate polygon corner positions for a Hex location on a flat Layout" <|
            \_ ->
                Hexagons.Layout.polygonCorners testLayoutFlat (IntCubeHex ( 2, 3, -5 ))
                    |> Expect.equal [ ( 40, 69.28 ), ( 35, 77.94 ), ( 25, 77.94 ), ( 20, 69.28 ), ( 25, 60.62 ), ( 35, 60.62 ) ]
        , test "Draw a line between 2 Hexes returning a list of Hex connections" <|
            \_ ->
                Hexagons.Layout.drawLine (IntCubeHex ( 2, 3, -5 )) (IntCubeHex ( 3, 3, -6 ))
                    |> Expect.equal [ IntCubeHex ( 2, 3, -5 ), IntCubeHex ( 3, 3, -6 ) ]
        , test "Draw a circle with the given radius around the Hex" <|
            \_ ->
                (List.length <| Hexagons.Layout.drawCircle (IntCubeHex ( 2, 3, -5 )) 2)
                    |> Expect.equal 19
        ]
