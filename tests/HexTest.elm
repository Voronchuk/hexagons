module HexTest exposing (hex)

import Dict exposing (Dict)
import Expect
import Hexagons.Hex exposing (Direction(..), Hex(..), add, eq, mul, noteq, sub)
import Hexagons.Layout exposing (Layout, Orientation, Point)
import Hexagons.Map exposing (Map)
import String
import Test exposing (..)


hex : Test
hex =
    let
        isEq arg1 arg2 =
            Expect.true "Expect that Hexes are equal" <|
                eq arg1 arg2
    in
    describe "Test Hexagons.Hex module"
        [ test "Generate Hex from Int pair" <|
            \_ ->
                Hexagons.Hex.intFactory ( 2, 3 )
                    |> isEq (IntCubeHex ( 2, 3, -5 ))
        , test "Generate Hex from Floor pair" <|
            \_ ->
                Hexagons.Hex.floatFactory ( 4.0, 5.0 )
                    |> isEq (FloatCubeHex ( 4.0, 5.0, -9.0 ))
        , test "Compare equal Hexes" <|
            \_ ->
                IntCubeHex ( 2, 3, -5 )
                    |> isEq (IntCubeHex ( 2, 3, -5 ))
        , test "Compare not equal Hexes" <|
            \_ ->
                FloatCubeHex ( 2.0, 3.0, -5.0 )
                    |> noteq (FloatCubeHex ( 2.1, 3.1, -5.2 ))
                    |> Expect.true "Not equal Hexes"
        , test "Convert between FloatCubeHex and IntCubeHex" <|
            \_ ->
                Hexagons.Hex.toIntHex (FloatCubeHex ( 4.7, 5.8, -10.5 ))
                    |> isEq (IntCubeHex ( 4, 6, -10 ))
        , test "Convert between IntCubeHex and FloatCubeHex" <|
            \_ ->
                Hexagons.Hex.toFloatHex (IntCubeHex ( 2, 3, -5 ))
                    |> isEq (FloatCubeHex ( 2.0, 3.0, -5.0 ))
        , test "Get q Float coordinate from Hex" <|
            \_ ->
                (Hexagons.Hex.q << Hexagons.Hex.intFactory) ( 2, 3 )
                    |> Expect.equal 2.0
        , test "Get r Float coordinate from Hex" <|
            \_ ->
                (Hexagons.Hex.r << Hexagons.Hex.intFactory) ( 2, 3 )
                    |> Expect.equal 3.0
        , test "Get s Float coordinate from Hex" <|
            \_ ->
                (Hexagons.Hex.s << Hexagons.Hex.intFactory) ( 2, 3 )
                    |> Expect.equal -5.0
        , test "Get q Int coordinate from Hex" <|
            \_ ->
                (Hexagons.Hex.intQ << Hexagons.Hex.intFactory) ( 2, 3 )
                    |> Expect.equal 2
        , test "Get r Int coordinate from Hex" <|
            \_ ->
                (Hexagons.Hex.intR << Hexagons.Hex.intFactory) ( 2, 3 )
                    |> Expect.equal 3
        , test "Get s Int coordinate from Hex" <|
            \_ ->
                (Hexagons.Hex.intS << Hexagons.Hex.intFactory) ( 2, 3 )
                    |> Expect.equal -5
        , test "Addition of Hexes" <|
            \_ ->
                add (IntCubeHex ( 2, 3, -5 )) (IntCubeHex ( 1, 2, -3 ))
                    |> isEq (IntCubeHex ( 3, 5, -8 ))
        , test "Subtraction of Hexes" <|
            \_ ->
                sub (IntCubeHex ( 2, 3, -5 )) (IntCubeHex ( 1, 2, -3 ))
                    |> isEq (IntCubeHex ( 1, 1, -2 ))
        , test "Multiplication of Hexes" <|
            \_ ->
                mul 5 (IntCubeHex ( 2, 3, -5 ))
                    |> isEq (IntCubeHex ( 10, 15, -25 ))
        , test "Length of Int Hex" <|
            \_ ->
                Hexagons.Hex.length (IntCubeHex ( 2, 3, -5 ))
                    |> Expect.equal 5
        , test "Length of Float Hex" <|
            \_ ->
                Hexagons.Hex.length (FloatCubeHex ( 2.2, 3.3, -5.5 ))
                    |> Expect.equal 5
        , test "Distance between Hexes" <|
            \_ ->
                Hexagons.Hex.distance (IntCubeHex ( 2, 3, -5 )) (FloatCubeHex ( 3.2, 4.3, -7.5 ))
                    |> Expect.equal 2
        , test "Get neighboring Hex by Direction" <|
            \_ ->
                Hexagons.Hex.neighbor (IntCubeHex ( 2, 3, -5 )) NW
                    |> isEq (IntCubeHex ( 2, 4, -6 ))
        ]
