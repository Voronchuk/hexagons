module MapTest exposing (map)

import Dict exposing (Dict)
import Expect
import Hexagons.Hex exposing (Hex(..))
import Hexagons.Map exposing (Map)
import String
import Test exposing (..)


map : Test
map =
    describe "Test Hexagons.Map module"
        [ test "Generation of rectangular map with pointy hexagons" <|
            \_ ->
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
                    |> Dict.fromList
                    |> Dict.diff (Hexagons.Map.rectangularPointyTopMap 2 2)
                    |> Dict.isEmpty
                    |> Expect.true "Expect that generated map is the same as sample"
        , test "Generation of rectangular map with flat-top hexagons" <|
            \_ ->
                [ ( ( 0, 0, 0 ), IntCubeHex ( 0, 0, 0 ) )
                , ( ( 0, 1, -1 ), IntCubeHex ( 0, 1, -1 ) )
                , ( ( 0, 2, -2 ), IntCubeHex ( 0, 2, -2 ) )
                , ( ( 1, 0, -1 ), IntCubeHex ( 1, 0, -1 ) )
                , ( ( 1, 1, -2 ), IntCubeHex ( 1, 1, -2 ) )
                , ( ( 1, 2, -3 ), IntCubeHex ( 1, 2, -3 ) )
                , ( ( 2, -1, -1 ), IntCubeHex ( 2, -1, -1 ) )
                , ( ( 2, 0, -2 ), IntCubeHex ( 2, 0, -2 ) )
                , ( ( 2, 1, -3 ), IntCubeHex ( 2, 1, -3 ) )
                ]
                    |> Dict.fromList
                    |> Dict.diff (Hexagons.Map.rectangularFlatTopMap 2 2)
                    |> Dict.isEmpty
                    |> Expect.true "Expect that generated map is the same as sample"
        ]
