module Hexagons.Map exposing
    ( Map, Hash
    , hashHex
    , getHex
    , rectangularPointyTopMap
    , rectangularFlatTopMap
    )

{-| This module solves the problem of generating and storing the Map data. We are using Elm dictionary as the Map storage engine with Hex coordinate tuple as the key.

See <http://www.redblobgames.com/grids/hexagons/implementation.html> for reference.


# Types

@docs Map, Hash


# Accessing map storage

@docs hashHex
@docs getHex


# Map generators

@docs rectangularPointyTopMap, rectangularFlatTopMap

-}

import Dict exposing (Dict)
import Hexagons.Hex exposing (Hex)
import List
import Maybe


{-| Dictionary storage to keep map of hexes
-}
type alias Map =
    Dict Hash Hex


{-| Hash key to access Map cell
-}
type alias Hash =
    ( Int, Int, Int )


{-| Hash function to get a uniform token to address stored hex Hex
-}
hashHex : Hex -> ( Int, Int, Int )
hashHex hex =
    let
        hex_ =
            Hexagons.Hex.toIntHex hex

        q =
            Hexagons.Hex.intQ hex_

        r =
            Hexagons.Hex.intR hex_

        s =
            Hexagons.Hex.intS hex_
    in
    ( q, r, s )


{-| Fetch hex from map storage, using a default value in case of missing Hex
-}
getHex : Hex -> Map -> Hash -> Hex
getHex defaultHex map key =
    Maybe.withDefault defaultHex (Dict.get key map)


{-| Generate Map of rectangular shape given its height and width
-}
rectangularPointyTopMap : Int -> Int -> Map
rectangularPointyTopMap height width =
    let
        widthLine =
            List.range 0 width

        heightLine =
            List.range 0 height

        createHex : Int -> Int -> Hex
        createHex r q =
            Hexagons.Hex.intFactory ( q, r )

        offsetWidth : Int -> Int -> Int
        offsetWidth r q =
            q - (r // 2)

        widthRowLine : Int -> List Hex
        widthRowLine r =
            List.map (createHex r) <|
                List.map (offsetWidth r) widthLine

        allLines : List Hex
        allLines =
            List.concat <|
                List.map widthRowLine heightLine

        makeDictRecord : Hex -> ( Hash, Hex )
        makeDictRecord hex =
            ( hashHex hex, hex )
    in
    Dict.fromList <| List.map makeDictRecord allLines

{-| Generate Map of rectangular shape given its height and width
-}
rectangularFlatTopMap : Int -> Int -> Map
rectangularFlatTopMap height width =
    let
        widthLine =
            List.range 0 width

        heightLine =
            List.range 0 height

        createHex : Int -> Int -> Hex
        createHex q r =
            Hexagons.Hex.intFactory ( q, r )

        offsetHeight : Int -> Int -> Int
        offsetHeight q r =
            r - (q // 2)

        widthColumnLine : Int -> List Hex
        widthColumnLine q =
            List.map (createHex q) <|
                List.map (offsetHeight q) heightLine

        allLines : List Hex
        allLines =
            List.concat <|
                List.map widthColumnLine widthLine

        makeDictRecord : Hex -> ( Hash, Hex )
        makeDictRecord hex =
            ( hashHex hex, hex )
    in
    Dict.fromList <| List.map makeDictRecord allLines
