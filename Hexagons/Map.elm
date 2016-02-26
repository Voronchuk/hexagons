module Hexagons.Map (
        Map,
        
        rectangularPointyTopMap
    ) where

{-| This module solves the problem of generating and storing the Map data. We are using Elm dictionary as the Map storage engine with Hex coordinate tuple as the key.

See http://www.redblobgames.com/grids/hexagons/implementation.html for reference.

# Types
@docs Map

# Map generators
@docs rectangularPointyTopMap

-}


import Maybe
import Dict
import Dict exposing (Dict)
import List
import Hexagons.Hex
import Hexagons.Hex exposing (Hex)


{-| Dictionary storage to keep map of hexes -}
type alias Map = Dict (Int, Int, Int) Hex


{-| Hash function to get a uniform token to address stored hex Hex -}
hashHex : Hex -> (Int, Int, Int)
hashHex hex = 
    let
        hex_ = Hexagons.Hex.toIntHex hex
        q = Hexagons.Hex.intQ hex_
        r = Hexagons.Hex.intR hex_
        s = Hexagons.Hex.intS hex_
    in
        (q, r, s)


{-| Generate Map of rectangular shape given its height and width -}
rectangularPointyTopMap: Int -> Int -> Map
rectangularPointyTopMap height width = 
    let 
        widthLine = [0..width]
        heightLine = [0..height]
        
        createHex : Int -> Int -> Hex
        createHex r q = Hexagons.Hex.intFactory (q, r)
        
        offsetWidth : Int -> Int -> Int
        offsetWidth r q = q - (r // 2)
        
        widthRowLine : Int -> List Hex
        widthRowLine r = 
            List.map (createHex r)
            <| List.map (offsetWidth r) widthLine
            
        allLines : List Hex
        allLines =
            List.concat
            <| List.map widthRowLine heightLine
            
        makeDictRecord : Hex -> ((Int, Int, Int), Hex)
        makeDictRecord hex =
            ((hashHex hex), hex)
    in
        Dict.fromList <| List.map makeDictRecord allLines