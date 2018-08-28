module Main


import ElmTest exposing (..)
import Console exposing (..)
import Task
import List

import Hexagons.Tests


allTests : List Test
allTests = 
    [
        Hexagons.Tests.hex,
        Hexagons.Tests.layout,
        Hexagons.Tests.map
    ]


port runner : Signal (Task.Task x ())
port runner = 
    run 
        <| putStrLn 
        <| List.foldr (++) "" 
        <| List.map stringRunner allTests
