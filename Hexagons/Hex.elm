module Hexagons.Hex (
        Hex(..),
        Direction(..), 
        
        q, 
        intQ,
        r, 
        intR,
        s, 
        intS,
        intFactory,
        floatFactory,
        toIntHex,
        toFloatHex,
        
        eq, (===), 
        noteq, (!==), 
        
        add, (+++), 
        sub, (=--), 
        mul, (***), 
        
        length,
        distance,
        
        direction,
        neighbor
    ) where

{-| We treat Cube and Axial systems separately. Cube coordinates are a plane in x,y,z space, where x+y+z = 0. Axial coordinates have two axes q,r that are 60° or 120° apart. 

See http://www.redblobgames.com/grids/hexagons/implementation.html

# Types
@docs Hex, Direction

# Helpers
@docs q, intQ, r, intR, s, intS, intFactory, floatFactory, toIntHex, toFloatHex

# Equality
@docs eq, (===), noteq, (!==)

# Coordinate arithmetic
@docs add, (+++), sub, (=--), mul, (***)

# Distance
@docs length, distance

# Neighbors
@docs direction, neighbor
-}


import Graphics.Element exposing (show)


{-| Cubic coordinates -}
type alias CubeCoords number = (number, number, number)
type alias FloatCubeCoords = CubeCoords Float
type alias IntCubeCoords = CubeCoords Int

{-| AxialCoords coordinates of an hexagon with a grid -}
type alias AxialCoords = (Int, Int)

{-| Generic hex field definition -}
type Hex
    = FloatCubeHex FloatCubeCoords
    | IntCubeHex IntCubeCoords
    | AxialHex AxialCoords
    
{-| Direction ranges from 0 to 5 by sides of the hexagon, we use North, South, West, East definitions for simplicity -}
type Direction
    = NE
    | E
    | SE
    | SW
    | W
    | NW


{-| Get q coordinate for Hex as Float value 

    q IntCubeHex (2, 3, -5) == 2.0
-}
q : Hex -> Float
q a =
    case a of
        AxialHex (a1, a2) ->
            toFloat a1
        IntCubeHex (a1, a2, a3) ->
            toFloat a1
        FloatCubeHex (a1, a2, a3) ->
            a1    
              
{-| Get q coordinate for Hex as Int value, its generally not recommended to use on FloatCubeHex

    intQ IntCubeHex (2, 3, -5) == 2
-}   
intQ : Hex -> Int
intQ a =
    case a of
        AxialHex (a1, a2) ->
            a1
        IntCubeHex (a1, a2, a3) ->
            a1
        FloatCubeHex (a1, a2, a3) ->
            floor a1
          
{-| Get r coordinate for Hex as Float value 

    r IntCubeHex (2, 3, -5) == 3.0
-}  
r : Hex -> Float
r a =
    case a of
        AxialHex (a1, a2) ->
            toFloat a2
        IntCubeHex (a1, a2, a3) ->
            toFloat a2
        FloatCubeHex (a1, a2, a3) ->
            a2
            
{-| Get r coordinate for Hex as Int value, its generally not recommended to use on FloatCubeHex

    intR IntCubeHex (2, 3, -5) == 3
-} 
intR : Hex -> Int
intR a =
    case a of
        AxialHex (a1, a2) ->
            a2
        IntCubeHex (a1, a2, a3) ->
            a2
        FloatCubeHex (a1, a2, a3) ->
            floor a2
            
{-| Get s coordinate for Hex as Float value 

    s IntCubeHex (2, 3, -5) == -5.0
-} 
s : Hex -> Float
s a =
    case a of
        AxialHex (a1, a2) ->
            toFloat (-a1 - a2)
        IntCubeHex (a1, a2, a3) ->
            toFloat a3
        FloatCubeHex (a1, a2, a3) ->
            a3
            
{-| Get s coordinate for Hex as Int value, its generally not recommended to use on FloatCubeHex

    intS IntCubeHex (2, 3, -5) == 3
-}
intS : Hex -> Int
intS a =
    case a of
        AxialHex (a1, a2) ->
            -a1 - a2
        IntCubeHex (a1, a2, a3) ->
            a3
        FloatCubeHex (a1, a2, a3) ->
            floor a3

{-| Build Hex object from Int coordinates

    intFactory (2, 3) === IntCubeHex (2, 3, -5)
-}
intFactory : (Int, Int) -> Hex
intFactory (q, r) =
    IntCubeHex (q, r, -q - r)
    
{-| Build Hex object from Float coordinates

    floatFactory (2.5, 3.5) === FloatCubeHex (2.5, 3.5, -6.0)
-}
floatFactory : (Float, Float) -> Hex
floatFactory (q, r) =
    FloatCubeHex (q, r, -q - r)
    
{-| Convert Hex to IntCubeHex coordinate systems 

    toIntHex (FloatCubeHex (2.5, 3.5, -6.0)) === IntCubeHex (2, 4, -6)
-}
toIntHex : Hex -> Hex
toIntHex hex =
    case hex of
        AxialHex (q, r) ->
            IntCubeHex (q, r, -q - r)
        IntCubeHex (q, r, s) -> 
            hex
        FloatCubeHex (q_, r_, s_) -> 
            let
                q = round q_
                r = round r_
                s = round s_
                q_diff = abs ((toFloat q) - q_)
                r_diff = abs ((toFloat r) - r_)
                s_diff = abs ((toFloat s) - s_)
            in
                if q_diff > r_diff && q_diff > r_diff then
                    IntCubeHex (-r - s, r, s)
                else if r_diff > s_diff then
                    IntCubeHex (q, -q - s, s)
                else
                    IntCubeHex (q, r, -q - r)
                    
{-| Convert Hex to FloatCubeHex coordinate systems 

    toFloatHex (IntCubeHex (2, 3, -5)) === FloatCubeHex (2.0, 3.0, -5.0)
-}
toFloatHex : Hex -> Hex
toFloatHex hex =
    case hex of
        AxialHex (q, r) ->
            let
                q_ = toFloat q
                r_ = toFloat r
            in
                FloatCubeHex (q_, r_, -q_ - r_)
        IntCubeHex (q, r, s) -> 
            FloatCubeHex (toFloat q, toFloat r, toFloat s)
        FloatCubeHex (q_, r_, s_) -> 
            hex


{-| Compare two Hex definitions, support both axial and cubic coordinates.

Not a strict comparation, FloatCubeHex is converted to IntCubeHex.

    IntCubeHex (2, 3, -5) === IntCubeHex (2, 3, -5)
    AxialHex (2, 3) === AxialHex (2, 3)
-}   
eq : Hex -> Hex -> Bool
eq a b =
    case a of
        AxialHex (a1, a2) ->
            case b of 
                AxialHex (b1, b2) -> 
                    a1 == b1 && a2 == b2
                IntCubeHex (b1, b2, b3) -> 
                    a1 == b1 && a2 == b2 && (-a1-a2) == b3
                FloatCubeHex (b1, b2, b3) -> 
                    let
                        b_ = toIntHex b
                        b1_ = intQ b_
                        b2_ = intR b_
                        b3_ = intS b_
                    in   
                        a1 == b1_ && a2 == b2_ && (-a1-a2) == b3_
        IntCubeHex (a1, a2, a3) ->
            case b of 
                AxialHex (b1, b2) -> 
                    a1 == b1 && a2 == b2 && a3 == (-b1-b2)
                IntCubeHex (b1, b2, b3) -> 
                    a1 == b1 && a2 == b2 && a3 == b3
                FloatCubeHex (b1, b2, b3) -> 
                    let
                        b_ = toIntHex b
                        b1_ = intQ b_
                        b2_ = intR b_
                        b3_ = intS b_
                    in   
                        a1 == b1_ && a2 == b2_ && a3 == b3_
        FloatCubeHex (a1, a2, a3) ->
            let
                a_ = toIntHex a
                a1_ = intQ a_
                a2_ = intR a_
                a3_ = intS a_
            in
                case b of 
                    AxialHex (b1, b2) -> 
                        a1_ == b1 && a2_ == b2 && a3_ == -b1-b2
                    IntCubeHex (b1, b2, b3) -> 
                        a1_ == b1 && a2_ == b2 && a3_ == b3
                    FloatCubeHex (b1, b2, b3) -> 
                        a1 == b1 && a2 == b2 && a3 == b3
             
{-| Infixr operator for `eq` -}
(===) : Hex -> Hex -> Bool
(===) = 
    eq
infixr 1 ===
    
{-| Compare two Hex definitions, if they are not equal, inversion of `eq` 

    IntCubeHex (2, 3, -5) !== IntCubeHex (1, 1, -2)
    AxialHex (2, 3) !== AxialHex (2, 1)
-}
noteq : Hex -> Hex -> Bool
noteq a b =
    not (a === b)
    
{-| Infixr operator for `noteq` -}
(!==) : Hex -> Hex -> Bool
(!==) =
    noteq
infixr 1 !==    
    
    
{-| Since cube coordinates come from 3d cartesian coordinates, I automatically get things like addition, subtraction, multiplication, and division. For example, you can have Hex(2, 0, -2) that represents two steps northeast, and add that to location Hex(3, -5, 2) the obvious way: Hex(2 + 3, 0 + -5, -2 + -2). With other coordinate systems like offset coordinates, you can’t do that and get what you want. These operations are just what you’d implement with 3d cartesian vectors, but I am using q, r, s names in this class instead of x, y, z

    IntCubeHex (2, 3, -5) +++ IntCubeHex (1, 2, -3) === IntCubeHex (3, 5, -8)
-}
add : Hex -> Hex -> Hex
add a b =
    case a of
        AxialHex (a1, a2) ->
            case b of 
                AxialHex (b1, b2) -> 
                    IntCubeHex (a1 + b1, a2 + b2, (-a1-a2) + (-b1-b2))
                IntCubeHex (b1, b2, b3) -> 
                    IntCubeHex (a1 + b1, a2 + b2, (-a1-a2) + b3)
                FloatCubeHex (b1, b2, b3) -> 
                    let
                        a1_ = toFloat a1
                        a2_ = toFloat a2
                        a3_ = -a1_-a2_
                    in
                        FloatCubeHex (a1_ + b1, a2_ + b2, a3_ + b3)
        IntCubeHex (a1, a2, a3) ->
            case b of 
                AxialHex (b1, b2) -> 
                    IntCubeHex (a1 + b1, a2 + b2, a3 + (-b1-b2))
                IntCubeHex (b1, b2, b3) -> 
                    IntCubeHex (a1 + b1, a2 + b2, a3 + b3)
                FloatCubeHex (b1, b2, b3) -> 
                    let
                        a1_ = toFloat a1
                        a2_ = toFloat a2
                        a3_ = toFloat a3
                    in
                        FloatCubeHex (a1_ + b1, a2_ + b2, a3_ + b3)
        FloatCubeHex (a1, a2, a3) ->
            case b of 
                AxialHex (b1, b2) -> 
                    let
                        b1_ = toFloat b1
                        b2_ = toFloat b2
                        b3_ = -b1_-b2_
                    in
                        FloatCubeHex (a1 + b1_, a2 + b2_, a3 + b3_)
                IntCubeHex (b1, b2, b3) -> 
                    let
                        b1_ = toFloat b1
                        b2_ = toFloat b2
                        b3_ = toFloat b3
                    in
                        FloatCubeHex (a1 + b1_, a2 + b2_, a3 + b3_)
                FloatCubeHex (b1, b2, b3) -> 
                    FloatCubeHex (a1 + b1, a2 + b2, a3 + b3)

{-| Infixr operator for `sum` -}
(+++) : Hex -> Hex -> Hex
(+++) =
    add
infixr 4 +++

{-| Subtraction of Hexes, more info in `sum` description

    (IntCubeHex (2, 3, -5) =-- IntCubeHex (1, 2, -3)) === IntCubeHex (1, 1, -2)
-}
sub : Hex -> Hex -> Hex
sub a b =
    case a of
        AxialHex (a1, a2) ->
            case b of 
                AxialHex (b1, b2) -> 
                    IntCubeHex (a1 - b1, a2 - b2, (-a1-a2) - (-b1-b2))
                IntCubeHex (b1, b2, b3) -> 
                    IntCubeHex (a1 - b1, a2 - b2, (-a1-a2) - b3)
                FloatCubeHex (b1, b2, b3) -> 
                    let
                        a1_ = toFloat a1
                        a2_ = toFloat a2
                        a3_ = -a1_-a2_
                    in
                        FloatCubeHex (a1_ - b1, a2_ - b2, a3_ - b3)
        IntCubeHex (a1, a2, a3) ->
            case b of 
                AxialHex (b1, b2) -> 
                    IntCubeHex (a1 - b1, a2 - b2, a3 - (-b1-b2))
                IntCubeHex (b1, b2, b3) -> 
                    IntCubeHex (a1 - b1, a2 - b2, a3 - b3)
                FloatCubeHex (b1, b2, b3) -> 
                    let
                        a1_ = toFloat a1
                        a2_ = toFloat a2
                        a3_ = toFloat a3
                    in
                        FloatCubeHex (a1_ - b1, a2_ - b2, a3_ - b3)
        FloatCubeHex (a1, a2, a3) ->
            case b of 
                AxialHex (b1, b2) -> 
                    let
                        b1_ = toFloat b1
                        b2_ = toFloat b2
                        b3_ = -b1_-b2_
                    in
                        FloatCubeHex (a1 - b1_, a2 - b2_, a3 - b3_)
                IntCubeHex (b1, b2, b3) -> 
                    let
                        b1_ = toFloat b1
                        b2_ = toFloat b2
                        b3_ = toFloat b3
                    in
                        FloatCubeHex (a1 - b1_, a2 - b2_, a3 - b3_)
                FloatCubeHex (b1, b2, b3) -> 
                    FloatCubeHex (a1 - b1, a2 - b2, a3 - b3)    

{-| Infixr operator for `sub` -}
(=--) : Hex -> Hex -> Hex
(=--) =
    sub
infixr 4 =--

{-| Multiplication of Hexes, more info in `sum` description

    5 *** (IntCubeHex (2, 3, -5) === IntCubeHex (10, 15, -25))
-}
mul : Int -> Hex -> Hex
mul k a =
    case a of
        AxialHex (a1, a2) ->
            AxialHex (a1 * k, a2 * k)
        IntCubeHex (a1, a2, a3) ->
            IntCubeHex (a1 * k, a2 * k, a3 * k)
        FloatCubeHex (a1, a2, a3) ->
            let
                k_ = toFloat k
            in
                FloatCubeHex (a1 * k_, a2 * k_, a3 * k_)   

{-| Infixr operator for `mul` -}
(***) : Int -> Hex -> Hex
(***) =
    mul
infixr 5 *** 


{-| Length of Hex.

    length (IntCubeHex (2, 3, -5)) == 5
    length (FloatCubeHex (2.2, 3.3, -5.5)) == 5
    
-}
length : Hex -> Int
length a = 
    let 
        a1 = abs << q <| a
        a2 = abs << r <| a
        a3 = abs << s <| a
    in
        floor <| (a1 + a2 + a3) / 2

{-| The distance between two hexes is the length of the line between them. 

    distance (IntCubeHex (2, 3, -5)) (FloatCubeHex (3.2, 4.3, -7.5)) == 2
-}
distance : Hex -> Hex -> Int
distance a b =
    length (a =-- b)
    
    
{-| Direction relative to Hex polygon lines, we used shortcuts for the mix of North, East, South, West directions -}
direction : Direction -> Hex
direction dir =
    case dir of
        NE -> IntCubeHex (1, 0, -1)
        E -> IntCubeHex (1, -1, 0)
        SE -> IntCubeHex (0, -1, 1)
        SW -> IntCubeHex (-1, 0, 1)
        W -> IntCubeHex (-1, 1, 0)
        NW -> IntCubeHex (0, 1, -1)
        
{-| With distance, we defined two functions: length works on one argument and distance works with two. The same is true with neighbors. The direction function is with one argument and the neighbor function is with two.

    neighbor (IntCubeHex (2, 3, -5)) NW === IntCubeHex (2, 4, -6)
-}
neighbor : Hex -> Direction -> Hex
neighbor hex dir =
    hex +++ (direction dir)