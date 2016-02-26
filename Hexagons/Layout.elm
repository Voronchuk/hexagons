module Hexagons.Layout (
        Point,
        Orientation,
        Layout,
        
        orientationLayoutPointy,
        
        hexToPoint,
        pointToHex,
        
        polygonCorners,
        
        drawLine
    ) where

{-| The next major piece of functionality we need is a way to convert between hex coordinates and screen coordinates. There’s a pointy top layout and a flat top hex layout. The conversion uses a matrix as well as the inverse of the matrix, so we need a way to store those. Also, for drawing the corners, pointy top starts at 30° and flat top starts at 0°, so we need a place to store that too.

See http://www.redblobgames.com/grids/hexagons/implementation.html for reference.

# Types
@docs Point, Orientation, Layout

# Contants
@docs orientationLayoutPointy

# Hex to point and point to hex conversions
@docs hexToPoint, pointToHex

# Hex corner coordinates
@docs polygonCorners

# Drawing
@docs drawLine
-}


import List
import Hexagons.Hex
import Hexagons.Hex exposing (Hex)


{-| Point on screen (pixel) -}
type alias Point = (Float, Float)

{-| Orientation helper type to store these: the 2×2 forward matrix, the 2×2 inverse matrix, and the starting angle -}
type alias Orientation = 
    {
        forward_matrix : (Float, Float, Float, Float),
        inverse_matrix : (Float, Float, Float, Float),
        start_angle : Float
    }
    
{-| Composite layout definition -}
type alias Layout = 
    {
        orientation : Orientation,
        size : Point,
        origin : Point
    }
  
    
{-| Round Float number to some division -}
precision : Int -> Float -> Float
precision division number =
    let
        k = toFloat <| 10 ^ division
    in
        ((toFloat << round) (number * k)) / k
    
{-| Contant definition of pointy hexagon orientation -}
orientationLayoutPointy : Orientation
orientationLayoutPointy =
    {
        forward_matrix = (sqrt 3.0, (sqrt 3.0) / 2.0, 0.0, 3.0 / 2.0),
        inverse_matrix = ((sqrt 3.0) / 3.0, -1.0 / 3.0, 0.0, 2.0 / 3.0),
        start_angle = 0.5
    }
    
    
{-| Turn Hex coordinates into a Point location on a Layout -}
hexToPoint : Layout -> Hex -> Point
hexToPoint layout hex =
    let
        (f0, f1, f2, f3) = layout.orientation.forward_matrix
        (xl, yl) = layout.size
        (xo, yo) = layout.origin
        q = Hexagons.Hex.q hex
        r = Hexagons.Hex.r hex
        x = precision 2 <| (((f0 * q) + (f1 * r)) * xl) + xo
        y = precision 2 <| (((f2 * q) + (f3 * r)) * yl) + yo
    in
        (x, y)
      
{-| Turn Point coordinates on a Layout into a Hex coordinates -}
pointToHex : Layout -> Point -> Hex
pointToHex layout point =
    let 
        (b0, b1, b2, b3) = layout.orientation.inverse_matrix
        (xl, yl) = layout.size
        (xo, yo) = layout.origin
        (x, y) = point
        x1 = (x - xo) / xl
        y1 = (y - yo) / yl
        q = (b0 * x1) + (b1 * y1) 
        r = (b2 * x1) + (b3 * y1)
    in
        Hexagons.Hex.floatFactory (q, r)
        
        
{-| Calculate corner offset from a center of the Hex -}
hexCornerOffset : Layout -> Int -> Point
hexCornerOffset layout corner = 
    let
        (xl, yl) = layout.size
        startAngle = layout.orientation.start_angle
        angle = ((2.0 * pi) * ((toFloat corner) + startAngle)) / 6
        x = precision 2 <| xl * (cos angle)
        y = precision 2 <| yl * (sin angle)
    in
        (x, y)

{-| Once we know where the corners are relative to the center, we can calculate the corners in screen locations by adding the center to each corner, and putting the coordinates into a list. -}
polygonCorners : Layout -> Hex -> List Point
polygonCorners layout hex =
    let
        center = hexToPoint layout hex
    in
        List.map (hexCornerOffset layout) [0..5]
        
        
{-| Linear interpolation of hexes -}
hexLerp : Hex -> Hex -> Float -> Hex
hexLerp a b t =
    let
        a_ = Hexagons.Hex.toFloatHex a
        b_ = Hexagons.Hex.toFloatHex b
        q1 = Hexagons.Hex.q a_
        q2 = Hexagons.Hex.q b_
        r1 = Hexagons.Hex.r a_
        r2 = Hexagons.Hex.r b_
        q = q1 + ((q2 - q1) * t)
        r = r1 + ((r2 - r1) * t)
    in
        Hexagons.Hex.floatFactory (q, r)
       
{-| Drawing the line between hexes using the linear interpolation -} 
drawLine : Hex -> Hex -> List Hex
drawLine a b =
    let
        n = toFloat <| Hexagons.Hex.distance a b
        step = 1.0 / (max n 1.0)
        steps = List.map ((+) step) [0.0..n]
    in
        List.map (Hexagons.Hex.toIntHex << (hexLerp a b)) steps