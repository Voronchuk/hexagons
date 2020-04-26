module Hexagons.Layout exposing
    ( Point, Orientation, Layout
    , orientationLayoutPointy
    , orientationLayoutFlat
    , hexToPoint, pointToHex
    , hexToOffset, offsetToHex
    , polygonCorners
    , drawLine, drawCircle
    )

{-| The next major piece of functionality we need is a way to convert between hex coordinates and screen coordinates. There’s a pointy top layout and a flat top hex layout. The conversion uses a matrix as well as the inverse of the matrix, so we need a way to store those. Also, for drawing the corners, pointy top starts at 30° and flat top starts at 0°, so we need a place to store that too.

See <http://www.redblobgames.com/grids/hexagons/implementation.html> for reference.


# Types

@docs Point, Orientation, Layout


# Constants

@docs orientationLayoutPointy, orientationLayoutFlat


# Hex to point and point to hex conversions

@docs hexToPoint, pointToHex


# Hex to offset and offset to hex coordinate conversions

@docs hexToOffset, offsetToHex


# Hex corner coordinates

@docs polygonCorners


# Drawing

@docs drawLine, drawCircle

-}

import Debug
import Hexagons.Hex exposing (Hex(..), add)
import List


{-| Point on screen (pixel)
-}
type alias Point =
    ( Float, Float )


{-| Offset coordinates of Hex as row and column
-}
type alias OffsetCoord =
    ( Int, Int )


{-| 2x2 matrix, by x and y coordinates
-}
type alias Square2Matrix =
    { f0 : Float
    , f1 : Float
    , f2 : Float
    , f3 : Float
    }


{-| Orientation helper type to store these: the 2×2 forward matrix, the 2×2 inverse matrix, and the starting angle
-}
type alias Orientation =
    { forward_matrix : Square2Matrix
    , inverse_matrix : Square2Matrix
    , start_angle : Float
    }


{-| Composite layout definition
-}
type alias Layout =
    { orientation : Orientation
    , size : Point
    , origin : Point
    }


{-| Round Float number to some division
-}
precision : Int -> Float -> Float
precision division number =
    let
        k =
            toFloat <| 10 ^ division
    in
    (toFloat << round) (number * k) / k


{-| Constant definition of pointy hexagon orientation
-}
orientationLayoutPointy : Orientation
orientationLayoutPointy =
    { forward_matrix =
        { f0 = sqrt 3.0
        , f1 = sqrt 3.0 / 2.0
        , f2 = 0.0
        , f3 = 3.0 / 2.0
        }
    , inverse_matrix =
        { f0 = sqrt 3.0 / 3.0
        , f1 = -1.0 / 3.0
        , f2 = 0.0
        , f3 = 2.0 / 3.0
        }
    , start_angle = 0.5
    }

{-| Constant definition for flat-top hexagon orientation -}
orientationLayoutFlat : Orientation
orientationLayoutFlat =
    { forward_matrix =
        { f0 = 3.0 / 2.0
        , f1 = 0.0
        , f2 = sqrt 3.0 / 2
        , f3 = sqrt 3
        }
    , inverse_matrix =
        { f0 = 2.0 / 3.0
        , f1 = 0.0
        , f2 = -1.0 / 3.0
        , f3 = 1.0 / sqrt 3
        }
    , start_angle = 0
    }

{-| Turn Hex coordinates into a Point location on a Layout
-}
hexToPoint : Layout -> Hex -> Point
hexToPoint layout hex =
    let
        { f0, f1, f2, f3 } =
            layout.orientation.forward_matrix

        ( xl, yl ) =
            layout.size

        ( xo, yo ) =
            layout.origin

        q =
            Hexagons.Hex.q hex

        r =
            Hexagons.Hex.r hex

        x =
            precision 2 <| (((f0 * q) + (f1 * r)) * xl) + xo

        y =
            precision 2 <| (((f2 * q) + (f3 * r)) * yl) + yo
    in
    ( x, y )


{-| Turn Point coordinates on a Layout into a Hex coordinates
-}
pointToHex : Layout -> Point -> Hex
pointToHex layout point =
    let
        { f0, f1, f2, f3 } =
            layout.orientation.inverse_matrix

        ( xl, yl ) =
            layout.size

        ( xo, yo ) =
            layout.origin

        ( x, y ) =
            point

        x1 =
            (x - xo) / xl

        y1 =
            (y - yo) / yl

        q =
            (f0 * x1) + (f1 * y1)

        r =
            (f2 * x1) + (f3 * y1)
    in
    Hexagons.Hex.floatFactory ( q, r )


{-| Convert Hex coordinates to offset
-}
hexToOffset : Hex -> OffsetCoord
hexToOffset hex =
    let
        offset =
            0

        q =
            Hexagons.Hex.intQ hex

        r =
            Hexagons.Hex.intR hex

        col =
            q + ((r + offset * modBy 2 (abs r)) // 2)

        row =
            r
    in
    ( col, row )


{-| Convert offset coordinates to hex
-}
offsetToHex : OffsetCoord -> Hex
offsetToHex ( col, row ) =
    let
        offset =
            0

        q =
            col - ((row + offset * modBy 2 (abs row)) // 2)

        r =
            row
    in
    Hexagons.Hex.intFactory ( q, r )


{-| Calculate corner offset from a center of the Hex
-}
hexCornerOffset : Layout -> Int -> Point
hexCornerOffset layout corner =
    let
        ( xl, yl ) =
            layout.size

        startAngle =
            layout.orientation.start_angle

        angle =
            ((2.0 * pi) * (toFloat corner + startAngle)) / 6

        x =
            precision 2 <| xl * cos angle

        y =
            precision 2 <| yl * sin angle
    in
    ( x, y )


{-| Once we know where the corners are relative to the center, we can calculate the corners in screen locations by adding the center to each corner, and putting the coordinates into a list.
-}
polygonCorners : Layout -> Hex -> List Point
polygonCorners layout hex =
    let
        ( x, y ) =
            hexToPoint layout hex

        offsetHex ( x__, y__ ) ( x_, y_ ) =
            ( precision 2 <| x__ + x_, precision 2 <| y__ + y_ )
    in
    List.map (offsetHex ( x, y )) <|
        List.map (hexCornerOffset layout) (List.range 0 5)


{-| Linear interpolation of hexes
-}
hexLerp : Hex -> Hex -> Float -> Hex
hexLerp a b t =
    let
        a_ =
            Hexagons.Hex.toFloatHex a

        b_ =
            Hexagons.Hex.toFloatHex b

        q1 =
            Hexagons.Hex.q a_

        q2 =
            Hexagons.Hex.q b_

        r1 =
            Hexagons.Hex.r a_

        r2 =
            Hexagons.Hex.r b_

        q =
            q1 + ((q2 - q1) * t)

        r =
            r1 + ((r2 - r1) * t)
    in
    Hexagons.Hex.floatFactory ( q, r )


{-| Draw the line between hexes using the linear interpolation
-}
drawLine : Hex -> Hex -> List Hex
drawLine a b =
    let
        n =
            toFloat <| Hexagons.Hex.distance a b

        step =
            1.0 / max n 1.0

        aq =
            Hexagons.Hex.q a

        ar =
            Hexagons.Hex.r a

        as_ =
            Hexagons.Hex.s a

        bq =
            Hexagons.Hex.q b

        br =
            Hexagons.Hex.r b

        bs =
            Hexagons.Hex.s b

        a_nudge =
            FloatCubeHex ( aq + 1.0e-6, ar + 1.0e-6, as_ - 2.0e-6 )

        b_nudge =
            FloatCubeHex ( bq + 1.0e-6, br + 1.0e-6, bs - 2.0e-6 )

        float_steps =
            List.map toFloat (List.range 0 (truncate n))

        steps =
            List.map ((*) step) float_steps
    in
    List.map (Hexagons.Hex.toIntHex << hexLerp a_nudge b_nudge) steps


{-| Draw the circle of a defined redius with the hex in a center
-}
drawCircle : Hex -> Int -> List Hex
drawCircle hex radius =
    let
        q =
            Hexagons.Hex.intQ hex

        r =
            Hexagons.Hex.intR hex

        calcHex q2 r2 =
            Hexagons.Hex.intFactory ( q2, r2 )
                |> add hex

        calcRow q2 =
            let
                q1 =
                    max -radius (-q2 - radius)

                r1 =
                    min radius (-q2 + radius)
            in
            List.map (calcHex q2) (List.range q1 r1)
    in
    List.concat <| List.map calcRow (List.range -radius radius)
