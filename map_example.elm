module MapExample exposing (main)

import Browser
import Dict
import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)
import Hexagons.Map exposing (..)
import Html exposing (Html)
import Json.Decode as Json
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy exposing (lazy, lazy2, lazy3)
import Task


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { map : Map
    , greenCells : List Hash
    }


emptyModel : Model
emptyModel =
    { map = rectangularPointyTopMap 10 10
    , greenCells = []
    }


init : Model
init =
    emptyModel



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | SetGreen Hash



-- How we update our Model on a given Msg?


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SetGreen cell ->
            { model | greenCells = model.greenCells ++ [ cell ] }



-- VIEW


cellWidth =
    20.0


cellHeight =
    20.0


svgWidth =
    500


svgHeight =
    500


layout =
    { orientation = Hexagons.Layout.orientationLayoutPointy
    , size = ( 20.0, 20.0 )
    , origin = ( 0.0, 0.0 )
    }


viewBoxStringCoords : String
viewBoxStringCoords =
    String.fromFloat (-cellWidth + cellWidth * 0.1)
        ++ " "
        ++ String.fromFloat -(cellHeight + 0)
        ++ " "
        ++ String.fromInt svgWidth
        ++ " "
        ++ String.fromInt svgHeight


view : Model -> Html Msg
view model =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , Svg.Attributes.height (String.fromInt svgHeight)
        , Svg.Attributes.width (String.fromInt svgWidth)
        , viewBox viewBoxStringCoords
        ]
        [ lazy hexGrid model
        ]


hexGrid : Model -> Html Msg
hexGrid model =
    let
        toSvg : Hash -> String -> Svg Msg
        toSvg hexLocation cornersCoords =
            g
                []
                (toPolygon hexLocation cornersCoords)

        toPolygon : Hash -> String -> List (Svg Msg)
        toPolygon hexLocation cornersCoords =
            [ polygon
                [ Svg.Attributes.style "cursor: pointer"
                , stroke "#ffff00"
                , strokeWidth "1px"
                , fill <|
                    if List.member hexLocation model.greenCells then
                        "#179f83"

                    else
                        "#777777"
                , points cornersCoords
                , Svg.Events.onClick <|
                    SetGreen hexLocation
                ]
                []
            ]
    in
    g
        []
    <|
        List.map2 toSvg
            (List.map getCellKey (Dict.toList model.map))
            (List.map (pointsToString << mapPolygonCorners << getCell) (Dict.toList model.map))


{-| Helper to convert points to SVG string coordinates
-}
pointsToString : List Point -> String
pointsToString points =
    String.join " " (List.map pointToStringCoords points)


{-| Helper to convert points to SVG string coordinates
-}
pointToStringCoords : Point -> String
pointToStringCoords ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


getCell : ( Hash, Hex ) -> Hex
getCell ( key, hex ) =
    hex


getCellKey : ( Hash, Hex ) -> Hash
getCellKey ( key, hex ) =
    key


mapPolygonCorners : Hex -> List Point
mapPolygonCorners =
    polygonCorners layout
