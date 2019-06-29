module Main exposing (main)

import Html
import Html.Attributes
import Json.Decode
import Keyboard.Key
import Simplegamedev20190510 exposing (..)
import Svg
import Svg.Attributes


displaySizeX : Int
displaySizeX =
    400


displaySizeY : Int
displaySizeY =
    300


waterLevel : Int
waterLevel =
    (displaySizeY * 3) // 4


iceFloeHeight : Int
iceFloeHeight =
    30


type alias IceFloe =
    { size : Int
    , location : Location
    }


type alias GameState =
    { iceFloes : List IceFloe }


type alias Location =
    { x : Int, y : Int }


main : SimpleGame GameState ()
main =
    composeSimpleGame
        { updateIntervalInMilliseconds = 125
        , updatePerInterval = moveSnakeForwardOneStep
        , updateOnKeyDown = onKeyDown
        , updateOnKeyUp = always identity
        , renderToHtml = renderToHtml
        , initialState = initialState
        , updateForEventFromHtml = always identity
        }


initialState : GameState
initialState =
    { iceFloes = [ { size = 40, location = { x = 100, y = waterLevel } } ] }


onKeyDown : KeyboardEvent -> GameState -> GameState
onKeyDown keyboardEvent gameStateBefore =
    gameStateBefore


moveSnakeForwardOneStep : GameState -> GameState
moveSnakeForwardOneStep gameStateBefore =
    gameStateBefore


renderToHtml : GameState -> Html.Html ()
renderToHtml gameState =
    let
        skyHtml =
            svgRectFrom_Fill_Left_Top_Width_Height "skyblue" ( 0, 0 ) ( 1000, 1000 )

        waterHtml =
            svgRectFrom_Fill_Left_Top_Width_Height "#1795d1" ( 0, waterLevel ) ( 1000, 1000 )

        iceFloeHtmlFromState : IceFloe -> Svg.Svg ()
        iceFloeHtmlFromState iceFloeState =
            svgRectFrom_Fill_Left_Top_Width_Height
                "whitesmoke"
                ( iceFloeState.location.x - iceFloeState.size // 2, iceFloeState.location.y - iceFloeHeight // 2 )
                ( iceFloeState.size, iceFloeHeight )

        iceHtml : Svg.Svg ()
        iceHtml =
            gameState.iceFloes
                |> List.map iceFloeHtmlFromState
                |> Svg.g []
    in
    Svg.svg
        [ Svg.Attributes.width (displaySizeX |> String.fromInt)
        , Svg.Attributes.height (displaySizeY |> String.fromInt)
        , Html.Attributes.style "background" "black"
        ]
        [ skyHtml, waterHtml, iceHtml ]
