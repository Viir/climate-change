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


playerCharHeight : Int
playerCharHeight =
    30


type alias IceFloe =
    { size : Int
    , location : Location
    }


type alias GameState =
    { iceFloes : List IceFloe
    , playerCharacter : PlayerCharacter
    }


type alias PlayerCharacter =
    { location : Location
    , velocityMilli : { x : Int, y : Int }
    }


type alias Location =
    { x : Int, y : Int }


main : SimpleGame GameState ()
main =
    composeSimpleGame
        { updateIntervalInMilliseconds = 30
        , updatePerInterval = updatePerInterval
        , updateOnKeyDown = onKeyDown
        , updateOnKeyUp = always identity
        , renderToHtml = renderToHtml
        , initialState = initialState
        , updateForEventFromHtml = always identity
        }


initialState : GameState
initialState =
    { iceFloes = [ { size = 140, location = { x = 80, y = waterLevel } } ]
    , playerCharacter = { location = { x = 30, y = 100 }, velocityMilli = { x = 0, y = 0 } }
    }


onKeyDown : KeyboardEvent -> GameState -> GameState
onKeyDown keyboardEvent gameStateBefore =
    if gameStateBefore |> isPlayerCharStandingOnIce |> not then
        gameStateBefore

    else
        case keyboardEvent.keyCode of
            Keyboard.Key.Up ->
                let
                    playerCharacterBefore =
                        gameStateBefore.playerCharacter

                    playerCharacter =
                        { playerCharacterBefore
                            | velocityMilli = { x = playerCharacterBefore.velocityMilli.x, y = -10 }
                            , location = { x = playerCharacterBefore.location.x, y = playerCharacterBefore.location.y - 3 }
                        }
                in
                { gameStateBefore | playerCharacter = playerCharacter }

            _ ->
                gameStateBefore


updatePerInterval : GameState -> GameState
updatePerInterval gameStateBefore =
    let
        playerCharStandsOnIce =
            gameStateBefore |> isPlayerCharStandingOnIce

        playerCharacterBefore =
            gameStateBefore.playerCharacter

        playerVelocityMilli =
            { x = playerCharacterBefore.velocityMilli.x
            , y =
                if playerCharStandsOnIce then
                    0

                else
                    playerCharacterBefore.velocityMilli.y
                        + 1
            }

        playerCharNewLocation =
            { x = playerCharacterBefore.location.x + playerVelocityMilli.x
            , y =
                if playerCharStandsOnIce then
                    waterLevel - (playerCharHeight + iceFloeHeight) // 2 + 1

                else
                    playerCharacterBefore.location.y + playerCharacterBefore.velocityMilli.y
            }

        playerCharacter =
            { playerCharacterBefore | velocityMilli = playerVelocityMilli, location = playerCharNewLocation }
    in
    { gameStateBefore | playerCharacter = playerCharacter }


isPlayerCharStandingOnIce : GameState -> Bool
isPlayerCharStandingOnIce gameState =
    gameState.iceFloes
        |> List.any
            (\iceFloe ->
                gameState.playerCharacter.location.x
                    < iceFloe.location.x
                    + iceFloe.size
                    // 2
                    && gameState.playerCharacter.location.x
                    > iceFloe.location.x
                    - iceFloe.size
                    // 2
                    && gameState.playerCharacter.location.y
                    > iceFloe.location.y
                    - (iceFloeHeight + playerCharHeight)
                    // 2
            )


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

        playerCharacterHtml =
            svgRectFrom_Fill_Left_Top_Width_Height
                "black"
                ( gameState.playerCharacter.location.x, gameState.playerCharacter.location.y - playerCharHeight // 2 )
                ( 16, playerCharHeight )
    in
    Svg.svg
        [ Svg.Attributes.width (displaySizeX |> String.fromInt)
        , Svg.Attributes.height (displaySizeY |> String.fromInt)
        , Html.Attributes.style "background" "black"
        ]
        [ skyHtml, waterHtml, iceHtml, playerCharacterHtml ]
