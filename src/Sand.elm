module Sand exposing (SandState, initSand, viewSandPanel)

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, style)
import Html.Lazy
import Theme



type alias SandState =
    { mouseDown : Bool
    }


initSand : SandState
initSand =
    { mouseDown = False
    }



-- VIEW


viewSandPanel : SandState -> Html msg
viewSandPanel state =
    Html.Lazy.lazy viewSandPanelInner state


viewSandPanelInner : SandState -> Html msg
viewSandPanelInner _ =
    div
        [ style "animation" "fadeUp 0.6s ease 0.2s both"
        ]
        [ Theme.viewPanelTitleBar "SAND.EXE"
        , div
            [ id "sand-container"
            , style "position" "relative"
            , style "width" "100%"
            , style "aspect-ratio" "3 / 2"
            , style "border" ("1px solid " ++ Theme.purpleA 0.1)
            , style "cursor" "crosshair"
            , style "overflow" "hidden"
            , style "background" "#000"
            ]
            []
        , Theme.viewPanelBottomBar
            [ id "sand-bottom-bar" ]
            [ text "R to reset" ]
        ]
