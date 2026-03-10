module Sand exposing (SandState, initSand, viewSandPanel)

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, style)
import Html.Lazy


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
        [ -- Title bar
          div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "space-between"
            , style "padding" "8px 12px"
            , style "background" "rgba(168,85,247,0.04)"
            , style "border" "1px solid rgba(168,85,247,0.1)"
            , style "border-bottom" "none"
            ]
            [ div
                [ style "font-family" "'Space Mono', monospace"
                , style "font-size" "10px"
                , style "color" "rgba(168,85,247,0.5)"
                , style "letter-spacing" "0.1em"
                ]
                [ text "SAND.EXE" ]
            , div
                [ style "display" "flex"
                , style "gap" "6px"
                ]
                [ div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" "rgba(168,85,247,0.2)" ] []
                , div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" "rgba(168,85,247,0.15)" ] []
                , div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" "rgba(168,85,247,0.1)" ] []
                ]
            ]
        , -- Canvas container — JS injects the <canvas> here
          div
            [ id "sand-container"
            , style "position" "relative"
            , style "width" "100%"
            , style "aspect-ratio" "3 / 2"
            , style "border" "1px solid rgba(168,85,247,0.1)"
            , style "cursor" "crosshair"
            , style "overflow" "hidden"
            , style "background" "#000"
            ]
            []
        , -- Bottom bar
          div
            [ style "padding" "8px 12px"
            , style "background" "rgba(168,85,247,0.02)"
            , style "border" "1px solid rgba(168,85,247,0.06)"
            , style "border-top" "none"
            , style "font-family" "'Space Mono', monospace"
            , style "font-size" "9px"
            , style "color" "rgba(168,85,247,0.25)"
            , style "letter-spacing" "0.05em"
            ]
            [ text "R to reset" ]
        ]
