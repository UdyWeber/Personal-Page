module Theme exposing
    ( bgDark
    , blue
    , emailUrl
    , frac
    , fuchsia
    , getAt
    , githubUrl
    , linkedinUrl
    , monoFont
    , purple
    , purpleA
    , rgbaStr
    , sansFont
    , textGray
    , textLight
    , viewPanelBottomBar
    , viewPanelTitleBar
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)



-- COLORS


purple : { r : Int, g : Int, b : Int }
purple =
    { r = 168, g = 85, b = 247 }


blue : { r : Int, g : Int, b : Int }
blue =
    { r = 59, g = 130, b = 246 }


fuchsia : { r : Int, g : Int, b : Int }
fuchsia =
    { r = 217, g = 70, b = 239 }


textLight : String
textLight =
    "#eeedf0"


textGray : String
textGray =
    "rgba(196,195,200,0.9)"


bgDark : String
bgDark =
    "#0a0a0c"



-- COLOR HELPERS


rgbaStr : { r : Int, g : Int, b : Int } -> Float -> String
rgbaStr color alpha =
    "rgba("
        ++ String.fromInt color.r
        ++ ","
        ++ String.fromInt color.g
        ++ ","
        ++ String.fromInt color.b
        ++ ","
        ++ String.fromFloat alpha
        ++ ")"


purpleA : Float -> String
purpleA alpha =
    rgbaStr purple alpha



-- FONTS


monoFont : String
monoFont =
    "'Space Mono', monospace"


sansFont : String
sansFont =
    "'DM Sans', sans-serif"



-- LINKS


githubUrl : String
githubUrl =
    "https://github.com/udyweber"


linkedinUrl : String
linkedinUrl =
    "https://linkedin.com/in/joaoarthurweber"


emailUrl : String
emailUrl =
    "mailto:joaoarthurweber@gmail.com"



-- UTILITIES


frac : Float -> Float
frac f =
    f - toFloat (floor f)


getAt : Int -> List a -> Maybe a
getAt idx list =
    List.head (List.drop idx list)



-- SHARED PANEL CHROME


viewPanelTitleBar : String -> Html msg
viewPanelTitleBar title =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "space-between"
        , style "padding" "8px 12px"
        , style "background" (purpleA 0.04)
        , style "border" ("1px solid " ++ purpleA 0.1)
        , style "border-bottom" "none"
        ]
        [ div
            [ style "font-family" monoFont
            , style "font-size" "10px"
            , style "color" (purpleA 0.7)
            , style "letter-spacing" "0.1em"
            ]
            [ text title ]
        , div
            [ style "display" "flex"
            , style "gap" "6px"
            ]
            [ div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" (purpleA 0.2) ] []
            , div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" (purpleA 0.15) ] []
            , div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" (purpleA 0.1) ] []
            ]
        ]


viewPanelBottomBar : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewPanelBottomBar extraAttrs children =
    div
        ([ style "display" "flex"
         , style "align-items" "center"
         , style "justify-content" "space-between"
         , style "padding" "8px 12px"
         , style "background" (purpleA 0.02)
         , style "border" ("1px solid " ++ purpleA 0.06)
         , style "border-top" "none"
         , style "font-family" monoFont
         , style "font-size" "9px"
         , style "color" (purpleA 0.45)
         , style "letter-spacing" "0.05em"
         ]
            ++ extraAttrs
        )
        children
