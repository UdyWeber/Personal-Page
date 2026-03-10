module Starfield exposing (Star, BlackHole, GravAcc, starField, blackHoles, viewStarfield)

import Html exposing (Html, div, text)
import Html.Attributes exposing (src, style)


type alias Star =
    { x : Float, y : Float, depth : Float, size : Float, brightness : Float, seed : Float }


type alias BlackHole =
    { x : Float, y : Float, mass : Float, cycleSpeed : Float, phase : Float }


type alias GravAcc =
    { gx : Float, gy : Float, gs : Float, go : Float }


blackHoles : List BlackHole
blackHoles =
    [ { x = 18, y = 25, mass = 22, cycleSpeed = 18000, phase = 0 }
    , { x = 78, y = 45, mass = 28, cycleSpeed = 22000, phase = 4000 }
    , { x = 45, y = 75, mass = 20, cycleSpeed = 16000, phase = 9000 }
    ]


starField : List Star
starField =
    List.indexedMap
        (\i _ ->
            let
                fi = toFloat i
                h1 = frac (fi * 127.1 + 311.7)
                h2 = frac (fi * 269.5 + 183.3)
                h3 = frac (fi * 419.2 + 371.9)
            in
            { x = frac (h1 * h2 * 43758.5453) * 100
            , y = frac (h2 * h3 * 29187.3127) * 100
            , depth = 0.1 + frac (h3 * 17.31) * 0.15
            , size = 0.8 + frac (h1 * 3.71) * 0.5
            , brightness = 0.25 + frac (h2 * 11.37) * 0.25
            , seed = fi * 2.718 + h1 * 100
            }
        )
        (List.range 0 49)
        ++ List.indexedMap
            (\i _ ->
                let
                    fi = toFloat (i + 100)
                    h1 = frac (fi * 157.3 + 217.1)
                    h2 = frac (fi * 337.9 + 113.7)
                    h3 = frac (fi * 281.5 + 479.3)
                in
                { x = frac (h1 * h2 * 43758.5453) * 100
                , y = frac (h2 * h3 * 29187.3127) * 100
                , depth = 0.35 + frac (h3 * 13.17) * 0.2
                , size = 1.0 + frac (h1 * 7.93) * 0.8
                , brightness = 0.35 + frac (h2 * 5.43) * 0.3
                , seed = fi * 1.414 + h2 * 80
                }
            )
            (List.range 0 29)
        ++ List.indexedMap
            (\i _ ->
                let
                    fi = toFloat (i + 200)
                    h1 = frac (fi * 213.7 + 511.3)
                    h2 = frac (fi * 179.1 + 389.7)
                    h3 = frac (fi * 347.3 + 197.1)
                in
                { x = frac (h1 * h2 * 43758.5453) * 100
                , y = frac (h2 * h3 * 29187.3127) * 100
                , depth = 0.7 + frac (h3 * 9.71) * 0.25
                , size = 1.8 + frac (h1 * 5.17) * 1.2
                , brightness = 0.5 + frac (h2 * 7.91) * 0.5
                , seed = fi * 3.141 + h3 * 60
                }
            )
            (List.range 0 14)


viewStarfield : Float -> Html msg
viewStarfield time =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100vw"
        , style "height" "100vh"
        , style "pointer-events" "none"
        , style "z-index" "0"
        , style "overflow" "hidden"
        ]
        (List.map (viewBlackHole time) blackHoles
            ++ List.map (viewStar time) starField
            ++ List.map (viewFerris time) ferrisField
        )


viewStar : Float -> Star -> Html msg
viewStar time star =
    let
        timeDriftY =
            sin (time / 20000 + star.seed) * 8 * star.depth

        timeDriftX =
            cos (time / 25000 + star.seed * 0.7) * 4 * star.depth

        grav =
            List.foldl (applyGravity time) { gx = star.x + timeDriftX, gy = star.y + timeDriftY, gs = 1.0, go = 1.0 } blackHoles

        twinkle =
            let
                t1 = sin (time / 1200 + star.seed * 3.7)
                t2 = sin (time / 2800 + star.seed * 7.13)
                t3 = cos (time / 1900 + star.seed * 1.91)
            in
            0.75 + (t1 * 0.1 + t2 * 0.08 + t3 * 0.07)

        finalOpacity =
            star.brightness * twinkle * grav.go

        finalSize =
            star.size * grav.gs

        glowRadius =
            if star.depth > 0.6 then
                finalSize * 3

            else
                0
    in
    if finalOpacity < 0.01 then
        text ""

    else
        div
            [ style "position" "absolute"
            , style "left" (String.fromFloat grav.gx ++ "%")
            , style "top" (String.fromFloat grav.gy ++ "%")
            , style "width" (String.fromFloat finalSize ++ "px")
            , style "height" (String.fromFloat finalSize ++ "px")
            , style "border-radius" "50%"
            , style "background" "rgba(220,215,240,0.9)"
            , style "opacity" (String.fromFloat finalOpacity)
            , style "box-shadow"
                (if glowRadius > 0 then
                    "0 0 " ++ String.fromFloat glowRadius ++ "px rgba(168,85,247,0.3)"

                 else
                    "none"
                )
            , style "transform" "translate(-50%, -50%)"
            ]
            []


applyGravity : Float -> BlackHole -> GravAcc -> GravAcc
applyGravity time bh acc =
    let
        sx =
            acc.gx

        sy =
            acc.gy

        cycleT =
            frac ((time + bh.phase) / bh.cycleSpeed)

        activity =
            if cycleT < 0.15 then
                0

            else if cycleT < 0.4 then
                (cycleT - 0.15) / 0.25

            else if cycleT < 0.6 then
                1.0

            else if cycleT < 0.75 then
                -(1.0 - (cycleT - 0.6) / 0.15)

            else
                -(1.0 - (cycleT - 0.75) / 0.25) * 0.3

        dx =
            sx - bh.x

        dy =
            sy - bh.y

        dist =
            sqrt (dx * dx + dy * dy)

        influence =
            if dist < bh.mass then
                (1 - dist / bh.mass) ^ 2

            else
                0

        pullStrength =
            activity * influence

        pullX =
            if dist < 0.01 then
                0

            else
                dx / dist * pullStrength * bh.mass * 0.6

        pullY =
            if dist < 0.01 then
                0

            else
                dy / dist * pullStrength * bh.mass * 0.6

        spiralAngle =
            if pullStrength > 0 then
                pullStrength * 2.5

            else
                0

        cosA =
            cos spiralAngle

        sinA =
            sin spiralAngle

        scaleEffect =
            if pullStrength > 0 then
                1 - pullStrength * 0.7

            else
                1 + abs pullStrength * 0.5

        opacityEffect =
            if pullStrength > 0.8 then
                1 - (pullStrength - 0.8) / 0.2

            else if pullStrength < -0.5 then
                0.4

            else
                1.0
    in
    { gx = sx - (pullX * cosA - pullY * sinA)
    , gy = sy - (pullX * sinA + pullY * cosA)
    , gs = acc.gs * scaleEffect
    , go = acc.go * opacityEffect
    }


viewBlackHole : Float -> BlackHole -> Html msg
viewBlackHole time bh =
    let
        cycleT =
            frac ((time + bh.phase) / bh.cycleSpeed)

        coreVisibility =
            if cycleT < 0.15 then
                0

            else if cycleT < 0.4 then
                (cycleT - 0.15) / 0.25

            else if cycleT < 0.6 then
                1.0

            else if cycleT < 0.75 then
                1.0 - (cycleT - 0.6) / 0.15

            else
                0

        diskSize =
            bh.mass * 0.8 * coreVisibility

        coreGlow =
            coreVisibility * 0.6

        flashOpacity =
            if cycleT > 0.58 && cycleT < 0.65 then
                (1 - abs (cycleT - 0.615) / 0.035) * 0.4

            else
                0
    in
    div []
        [ if diskSize > 0.5 then
            div
                [ style "position" "absolute"
                , style "left" ("calc(" ++ String.fromFloat bh.x ++ "% - " ++ String.fromFloat (diskSize * 2) ++ "px)")
                , style "top" ("calc(" ++ String.fromFloat bh.y ++ "% - " ++ String.fromFloat (diskSize * 0.6) ++ "px)")
                , style "width" (String.fromFloat (diskSize * 4) ++ "px")
                , style "height" (String.fromFloat (diskSize * 1.2) ++ "px")
                , style "border-radius" "50%"
                , style "border" ("1px solid rgba(168,85,247," ++ String.fromFloat (coreVisibility * 0.3) ++ ")")
                , style "box-shadow"
                    ("0 0 " ++ String.fromFloat (diskSize * 0.5) ++ "px rgba(168,85,247," ++ String.fromFloat (coreVisibility * 0.15) ++ ")")
                , style "animation" "accrete 4s linear infinite"
                , style "opacity" (String.fromFloat coreVisibility)
                ]
                []

          else
            text ""
        , if coreGlow > 0.01 then
            div
                [ style "position" "absolute"
                , style "left" ("calc(" ++ String.fromFloat bh.x ++ "% - 6px)")
                , style "top" ("calc(" ++ String.fromFloat bh.y ++ "% - 6px)")
                , style "width" "12px"
                , style "height" "12px"
                , style "border-radius" "50%"
                , style "background" "radial-gradient(circle, #020005 40%, rgba(168,85,247,0.3) 70%, transparent 100%)"
                , style "box-shadow"
                    ("0 0 20px rgba(168,85,247," ++ String.fromFloat (coreGlow * 0.4) ++ "), 0 0 40px rgba(100,50,200," ++ String.fromFloat (coreGlow * 0.2) ++ ")")
                , style "opacity" (String.fromFloat coreGlow)
                ]
                []

          else
            text ""
        , if flashOpacity > 0.01 then
            div
                [ style "position" "absolute"
                , style "left" ("calc(" ++ String.fromFloat bh.x ++ "% - 40px)")
                , style "top" ("calc(" ++ String.fromFloat bh.y ++ "% - 40px)")
                , style "width" "80px"
                , style "height" "80px"
                , style "border-radius" "50%"
                , style "background" "radial-gradient(circle, rgba(200,170,255,0.8) 0%, rgba(168,85,247,0.3) 30%, transparent 70%)"
                , style "opacity" (String.fromFloat flashOpacity)
                ]
                []

          else
            text ""
        ]


type alias Ferris =
    { x : Float, y : Float, rotation : Float, size : Float, opacity : Float, seed : Float }


ferrisField : List Ferris
ferrisField =
    List.indexedMap
        (\i _ ->
            let
                fi =
                    toFloat (i + 500)

                h1 =
                    frac (fi * 173.7 + 491.3)

                h2 =
                    frac (fi * 311.1 + 257.9)

                h3 =
                    frac (fi * 523.7 + 139.1)
            in
            { x = frac (h1 * h2 * 43758.5453) * 100
            , y = frac (h2 * h3 * 29187.3127) * 100
            , rotation = frac (h3 * h1 * 17593.1) * 360
            , size = 35 + frac (h1 * 7.31) * 25
            , opacity = 0.12 + frac (h2 * 3.91) * 0.1
            , seed = fi * 1.618 + h3 * 50
            }
        )
        (List.range 0 9)


viewFerris : Float -> Ferris -> Html msg
viewFerris time ferris =
    let
        driftX =
            cos (time / 15000 + ferris.seed) * 3

        driftY =
            sin (time / 18000 + ferris.seed * 0.7) * 4

        spin =
            sin (time / 30000 + ferris.seed * 1.3) * 15

        finalRotation =
            ferris.rotation + spin

        s =
            ferris.size

        sw =
            s * 1.5
    in
    div
        [ style "position" "absolute"
        , style "left" ("calc(" ++ String.fromFloat (ferris.x + driftX) ++ "% - " ++ String.fromFloat (sw / 2) ++ "px)")
        , style "top" ("calc(" ++ String.fromFloat (ferris.y + driftY) ++ "% - " ++ String.fromFloat (s / 2) ++ "px)")
        , style "width" (String.fromFloat sw ++ "px")
        , style "height" (String.fromFloat s ++ "px")
        , style "opacity" (String.fromFloat ferris.opacity)
        , style "transform" ("rotate(" ++ String.fromFloat finalRotation ++ "deg)")
        ]
        [ -- Helmet glow behind Ferris
          div
            [ style "position" "absolute"
            , style "top" "-20%"
            , style "left" "10%"
            , style "width" "80%"
            , style "height" "140%"
            , style "border-radius" "50%"
            , style "background" "rgba(168,85,247,0.04)"
            , style "border" "1px solid rgba(168,85,247,0.15)"
            , style "box-shadow" "0 0 8px rgba(168,85,247,0.1)"
            ]
            []

        -- Actual Ferris image
        , Html.img
            [ src "assets/ferris.svg"
            , style "width" "100%"
            , style "height" "100%"
            , style "object-fit" "contain"
            , style "position" "relative"
            ]
            []

        -- Helmet antenna
        , div
            [ style "position" "absolute"
            , style "top" "-8%"
            , style "right" "28%"
            , style "width" "2px"
            , style "height" "18%"
            , style "background" "rgba(168,85,247,0.4)"
            , style "transform" "rotate(-15deg)"
            , style "transform-origin" "bottom center"
            ]
            [ div
                [ style "position" "absolute"
                , style "top" "-4px"
                , style "left" "-3px"
                , style "width" "8px"
                , style "height" "8px"
                , style "border-radius" "50%"
                , style "background" "rgba(168,85,247,0.35)"
                ]
                []
            ]
        ]


frac : Float -> Float
frac f =
    f - toFloat (floor f)
