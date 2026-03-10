module Starfield exposing (Star, BlackHole, GravityEffect, starField, blackHoles, viewStarfield, generateFerrisField)

import Html exposing (Html, div, text)
import Html.Attributes exposing (src, style)
import Theme


type alias Star =
    { x : Float, y : Float, depth : Float, size : Float, brightness : Float, seed : Float }


type alias BlackHole =
    { x : Float, y : Float, mass : Float, cycleSpeed : Float, phase : Float }


type alias GravityEffect =
    { posX : Float, posY : Float, scale : Float, opacity : Float }


blackHoles : List BlackHole
blackHoles =
    [ { x = 18, y = 25, mass = 22, cycleSpeed = 18000, phase = 0 }
    , { x = 78, y = 45, mass = 28, cycleSpeed = 22000, phase = 4000 }
    , { x = 45, y = 75, mass = 20, cycleSpeed = 16000, phase = 9000 }
    ]


type alias StarLayerConfig =
    { count : Int
    , indexOffset : Int
    , hashSeeds : { a : Float, b : Float, c : Float, d : Float, e : Float, f : Float }
    , depthMin : Float
    , depthRange : Float
    , depthMul : Float
    , sizeMin : Float
    , sizeRange : Float
    , sizeMul : Float
    , brightMin : Float
    , brightRange : Float
    , brightMul : Float
    , seedFactor : Float
    , seedMix : Float
    , seedHashIndex : Int
    }


generateStarLayer : StarLayerConfig -> List Star
generateStarLayer config =
    List.indexedMap
        (\i _ ->
            let
                fi =
                    toFloat (i + config.indexOffset)

                h1 =
                    Theme.frac (fi * config.hashSeeds.a + config.hashSeeds.b)

                h2 =
                    Theme.frac (fi * config.hashSeeds.c + config.hashSeeds.d)

                h3 =
                    Theme.frac (fi * config.hashSeeds.e + config.hashSeeds.f)

                seedHash =
                    if config.seedHashIndex == 1 then
                        h1

                    else if config.seedHashIndex == 2 then
                        h2

                    else
                        h3
            in
            { x = Theme.frac (h1 * h2 * 43758.5453) * 100
            , y = Theme.frac (h2 * h3 * 29187.3127) * 100
            , depth = config.depthMin + Theme.frac (h3 * config.depthMul) * config.depthRange
            , size = config.sizeMin + Theme.frac (h1 * config.sizeMul) * config.sizeRange
            , brightness = config.brightMin + Theme.frac (h2 * config.brightMul) * config.brightRange
            , seed = fi * config.seedFactor + seedHash * config.seedMix
            }
        )
        (List.range 0 (config.count - 1))


starField : List Star
starField =
    generateStarLayer
        { count = 50
        , indexOffset = 0
        , hashSeeds = { a = 127.1, b = 311.7, c = 269.5, d = 183.3, e = 419.2, f = 371.9 }
        , depthMin = 0.1
        , depthRange = 0.15
        , depthMul = 17.31
        , sizeMin = 0.8
        , sizeRange = 0.5
        , sizeMul = 3.71
        , brightMin = 0.25
        , brightRange = 0.25
        , brightMul = 11.37
        , seedFactor = 2.718
        , seedMix = 100
        , seedHashIndex = 1
        }
        ++ generateStarLayer
            { count = 30
            , indexOffset = 100
            , hashSeeds = { a = 157.3, b = 217.1, c = 337.9, d = 113.7, e = 281.5, f = 479.3 }
            , depthMin = 0.35
            , depthRange = 0.2
            , depthMul = 13.17
            , sizeMin = 1.0
            , sizeRange = 0.8
            , sizeMul = 7.93
            , brightMin = 0.35
            , brightRange = 0.3
            , brightMul = 5.43
            , seedFactor = 1.414
            , seedMix = 80
            , seedHashIndex = 2
            }
        ++ generateStarLayer
            { count = 15
            , indexOffset = 200
            , hashSeeds = { a = 213.7, b = 511.3, c = 179.1, d = 389.7, e = 347.3, f = 197.1 }
            , depthMin = 0.7
            , depthRange = 0.25
            , depthMul = 9.71
            , sizeMin = 1.8
            , sizeRange = 1.2
            , sizeMul = 5.17
            , brightMin = 0.5
            , brightRange = 0.5
            , brightMul = 7.91
            , seedFactor = 3.141
            , seedMix = 60
            , seedHashIndex = 3
            }


viewStarfield : Float -> Float -> Html msg
viewStarfield time seed =
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
            ++ List.map (viewFerris time) (generateFerrisField seed)
        )


viewStar : Float -> Star -> Html msg
viewStar time star =
    let
        timeDriftY =
            sin (time / 20000 + star.seed) * 8 * star.depth

        timeDriftX =
            cos (time / 25000 + star.seed * 0.7) * 4 * star.depth

        grav =
            List.foldl (applyGravity time) { posX = star.x + timeDriftX, posY = star.y + timeDriftY, scale = 1.0, opacity = 1.0 } blackHoles

        twinkle =
            let
                t1 = sin (time / 1200 + star.seed * 3.7)
                t2 = sin (time / 2800 + star.seed * 7.13)
                t3 = cos (time / 1900 + star.seed * 1.91)
            in
            0.75 + (t1 * 0.1 + t2 * 0.08 + t3 * 0.07)

        finalOpacity =
            star.brightness * twinkle * grav.opacity

        finalSize =
            star.size * grav.scale

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
            , style "left" (String.fromFloat grav.posX ++ "%")
            , style "top" (String.fromFloat grav.posY ++ "%")
            , style "width" (String.fromFloat finalSize ++ "px")
            , style "height" (String.fromFloat finalSize ++ "px")
            , style "border-radius" "50%"
            , style "background" "rgba(220,215,240,0.9)"
            , style "opacity" (String.fromFloat finalOpacity)
            , style "box-shadow"
                (if glowRadius > 0 then
                    "0 0 " ++ String.fromFloat glowRadius ++ "px " ++ Theme.purpleA 0.3

                 else
                    "none"
                )
            , style "transform" "translate(-50%, -50%)"
            ]
            []


blackHoleCycleActivity : Float -> Float
blackHoleCycleActivity cycleT =
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


blackHoleCoreVisibility : Float -> Float
blackHoleCoreVisibility cycleT =
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


applyGravity : Float -> BlackHole -> GravityEffect -> GravityEffect
applyGravity time blackHole acc =
    let
        sx =
            acc.posX

        sy =
            acc.posY

        cycleT =
            Theme.frac ((time + blackHole.phase) / blackHole.cycleSpeed)

        activity =
            blackHoleCycleActivity cycleT

        dx =
            sx - blackHole.x

        dy =
            sy - blackHole.y

        dist =
            sqrt (dx * dx + dy * dy)

        influence =
            if dist < blackHole.mass then
                (1 - dist / blackHole.mass) ^ 2

            else
                0

        pullStrength =
            activity * influence

        pullX =
            if dist < 0.01 then
                0

            else
                dx / dist * pullStrength * blackHole.mass * 0.6

        pullY =
            if dist < 0.01 then
                0

            else
                dy / dist * pullStrength * blackHole.mass * 0.6

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
    { posX = sx - (pullX * cosA - pullY * sinA)
    , posY = sy - (pullX * sinA + pullY * cosA)
    , scale = acc.scale * scaleEffect
    , opacity = acc.opacity * opacityEffect
    }


viewBlackHole : Float -> BlackHole -> Html msg
viewBlackHole time blackHole =
    let
        cycleT =
            Theme.frac ((time + blackHole.phase) / blackHole.cycleSpeed)

        coreVisibility =
            blackHoleCoreVisibility cycleT

        diskSize =
            blackHole.mass * 0.8 * coreVisibility

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
                , style "left" ("calc(" ++ String.fromFloat blackHole.x ++ "% - " ++ String.fromFloat (diskSize * 2) ++ "px)")
                , style "top" ("calc(" ++ String.fromFloat blackHole.y ++ "% - " ++ String.fromFloat (diskSize * 0.6) ++ "px)")
                , style "width" (String.fromFloat (diskSize * 4) ++ "px")
                , style "height" (String.fromFloat (diskSize * 1.2) ++ "px")
                , style "border-radius" "50%"
                , style "border" ("1px solid " ++ Theme.purpleA (coreVisibility * 0.3))
                , style "box-shadow"
                    ("0 0 " ++ String.fromFloat (diskSize * 0.5) ++ "px " ++ Theme.purpleA (coreVisibility * 0.15))
                , style "animation" "accrete 4s linear infinite"
                , style "opacity" (String.fromFloat coreVisibility)
                ]
                []

          else
            text ""
        , if coreGlow > 0.01 then
            div
                [ style "position" "absolute"
                , style "left" ("calc(" ++ String.fromFloat blackHole.x ++ "% - 6px)")
                , style "top" ("calc(" ++ String.fromFloat blackHole.y ++ "% - 6px)")
                , style "width" "12px"
                , style "height" "12px"
                , style "border-radius" "50%"
                , style "background" ("radial-gradient(circle, #020005 40%, " ++ Theme.purpleA 0.3 ++ " 70%, transparent 100%)")
                , style "box-shadow"
                    ("0 0 20px " ++ Theme.purpleA (coreGlow * 0.4) ++ ", 0 0 40px rgba(100,50,200," ++ String.fromFloat (coreGlow * 0.2) ++ ")")
                , style "opacity" (String.fromFloat coreGlow)
                ]
                []

          else
            text ""
        , if flashOpacity > 0.01 then
            div
                [ style "position" "absolute"
                , style "left" ("calc(" ++ String.fromFloat blackHole.x ++ "% - 40px)")
                , style "top" ("calc(" ++ String.fromFloat blackHole.y ++ "% - 40px)")
                , style "width" "80px"
                , style "height" "80px"
                , style "border-radius" "50%"
                , style "background" ("radial-gradient(circle, rgba(200,170,255,0.8) 0%, " ++ Theme.purpleA 0.3 ++ " 30%, transparent 70%)")
                , style "opacity" (String.fromFloat flashOpacity)
                ]
                []

          else
            text ""
        ]


type alias Ferris =
    { x : Float, y : Float, rotation : Float, size : Float, opacity : Float, seed : Float }


generateFerrisField : Float -> List Ferris
generateFerrisField seed =
    List.indexedMap
        (\i _ ->
            let
                fi =
                    toFloat (i + 500) * (seed * 99991 + 1)

                h1 =
                    Theme.frac (fi * 173.7 + 491.3)

                h2 =
                    Theme.frac (fi * 311.1 + 257.9)

                h3 =
                    Theme.frac (fi * 523.7 + 139.1)
            in
            { x = Theme.frac (h1 * h2 * 43758.5453) * 100
            , y = Theme.frac (h2 * h3 * 29187.3127) * 100
            , rotation = Theme.frac (h3 * h1 * 17593.1) * 360
            , size = 35 + Theme.frac (h1 * 7.31) * 25
            , opacity = 0.12 + Theme.frac (h2 * 3.91) * 0.1
            , seed = fi * 1.618 + h3 * 50
            }
        )
        (List.range 0 19)


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
            , style "background" (Theme.purpleA 0.04)
            , style "border" ("1px solid " ++ Theme.purpleA 0.15)
            , style "box-shadow" ("0 0 8px " ++ Theme.purpleA 0.1)
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
            , style "background" (Theme.purpleA 0.4)
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
                , style "background" (Theme.purpleA 0.35)
                ]
                []
            ]
        ]
