module Starfield exposing (Star, BlackHole, GravAcc, starField, blackHoles, viewStarfield)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Svg exposing (svg)
import Svg.Attributes as SA


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
        [ svg
            [ SA.viewBox "0 0 1200 800"
            , SA.width (String.fromFloat sw)
            , SA.height (String.fromFloat s)
            ]
            [ -- Space helmet (glass dome behind Ferris)
              Svg.ellipse
                [ SA.cx "600"
                , SA.cy "420"
                , SA.rx "380"
                , SA.ry "340"
                , SA.fill "rgba(168,85,247,0.06)"
                , SA.stroke "rgba(168,85,247,0.35)"
                , SA.strokeWidth "4"
                ]
                []
            , -- Helmet reflection
              Svg.path
                [ SA.d "M340 220 Q460 120 720 180"
                , SA.stroke "rgba(200,180,255,0.25)"
                , SA.strokeWidth "3"
                , SA.fill "none"
                ]
                []

            -- Body shell (dark orange base) — g translate(597.344,637.02)
            , Svg.path
                [ SA.d "M597.344,357.461 C476.106,357.461 365.954,372.037 284.405,395.79 L284.405,598.691 C365.954,622.445 476.106,637.02 597.344,637.02 C736.104,637.02 860.331,617.928 943.775,587.834 L943.775,406.65 C860.331,376.555 736.104,357.461 597.344,357.461"
                , SA.fill "rgb(165,43,0)"
                ]
                []

            -- Right leg group — g translate(1068.75,575.642)
            , Svg.path
                [ SA.d "M1068.75,522.322 L1054.539,492.881 C1054.612,491.763 1054.67,490.644 1054.67,489.521 C1054.67,456.146 1019.964,425.386 961.573,400.759 L961.573,578.285 C988.818,566.793 1010.921,553.968 1026.729,540.16 C1022.077,558.867 1006.165,596.713 993.479,623.328 C972.629,661.394 965.079,694.531 966.047,696.172 C966.664,697.205 973.777,686.232 984.266,668.451 C1008.676,633.67 1054.93,567.269 1064.175,550.355 C1074.647,531.181 1068.75,522.322 1068.75,522.322"
                , SA.fill "rgb(165,43,0)"
                ]
                []

            -- Left leg group — g translate(149.064,591.421)
            , Svg.path
                [ SA.d "M149.064,491.467 C149.064,497.895 150.357,504.227 152.852,510.436 L144.341,525.586 C144.341,525.586 137.523,534.432 149.529,553.094 C160.119,569.549 213.164,633.961 241.161,667.692 C253.187,684.985 261.34,695.637 262.054,694.608 C263.178,692.975 254.578,660.508 230.695,623.467 C219.551,603.572 206.241,577.215 198.253,557.746 C220.556,571.862 249.736,584.666 284.405,595.686 L284.405,387.251 C200.861,413.799 149.064,450.684 149.064,491.467"
                , SA.fill "rgb(165,43,0)"
                ]
                []

            -- Crown/top (bright orange) — g translate(1151.27,281.813)
            , Svg.path
                [ SA.d "M1151.27,522.156 L1057.855,453.345 C1056.975,450.281 1056.099,447.218 1055.193,444.183 L1085.876,399.732 C1089.006,395.21 1089.641,389.334 1087.607,384.177 C1085.57,379.047 1081.116,375.367 1075.844,374.467 L1023.96,365.662 C1021.952,361.56 1019.844,357.52 1017.73,353.512 L1039.527,303.609 C1041.77,298.522 1041.296,292.614 1038.324,288.001 C1035.363,283.365 1030.334,280.657 1024.975,280.868 L972.319,282.781 C969.592,279.231 966.823,275.713 963.998,272.26 L976.098,218.77 C977.323,213.337 975.776,207.652 971.995,203.706 C968.233,199.774 962.766,198.147 957.569,199.423 L906.28,212.031 C902.959,209.096 899.582,206.198 896.166,203.358 L898.014,148.444 C898.212,142.885 895.621,137.602 891.17,134.519 C886.724,131.415 881.077,130.946 876.214,133.253 L828.367,156.0 C824.513,153.79 820.639,152.0 816.723,149.883 L808.268,95.368 C807.411,89.885 803.883,85.229 798.942,83.102 C794.019,80.965 788.374,81.655 784.051,84.91 L741.392,116.917 C737.192,118.312 732.973,119.649 728.7,120.928 L710.314,61.59 C708.444,56.361 704.133,52.519 698.876,51.439 C693.637,50.367 688.246,52.181 684.613,56.241 L648.707,96.412 C644.364,96.912 640.021,97.356 635.664,97.731 L607.94,48.368 C605.13,43.636 600.17,40.756 594.824,40.756 C589.49,40.756 584.52,43.636 581.734,48.368 L554.001,97.731 C549.643,97.356 545.279,96.912 540.945,96.412 L505.035,56.241 C501.399,52.181 495.988,50.367 490.767,51.439 C485.512,52.531 481.194,56.361 479.334,61.59 L460.932,120.928 C456.672,119.649 452.451,118.533 448.241,116.917 L405.597,84.91 C401.261,81.644 395.617,80.955 390.681,83.102 C385.762,85.229 382.22,89.885 381.368,95.368 L372.907,149.883 C368.993,152.0 365.118,153.777 361.254,156.0 L313.412,133.253 C308.554,130.937 302.883,131.415 298.458,134.519 C294.013,137.602 291.416,142.885 291.618,148.444 L293.453,203.358 C290.048,206.198 286.679,209.096 283.341,212.031 L232.062,199.423 C226.852,198.158 221.392,199.774 217.621,203.706 C213.826,207.652 212.29,213.337 213.508,218.77 L225.587,272.26 C222.785,275.727 220.012,279.231 217.294,282.781 L164.639,280.868 C159.325,280.711 154.253,283.365 151.283,288.001 C148.31,292.614 147.86,298.522 150.07,303.609 L171.886,353.512 C169.767,357.52 167.662,361.56 165.637,365.662 L113.75,374.467 C108.48,375.355 104.04,379.033 101.993,384.177 C99.948,389.334 100.618,395.21 103.718,399.732 L134.418,444.183 C134.178,444.967 133.958,445.76 133.718,446.547 L46.948,538.717 C46.948,538.717 33.658,549.14 53.018,573.633 C70.088,595.238 157.744,680.885 204.038,725.756 C223.592,748.535 236.986,762.642 238.387,761.422 C240.595,759.482 229.0,718.037 190.485,669.41 C160.8,625.781 122.27,558.677 131.31,550.943 C131.31,550.943 141.58,537.898 162.203,528.508 C162.956,529.111 161.422,527.91 162.203,528.508 C162.203,528.508 597.355,729.24 1001.0,531.904 C1047.108,523.631 1075.023,548.334 1075.023,548.334 C1084.651,553.914 1059.722,622.912 1039.225,668.588 C1011.344,720.451 1007.255,760.92 1009.621,762.324 C1011.112,763.213 1021.255,747.779 1035.725,723.215 C1071.427,673.467 1138.916,578.629 1151.27,555.595 C1165.276,529.476 1151.27,522.156 1151.27,522.156"
                , SA.fill "rgb(247,76,0)"
                ]
                []

            -- Left eye socket (black) — g translate(450.328,483.629) paths translated
            , Svg.path
                [ SA.d "M450.328,650.959 C448.664,649.539 447.792,648.697 447.792,648.697 L590.334,637.02 C474.061,483.629 381.91,605.822 370.995,619.484 L370.995,650.959 Z"
                , SA.fill "black"
                ]
                []

            -- Right eye socket (black) — g translate(747.12,477.333) paths translated
            , Svg.path
                [ SA.d "M747.12,649.307 C748.783,647.887 749.656,647.043 749.656,647.043 L612.672,637.02 C729.0,477.333 816.541,604.168 826.455,617.83 L826.455,649.307 Z"
                , SA.fill "black"
                ]
                []

            -- Left eye (white) — g translate(520.766,436.428)
            , Svg.ellipse
                [ SA.cx "495.593"
                , SA.cy "436.428"
                , SA.rx "25.173"
                , SA.ry "34.627"
                , SA.fill "white"
                ]
                []

            -- Right eye (white) — g translate(727.738,435.209)
            , Svg.ellipse
                [ SA.cx "703.33"
                , SA.cy "435.209"
                , SA.rx "24.408"
                , SA.ry "33.574"
                , SA.fill "white"
                ]
                []

            -- Mouth/chin area — g translate(441.397,687.635)
            , Svg.path
                [ SA.d "M441.397,662.533 C533.23,650.957 586.301,649.879 586.301,649.879 C463.434,487.895 363.736,634.535 363.736,634.535 C338.754,625.603 313.283,590.922 294.259,558.945 L218.022,536.365 C305.895,685.506 371.317,687.779 371.317,687.779 C507.531,862.369 572.06,722.074 572.06,722.074 C495.592,713.392 441.397,662.533 441.397,662.533"
                , SA.fill "rgb(247,76,0)"
                ]
                []

            -- Right face/cheek area — g translate(966.094,811.034)
            , Svg.path
                [ SA.d "M966.094,497.02 C966.094,497.02 950.518,559.061 853.641,624.258 L826.475,630.625 C826.475,630.625 738.594,470.366 614.092,650.959 C614.092,650.959 652.894,628.368 756.914,655.879 C756.914,655.879 709.064,729.118 612.672,726.868 C612.672,726.868 705.045,837.688 845.612,677.616 C845.612,677.616 994.207,620.153 1006.258,497.02 L966.094,497.02"
                , SA.fill "rgb(247,76,0)"
                ]
                []

            -- Helmet antenna
            , Svg.line
                [ SA.x1 "780"
                , SA.y1 "140"
                , SA.x2 "820"
                , SA.y2 "60"
                , SA.stroke "rgba(168,85,247,0.5)"
                , SA.strokeWidth "5"
                , SA.strokeLinecap "round"
                ]
                []
            , Svg.circle
                [ SA.cx "820"
                , SA.cy "50"
                , SA.r "14"
                , SA.fill "rgba(168,85,247,0.4)"
                ]
                []
            ]
        ]


frac : Float -> Float
frac f =
    f - toFloat (floor f)
