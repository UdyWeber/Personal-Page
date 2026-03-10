module Cards exposing (Crystal, CrystalCategory(..), crystalData, viewCrystalCard)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Set exposing (Set)


type CrystalCategory
    = Experience
    | Education
    | Project


type alias Crystal =
    { category : CrystalCategory
    , title : String
    , org : String
    , period : String
    , summary : String
    , details : List String
    }


crystalData : List Crystal
crystalData =
    [ { category = Experience
      , title = "Software Developer"
      , org = "SAP Latin America // Datasphere Spark Replication"
      , period = "2025 - present"
      , summary = "Data replication platform using Apache Spark with Java and modern tooling."
      , details =
            [ "Developed and maintained a platform for data replication jobs using Apache Spark"
            , "Introduced Just, UV and Apache Datafusion to the team"
            , "Improved team performance enhancing dev experience and onboarding"
            , "Reduced code complexity leveraging Java functional features and data oriented design"
            , "Implemented end-to-end integration tests using dev containers"
            ]
      }
    , { category = Experience
      , title = "Inner Source Maintainer"
      , org = "SAP Latin America // Hermes"
      , period = "2024 - present"
      , summary = "Maintained inner source project with strict typing, linting and security tooling."
      , details =
            [ "Improved dev experience introducing new tools and automations"
            , "Introduced strict code linting with Ruff, Mypy and Pyrefly"
            , "Added strict typing for better maintainability"
            , "Implemented CheckMarxOne security scan reports for auditing"
            , "Refactored legacy code using Pydantic and modern Python features"
            ]
      }
    , { category = Experience
      , title = "Software Developer"
      , org = "SAP Latin America // CIDS"
      , period = "2024"
      , summary = "Backend APIs and multi-tenant migration tooling with performance-focused SQL."
      , details =
            [ "Optimized backend queries providing SQL performance insights to coworkers"
            , "Delivered backend APIs for multi-tenant environment migration tool"
            , "Collaborated in code review ensuring best practices"
            , "Delivered UI and Backend tests using JUnit5, Mockito and Selenium"
            ]
      }
    , { category = Education
      , title = "B.Sc Computer Science"
      , org = "Universidade do Vale dos Sinos"
      , period = "2023 - 2027"
      , summary = "Undergraduate focusing on systems programming, algorithms and OS design."
      , details =
            [ "Analysis and Application of Operating Systems"
            , "Data Structures and Algorithms"
            , "Digital Systems Design"
            , "Low Level Programming using C for Linux"
            , "Basic UML design concepts and Business Logic"
            ]
      }
    , { category = Project
      , title = "Sand Simulator"
      , org = "C++ / SDL2"
      , period = "2025"
      , summary = "Graphic simulation of falling sand with mouse interaction and color effects."
      , details =
            [ "Parametrizable properties for custom simulations"
            , "HUE color changing effect based on current tick"
            , "Mouse interaction to spawn particles on click and drag"
            , "1D array optimization for falling and stacking physics"
            ]
      }
    , { category = Project
      , title = "Spylogger"
      , org = "Go / Educational"
      , period = "2024"
      , summary = "Educational keylogger and screenshot tool with real-time Discord integration."
      , details =
            [ "Background keylogging via Windows syscalls"
            , "Screenshot capture on every Enter key press"
            , "Discord integration for real-time log + screenshot delivery"
            , "Background process using fork and detached Windows processes"
            ]
      }
    ]


viewCrystalCard : (Int -> msg) -> Set Int -> Int -> Crystal -> Html msg
viewCrystalCard toggleMsg expandedSet idx crystal =
    let
        isExpanded =
            Set.member idx expandedSet

        catLabel =
            case crystal.category of
                Experience ->
                    "EXP"

                Education ->
                    "EDU"

                Project ->
                    "PRJ"

        catColor =
            case crystal.category of
                Experience ->
                    "168,85,247"

                Education ->
                    "59,130,246"

                Project ->
                    "217,70,239"

        idxStr =
            String.padLeft 2 '0' (String.fromInt (idx + 1))

        delayStr =
            String.fromFloat (toFloat idx * 0.08) ++ "s"

        arrowChar =
            if isExpanded then
                "v"

            else
                ">"
    in
    div
        [ class "card-link"
        , style "display" "grid"
        , style "grid-template-columns" "48px 1fr auto"
        , style "gap" "0"
        , style "border" ("1px solid rgba(" ++ catColor ++ ",0.08)")
        , style "background" ("linear-gradient(135deg, rgba(" ++ catColor ++ ",0.02) 0%, rgba(10,10,12,0.95) 100%)")
        , style "padding" "0"
        , style "position" "relative"
        , style "overflow" "hidden"
        , style "animation" ("fadeUp 0.6s ease " ++ delayStr ++ " both")
        , style "cursor" "pointer"
        , onClick (toggleMsg idx)
        ]
        [ div
            [ class "card-scan"
            , style "position" "absolute"
            , style "top" "-4px"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "1px"
            , style "background" ("linear-gradient(90deg, transparent, rgba(" ++ catColor ++ ",0.4), transparent)")
            , style "opacity" "0"
            , style "pointer-events" "none"
            , style "z-index" "5"
            ]
            []
        , div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "padding" "20px 0"
            , style "border-right" ("1px solid rgba(" ++ catColor ++ ",0.06)")
            ]
            [ div
                [ class "card-idx"
                , style "font-family" "'Space Mono', monospace"
                , style "font-size" "13px"
                , style "color" ("rgba(" ++ catColor ++ ",0.35)")
                , style "transition" "color 0.3s ease"
                ]
                [ text idxStr ]
            ]
        , div
            [ style "padding" "20px 24px" ]
            ([ div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "12px"
                , style "margin-bottom" "10px"
                ]
                [ span
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "11px"
                    , style "letter-spacing" "0.2em"
                    , style "color" ("rgba(" ++ catColor ++ ",0.55)")
                    , style "border" ("1px solid rgba(" ++ catColor ++ ",0.15)")
                    , style "padding" "2px 8px"
                    ]
                    [ text catLabel ]
                , span
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "12px"
                    , style "color" "rgba(196,195,200,0.3)"
                    ]
                    [ text crystal.period ]
                ]
             , div
                [ style "font-size" "18px"
                , style "font-weight" "700"
                , style "color" "#eeedf0"
                , style "letter-spacing" "0.02em"
                , style "margin-bottom" "3px"
                ]
                [ text crystal.title ]
             , div
                [ style "font-family" "'Space Mono', monospace"
                , style "font-size" "13px"
                , style "color" ("rgba(" ++ catColor ++ ",0.4)")
                , style "margin-bottom" "10px"
                ]
                [ text crystal.org ]
             , div
                [ style "font-size" "14px"
                , style "color" "rgba(196,195,200,0.5)"
                , style "line-height" "1.5"
                , style "margin-bottom" (if isExpanded then "14px" else "0")
                ]
                [ text crystal.summary ]
             ]
                ++ (if isExpanded then
                        [ div
                            [ style "border-top" ("1px solid rgba(" ++ catColor ++ ",0.08)")
                            , style "padding-top" "12px"
                            ]
                            (List.indexedMap
                                (\di detail ->
                                    div
                                        [ style "display" "flex"
                                        , style "gap" "10px"
                                        , style "padding" "4px 0"
                                        ]
                                        [ span
                                            [ style "font-family" "'Space Mono', monospace"
                                            , style "font-size" "11px"
                                            , style "color" ("rgba(" ++ catColor ++ ",0.25)")
                                            , style "min-width" "12px"
                                            , style "padding-top" "2px"
                                            ]
                                            [ text (String.fromInt (di + 1) ++ ".") ]
                                        , span
                                            [ style "font-size" "13px"
                                            , style "color" "rgba(196,195,200,0.55)"
                                            , style "line-height" "1.5"
                                            ]
                                            [ text detail ]
                                        ]
                                )
                                crystal.details
                            )
                        ]

                    else
                        []
                   )
            )
        , div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "padding" "0 20px"
            ]
            [ div
                [ class "card-arrow"
                , style "font-family" "'Space Mono', monospace"
                , style "font-size" "16px"
                , style "color" ("rgba(" ++ catColor ++ ",0.2)")
                , style "opacity" "0.5"
                , style "transition" "opacity 0.3s ease, transform 0.3s ease"
                ]
                [ text arrowChar ]
            ]
        ]
