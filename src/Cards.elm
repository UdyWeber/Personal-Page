module Cards exposing (Crystal, CrystalCategory(..), crystalData, viewCrystalCard)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Theme


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
      , title = "Platform Developer"
      , org = "SAP Latin America // Kings"
      , period = "2025 - present"
      , summary = "High-resilience microservices and data pipelines in Rust with Kafka."
      , details =
            [ "Leveraging Rust to develop high resilient microservices and infrastructure for Data Processing Jobs"
            , "Applying data oriented design principles to build efficient and maintainable codebase for distributed systems"
            , "Collaborating with cross-functional teams to design and implement scalable solutions for data challenges"
            , "Implementing production ready performant solutions using Rust with Kafka for reactive event driven data pipelines"
            , "Bringing innovative solutions and building tools to improve developers experience and satisfaction"
            ]
      }
    , { category = Experience
      , title = "Software Developer"
      , org = "SAP Latin America // Datasphere Spark Replication"
      , period = "2025"
      , summary = "Maintained Spark data replication pipelines while modernizing developer tooling and workflows."
      , details =
            [ "Maintained and evolved Python-based data replication pipelines running on Apache Spark"
            , "Introduced Just, UV and Apache DataFusion to streamline builds and local development"
            , "Improved onboarding and day-to-day developer experience across the team"
            , "Reduced code complexity by leveraging Java functional features and data-oriented design"
            , "Implemented end-to-end integration tests using dev containers for reliable CI validation"
            ]
      }
    , { category = Experience
      , title = "Inner Source Maintainer"
      , org = "SAP Latin America // Hermes"
      , period = "2024 - present"
      , summary = "CI/CD compliance tool that aggregates vulnerability scans across projects for audit reporting."
      , details =
            [ "Maintained an inner-source compliance evaluation tool integrated into CI/CD pipelines"
            , "Enforced strict code quality with Ruff, Mypy and Pyrefly linting"
            , "Introduced full type coverage with Pydantic models for better maintainability"
            , "Implemented CheckMarxOne security scan report aggregation for centralized auditing"
            , "Refactored legacy modules using modern Python patterns and tooling"
            ]
      }
    , { category = Experience
      , title = "Software Developer"
      , org = "SAP Latin America // CIDS"
      , period = "2024"
      , summary = "Built migration APIs to move client data from legacy systems to a Kubernetes-based platform."
      , details =
            [ "Developed backend APIs for migrating client data from legacy environments to a new K8s-based platform"
            , "Optimized SQL queries and shared performance insights with the team"
            , "Participated in code reviews enforcing best practices and consistency"
            , "Delivered UI and backend test suites using JUnit5, Mockito and Selenium"
            ]
      }
    , { category = Education
      , title = "B.Sc Computer Science"
      , org = "Universidade do Vale dos Sinos"
      , period = "2023 - 2027"
      , summary = "Undergraduate degree with emphasis on systems programming, algorithms and OS internals."
      , details =
            [ "OS internals — process scheduling, memory management, kernel design and system calls"
            , "Algorithms and data structures — complexity analysis, graphs, trees and sorting"
            , "Digital systems — logic design, CPU architecture and hardware-software interfaces"
            , "Low-level programming — C on Linux, pointers, manual memory management and syscalls"
            , "Software design — UML modeling, requirements analysis and business logic patterns"
            ]
      }
    , { category = Project
      , title = "Sand Simulator"
      , org = "C++ / SDL2"
      , period = "2025"
      , summary = "Pixel-based falling sand simulation with real-time mouse interaction and hue-cycling visuals."
      , details =
            [ "Configurable particle properties for custom simulation behaviors"
            , "Real-time hue-cycling color effect driven by the simulation tick"
            , "Click-and-drag mouse interaction for spawning particles"
            , "Flat 1D array grid with optimized falling and stacking physics"
            ]
      }
    , { category = Project
      , title = "Spylogger"
      , org = "Go / Educational"
      , period = "2024"
      , summary = "Educational security tool demonstrating keylogging, screen capture and remote exfiltration."
      , details =
            [ "Low-level keylogging through direct Windows syscalls"
            , "Automatic screenshot capture triggered on Enter key press"
            , "Real-time log and screenshot delivery via Discord webhook integration"
            , "Stealth execution using forked and detached background processes"
            ]
      }
    ]


categoryLabel : CrystalCategory -> String
categoryLabel category =
    case category of
        Experience ->
            "EXP"

        Education ->
            "EDU"

        Project ->
            "PRJ"


categoryColor : CrystalCategory -> { r : Int, g : Int, b : Int }
categoryColor category =
    case category of
        Experience ->
            Theme.purple

        Education ->
            Theme.blue

        Project ->
            Theme.fuchsia


viewCardDetail : { r : Int, g : Int, b : Int } -> Int -> String -> Html msg
viewCardDetail color detailIndex detail =
    div
        [ style "display" "flex"
        , style "gap" "10px"
        , style "padding" "4px 0"
        ]
        [ span
            [ style "font-family" Theme.monoFont
            , style "font-size" "11px"
            , style "color" (Theme.rgbaStr color 0.6)
            , style "min-width" "12px"
            , style "padding-top" "2px"
            ]
            [ text (String.fromInt (detailIndex + 1) ++ ".") ]
        , span
            [ style "font-size" "13px"
            , style "color" Theme.textGray
            , style "line-height" "1.5"
            ]
            [ text detail ]
        ]


viewCrystalCard : (Int -> msg) -> Set Int -> Int -> Crystal -> Html msg
viewCrystalCard toggleMsg expandedSet idx crystal =
    let
        isExpanded =
            Set.member idx expandedSet

        label =
            categoryLabel crystal.category

        color =
            categoryColor crystal.category

        indexLabel =
            String.padLeft 2 '0' (String.fromInt (idx + 1))

        animDelay =
            String.fromFloat (toFloat idx * 0.08) ++ "s"

        arrowChar =
            ">"
    in
    div
        [ class "card-link"
        , style "display" "grid"
        , style "grid-template-columns" "48px 1fr auto"
        , style "gap" "0"
        , style "border" ("1px solid " ++ Theme.rgbaStr color 0.08)
        , style "background" ("linear-gradient(135deg, " ++ Theme.rgbaStr color 0.02 ++ " 0%, rgba(10,10,12,0.95) 100%)")
        , style "padding" "0"
        , style "position" "relative"
        , style "overflow" "hidden"
        , style "animation" ("fadeUp 0.6s ease " ++ animDelay ++ " both")
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
            , style "background" ("linear-gradient(90deg, transparent, " ++ Theme.rgbaStr color 0.4 ++ ", transparent)")
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
            , style "border-right" ("1px solid " ++ Theme.rgbaStr color 0.06)
            ]
            [ div
                [ class "card-idx"
                , style "font-family" Theme.monoFont
                , style "font-size" "13px"
                , style "color" (Theme.rgbaStr color 0.7)
                , style "transition" "color 0.3s ease"
                ]
                [ text indexLabel ]
            ]
        , div
            [ style "padding" "20px 24px" ]
            [ div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "12px"
                , style "margin-bottom" "10px"
                ]
                [ span
                    [ style "font-family" Theme.monoFont
                    , style "font-size" "11px"
                    , style "letter-spacing" "0.2em"
                    , style "color" (Theme.rgbaStr color 0.85)
                    , style "border" ("1px solid " ++ Theme.rgbaStr color 0.15)
                    , style "padding" "2px 8px"
                    ]
                    [ text label ]
                , span
                    [ style "font-family" Theme.monoFont
                    , style "font-size" "12px"
                    , style "color" Theme.textGray
                    ]
                    [ text crystal.period ]
                ]
             , div
                [ style "font-size" "18px"
                , style "font-weight" "700"
                , style "color" Theme.textLight
                , style "letter-spacing" "0.02em"
                , style "margin-bottom" "3px"
                ]
                [ text crystal.title ]
             , div
                [ style "font-family" Theme.monoFont
                , style "font-size" "13px"
                , style "color" (Theme.rgbaStr color 0.75)
                , style "margin-bottom" "10px"
                ]
                [ text crystal.org ]
             , div
                [ style "font-size" "14px"
                , style "color" Theme.textGray
                , style "line-height" "1.5"
                ]
                [ text crystal.summary ]
             , div
                [ style "overflow" "hidden"
                , style "transition" "max-height 0.35s ease, opacity 0.3s ease"
                , style "max-height"
                    (if isExpanded then
                        "600px"

                     else
                        "0"
                    )
                , style "opacity"
                    (if isExpanded then
                        "1"

                     else
                        "0"
                    )
                ]
                [ div
                    [ style "border-top" ("1px solid " ++ Theme.rgbaStr color 0.08)
                    , style "padding-top" "12px"
                    , style "margin-top" "14px"
                    ]
                    (List.indexedMap
                        (viewCardDetail color)
                        crystal.details
                    )
                ]
             ]
        , div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "padding" "0 20px"
            ]
            [ div
                [ class "card-arrow"
                , style "font-family" Theme.monoFont
                , style "font-size" "16px"
                , style "color" (Theme.rgbaStr color 0.55)
                , style "opacity" "0.5"
                , style "transition" "opacity 0.3s ease, transform 0.3s ease"
                , style "transform"
                    (if isExpanded then
                        "rotate(90deg)"

                     else
                        "rotate(0deg)"
                    )
                ]
                [ text arrowChar ]
            ]
        ]
