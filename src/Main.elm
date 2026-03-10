module Main exposing (main)

import Browser
import Browser.Events
import Cards exposing (crystalData, viewCrystalCard)
import Game exposing (GameState, Keys, initGame, updateGame, viewAsteroidsGame)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, src, style, target)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Sand exposing (SandState, initSand, viewSandPanel)
import Set exposing (Set)
import Starfield exposing (viewStarfield)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type FocusedPanel
    = NoPanel
    | AsteroidsPanel
    | SandPanel


type alias Model =
    { time : Float
    , keys : Keys
    , game : GameState
    , sand : SandState
    , focusedPanel : FocusedPanel
    , expandedCards : Set Int
    , terminalLines : List String
    , terminalCurrentLine : String
    , terminalCharIdx : Int
    , terminalTimer : Float
    }


terminalContent : List String
terminalContent =
    [ "$ whoami"
    , "jaw // joao arthur weber"
    , "$ cat /etc/motd"
    , "welcome to the void. you are visitor #4091."
    , "$ uptime"
    , "system has been running for 22 years, 8 months"
    , "$ ls -la ~/skills"
    , "drwxr-xr-x  python  sql  rust  java  scala"
    , "drwxr-xr-x  golang  c++  c  elixir  lua"
    , "drwxr-xr-x  docker  neovim  git  unix  spark"
    , "$ cat ~/status"
    , "currently building data pipelines @ SAP"
    , "studying CS @ Unisinos"
    , "hacking on side projects in Rust and C++"
    , "$ fortune"
    , "\"the best code is no code at all\" - someone wise"
    , "$ ping github.com/joaoarthurweber"
    , "PING github.com ... 64 bytes: time=0.42ms"
    , "$ echo $EDITOR"
    , "neovim"
    , "$ history | tail -5"
    , "1337  elm make src/Main.elm --output=elm.js"
    , "1338  cargo build --release"
    , "1339  just test"
    , "1340  docker compose up -d"
    , "1341  git push origin main"
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , keys = { left = False, right = False, up = False, space = False }
      , game = initGame
      , sand = initSand
      , focusedPanel = NoPanel
      , expandedCards = Set.empty
      , terminalLines = []
      , terminalCurrentLine = ""
      , terminalCharIdx = 0
      , terminalTimer = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Frame Float
    | KeyDown String
    | KeyUp String
    | ToggleCard Int
    | FocusPanel FocusedPanel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame delta ->
            let
                newTime =
                    model.time + delta

                ts =
                    advanceTerminal model.terminalTimer delta model.terminalLines model.terminalCurrentLine model.terminalCharIdx
            in
            ( { model
                | time = newTime
                , game = updateGame delta model.keys model.game
                , terminalLines = ts.lines
                , terminalCurrentLine = ts.current
                , terminalCharIdx = ts.charIdx
                , terminalTimer = ts.timer
              }
            , Cmd.none
            )

        KeyDown key ->
            if model.focusedPanel == AsteroidsPanel then
                ( { model | keys = setKey key True model.keys }, Cmd.none )

            else
                ( model, Cmd.none )

        KeyUp key ->
            if model.focusedPanel == AsteroidsPanel then
                let
                    newKeys =
                        setKey key False model.keys

                    newGame =
                        if key == "r" && model.game.gameOver then
                            initGame

                        else
                            model.game
                in
                ( { model | keys = newKeys, game = newGame }, Cmd.none )

            else
                ( model, Cmd.none )

        ToggleCard idx ->
            let
                newSet =
                    if Set.member idx model.expandedCards then
                        Set.remove idx model.expandedCards

                    else
                        Set.insert idx model.expandedCards
            in
            ( { model | expandedCards = newSet }, Cmd.none )

        FocusPanel panel ->
            ( { model | focusedPanel = panel }, Cmd.none )


type alias TerminalState =
    { lines : List String
    , current : String
    , charIdx : Int
    , timer : Float
    }


advanceTerminal : Float -> Float -> List String -> String -> Int -> TerminalState
advanceTerminal timer delta lines current charIdx =
    let
        newTimer =
            timer + delta

        typingSpeed =
            35

        totalLines =
            List.length terminalContent
    in
    if totalLines == 0 then
        { lines = lines, current = current, charIdx = charIdx, timer = newTimer }

    else
        let
            lineIdx =
                List.length lines

            wrappedLineIdx =
                modBy totalLines lineIdx
        in
        if newTimer >= typingSpeed then
            case getAt wrappedLineIdx terminalContent of
                Nothing ->
                    { lines = lines, current = current, charIdx = charIdx, timer = 0 }

                Just targetLine ->
                    if charIdx >= String.length targetLine then
                        if newTimer >= typingSpeed + 400 then
                            { lines = lines ++ [ targetLine ], current = "", charIdx = 0, timer = 0 }

                        else
                            { lines = lines, current = current, charIdx = charIdx, timer = newTimer }

                    else
                        { lines = lines, current = String.left (charIdx + 1) targetLine, charIdx = charIdx + 1, timer = 0 }

        else
            { lines = lines, current = current, charIdx = charIdx, timer = newTimer }


getAt : Int -> List a -> Maybe a
getAt idx list =
    List.head (List.drop idx list)


setKey : String -> Bool -> Keys -> Keys
setKey key val keys =
    case key of
        "ArrowLeft" ->
            { keys | left = val }

        "a" ->
            { keys | left = val }

        "ArrowRight" ->
            { keys | right = val }

        "d" ->
            { keys | right = val }

        "ArrowUp" ->
            { keys | up = val }

        "w" ->
            { keys | up = val }

        " " ->
            { keys | space = val }

        _ ->
            keys



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onKeyDown (Decode.field "key" Decode.string |> Decode.map KeyDown)
        , Browser.Events.onKeyUp (Decode.field "key" Decode.string |> Decode.map KeyUp)
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "JAW"
    , body =
        [ injectStyles
        , viewPage model
        ]
    }


injectStyles : Html Msg
injectStyles =
    Html.node "style"
        []
        [ text (String.join "\n"
            [ "@import url('https://fonts.googleapis.com/css2?family=Space+Mono:ital,wght@0,400;0,700;1,400&family=DM+Sans:wght@300;400;500;700&display=swap');"
            , ""
            , "*, *::before, *::after { margin: 0; padding: 0; box-sizing: border-box; }"
            , "body { overflow-x: hidden; background: #0a0a0c; }"
            , "::selection { background: rgba(168,85,247,0.35); color: #fff; }"
            , ""
            , "@keyframes blink { 0%,100%{opacity:1} 50%{opacity:0} }"
            , "@keyframes fadeUp { from{opacity:0;transform:translateY(20px)} to{opacity:1;transform:translateY(0)} }"
            , "@keyframes fadeIn { from{opacity:0} to{opacity:1} }"
            , "@keyframes scanline { from{top:-4px} to{top:100%} }"
            , "@keyframes drift { 0%,100%{transform:translateY(0)} 50%{transform:translateY(-6px)} }"
            , "@keyframes glitchSlice { 0%,100%{clip-path:inset(0 0 0 0)} 25%{clip-path:inset(20% 0 60% 0)} 50%{clip-path:inset(60% 0 10% 0)} 75%{clip-path:inset(40% 0 30% 0)} }"
            , "@keyframes flicker { 0%,19%,21%,23%,25%,54%,56%,100%{opacity:1} 20%,24%,55%{opacity:0.4} }"
            , "@keyframes marquee { from{transform:translateX(100%)} to{transform:translateX(-100%)} }"
            , "@keyframes accrete { 0%{transform:rotate(0deg)} 100%{transform:rotate(360deg)} }"
            , "@keyframes termCursor { 0%,100%{opacity:1} 50%{opacity:0} }"
            , ""
            , ".card-link { transition: border-color 0.3s ease, box-shadow 0.3s ease, transform 0.3s ease; }"
            , ".card-link:hover { border-color: rgba(168,85,247,0.5) !important; box-shadow: 0 0 30px rgba(168,85,247,0.12), inset 0 1px 0 rgba(168,85,247,0.1) !important; transform: translateY(-4px) !important; }"
            , ".card-link:hover .card-idx { color: rgba(168,85,247,0.8) !important; }"
            , ".card-link:hover .card-arrow { opacity: 1 !important; transform: translateX(4px) !important; }"
            , ".card-link:hover .card-scan { opacity: 1 !important; animation: scanline 1.5s linear infinite !important; }"
            , ""
            , ".footer-link { color: rgba(168,85,247,0.6); text-decoration: none; transition: color 0.2s; font-family: 'Space Mono', monospace; font-size: 12px; letter-spacing: 0.05em; }"
            , ".footer-link:hover { color: rgba(168,85,247,0.8); }"
            , ""
            , ".social-link { color: rgba(168,85,247,0.55); text-decoration: none; transition: color 0.3s ease, text-shadow 0.3s ease; }"
            , ".social-link:hover { color: rgba(168,85,247,0.9); text-shadow: 0 0 12px rgba(168,85,247,0.3); }"
            , ""
            , "@media (max-width: 900px) {"
            , "  .section-grid { grid-template-columns: 1fr !important; }"
            , "}"
            , ""
            , ".ase-checkerboard {"
            , "  background-image: linear-gradient(45deg, #1a1a2e 25%, transparent 25%), linear-gradient(-45deg, #1a1a2e 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #1a1a2e 75%), linear-gradient(-45deg, transparent 75%, #1a1a2e 75%);"
            , "  background-size: 16px 16px;"
            , "  background-position: 0 0, 0 8px, 8px -8px, -8px 0px;"
            , "  background-color: #252540;"
            , "}"
            , ".ase-tool { width: 18px; height: 18px; border: 1px solid #3a3a5c; background: #2a2a48; display: flex; align-items: center; justify-content: center; font-size: 9px; color: #8888aa; cursor: default; }"
            , ".ase-tool:hover { background: #3a3a5c; color: #bbbbdd; }"
            , ".ase-color-swatch { width: 14px; height: 14px; border: 1px solid #4a4a6c; }"
            , ""
            ])
        ]


viewPage : Model -> Html Msg
viewPage model =
    div
        [ style "width" "100vw"
        , style "min-height" "100vh"
        , style "background" "#0a0a0c"
        , style "color" "#c4c3c8"
        , style "font-family" "'DM Sans', sans-serif"
        , style "position" "relative"
        , onClick (FocusPanel NoPanel)
        ]
        [ viewStarfield model.time
        , viewCrt
        , viewHero
        , viewMarquee
        , viewMainContent model
        , viewFooter
        ]


viewCrt : Html Msg
viewCrt =
    div []
        [ div
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "background" "repeating-linear-gradient(0deg, transparent, transparent 2px, rgba(0,0,0,0.08) 2px, rgba(0,0,0,0.08) 4px)"
            , style "pointer-events" "none"
            , style "z-index" "9999"
            ]
            []
        , div
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "background" "radial-gradient(ellipse at center, transparent 50%, rgba(10,10,12,0.8) 100%)"
            , style "pointer-events" "none"
            , style "z-index" "9998"
            ]
            []
        ]


viewHero : Html Msg
viewHero =
    div
        [ style "position" "relative"
        , style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "overflow" "hidden"
        , style "z-index" "1"
        ]
        [ viewGridFloor
        , div
            [ style "position" "relative"
            , style "z-index" "10"
            , style "text-align" "center"
            , style "animation" "fadeUp 1.2s cubic-bezier(0.16,1,0.3,1) forwards"
            ]
            [ div
                [ style "font-family" "'Space Mono', monospace"
                , style "font-size" "13px"
                , style "color" "rgba(168,85,247,0.6)"
                , style "letter-spacing" "0.15em"
                , style "margin-bottom" "32px"
                , style "animation" "fadeIn 1s ease 0.3s both"
                ]
                [ text "visitor@jaw ~ $ whoami" ]
            , div
                [ style "position" "relative"
                , style "display" "inline-block"
                ]
                [ div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "clamp(80px, 16vw, 220px)"
                    , style "font-weight" "700"
                    , style "color" "rgba(168,85,247,0.08)"
                    , style "letter-spacing" "0.3em"
                    , style "line-height" "0.9"
                    , style "position" "absolute"
                    , style "top" "4px"
                    , style "left" "4px"
                    , style "user-select" "none"
                    , style "animation" "glitchSlice 8s steps(1) infinite 3s"
                    ]
                    [ text "JAW" ]
                , div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "clamp(80px, 16vw, 220px)"
                    , style "font-weight" "700"
                    , style "color" "#eeedf0"
                    , style "letter-spacing" "0.3em"
                    , style "line-height" "0.9"
                    , style "position" "relative"
                    , style "user-select" "none"
                    , style "text-shadow" "0 0 80px rgba(168,85,247,0.2), 0 0 160px rgba(168,85,247,0.05)"
                    , style "animation" "flicker 10s ease infinite 2s"
                    ]
                    [ text "JAW" ]
                ]
            , div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , style "gap" "8px"
                , style "margin-top" "28px"
                , style "animation" "fadeIn 1s ease 0.8s both"
                ]
                [ div [ style "width" "40px", style "height" "1px", style "background" "rgba(168,85,247,0.2)" ] []
                , div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "14px"
                    , style "color" "rgba(168,85,247,0.7)"
                    , style "letter-spacing" "0.3em"
                    ]
                    [ text "JOAO ARTHUR WEBER" ]
                , div
                    [ style "width" "8px"
                    , style "height" "16px"
                    , style "background" "rgba(168,85,247,0.6)"
                    , style "animation" "blink 1s steps(1) infinite"
                    ]
                    []
                , div [ style "width" "40px", style "height" "1px", style "background" "rgba(168,85,247,0.2)" ] []
                ]
            , div
                [ style "max-width" "540px"
                , style "margin" "24px auto 0"
                , style "font-size" "15px"
                , style "color" "rgba(196,195,200,0.8)"
                , style "line-height" "1.7"
                , style "text-align" "center"
                , style "letter-spacing" "0.01em"
                , style "animation" "fadeIn 1s ease 1.0s both"
                ]
                [ text "Software developer, enamoured with learning programming languages and weird tech, with an estrange appeal for distributed systems and Rust — always enhancing my craft and looking for problems to solve. When I'm not buried in code (on neovim btw), you'll find me drawing pixel art (with far more enthusiasm than skill), singing my heart out to whatever song is stuck in my head, or diving into some obscure technology rabbit hole just because it looked interesting. I believe the best way to learn is to build things that probably shouldn't exist." ]
            , div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , style "gap" "24px"
                , style "margin-top" "24px"
                , style "animation" "fadeIn 1s ease 1.1s both"
                ]
                [ viewSocialLink "github" "https://github.com/joaoarthurweber"
                , viewSocialLink "linkedin" "https://linkedin.com/in/joaoarthurweber"
                , viewSocialLink "email" "mailto:joaoarthurweber@gmail.com"
                ]
            , div
                [ style "margin-top" "48px"
                , style "font-family" "'Space Mono', monospace"
                , style "font-size" "12px"
                , style "color" "rgba(168,85,247,0.4)"
                , style "letter-spacing" "0.2em"
                , style "animation" "fadeIn 1s ease 1.4s both, drift 3s ease infinite 2s"
                ]
                [ text "[ scroll ]" ]
            ]
        ]


viewSocialLink : String -> String -> Html Msg
viewSocialLink label url =
    a
        [ class "social-link"
        , href url
        , target "_blank"
        , style "font-family" "'Space Mono', monospace"
        , style "font-size" "12px"
        , style "letter-spacing" "0.15em"
        , style "text-transform" "uppercase"
        ]
        [ text label ]


viewGridFloor : Html Msg
viewGridFloor =
    div
        [ style "position" "absolute"
        , style "bottom" "0"
        , style "left" "-10%"
        , style "width" "120%"
        , style "height" "45vh"
        , style "background-image"
            "linear-gradient(rgba(168,85,247,0.04) 1px, transparent 1px), linear-gradient(90deg, rgba(168,85,247,0.04) 1px, transparent 1px)"
        , style "background-size" "70px 70px"
        , style "transform" "perspective(400px) rotateX(55deg)"
        , style "transform-origin" "center bottom"
        , style "mask-image" "linear-gradient(to top, rgba(0,0,0,0.3) 0%, transparent 70%)"
        , style "-webkit-mask-image" "linear-gradient(to top, rgba(0,0,0,0.3) 0%, transparent 70%)"
        ]
        []


viewMarquee : Html Msg
viewMarquee =
    div
        [ style "position" "relative"
        , style "overflow" "hidden"
        , style "border-top" "1px solid rgba(168,85,247,0.08)"
        , style "border-bottom" "1px solid rgba(168,85,247,0.08)"
        , style "padding" "12px 0"
        , style "background" "rgba(168,85,247,0.015)"
        , style "z-index" "1"
        ]
        [ div
            [ style "display" "inline-block"
            , style "white-space" "nowrap"
            , style "animation" "marquee 30s linear infinite"
            , style "font-family" "'Space Mono', monospace"
            , style "font-size" "12px"
            , style "letter-spacing" "0.2em"
            , style "color" "rgba(168,85,247,0.7)"
            , style "text-transform" "uppercase"
            ]
            [ text "python  //  sql  //  rust  //  java  //  scala  //  golang  //  c++  //  c  //  elixir  //  lua  //  bash  //  docker  //  neovim  //  git  //  unix  //  spark  //  datafusion  //  python  //  sql  //  rust  //  java" ]
        ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    let
        cards =
            List.indexedMap (viewCrystalCard ToggleCard model.expandedCards) crystalData

        getCard idx =
            case List.head (List.drop idx cards) of
                Just card ->
                    card

                Nothing ->
                    text ""
    in
    div
        [ style "max-width" "1200px"
        , style "margin" "0 auto"
        , style "padding" "80px 24px 120px"
        , style "position" "relative"
        , style "z-index" "1"
        ]
        [ div
            [ style "margin-bottom" "56px" ]
            [ div
                [ style "font-family" "'Space Mono', monospace"
                , style "font-size" "12px"
                , style "color" "rgba(168,85,247,0.55)"
                , style "letter-spacing" "0.15em"
                , style "margin-bottom" "12px"
                ]
                [ text "$ cat /data/fragments.log" ]
            , div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "16px"
                ]
                [ div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "clamp(24px, 3.5vw, 36px)"
                    , style "font-weight" "700"
                    , style "color" "#eeedf0"
                    , style "letter-spacing" "0.1em"
                    ]
                    [ text "INDEX" ]
                , div [ style "flex" "1", style "height" "1px", style "background" "linear-gradient(90deg, rgba(168,85,247,0.15), transparent)" ] []
                , div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "12px"
                    , style "color" "rgba(168,85,247,0.45)"
                    ]
                    [ text (String.fromInt (List.length crystalData) ++ " entries") ]
                ]
            ]

        -- Section: Sandbox (first experience)
        , viewSectionDivider "SANDBOX"
        , div
            [ style "margin-bottom" "48px"
            ]
            [ viewSandPanel model.sand
            ]

        -- Section: Experience
        , viewSectionDivider "EXPERIENCE"
        , div
            [ class "section-grid"
            , style "display" "grid"
            , style "grid-template-columns" "1fr 420px"
            , style "gap" "20px"
            , style "align-items" "start"
            , style "margin-bottom" "48px"
            ]
            [ div [ style "display" "flex", style "flex-direction" "column", style "gap" "20px" ]
                [ getCard 0
                , getCard 1
                , getCard 2
                ]
            , div
                [ stopPropagationOn "click" (Decode.succeed ( FocusPanel AsteroidsPanel, True ))
                ]
                [ viewAsteroidsGame model.game
                ]
            ]

        -- Section: Education (full width)
        , viewSectionDivider "EDUCATION"
        , div
            [ style "margin-bottom" "48px"
            ]
            [ getCard 3
            ]

        -- Section: Projects
        , viewSectionDivider "PROJECTS"
        , div
            [ class "section-grid"
            , style "display" "grid"
            , style "grid-template-columns" "1fr 1fr"
            , style "gap" "20px"
            , style "align-items" "start"
            , style "margin-bottom" "48px"
            ]
            [ div [ style "display" "flex", style "flex-direction" "column", style "gap" "20px" ]
                [ getCard 4
                , getCard 5
                ]
            , viewTerminal model
            ]

        -- Gallery
        , viewSectionDivider "GALLERY"
        , div
            [ class "section-grid"
            , style "display" "grid"
            , style "grid-template-columns" "1fr 1fr"
            , style "gap" "20px"
            , style "align-items" "start"
            ]
            [ viewAsepritePanel "doggo.png" "doggo.png" 64 64 0
            , viewAsepritePanel "quacky.png" "quacky.png" 64 64 2
            ]
        ]


viewSectionDivider : String -> Html Msg
viewSectionDivider label =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "12px"
        , style "margin-bottom" "24px"
        ]
        [ div
            [ style "font-family" "'Space Mono', monospace"
            , style "font-size" "11px"
            , style "color" "rgba(168,85,247,0.5)"
            , style "letter-spacing" "0.15em"
            , style "white-space" "nowrap"
            ]
            [ text ("// " ++ label) ]
        , div
            [ style "flex" "1"
            , style "height" "1px"
            , style "background" "linear-gradient(90deg, rgba(168,85,247,0.1), transparent)"
            ]
            []
        ]


viewAsepritePanel : String -> String -> Int -> Int -> Int -> Html Msg
viewAsepritePanel filename imagePath width height selectedFrame =
    let
        aseBg =
            "#232338"

        aseBorder =
            "#3a3a5c"

        aseText =
            "#8888aa"

        aseTextBright =
            "#bbbbdd"

        aseHighlight =
            "#5c5c8a"
    in
    div
        [ style "border" ("1px solid " ++ aseBorder)
        , style "background" aseBg
        , style "overflow" "hidden"
        , style "animation" "fadeUp 0.6s ease 0.4s both"
        ]
        [ -- Title bar
          div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "space-between"
            , style "padding" "4px 8px"
            , style "background" "#1e1e32"
            , style "border-bottom" ("1px solid " ++ aseBorder)
            ]
            [ div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "8px"
                ]
                [ div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "9px"
                    , style "color" aseTextBright
                    , style "letter-spacing" "0.05em"
                    ]
                    [ text ("Aseprite - " ++ filename) ]
                ]
            , div
                [ style "display" "flex"
                , style "gap" "4px"
                ]
                [ div [ style "width" "10px", style "height" "10px", style "border" ("1px solid " ++ aseBorder), style "font-size" "7px", style "color" aseText, style "display" "flex", style "align-items" "center", style "justify-content" "center" ] [ text "_" ]
                , div [ style "width" "10px", style "height" "10px", style "border" ("1px solid " ++ aseBorder), style "font-size" "7px", style "color" aseText, style "display" "flex", style "align-items" "center", style "justify-content" "center" ] [ text "□" ]
                , div [ style "width" "10px", style "height" "10px", style "border" ("1px solid " ++ aseBorder), style "font-size" "7px", style "color" aseText, style "display" "flex", style "align-items" "center", style "justify-content" "center" ] [ text "x" ]
                ]
            ]
        , -- Menu bar
          div
            [ style "display" "flex"
            , style "gap" "0"
            , style "padding" "2px 4px"
            , style "background" "#282845"
            , style "border-bottom" ("1px solid " ++ aseBorder)
            ]
            (List.map
                (\item ->
                    div
                        [ style "font-family" "'Space Mono', monospace"
                        , style "font-size" "9px"
                        , style "color" aseText
                        , style "padding" "2px 8px"
                        ]
                        [ text item ]
                )
                [ "File", "Edit", "Sprite", "Layer", "Frame", "Select", "View" ]
            )
        , -- Main content area with toolbar + canvas
          div
            [ style "display" "flex" ]
            [ -- Left toolbar
              div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "1px"
                , style "padding" "4px 3px"
                , style "background" "#262642"
                , style "border-right" ("1px solid " ++ aseBorder)
                ]
                (List.map
                    (\icon ->
                        div [ class "ase-tool" ] [ text icon ]
                    )
                    [ "✎", "◉", "▬", "◇", "⬚", "✦", "◫", "⊘", "↔", "▲" ]
                )
            , -- Canvas area
              div
                [ style "flex" "1"
                , style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , style "min-height" "300px"
                , class "ase-checkerboard"
                , style "position" "relative"
                ]
                [ Html.img
                    [ src ("assets/" ++ imagePath)
                    , style "image-rendering" "pixelated"
                    , style "image-rendering" "crisp-edges"
                    , style "max-width" "90%"
                    , style "max-height" "280px"
                    , style "object-fit" "contain"
                    ]
                    []
                ]
            ]
        , -- Bottom status / timeline bar
          div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "space-between"
            , style "padding" "3px 8px"
            , style "background" "#1e1e32"
            , style "border-top" ("1px solid " ++ aseBorder)
            ]
            [ div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "12px"
                ]
                [ -- FG/BG color swatches
                  div
                    [ style "position" "relative"
                    , style "width" "24px"
                    , style "height" "24px"
                    ]
                    [ div
                        [ class "ase-color-swatch"
                        , style "position" "absolute"
                        , style "bottom" "0"
                        , style "right" "0"
                        , style "background" "#eeedf0"
                        ]
                        []
                    , div
                        [ class "ase-color-swatch"
                        , style "position" "absolute"
                        , style "top" "0"
                        , style "left" "0"
                        , style "background" "#0a0a0c"
                        ]
                        []
                    ]
                , div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "8px"
                    , style "color" aseText
                    ]
                    [ text (String.fromInt width ++ "x" ++ String.fromInt height ++ "px") ]
                ]
            , -- Timeline frames
              div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "2px"
                ]
                (List.indexedMap
                    (\i _ ->
                        div
                            [ style "width" "20px"
                            , style "height" "16px"
                            , style "border" ("1px solid " ++ (if i == selectedFrame then aseHighlight else aseBorder))
                            , style "background" (if i == selectedFrame then "#3a3a5c" else "#222240")
                            , style "font-family" "'Space Mono', monospace"
                            , style "font-size" "7px"
                            , style "color" (if i == selectedFrame then aseTextBright else aseText)
                            , style "display" "flex"
                            , style "align-items" "center"
                            , style "justify-content" "center"
                            ]
                            [ text (String.fromInt (i + 1)) ]
                    )
                    (List.range 0 4)
                )
            , div
                [ style "font-family" "'Space Mono', monospace"
                , style "font-size" "8px"
                , style "color" aseText
                ]
                [ text ("Frame " ++ String.fromInt (selectedFrame + 1) ++ "/5") ]
            ]
        , -- Color palette bar
          div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "padding" "4px 6px"
            , style "gap" "2px"
            , style "background" "#1a1a30"
            , style "border-top" ("1px solid " ++ aseBorder)
            , style "flex-wrap" "wrap"
            ]
            (List.map
                (\color ->
                    div
                        [ style "width" "10px"
                        , style "height" "10px"
                        , style "background" color
                        , style "border" "1px solid rgba(255,255,255,0.06)"
                        ]
                        []
                )
                [ "#000000", "#1a1a2e", "#2a2a48", "#3a3a5c"
                , "#5c5c8a", "#8888aa", "#bbbbdd", "#eeedf0"
                , "#a855f7", "#7c3aed", "#6d28d9", "#5b21b6"
                , "#d946ef", "#c026d3", "#a21caf", "#86198f"
                , "#3b82f6", "#2563eb", "#1d4ed8", "#1e40af"
                , "#ef4444", "#dc2626", "#b91c1c", "#991b1b"
                , "#f59e0b", "#d97706", "#b45309", "#92400e"
                , "#22c55e", "#16a34a", "#15803d", "#166534"
                ]
            )
        ]


viewTerminal : Model -> Html Msg
viewTerminal model =
    let
        maxVisibleLines =
            12

        allLines =
            model.terminalLines

        visibleLines =
            List.drop (max 0 (List.length allLines - maxVisibleLines)) allLines

        currentTyping =
            model.terminalCurrentLine
    in
    div []
        [ div
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
                , style "font-size" "12px"
                , style "color" "rgba(168,85,247,0.7)"
                , style "letter-spacing" "0.1em"
                ]
                [ text "TERMINAL.SH" ]
            , div
                [ style "display" "flex"
                , style "gap" "6px"
                ]
                [ div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" "rgba(168,85,247,0.2)" ] []
                , div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" "rgba(168,85,247,0.15)" ] []
                , div [ style "width" "8px", style "height" "8px", style "border-radius" "50%", style "background" "rgba(168,85,247,0.1)" ] []
                ]
            ]
        , div
            [ style "background" "rgba(0,0,0,0.6)"
            , style "border" "1px solid rgba(168,85,247,0.1)"
            , style "padding" "16px"
            , style "min-height" "280px"
            , style "max-height" "360px"
            , style "overflow" "hidden"
            , style "font-family" "'Space Mono', monospace"
            , style "font-size" "13px"
            , style "line-height" "1.7"
            ]
            (List.map viewTerminalLine visibleLines
                ++ [ viewTerminalActiveLine currentTyping ]
            )
        ]


viewTerminalLine : String -> Html Msg
viewTerminalLine line =
    let
        isCommand =
            String.startsWith "$" line

        color =
            if isCommand then
                "rgba(168,85,247,0.85)"

            else
                "rgba(196,195,200,0.75)"
    in
    div
        [ style "color" color
        , style "white-space" "pre-wrap"
        , style "word-break" "break-all"
        ]
        [ text line ]


viewTerminalActiveLine : String -> Html Msg
viewTerminalActiveLine current =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        ]
        [ span
            [ style "color" "rgba(168,85,247,0.85)"
            , style "white-space" "pre-wrap"
            ]
            [ text current ]
        , span
            [ style "display" "inline-block"
            , style "width" "7px"
            , style "height" "14px"
            , style "background" "rgba(168,85,247,0.6)"
            , style "margin-left" "1px"
            , style "animation" "termCursor 1s steps(1) infinite"
            ]
            []
        ]


viewFooter : Html Msg
viewFooter =
    div
        [ style "max-width" "1200px"
        , style "margin" "0 auto"
        , style "padding" "0 24px 80px"
        , style "position" "relative"
        , style "z-index" "1"
        ]
        [ div
            [ style "border-top" "1px solid rgba(168,85,247,0.06)"
            , style "padding-top" "32px"
            , style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "center"
            , style "flex-wrap" "wrap"
            , style "gap" "16px"
            ]
            [ div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "gap" "20px"
                ]
                [ div
                    [ style "font-family" "'Space Mono', monospace"
                    , style "font-size" "12px"
                    , style "color" "rgba(168,85,247,0.4)"
                    , style "letter-spacing" "0.1em"
                    ]
                    [ text "JAW // 2026" ]
                , a
                    [ class "social-link"
                    , href "https://github.com/joaoarthurweber"
                    , target "_blank"
                    , style "font-family" "'Space Mono', monospace"
                    , style "font-size" "12px"
                    , style "letter-spacing" "0.05em"
                    ]
                    [ text "github" ]
                , a
                    [ class "social-link"
                    , href "https://linkedin.com/in/joaoarthurweber"
                    , target "_blank"
                    , style "font-family" "'Space Mono', monospace"
                    , style "font-size" "12px"
                    , style "letter-spacing" "0.05em"
                    ]
                    [ text "linkedin" ]
                , a
                    [ class "social-link"
                    , href "mailto:joaoarthurweber@gmail.com"
                    , style "font-family" "'Space Mono', monospace"
                    , style "font-size" "12px"
                    , style "letter-spacing" "0.05em"
                    ]
                    [ text "email" ]
                ]
            , div
                [ style "font-family" "'Space Mono', monospace"
                , style "font-size" "12px"
                , style "color" "rgba(168,85,247,0.35)"
                ]
                [ text "rendered in elm" ]
            ]
        ]
