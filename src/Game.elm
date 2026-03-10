module Game exposing (GameState, Keys, initGame, updateGame, viewAsteroidsGame)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Set exposing (Set)
import Theme



-- CONSTANTS


arenaWidth : Float
arenaWidth =
    400


arenaHeight : Float
arenaHeight =
    400


rotationSpeed : Float
rotationSpeed =
    0.06


thrustPower : Float
thrustPower =
    0.08


friction : Float
friction =
    0.99


bulletSpeed : Float
bulletSpeed =
    4


bulletLifetime : Float
bulletLifetime =
    600


shootCooldown : Float
shootCooldown =
    150


spawnInterval : Float
spawnInterval =
    4000


shipCollisionRadius : Float
shipCollisionRadius =
    8


minSplitSize : Float
minSplitSize =
    14


splitSizeFactor : Float
splitSizeFactor =
    0.55


pctX : Float -> String
pctX x =
    String.fromFloat (x / arenaWidth * 100) ++ "%"


pctY : Float -> String
pctY y =
    String.fromFloat (y / arenaHeight * 100) ++ "%"



-- TYPES


type alias Vec =
    { x : Float, y : Float }


type alias Ship =
    { pos : Vec
    , vel : Vec
    , angle : Float
    , thrust : Bool
    }


type alias Bullet =
    { pos : Vec, vel : Vec, life : Float }


type alias Asteroid =
    { pos : Vec, vel : Vec, size : Float, seed : Float }


type alias GameState =
    { ship : Ship
    , bullets : List Bullet
    , asteroids : List Asteroid
    , score : Int
    , cooldown : Float
    , gameOver : Bool
    , spawnTimer : Float
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , space : Bool
    }



-- INIT


initGame : GameState
initGame =
    { ship = { pos = { x = arenaWidth / 2, y = arenaHeight / 2 }, vel = { x = 0, y = 0 }, angle = -pi / 2, thrust = False }
    , bullets = []
    , asteroids = initAsteroids
    , score = 0
    , cooldown = 0
    , gameOver = False
    , spawnTimer = 0
    }


initAsteroids : List Asteroid
initAsteroids =
    [ { pos = { x = 50, y = 50 }, vel = { x = 0.4, y = 0.3 }, size = 30, seed = 1.1 }
    , { pos = { x = 350, y = 80 }, vel = { x = -0.3, y = 0.5 }, size = 25, seed = 2.3 }
    , { pos = { x = 100, y = 350 }, vel = { x = 0.5, y = -0.2 }, size = 35, seed = 3.7 }
    , { pos = { x = 300, y = 300 }, vel = { x = -0.4, y = -0.4 }, size = 28, seed = 4.9 }
    ]



-- UPDATE


updateGame : Float -> Keys -> GameState -> GameState
updateGame delta keys game =
    if game.gameOver then
        game

    else
        let
            normalizedDelta =
                delta / 16.667

            newShip =
                updateShip normalizedDelta keys game.ship

            newCooldown =
                max 0 (game.cooldown - delta)

            shouldSpawnBullet =
                keys.space && newCooldown <= 0

            updatedBullets =
                updateBullets normalizedDelta delta newShip shouldSpawnBullet game.bullets

            movedAsteroids =
                List.map
                    (\asteroid ->
                        { asteroid
                            | pos =
                                { x = wrapCoord (asteroid.pos.x + asteroid.vel.x * normalizedDelta) arenaWidth
                                , y = wrapCoord (asteroid.pos.y + asteroid.vel.y * normalizedDelta) arenaHeight
                                }
                        }
                    )
                    game.asteroids

            ( survivingAsteroids, survivingBullets, destroyed ) =
                resolveCollisions movedAsteroids updatedBullets

            newSmallAsteroids =
                List.concatMap splitAsteroid destroyed

            ( spawnedAsteroids, newSpawnTimer ) =
                spawnAsteroids delta game.spawnTimer game.asteroids

            finalAsteroids =
                survivingAsteroids ++ newSmallAsteroids ++ spawnedAsteroids

            shipHit =
                List.any
                    (\asteroid ->
                        let
                            dx =
                                newShip.pos.x - asteroid.pos.x

                            dy =
                                newShip.pos.y - asteroid.pos.y
                        in
                        sqrt (dx * dx + dy * dy) < asteroid.size + shipCollisionRadius
                    )
                    finalAsteroids
        in
        { ship = newShip
        , bullets = survivingBullets
        , asteroids = finalAsteroids
        , score = game.score + List.length destroyed * 100
        , cooldown =
            if shouldSpawnBullet then
                shootCooldown

            else
                newCooldown
        , gameOver = shipHit
        , spawnTimer = newSpawnTimer
        }


updateShip : Float -> Keys -> Ship -> Ship
updateShip normalizedDelta keys ship =
    let
        rotAmount =
            rotationSpeed * normalizedDelta

        newAngle =
            ship.angle
                + (if keys.right then
                    rotAmount

                   else
                    0
                  )
                - (if keys.left then
                    rotAmount

                   else
                    0
                  )

        thrustAmount =
            thrustPower * normalizedDelta

        ax =
            if keys.up then
                cos newAngle * thrustAmount

            else
                0

        ay =
            if keys.up then
                sin newAngle * thrustAmount

            else
                0

        newVx =
            (ship.vel.x + ax) * friction

        newVy =
            (ship.vel.y + ay) * friction

        newX =
            wrapCoord (ship.pos.x + newVx * normalizedDelta) arenaWidth

        newY =
            wrapCoord (ship.pos.y + newVy * normalizedDelta) arenaHeight
    in
    { pos = { x = newX, y = newY }
    , vel = { x = newVx, y = newVy }
    , angle = newAngle
    , thrust = keys.up
    }


updateBullets : Float -> Float -> Ship -> Bool -> List Bullet -> List Bullet
updateBullets normalizedDelta rawDelta ship shouldSpawn bullets =
    let
        freshBullet =
            if shouldSpawn then
                [ { pos = ship.pos
                  , vel =
                        { x = cos ship.angle * bulletSpeed + ship.vel.x * 0.5
                        , y = sin ship.angle * bulletSpeed + ship.vel.y * 0.5
                        }
                  , life = bulletLifetime
                  }
                ]

            else
                []
    in
    freshBullet
        ++ List.filterMap
            (\bullet ->
                let
                    movedBullet =
                        { bullet
                            | pos =
                                { x = wrapCoord (bullet.pos.x + bullet.vel.x * normalizedDelta) arenaWidth
                                , y = wrapCoord (bullet.pos.y + bullet.vel.y * normalizedDelta) arenaHeight
                                }
                            , life = bullet.life - rawDelta
                        }
                in
                if movedBullet.life > 0 then
                    Just movedBullet

                else
                    Nothing
            )
            bullets


spawnAsteroids : Float -> Float -> List Asteroid -> ( List Asteroid, Float )
spawnAsteroids rawDelta currentTimer asteroids =
    let
        newTimer =
            currentTimer + rawDelta
    in
    if newTimer > spawnInterval then
        ( [ { pos = { x = Theme.frac (currentTimer * 0.017) * arenaWidth, y = 0 }
            , vel = { x = Theme.frac (currentTimer * 0.031) * 0.8 - 0.4, y = 0.3 + Theme.frac (currentTimer * 0.013) * 0.3 }
            , size = 20 + Theme.frac (currentTimer * 0.023) * 15
            , seed = currentTimer
            }
          ]
        , 0
        )

    else
        ( [], newTimer )


wrapCoord : Float -> Float -> Float
wrapCoord v maxVal =
    if v < 0 then
        v + maxVal

    else if v > maxVal then
        v - maxVal

    else
        v


resolveCollisions : List Asteroid -> List Bullet -> ( List Asteroid, List Bullet, List Asteroid )
resolveCollisions asteroids bullets =
    let
        indexedBullets =
            List.indexedMap Tuple.pair bullets

        ( survivingAsteroids, destroyedAsteroids, hitBulletIndices ) =
            List.foldl
                (\asteroid ( surviving, destroyed, hitIndices ) ->
                    let
                        maybeHitIndex =
                            List.foldl
                                (\( idx, bullet ) acc ->
                                    case acc of
                                        Just _ ->
                                            acc

                                        Nothing ->
                                            let
                                                dx =
                                                    asteroid.pos.x - bullet.pos.x

                                                dy =
                                                    asteroid.pos.y - bullet.pos.y
                                            in
                                            if sqrt (dx * dx + dy * dy) < asteroid.size then
                                                Just idx

                                            else
                                                Nothing
                                )
                                Nothing
                                indexedBullets
                    in
                    case maybeHitIndex of
                        Just idx ->
                            ( surviving, asteroid :: destroyed, Set.insert idx hitIndices )

                        Nothing ->
                            ( asteroid :: surviving, destroyed, hitIndices )
                )
                ( [], [], Set.empty )
                asteroids

        survivingBullets =
            List.filterMap
                (\( idx, bullet ) ->
                    if Set.member idx hitBulletIndices then
                        Nothing

                    else
                        Just bullet
                )
                indexedBullets
    in
    ( List.reverse survivingAsteroids, survivingBullets, List.reverse destroyedAsteroids )


splitAsteroid : Asteroid -> List Asteroid
splitAsteroid asteroid =
    if asteroid.size < minSplitSize then
        []

    else
        let
            newSize =
                asteroid.size * splitSizeFactor

            spread =
                0.5
        in
        [ { pos = asteroid.pos
          , vel = { x = asteroid.vel.y * 1.2 + spread, y = -asteroid.vel.x * 1.2 }
          , size = newSize
          , seed = asteroid.seed + 10
          }
        , { pos = asteroid.pos
          , vel = { x = -asteroid.vel.y * 1.2 - spread, y = asteroid.vel.x * 1.2 }
          , size = newSize
          , seed = asteroid.seed + 20
          }
        ]



-- VIEW


viewAsteroidsGame : GameState -> Html msg
viewAsteroidsGame game =
    div []
        [ div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "space-between"
            , style "padding" "8px 12px"
            , style "background" (Theme.purpleA 0.04)
            , style "border" ("1px solid " ++ Theme.purpleA 0.1)
            , style "border-bottom" "none"
            ]
            [ div
                [ style "font-family" Theme.monoFont
                , style "font-size" "10px"
                , style "color" (Theme.purpleA 0.5)
                , style "letter-spacing" "0.1em"
                ]
                [ text "ASTEROIDS.EXE" ]
            , div
                [ style "font-family" Theme.monoFont
                , style "font-size" "10px"
                , style "color" (Theme.purpleA 0.3)
                ]
                [ text ("SCORE: " ++ String.fromInt game.score) ]
            ]
        , div
            [ style "position" "relative"
            , style "width" "100%"
            , style "aspect-ratio" "1"
            , style "background" "rgba(0,0,0,0.6)"
            , style "border" ("1px solid " ++ Theme.purpleA 0.1)
            , style "overflow" "hidden"
            ]
            ([ div
                [ style "position" "absolute"
                , style "top" "0"
                , style "left" "0"
                , style "width" "100%"
                , style "height" "100%"
                , style "background-image"
                    ("linear-gradient(" ++ Theme.purpleA 0.03 ++ " 1px, transparent 1px), linear-gradient(90deg, " ++ Theme.purpleA 0.03 ++ " 1px, transparent 1px)")
                , style "background-size" "40px 40px"
                ]
                []
             ]
                ++ List.map viewGameAsteroid game.asteroids
                ++ List.map viewGameBullet game.bullets
                ++ [ viewGameShip game.ship ]
                ++ (if game.gameOver then
                        [ viewGameOver game.score ]

                    else
                        []
                   )
                ++ [ viewCrtScanlines, viewCrtVignette ]
            )
        , div
            [ style "padding" "8px 12px"
            , style "background" (Theme.purpleA 0.02)
            , style "border" ("1px solid " ++ Theme.purpleA 0.06)
            , style "border-top" "none"
            , style "font-family" Theme.monoFont
            , style "font-size" "9px"
            , style "color" (Theme.purpleA 0.25)
            , style "letter-spacing" "0.05em"
            ]
            [ text "WASD / arrows + space to shoot" ]
        ]


viewGameShip : Ship -> Html msg
viewGameShip ship =
    let
        angleDeg =
            ship.angle * 180 / pi + 90
    in
    div
        [ style "position" "absolute"
        , style "left" (pctX ship.pos.x)
        , style "top" (pctY ship.pos.y)
        , style "width" "20px"
        , style "height" "20px"
        , style "transform" ("translate(-50%, -50%) rotate(" ++ String.fromFloat angleDeg ++ "deg)")
        ]
        [ div
            [ style "width" "0"
            , style "height" "0"
            , style "border-left" "8px solid transparent"
            , style "border-right" "8px solid transparent"
            , style "border-bottom" ("20px solid " ++ Theme.purpleA 0.8)
            , style "filter" ("drop-shadow(0 0 4px " ++ Theme.purpleA 0.6 ++ ")")
            , style "margin-left" "2px"
            ]
            []
        , if ship.thrust then
            div
                [ style "width" "0"
                , style "height" "0"
                , style "border-left" "4px solid transparent"
                , style "border-right" "4px solid transparent"
                , style "border-top" ("8px solid " ++ Theme.rgbaStr Theme.fuchsia 0.7)
                , style "margin-left" "6px"
                , style "margin-top" "1px"
                , style "filter" ("drop-shadow(0 0 3px " ++ Theme.rgbaStr Theme.fuchsia 0.5 ++ ")")
                ]
                []

          else
            text ""
        ]


viewGameBullet : Bullet -> Html msg
viewGameBullet bullet =
    div
        [ style "position" "absolute"
        , style "left" (pctX bullet.pos.x)
        , style "top" (pctY bullet.pos.y)
        , style "width" "4px"
        , style "height" "4px"
        , style "transform" "translate(-50%, -50%)"
        , style "background" (Theme.purpleA 0.9)
        , style "border-radius" "50%"
        , style "box-shadow" ("0 0 6px " ++ Theme.purpleA 0.6)
        ]
        []


viewGameAsteroid : Asteroid -> Html msg
viewGameAsteroid asteroid =
    div
        [ style "position" "absolute"
        , style "left" (pctX asteroid.pos.x)
        , style "top" (pctY asteroid.pos.y)
        , style "width" (String.fromFloat (asteroid.size * 2) ++ "px")
        , style "height" (String.fromFloat (asteroid.size * 2) ++ "px")
        , style "border" ("1px solid " ++ Theme.purpleA 0.3)
        , style "border-radius" "30% 70% 50% 40% / 60% 30% 70% 50%"
        , style "background" (Theme.purpleA 0.03)
        , style "box-shadow" ("inset 0 0 8px " ++ Theme.purpleA 0.05)
        , style "transform" ("translate(-50%, -50%) rotate(" ++ String.fromFloat (asteroid.seed * 45) ++ "deg)")
        ]
        []


viewGameOver : Int -> Html msg
viewGameOver score =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "background" "rgba(0,0,0,0.7)"
        ]
        [ div
            [ style "font-family" Theme.monoFont
            , style "font-size" "20px"
            , style "color" (Theme.purpleA 0.8)
            , style "letter-spacing" "0.2em"
            , style "margin-bottom" "8px"
            ]
            [ text "DESTROYED" ]
        , div
            [ style "font-family" Theme.monoFont
            , style "font-size" "12px"
            , style "color" (Theme.purpleA 0.4)
            , style "margin-bottom" "16px"
            ]
            [ text ("SCORE: " ++ String.fromInt score) ]
        , div
            [ style "font-family" Theme.monoFont
            , style "font-size" "10px"
            , style "color" (Theme.purpleA 0.3)
            , style "animation" "blink 1.5s steps(1) infinite"
            ]
            [ text "press R to restart" ]
        ]


viewCrtScanlines : Html msg
viewCrtScanlines =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background" "repeating-linear-gradient(0deg, transparent, transparent 2px, rgba(0,0,0,0.08) 2px, rgba(0,0,0,0.08) 4px)"
        , style "pointer-events" "none"
        , style "z-index" "10"
        ]
        []


viewCrtVignette : Html msg
viewCrtVignette =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background" "radial-gradient(ellipse at center, transparent 50%, rgba(0,0,0,0.6) 100%)"
        , style "pointer-events" "none"
        , style "z-index" "11"
        ]
        []
