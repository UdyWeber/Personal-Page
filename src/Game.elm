module Game exposing (GameState, Keys, initGame, updateGame, viewAsteroidsGame)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


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


initGame : GameState
initGame =
    { ship = { pos = { x = 200, y = 200 }, vel = { x = 0, y = 0 }, angle = -pi / 2, thrust = False }
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


gameW : Float
gameW =
    400


gameH : Float
gameH =
    400


updateGame : Float -> Keys -> GameState -> GameState
updateGame delta keys game =
    if game.gameOver then
        game

    else
        let
            dt =
                delta / 16.667

            rotSpeed =
                0.06 * dt

            newAngle =
                game.ship.angle
                    + (if keys.right then rotSpeed else 0)
                    - (if keys.left then rotSpeed else 0)

            thrustPower =
                0.08 * dt

            ax =
                if keys.up then cos newAngle * thrustPower else 0

            ay =
                if keys.up then sin newAngle * thrustPower else 0

            friction =
                0.99

            newVx =
                (game.ship.vel.x + ax) * friction

            newVy =
                (game.ship.vel.y + ay) * friction

            newShipX =
                wrapCoord (game.ship.pos.x + newVx * dt) gameW

            newShipY =
                wrapCoord (game.ship.pos.y + newVy * dt) gameH

            newShip =
                { pos = { x = newShipX, y = newShipY }
                , vel = { x = newVx, y = newVy }
                , angle = newAngle
                , thrust = keys.up
                }

            newCooldown =
                max 0 (game.cooldown - delta)

            spawnBullet =
                keys.space && newCooldown <= 0

            bulletSpeed =
                4

            freshBullet =
                if spawnBullet then
                    [ { pos = newShip.pos
                      , vel =
                            { x = cos newAngle * bulletSpeed + newVx * 0.5
                            , y = sin newAngle * bulletSpeed + newVy * 0.5
                            }
                      , life = 600
                      }
                    ]

                else
                    []

            updatedBullets =
                freshBullet
                    ++ List.filterMap
                        (\b ->
                            let
                                nb =
                                    { b
                                        | pos =
                                            { x = wrapCoord (b.pos.x + b.vel.x * dt) gameW
                                            , y = wrapCoord (b.pos.y + b.vel.y * dt) gameH
                                            }
                                        , life = b.life - delta
                                    }
                            in
                            if nb.life > 0 then Just nb else Nothing
                        )
                        game.bullets

            movedAsteroids =
                List.map
                    (\a ->
                        { a
                            | pos =
                                { x = wrapCoord (a.pos.x + a.vel.x * dt) gameW
                                , y = wrapCoord (a.pos.y + a.vel.y * dt) gameH
                                }
                        }
                    )
                    game.asteroids

            ( survivingAsteroids, survivingBullets, destroyed ) =
                resolveCollisions movedAsteroids updatedBullets

            newSmallAsteroids =
                List.concatMap splitAsteroid destroyed

            newSpawnTimer =
                game.spawnTimer + delta

            spawnedAsteroids =
                if newSpawnTimer > 4000 then
                    [ { pos = { x = frac (game.spawnTimer * 0.017) * gameW, y = 0 }
                      , vel = { x = frac (game.spawnTimer * 0.031) * 0.8 - 0.4, y = 0.3 + frac (game.spawnTimer * 0.013) * 0.3 }
                      , size = 20 + frac (game.spawnTimer * 0.023) * 15
                      , seed = game.spawnTimer
                      }
                    ]

                else
                    []

            finalAsteroids =
                survivingAsteroids ++ newSmallAsteroids ++ spawnedAsteroids

            shipHit =
                List.any
                    (\a ->
                        let
                            ddx = newShipX - a.pos.x
                            ddy = newShipY - a.pos.y
                        in
                        sqrt (ddx * ddx + ddy * ddy) < a.size + 8
                    )
                    finalAsteroids
        in
        { ship = newShip
        , bullets = survivingBullets
        , asteroids = finalAsteroids
        , score = game.score + List.length destroyed * 100
        , cooldown = if spawnBullet then 150 else newCooldown
        , gameOver = shipHit
        , spawnTimer = if newSpawnTimer > 4000 then 0 else newSpawnTimer
        }


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
        checkAsteroid a =
            let
                hitBullet =
                    List.any
                        (\b ->
                            let
                                ddx = a.pos.x - b.pos.x
                                ddy = a.pos.y - b.pos.y
                            in
                            sqrt (ddx * ddx + ddy * ddy) < a.size
                        )
                        bullets
            in
            ( a, hitBullet )

        results =
            List.map checkAsteroid asteroids

        surviving =
            List.filterMap (\( a, hit ) -> if hit then Nothing else Just a) results

        destroyed =
            List.filterMap (\( a, hit ) -> if hit then Just a else Nothing) results

        survivingBullets =
            List.filter
                (\b ->
                    not
                        (List.any
                            (\a ->
                                let
                                    ddx = a.pos.x - b.pos.x
                                    ddy = a.pos.y - b.pos.y
                                in
                                sqrt (ddx * ddx + ddy * ddy) < a.size
                            )
                            asteroids
                        )
                )
                bullets
    in
    ( surviving, survivingBullets, destroyed )


splitAsteroid : Asteroid -> List Asteroid
splitAsteroid a =
    if a.size < 14 then
        []

    else
        let
            newSize = a.size * 0.55
            spread = 0.5
        in
        [ { pos = a.pos
          , vel = { x = a.vel.y * 1.2 + spread, y = -a.vel.x * 1.2 }
          , size = newSize
          , seed = a.seed + 10
          }
        , { pos = a.pos
          , vel = { x = -a.vel.y * 1.2 - spread, y = a.vel.x * 1.2 }
          , size = newSize
          , seed = a.seed + 20
          }
        ]



-- VIEW


viewAsteroidsGame : GameState -> Html msg
viewAsteroidsGame g =
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
                , style "font-size" "10px"
                , style "color" "rgba(168,85,247,0.5)"
                , style "letter-spacing" "0.1em"
                ]
                [ text "ASTEROIDS.EXE" ]
            , div
                [ style "font-family" "'Space Mono', monospace"
                , style "font-size" "10px"
                , style "color" "rgba(168,85,247,0.3)"
                ]
                [ text ("SCORE: " ++ String.fromInt g.score) ]
            ]
        , div
            [ style "position" "relative"
            , style "width" "100%"
            , style "max-width" "400px"
            , style "aspect-ratio" "1"
            , style "background" "rgba(0,0,0,0.6)"
            , style "border" "1px solid rgba(168,85,247,0.1)"
            , style "overflow" "hidden"
            ]
            ([ div
                [ style "position" "absolute"
                , style "top" "0"
                , style "left" "0"
                , style "width" "100%"
                , style "height" "100%"
                , style "background-image"
                    "linear-gradient(rgba(168,85,247,0.03) 1px, transparent 1px), linear-gradient(90deg, rgba(168,85,247,0.03) 1px, transparent 1px)"
                , style "background-size" "40px 40px"
                ]
                []
             ]
                ++ List.map viewGameAsteroid g.asteroids
                ++ List.map viewGameBullet g.bullets
                ++ [ viewGameShip g.ship ]
                ++ (if g.gameOver then [ viewGameOver g.score ] else [])
            )
        , div
            [ style "padding" "8px 12px"
            , style "background" "rgba(168,85,247,0.02)"
            , style "border" "1px solid rgba(168,85,247,0.06)"
            , style "border-top" "none"
            , style "font-family" "'Space Mono', monospace"
            , style "font-size" "9px"
            , style "color" "rgba(168,85,247,0.25)"
            , style "letter-spacing" "0.05em"
            ]
            [ text "WASD / arrows + space to shoot" ]
        ]


viewGameShip : Ship -> Html msg
viewGameShip ship =
    let
        angleDeg = ship.angle * 180 / pi + 90
    in
    div
        [ style "position" "absolute"
        , style "left" (String.fromFloat (ship.pos.x - 10) ++ "px")
        , style "top" (String.fromFloat (ship.pos.y - 10) ++ "px")
        , style "width" "20px"
        , style "height" "20px"
        , style "transform" ("rotate(" ++ String.fromFloat angleDeg ++ "deg)")
        ]
        [ div
            [ style "width" "0"
            , style "height" "0"
            , style "border-left" "8px solid transparent"
            , style "border-right" "8px solid transparent"
            , style "border-bottom" "20px solid rgba(168,85,247,0.8)"
            , style "filter" "drop-shadow(0 0 4px rgba(168,85,247,0.6))"
            , style "margin-left" "2px"
            ]
            []
        , if ship.thrust then
            div
                [ style "width" "0"
                , style "height" "0"
                , style "border-left" "4px solid transparent"
                , style "border-right" "4px solid transparent"
                , style "border-top" "8px solid rgba(217,70,239,0.7)"
                , style "margin-left" "6px"
                , style "margin-top" "1px"
                , style "filter" "drop-shadow(0 0 3px rgba(217,70,239,0.5))"
                ]
                []

          else
            text ""
        ]


viewGameBullet : Bullet -> Html msg
viewGameBullet bullet =
    div
        [ style "position" "absolute"
        , style "left" (String.fromFloat (bullet.pos.x - 2) ++ "px")
        , style "top" (String.fromFloat (bullet.pos.y - 2) ++ "px")
        , style "width" "4px"
        , style "height" "4px"
        , style "background" "rgba(168,85,247,0.9)"
        , style "border-radius" "50%"
        , style "box-shadow" "0 0 6px rgba(168,85,247,0.6)"
        ]
        []


viewGameAsteroid : Asteroid -> Html msg
viewGameAsteroid a =
    div
        [ style "position" "absolute"
        , style "left" (String.fromFloat (a.pos.x - a.size) ++ "px")
        , style "top" (String.fromFloat (a.pos.y - a.size) ++ "px")
        , style "width" (String.fromFloat (a.size * 2) ++ "px")
        , style "height" (String.fromFloat (a.size * 2) ++ "px")
        , style "border" "1px solid rgba(168,85,247,0.3)"
        , style "border-radius" "30% 70% 50% 40% / 60% 30% 70% 50%"
        , style "background" "rgba(168,85,247,0.03)"
        , style "box-shadow" "inset 0 0 8px rgba(168,85,247,0.05)"
        , style "transform" ("rotate(" ++ String.fromFloat (a.seed * 45) ++ "deg)")
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
            [ style "font-family" "'Space Mono', monospace"
            , style "font-size" "20px"
            , style "color" "rgba(168,85,247,0.8)"
            , style "letter-spacing" "0.2em"
            , style "margin-bottom" "8px"
            ]
            [ text "DESTROYED" ]
        , div
            [ style "font-family" "'Space Mono', monospace"
            , style "font-size" "12px"
            , style "color" "rgba(168,85,247,0.4)"
            , style "margin-bottom" "16px"
            ]
            [ text ("SCORE: " ++ String.fromInt score) ]
        , div
            [ style "font-family" "'Space Mono', monospace"
            , style "font-size" "10px"
            , style "color" "rgba(168,85,247,0.3)"
            , style "animation" "blink 1.5s steps(1) infinite"
            ]
            [ text "press R to restart" ]
        ]


frac : Float -> Float
frac f =
    f - toFloat (floor f)
