{-# LANGUAGE DataKinds #-}
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import Data.List (foldl')

data Ball = Ball {
    position :: (Float, Float),
    velocity :: (Float, Float),
    currentColor :: Int
}

data GameState = GameState {
    balls :: [Ball],
    colors :: [Color],
    currentMouseEvent :: Int
}

magnification :: Float
magnification = 3

windowWidth :: Int
windowWidth = 320

scaledWindowWidth :: Int
scaledWindowWidth = floor (fromIntegral windowWidth * magnification)

windowHeight :: Int
windowHeight = 240

scaledWindowHeight :: Int
scaledWindowHeight = floor (fromIntegral windowHeight * magnification)

windowPosX :: Int
windowPosX = 10

windowPosY :: Int
windowPosY = 32

circleRadius :: Int
circleRadius = 10

scaledCircleRadius :: Int
scaledCircleRadius = floor (fromIntegral circleRadius * magnification)

window :: Display
window = InWindow "Bouncy balls" (scaledWindowWidth, scaledWindowHeight) (windowPosX, windowPosY)

background :: Color
background = black

renderBall :: Ball -> Picture
renderBall ball = translate x y $ color (colors initialGameState!!currentColor ball) $ circleSolid $ fromIntegral scaledCircleRadius
    where
        x = fst $ position ball
        y = snd $ position ball

renderer :: GameState -> Picture
renderer as = pictures $ map renderBall (balls as)

inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (MouseButton LeftButton) Down _ mpos) as = if currentMouseEvent as == 0 then GameState {
    balls = balls as ++ [Ball {
        position = mpos,
        velocity = (0, 0),
        currentColor = (currentColor (last $ balls as)+1) `mod` length (colors as)
    }],
     colors = colors as,
     currentMouseEvent = 1
} else as

inputHandler (EventKey (MouseButton LeftButton) Up _ mpos) as = let
    ball = last $ balls as
    bposx = fst $ position ball
    bposy = snd $ position ball
    mposx = fst mpos
    mposy = snd mpos
    bvx = mposx - bposx
    bvy = mposy - bposy
    in GameState {
    balls = init (balls as) ++ [Ball {
        position = position ball,
        velocity = (bvx, bvy),
        currentColor = currentColor ball
    }],
    colors = colors as,
    currentMouseEvent = 0
}

inputHandler _ as = GameState {
    balls = balls as,
    colors = colors as,
    currentMouseEvent = 0
}

areColliding :: Ball -> Ball -> Bool
areColliding ball1 ball2 = let
    (x1, y1) = position ball1
    (x2, y2) = position ball2
    dx = x1 - x2
    dy = y1 - y2
    distance = sqrt (dx*dx + dy*dy)
    in distance < 2 * fromIntegral scaledCircleRadius

handleCollision :: Ball -> Ball -> (Ball, Ball)
handleCollision ball1 ball2 = let
    (x1, y1) = position ball1
    (x2, y2) = position ball2
    (vx1, vy1) = velocity ball1
    (vx2, vy2) = velocity ball2
    (deltaX, deltaY) = (x2 - x1, y2 - y1)
    distance = sqrt (deltaX * deltaX + deltaY * deltaY)
    (normalX, normalY) = if distance > 0 then (deltaX/distance, deltaY/distance) else (1, 0)
    (dvx, dvy) = (vx1 - vx2, vy1 - vy2)
    velocityAlongNormal = dvx * normalX + dvy * normalY
    in if velocityAlongNormal > 0 then (ball1, ball2) else let
        impulse = 2 * fromIntegral scaledCircleRadius - distance
        (newVx1, newVy1) = (vx1 - impulse * normalX, vy1 - impulse * normalY)
        (newVx2, newVy2) = (vx2 + impulse * normalX, vy2 + impulse * normalY)

        overlap = 2 * fromIntegral scaledCircleRadius - distance
        separation = if distance > 0 then overlap / 2 else fromIntegral scaledCircleRadius
        (sepX, sepY) = (separation * normalX, separation * normalY)

        (newX1, newY1) = (x1 - sepX, y1 - sepY)
        (newX2, newY2) = (x2 + sepX, y2 + sepY)
    in
        ( Ball {
            position = (newX1, newY1),
            velocity = (newVx1, newVy1),
            currentColor = currentColor ball1
            },
            Ball {
                position = (newX2, newY2),
                velocity = (newVx2, newVy2),
                currentColor = currentColor ball2
            }
        )

processCollisions :: [Ball] -> [Ball]
processCollisions balls = M.elems $ foldl' process (M.fromList (zip [0..] balls)) [0..length balls - 1]
  where
    process :: M.Map Int Ball -> Int -> M.Map Int Ball
    process ballMap idx = 
        case M.lookup idx ballMap of
            Nothing -> ballMap
            Just ball -> foldl' (checkIdx idx) ballMap [(idx+1)..M.size ballMap - 1]
    
    checkIdx :: Int -> M.Map Int Ball -> Int -> M.Map Int Ball
    checkIdx idx ballMap otherIdx =
        case (M.lookup idx ballMap, M.lookup otherIdx ballMap) of
            (Just ball, Just otherBall) ->
                if areColliding ball otherBall
                then let (newBall, newOther) = handleCollision ball otherBall
                     in M.insert idx newBall (M.insert otherIdx newOther ballMap)
                else ballMap
            _ -> ballMap

handleWallCollision :: Ball -> Ball
handleWallCollision ball = let
    posx = fst $ position ball
    posy = snd $ position ball
    vx = fst $ velocity ball
    vy = snd $ velocity ball
    nvx = if abs posx >= hborder then -vx else vx
    nvy = if abs posy >= vborder then -vy else vy
    ncolor = (currentColor ball+fromEnum (nvx /= vx || nvy /= vy)) `mod` length (colors initialGameState)
    in Ball {
        position = position ball,
        velocity = (nvx, nvy),
        currentColor = ncolor
    }
    where
        hborder = fromIntegral scaledWindowWidth / 2 - fromIntegral scaledCircleRadius
        vborder = fromIntegral scaledWindowHeight / 2 - fromIntegral scaledCircleRadius

iterator :: Float -> GameState -> GameState
iterator deltaTime as = GameState {
    balls = map (\ball -> let
       posx = fst $ position ball
       posy = snd $ position ball

       wallBall = handleWallCollision ball
       (nvx, nvy) = velocity wallBall
       nposx = posx + nvx * deltaTime
       nposy = posy + nvy * deltaTime

       in Ball {
       position = (nposx, nposy),
       velocity = velocity wallBall,
       currentColor = currentColor wallBall
   }) (balls as),
    colors = colors as,
    currentMouseEvent = currentMouseEvent as
}

fps :: Int
fps = 60

initialGameState :: GameState
initialGameState = GameState {
    balls = [
        Ball {
            position = (0, 0),
            velocity = (50.0, 50.0),
            currentColor = 0
        },
        Ball {
            position = (0, 0),
            velocity = (50.0, -50.0),
            currentColor = 1
        },
        Ball {
            position = (0, 0),
            velocity = (-50.0, 50.0),
            currentColor = 2
        }
    ],
    colors = [
        makeColor ((6*16+7)/256) ((10*16+5)/256) ((13*16+14)/256) 1,
        makeColor 1 1 1 1,
        makeColor ((13*16+7)/256) ((8*16+12)/256) ((9*16+8)/256) 1
    ],
    currentMouseEvent = 0
}

main :: IO ()
main = play window background fps initialGameState renderer inputHandler iterator