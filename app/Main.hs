{-# LANGUAGE DataKinds #-}
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
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

iterator :: Float -> GameState -> GameState
iterator deltaTime as = GameState {
    balls = map (\ball -> let
        posx = fst $ position ball
        posy = snd $ position ball
        vx = fst $ velocity ball
        vy = snd $ velocity ball
        nvx = if abs posx >= hborder then -vx else vx
        nvy = if abs posy >= vborder then -vy else vy
        nposx = posx + nvx * deltaTime
        nposy = posy + nvy * deltaTime
        ncolor = (currentColor ball+fromEnum (nvx /= vx || nvy /= vy)) `mod` length (colors as)
        in Ball {
        position = (nposx, nposy),
        velocity = (nvx, nvy),
        currentColor = ncolor
    }) (balls as),
    colors = colors as,
    currentMouseEvent = currentMouseEvent as
}
    where
        hborder = fromIntegral scaledWindowWidth / 2 - fromIntegral scaledCircleRadius
        vborder = fromIntegral scaledWindowHeight / 2 - fromIntegral scaledCircleRadius

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