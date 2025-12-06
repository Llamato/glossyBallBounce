module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color

data AnimationState = AnimationState {
    position :: (Float, Float),
    velocity :: (Float, Float),
    currentColor :: Int,
    colors :: [Color]
}

windowWidth :: Int
windowWidth = 320

windowHeight :: Int
windowHeight = 240

windowPosX :: Int
windowPosX = 32

windowPosY :: Int
windowPosY = 32

circleRadius :: Int
circleRadius = 10

window :: Display
window = InWindow "Bouncy balls" (windowWidth, windowHeight) (windowPosX, windowPosY)

background :: Color
background = black

renderer :: AnimationState -> Picture
renderer as = translate x y $ color ((colors as)!!(currentColor as)) $ circleSolid $ fromIntegral circleRadius
    where
        x = fst $ position as
        y = snd $ position as

iterator :: ViewPort -> Float -> AnimationState -> AnimationState
iterator _ deltaTime as = AnimationState {
    position = (nposx, nposy),
    velocity = (nvx, nvy),
    currentColor = ncolor,
    colors = colors as
}
    where
        posx = fst $ position as
        posy = snd $ position as
        vx = fst $ velocity as
        vy = snd $ velocity as
        hborder = fromIntegral windowWidth / 2 - fromIntegral circleRadius
        vborder = fromIntegral windowHeight / 2 - fromIntegral circleRadius
        nvx = if abs posx >= hborder then -vx else vx
        nvy = if abs posy >= vborder then -vy else vy
        nposx = posx + nvx * deltaTime
        nposy = posy + nvy * deltaTime
        ncolor = ((currentColor as)+if nvx /= vx || nvy /= vy then 1 else 0) `mod` length (colors as)

fps :: Int
fps = 60

initialAnimationState :: AnimationState
initialAnimationState = AnimationState {
    position = (0, 0),
    velocity = (50, 50),
    currentColor = 1,
    colors = [
        makeColor ((6*16+7)/256) ((10*16+5)/256) ((13*16+14)/256) 1,
        makeColor 1 1 1 1,
        makeColor ((13*16+7)/256) ((8*16+12)/256) ((9*16+8)/256) 1
    ]
}

main :: IO ()
main = simulate window background fps initialAnimationState renderer iterator