module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data AnimationState = AnimationState {
    position :: (Float, Float),
    velocity :: (Float, Float)
}

windowWidth :: Int
windowWidth = 320

windowHeight :: Int
windowHeight = 240

windowPosX :: Int
windowPosX = 10

windowPosY :: Int
windowPosY = 10

circleRadius :: Int
circleRadius = 10

window :: Display
window = InWindow "Bouncy balls" (windowWidth, windowHeight) (windowPosX, windowPosY)

background :: Color
background = black

renderer :: AnimationState -> Picture
renderer as = translate x y $ color white $ circleSolid $ fromIntegral circleRadius
    where
        x = fst $ position as
        y = snd $ position as

interator :: ViewPort -> Float -> AnimationState -> AnimationState
interator _ deltaTime state = AnimationState {
    position = (nposx, nposy),
    velocity = (nvx, nvy)
}
    where
        posx = fst $ position state
        posy = snd $ position state
        vx = fst $ velocity state
        vy = snd $ velocity state
        nvx = if abs posx >= fromIntegral windowWidth / 2 - fromIntegral circleRadius then -vx else vx
        nvy = if abs posy >= fromIntegral windowHeight / 2 - fromIntegral circleRadius then -vy else vy
        nposx = posx + nvx * deltaTime
        nposy = posy + nvy * deltaTime

fps :: Int
fps = 60

initialAnimationState :: AnimationState
initialAnimationState = AnimationState {
    position = (0, 0),
    velocity = (10, 10)
}

main :: IO ()
main = simulate window background fps initialAnimationState renderer interator