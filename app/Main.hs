module Main(main) where

import Graphics.Gloss

data AnimationState = AnimationState {
    ballPosition :: (Float, Float),
    ballVelocity :: (Float, Float)
}

window :: Display
window = InWindow "Bouncy balls" (320, 240) (10, 10)

background :: Color
background = black

frame :: Float -> Picture
frame seconds = translate nx ny $ color white $ circleSolid 10
    where
        x = 0
        y = 0
        vx = 10
        vy = 10
        nx = x + vx * seconds
        ny = y + vy * seconds


main :: IO ()
main = animate window background frame