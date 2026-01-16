{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main(main) where

--Gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Graphics.Gloss

--OpenAL
import Control.Monad 
import Control.Concurrent
import Data.List
import Sound.ALUT
import Data.IORef
import System.IO.Unsafe
import System.Directory (doesFileExist)

data Ball = Ball {
    position :: (Float, Float),
    velocity :: (Float, Float),
    currentColor :: Int
}

data GameState = GameState {
    balls :: [Ball],
    colors :: [Color],
    currentMouseEvent :: Int,
    currentCollisions :: Int
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

hborder :: Float
hborder = fromIntegral scaledWindowWidth / 2 - fromIntegral scaledCircleRadius

vborder :: Float
vborder = fromIntegral scaledWindowHeight / 2 - fromIntegral scaledCircleRadius

scaledCircleRadius :: Int
scaledCircleRadius = floor (fromIntegral circleRadius * magnification)

window :: Display
window = InWindow "Bouncy balls" (scaledWindowWidth, scaledWindowHeight) (windowPosX, windowPosY)

background :: Color
background = black

soundBuffer :: IORef (Maybe Sound.ALUT.Buffer)
{-# NOINLINE soundBuffer #-}
soundBuffer = unsafePerformIO $ newIORef Nothing

replace :: Int -> a -> [a] -> [a]
replace idx newVal xs = take idx xs ++ [newVal] ++ drop (idx+1) xs

waitUntilSoundPlaybackFinished :: Source -> IO ()
waitUntilSoundPlaybackFinished source = do 
    state <- get (sourceState source)
    case state of
        Playing -> do
            threadDelay 10000  -- 10ms
            waitUntilSoundPlaybackFinished source
        _ -> return ()

playFile :: FilePath -> IO ()
playFile fileName = doesFileExist fileName >>= \fileExists ->
    if fileExists then do
        mBuf <- readIORef soundBuffer
        buf <- case mBuf of
            Just b -> return b
            Nothing -> do
                b <- createBuffer (File fileName)
                writeIORef soundBuffer (Just b)
                return b
        source <- genObjectName
        buffer source $= Just buf
        Sound.ALUT.play [source]
        void $ forkIO $ do
            -- threadDelay 1000000 -- 1s
            waitUntilSoundPlaybackFinished source
            deleteObjectNames [source]
    else
        return ()

playBounceSound :: IO ()
playBounceSound = playFile "assets/bounce.wav"

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
     currentMouseEvent = 1,
     currentCollisions = currentCollisions as
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
    currentMouseEvent = 0,
    currentCollisions = currentCollisions as
}

inputHandler _ as = GameState {
    balls = balls as,
    colors = colors as,
    currentMouseEvent = 0,
    currentCollisions = currentCollisions as
}

areBallsColliding :: Ball -> Ball -> Bool
areBallsColliding ball1 ball2 = let
         (x1, y1) = position ball1
         (x2, y2) = position ball2
         dx = x1 - x2
         dy = y1 - y2
         distance = sqrt (dx*dx + dy*dy)
         in distance < 2 * fromIntegral scaledCircleRadius

handleBallCollision :: Ball -> Ball -> (Ball, Ball)
handleBallCollision ball1 ball2 = let
        (x1, y1) = position ball1
        (x2, y2) = position ball2
        (vx1, vy1) = velocity ball1
        (vx2, vy2) = velocity ball2
        (deltaX, deltaY) = (x2 - x1, y2 - y1)
        distance = sqrt (deltaX * deltaX + deltaY * deltaY)
        (normalX, normalY) = if distance > 0 then (deltaX/distance, deltaY/distance) else (1, 0)
        (dvx, dvy) = (vx1 - vx2, vy1 - vy2)
        velocityAlongNormal = dvx * normalX + dvy * normalY
        impulse = velocityAlongNormal
        (newVx1, newVy1) = (vx1 - impulse * normalX, vy1 - impulse * normalY)
        (newVx2, newVy2) = (vx2 + impulse * normalX, vy2 + impulse * normalY)
        in (
          Ball { position = position ball1, velocity = (newVx1, newVy1), currentColor = currentColor ball1 },
          Ball { position = position ball2, velocity = (newVx2, newVy2), currentColor = currentColor ball2 }
        )

ballBallCollisionStates :: [Ball] -> [(Ball, Bool)]
ballBallCollisionStates balls = uncurry zip (foldl' checkPair (balls, replicate (length balls) False) allPairs)
  where
    allPairs = [(i, j) | i <- [0..length balls - 1], j <- [i+1..length balls - 1]]
    checkPair :: ([Ball], [Bool]) -> (Int, Int) -> ([Ball], [Bool])
    checkPair (currentBalls, hadCollisions) (i, j) = let
      ballI = currentBalls !! i
      ballJ = currentBalls !! j
        in if areBallsColliding ballI ballJ then let
                   (newI, newJ) = handleBallCollision ballI ballJ
                   updatedBalls = replace i newI $ replace j newJ currentBalls
                   updatedCollisions = replace i True $ replace j True hadCollisions
                   in (updatedBalls, updatedCollisions) else (currentBalls, hadCollisions)

processBallBallCollisions :: [Ball] -> ([Ball], Int)
processBallBallCollisions balls = let states = ballBallCollisionStates balls in
    (map (\pair -> let
        ball = fst pair
        collision = snd pair
        in Ball {
            position = position ball,
            velocity = velocity ball,
            currentColor = (currentColor ball+fromEnum collision) `mod` length (colors initialGameState)
        }) states, sum (map (fromEnum . snd) states))

ballWallCollisionState :: Ball -> (Bool, Bool)
ballWallCollisionState ball = let
   posx = fst $ position ball
   posy = snd $ position ball
   in (abs posx >= hborder , abs posy >= vborder)

handleWallCollision :: Ball -> Ball
handleWallCollision ball = let
    vx = fst $ velocity ball
    vy = snd $ velocity ball
    (hborderCollision, vborderCollision) = ballWallCollisionState ball
    nvx = if hborderCollision then -vx else vx
    nvy = if vborderCollision then -vy else vy
    ncolor = (currentColor ball+fromEnum  (hborderCollision || vborderCollision)) `mod` length (colors initialGameState)
    in Ball {
        position = position ball,
        velocity = (nvx, nvy),
        currentColor = ncolor
    }

processBallWallCollisions :: [Ball] -> ([Ball], Int)
processBallWallCollisions balls = (
        map handleWallCollision balls,
        sum (map ((fromEnum . uncurry (||)) . ballWallCollisionState) balls)
    )

repositionBall :: Float -> Ball -> Ball
repositionBall deltaTime ball = let
    (posx, posy) = position ball
    (vx, vy) = velocity ball
    npos = (posx + vx * deltaTime, posy + vy * deltaTime)
    in Ball {
      position = npos,
      velocity = (vx, vy),
      currentColor = currentColor ball
    }

iterator :: Float -> GameState -> IO GameState
iterator deltaTime as =
    let
        (afterBallBallCollisions, ballBallCollisionCount) = processBallBallCollisions (balls as)
        (afterBallWallCollisions, wallBallCollisionCount) = processBallWallCollisions afterBallBallCollisions
        moved = map (repositionBall deltaTime) afterBallWallCollisions
        collisionCount = ballBallCollisionCount + wallBallCollisionCount
    in do
        when (collisionCount > 0) $ do
            playBounceSound
            return ()
        return GameState {
            balls = moved,
            colors = colors as,
            currentMouseEvent = currentMouseEvent as,
            currentCollisions = ballBallCollisionCount + wallBallCollisionCount
        }

fps :: Int
fps = 320

initialGameState :: GameState
initialGameState = GameState {
    balls = [
        Ball {
            position = (50, 50),
            velocity = (50.0, 50.0),
            currentColor = 0
        },
        Ball {
            position = (50, -50),
            velocity = (50.0, -50.0),
            currentColor = 1
        },
        Ball {
            position = (-50, 50),
            velocity = (-50.0, 50.0),
            currentColor = 2
        }
    ],
    colors = [
        makeColor ((6*16+7)/256) ((10*16+5)/256) ((13*16+14)/256) 1,
        makeColor 1 1 1 1,
        makeColor ((13*16+7)/256) ((8*16+12)/256) ((9*16+8)/256) 1
    ],
    currentMouseEvent = 0,
    currentCollisions = 0
}

gameloop :: IO ()
gameloop = Graphics.Gloss.playIO window background fps initialGameState (return . renderer) (\event gs -> return $ inputHandler event gs) iterator

main :: IO ()
main = do
    withProgNameAndArgs runALUT $ \progName args -> do
        (Just device) <- openDevice Nothing
        (Just context) <- createContext device []
        currentContext $= Just context
        playFile "assets/bounce.wav"
        gameloop