module Graphics.UI.Extra.SDL where

import Data.IORef
import SDL

initializeTimeRef :: IO (IORef Int)
initializeTimeRef = do
  -- Weird shit I have to do to get accurate time!
  timeRef <- newIORef (0 :: Int)
  _       <- senseTimeRef timeRef
  _       <- senseTimeRef timeRef
  _       <- senseTimeRef timeRef
  _       <- senseTimeRef timeRef

  return timeRef

senseTimeRef :: IORef Int -> IO Int
senseTimeRef timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.ticks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  return dt

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

milisecsToSecs :: Int -> Double
milisecsToSecs m = fromIntegral m / 1000
