import Control.Applicative ((<$>))
import Control.Monad.IfElse
import Data.Text as Text
import FRP.Yampa as Yampa
import SDL

import Game
import Display
import Input
import Graphics.UI.Extra.SDL

main :: IO ()
main = do

  SDL.initializeAll
  screen <- SDL.createWindow (Text.pack "Test") defaultWindow
  SDL.showWindow screen

  cursorVisible $= False

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources

  reactimate (senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- milisecsToSecs <$> senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render screen res e >> return False)
               wholeGame
 
