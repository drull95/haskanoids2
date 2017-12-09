{-# LANGUAGE OverloadedStrings #-}

module Display where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Data.Text as Text
import Data.Word
import SDL
import SDL.Font as Font
import SDL.Image as Image

-- import Audio
import Constants
import GameState
import Objects
import Resources hiding (audio)
import Levels
import Paths_haskanoid

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music. 
render :: Window -> ResourceMgr -> GameState -> IO()
render win resourceManager shownState = do
  resources <- loadNewResources resourceManager shownState
  -- audio   resources shownState
  display win resources shownState

-- ** Audio
{-
audio :: Resources -> GameState -> IO()
audio resources shownState = do
  -- Start bg music if necessary
  playing <- musicPlaying
  unless playing $ awhen (bgMusic resources) playMusic 

  -- Play object hits
  mapM_ (audioObject resources) $ gameObjects shownState

audioObject :: Resources -> Object -> IO ()
audioObject resources object = when (objectHit object) $
  case objectKind object of
    (Block _ _) -> playFile (blockHitSnd resources) 3000
    _           -> return ()

-}

-- ** Painting

display :: Window -> Resources -> GameState -> IO()
display win resources shownState = do 
  -- Obtain surface
  screen <- getWindowSurface win

            {-
  awhen (bgImage resources) $ \bg' -> void $ do
    let bg     = imgSurface bg'
    SDL.surfaceBlit bg Nothing screen Nothing

  hud <- createRGBSurface (V2 (round width) (round gameTop))
                          RGBA8888
  paintGeneral hud resources (gameInfo shownState)
  SDL.surfaceBlit hud Nothing screen Nothing

  surface <- createRGBSurface (V2 (round gameWidth)
                                  (round gameHeight))
                              RGBA8888
  paintGeneralMsg surface resources (gameStatus (gameInfo shownState))
  mapM_ (paintObject resources surface) $ gameObjects shownState
  SDL.surfaceBlit surface Nothing screen $
                  Just (P (V2 (round gameLeft) (round gameTop)))

-}
  surfaceFillRect screen Nothing (V4 255 255 255 255)
  SDL.updateWindowSurface win

paintGeneral :: Surface -> Resources -> GameInfo -> IO ()
paintGeneral screen resources over = void $ do
  -- Paint screen green
  surfaceFillRect screen Nothing (V4 0x11 0x22 0x33 255)
  paintGeneralHUD screen resources over

paintGeneralMsg :: Surface -> Resources -> GameStatus -> IO ()
paintGeneralMsg screen resources GamePlaying     = return ()
paintGeneralMsg screen resources GamePaused      = paintGeneralMsg' screen resources (Text.pack "Paused")
paintGeneralMsg screen resources (GameLoading n) = paintGeneralMsg' screen resources (Text.pack ("Level " ++ show n))
paintGeneralMsg screen resources GameOver        = paintGeneralMsg' screen resources (Text.pack "GAME OVER!!!")
paintGeneralMsg screen resources GameFinished    = paintGeneralMsg' screen resources (Text.pack "You won!!! Well done :)")

gray :: V4 Word8
gray = V4 128 128 128 255
       
paintGeneralMsg' :: Surface -> Resources -> Text -> IO ()
paintGeneralMsg' screen resources msg = void $ do
  let font = resFont resources
  message <- Font.solid (unFont font) gray msg 
  V2 scrw scrh <- SDL.surfaceDimensions screen
  V2 w h <- SDL.surfaceDimensions message
  let x = ((scrw - w) `div` 2)
      y = ((scrh - h) `div` 2)
  SDL.surfaceBlit message Nothing screen $ Just (P (V2 x y))

paintGeneralHUD :: Surface -> Resources -> GameInfo -> IO ()
paintGeneralHUD screen resources over = void $ do
  let font = unFont $ resFont resources
  message1 <- Font.solid font gray (Text.pack ("Level: " ++ show (gameLevel over)))
  V2 w1 h1 <- SDL.surfaceDimensions  message1
  SDL.surfaceBlit message1 Nothing screen $ Just (P (V2 10 10))
  message2 <- Font.solid font gray (Text.pack ("Points: " ++ show (gamePoints over)))
  V2 w2 h2 <- SDL.surfaceDimensions message2
  SDL.surfaceBlit message2 Nothing screen $ Just (P (V2 10 (10 + h2 + 5)))
  message3 <- Font.solid font gray (Text.pack ("Lives: " ++ show (gameLives over)))
  V2 rightMargin _ <- SDL.surfaceDimensions screen
  V2 w2 h2 <- SDL.surfaceDimensions message3
  SDL.surfaceBlit message3 Nothing screen $
                  Just (P (V2 (rightMargin - 10 - w2) 10))

-- | Paints a game object on a surface.
paintObject :: Resources -> Surface -> Object -> IO ()
paintObject resources screen object =
  case objectKind object of
    (Paddle (w,h))  -> void $ do
       let bI = imgSurface $ paddleImg resources
       surfaceColorKey bI $= Just (V4 0 255 0 255)
       SDL.surfaceBlit bI Nothing screen $ Just (P (V2 x y))

    (Block e (w,h)) -> void $ do
       let bI = imgSurface $ blockImage e
       SDL.surfaceBlit bI Nothing screen $ Just (P (V2 x y)) 

    (Ball r) -> void $ do
       let x' = x - (fromInteger (round r))
           y' = y - (fromInteger (round r))
           sz = fromInteger $ round (2*r)
       -- b <- convertSurface (imgSurface $ ballImg resources) (format) []
       let bI = imgSurface $ ballImg resources
       surfaceColorKey bI $= Just (V4 0 255 0 255)
       SDL.surfaceBlit bI Nothing screen $ Just (P (V2 x' y'))
          
    _              -> return ()
  where -- format = surfaceFormat screen
        p      = objectPos object
        x      = round (fst p)
        y      = round (snd p)
        blockImage 3 = block1Img resources
        blockImage 2 = block2Img resources
        blockImage n = block3Img resources
-- | Ad-hoc resource loading
-- This function is ad-hoc in two senses: first, because it
-- has the paths to the files hard-coded inside. And second,
-- because it loads the specific resources that are needed,
-- not a general 
--
loadResources :: IO ResourceMgr
loadResources = do
  -- Font initialization
  Font.initialize
  
  gameFont <- getDataFileName "data/lacuna.ttf"
  -- Load the fonts we need
  font  <- Font.load gameFont 32
  let myFont = Resources.Font gameFont font

  -- blockHit <- loadAudio =<< getDataFileName "data/196106_aiwha_ding-cc-by.wav"

  -- bgM <- liftIO $ loadMusic "Ckotty_-_Game_Loop_11.ogg"
  -- bgM <- liftIO $ loadMusic "data/level0.mp3"

  -- let levelBg = "data/level0.png"
  -- img <- lift $ fmap (Image levelBg) $ load levelBg

  ballImg <- getDataFileName "data/ball2.png"
  ball <- Resources.Image ballImg <$> Image.load ballImg

  b1Img <- liftIO $ getDataFileName "data/block1.png"
  b1 <- Resources.Image b1Img <$> Image.load b1Img

  b2Img <- liftIO $ getDataFileName "data/block2.png"
  b2 <- Resources.Image b2Img <$> Image.load b2Img

  b3Img <- liftIO $ getDataFileName "data/block3.png"
  b3 <- Resources.Image b3Img <$> Image.load b3Img

  paddleImg <- liftIO $ getDataFileName "data/paddleBlu.png"
  paddle <- Resources.Image paddleImg <$> Image.load paddleImg

  -- Start playing music
  -- when (isJust bgM) $ lift (playMusic (fromJust bgM))

  liftIO $ ResourceMgr <$>
    newIORef (ResourceManager GameStarted (Resources myFont Nothing ball b1 b2 b3 paddle))


loadNewResources :: ResourceMgr ->  GameState -> IO Resources
loadNewResources mgr state = do
  manager <- readIORef (unResMgr mgr)
  let oldState = lastKnownStatus manager
      newState = gameStatus (gameInfo state)
      oldResources = resources manager

  newResources <- case newState of
                    (GameLoading _) | newState /= oldState
                                    -> updateAllResources oldResources newState
                    _               -> return oldResources 

  let manager' = ResourceManager { lastKnownStatus = newState
                                 , resources       = newResources
                                 }

  writeIORef (unResMgr mgr) manager'
  return newResources

updateAllResources :: Resources -> GameStatus -> IO Resources
updateAllResources res (GameLoading n) = do
  {-
  -- Load new music
  let newMusicFP' = _resourceFP $ levelMusic $ levels !! n
  newMusicFP <- getDataFileName newMusicFP'

  let oldMusic   = bgMusic res
      oldMusicFP = maybe "" musicName oldMusic

  newMusic <- if oldMusicFP == newMusicFP
              then return oldMusic
              else do -- Loading can fail, in which case we continue
                      -- with the old music
                      bgM <- loadMusic newMusicFP
                      if isNothing bgM
                       then do putStrLn $ "Could not load resource " ++ newMusicFP
                               return oldMusic
                       else do stopMusic
                               return bgM

  -}

  -- Load new background
  let newBgFP' = _resourceFP $ levelBg $ levels !! n

  newBgFP <- getDataFileName newBgFP'

  let oldBg   = bgImage res
      oldBgFP = maybe "" imgName oldBg

  newBg <- if oldBgFP == newBgFP
             then return oldBg
             else do img' <- Image.load newBgFP
                     return $ Just $ Resources.Image newBgFP img'

  return (res { bgImage = newBg {-, bgMusic = newMusic-} })
