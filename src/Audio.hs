-- | A layer of abstraction on top of SDL audio.
--
-- It plays audio soundfx asynchronously (in a new thread), which means that
-- programs must be compiled with the threaded Runtime System (ghc flag is
-- -threaded).
--
-- This module is 2010-2014 (c) Keera Studios, redistributed with permission.
module Audio
    (Music(..),
     Audio(..),
     initAudio,
     loadAudio,
     loadMusic,
     playMusic,
     playFile,
     stopMusic,
     musicPlaying) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Concurrent
-- import qualified Graphics.UI.SDL.Mixer.General as SDL.Mixer
-- import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
-- import qualified Graphics.UI.SDL.Mixer.Music as SDL.Mixer.Music
-- import qualified Graphics.UI.SDL.Mixer.Types as SDL.Mixer.Types
-- import qualified Graphics.UI.SDL.Mixer.Samples as SDL.Mixer.Samples
import SDL.Mixer as Mixer
import SDL.Mixer.Raw as RMixer

data Music = Music { musicName :: String, unMusic :: Mixer.Music }
data Audio = Audio { audioName :: String, unAudio :: Mixer.Chunk }

-- | Initialize the audio subsystem.
--
-- Audio quality and number of channels are fixed (16).
initAudio :: IO ()
initAudio = void $ do
  _result <- Mixer.openAudio defaultAudio 4096
  Mixer.setChannels 16

-- | Load a music file, returning a 'Music' if loaded successfully.
loadMusic :: String -> IO (Maybe Music)
loadMusic fp = fmap (Music fp) <$> RMixer.loadMUS fp

-- | Play music in a loop at max volume.
playMusic :: Music -> IO ()
playMusic m = do
  Mixer.setMusicVolume 100
  Mixer.playMusic Forever (unMusic m)

-- | Stop playing music
stopMusic :: IO ()
stopMusic = Mixer.haltMusic

-- | Is music playing?
musicPlaying :: IO Bool
musicPlaying = Mixer.playingMusic

-- | Load an audio file.
loadAudio :: String -> IO (Maybe Audio)
loadAudio fp = fmap (Audio fp) <$> RMixer.loadWAV fp

-- | Play an audio file for the given number of seconds.
--
-- This function spawns a new OS thread. Remember to compile your program
-- with the threaded RTS.
playFile :: Audio -> Int -> IO ()
playFile wav t = void $ forkOS $ do 
  _v <- SDL.Mixer.Channels.playChannel (-1) (unAudio wav) 0
  threadDelay (t * 1000)
