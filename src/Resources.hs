module Resources where

import Data.IORef
import SDL
import SDL.Font as Font
import GameState
    
data ResourceSpec = ResourceSpec
 { fonts  :: [FontResource]
 , images :: [ImageResource]
 , music  :: [MusicResource]
 , audio  :: [AudioResource]
 }

type FontResource  = Resource
type ImageResource = Resource
type MusicResource = Resource
type AudioResource = Resource

newtype Resource = Resource { _resourceFP :: FilePath }
 deriving Eq
-- * Resource management

newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { lastKnownStatus :: GameStatus
  , resources       :: Resources
  }

-- | Includes all the assets needed at the current time in the game.
data Resources = Resources
  { resFont     :: Resources.Font
  -- , blockHitSnd :: Audio
  , bgImage     :: Maybe Image
  , ballImg     :: Image
  , block1Img   :: Image
  , block2Img   :: Image
  , block3Img   :: Image
  , paddleImg   :: Image
  -- , bgMusic     :: Maybe Music
  }


data Image = Image { imgName  :: String, imgSurface :: Surface }
data Font  = Font  { fontName :: String, unFont :: Font.Font }

