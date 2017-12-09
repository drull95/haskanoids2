module Control.Extra.Monad where

import Control.Monad

whileLoopM :: Monad m => m a -> (a -> Bool) -> (a -> m ()) -> m ()
whileLoopM val cond act = r'
  where r' = do v <- val
                when (cond v) $ do
                  act v
                  whileLoopM val cond act

foldLoopM :: Monad m => a -> m b -> (a -> b -> m a) -> m a
foldLoopM val sense act = r'
  where r' = do s <- sense
                val' <- act val s
                foldLoopM val' sense act
