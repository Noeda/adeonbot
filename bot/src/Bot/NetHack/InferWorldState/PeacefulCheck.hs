{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Bot.NetHack.InferWorldState.PeacefulCheck
  ( checkPeacefulness )
  where

import Bot.NetHack.Logs
import Bot.NetHack.MonadAI
import Bot.NetHack.WorldState
import Control.Lens hiding ( levels, Level )
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import Data.Char ( ord )
import Data.Monoid
import qualified Data.Text as T
import Data.Word
import Text.Regex.TDFA

b8 :: Char -> Word8
b8 x =
  let o = ord x
   in if o > 255 || o < 0
        then error "b8: char cannot be turned into byte."
        else fromIntegral o

-- | Uses ; command to look at a monster and tell if its peaceful.
--
-- Updates the world state if it is.
checkPeacefulness :: (MonadState WorldState m, MonadAI m)
                  => (Int, Int)
                  -> m ()
checkPeacefulness (x, y) = do
  cl <- get
  (_, cx, cy) <- currentScreen

  let gorights = max 0 (x-cx)
      golefts  = max 0 (cx-x)
      goups    = max 0 (cy-y)
      godowns  = max 0 (y-cy)

  send $ ";" <> B.replicate gorights (b8 'l') <>
                B.replicate golefts (b8 'h') <>
                B.replicate goups (b8 'k') <>
                B.replicate godowns (b8 'j') <> "."

  -- Some sanity check that the thing we expected to happen after ; clicking
  -- actually resulted in a monster being looked up.

  line <- getScreenLine 0
  when (T.index line 1 == ' ' && T.index line 0 /= ' ') $ do
    case T.unpack line =~ (" \\((.+)\\) " :: String) of
      [[_whole, T.pack -> monname]] -> do
        let is_peaceful = T.isInfixOf "peaceful" monname
        levels.at (cl^.currentLevel)._Just.monsters.at (x, y)._Just.isPeaceful .= Just is_peaceful
      _ -> return ()

