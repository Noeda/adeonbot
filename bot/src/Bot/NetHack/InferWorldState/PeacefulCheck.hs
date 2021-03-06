{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Bot.NetHack.InferWorldState.PeacefulCheck
  ( checkPeacefulness )
  where

import Bot.NetHack.Direction
import Bot.NetHack.Logs
import Bot.NetHack.MonadAI
import Bot.NetHack.WorldState
import Control.Lens hiding ( levels, Level )
import Control.Monad.State.Strict
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Regex.TDFA

-- | Uses ; command to look at a monster and tell if its peaceful.
--
-- Updates the world state if it is.
checkPeacefulness :: (MonadState WorldState m, MonadAI m)
                  => (Int, Int)
                  -> m ()
checkPeacefulness (x, y) = do
  cl <- get
  (_, cx, cy) <- currentScreen

  send $ ";" <> birdMovementKeysTo (cx, cy) (x, y) <> "."

  -- Some sanity check that the thing we expected to happen after ; clicking
  -- actually resulted in a monster being looked up.
  --

  let ll = levels.at (cl^.currentLevel)._Just

  -- Is it a long name with a --More-- on the screen?
  more <- matchf "--More--"

  line1 <- getScreenLine 0
  line2 <- if more then getScreenLine 1 else return ""
  let line = cleanup $ line1 <> line2

  logTrace ("line: " <> show line) $ when (T.index line 1 == ' ' && T.index line 0 /= ' ') $ do
    case T.unpack line =~ (" \\((.+)\\)" :: String) of
      [[_whole, T.pack -> monname]] -> do
        -- Is it a statue?
        if T.isInfixOf "statue of" monname
          then do ll.monsters.at (x, y) .= Nothing
                  ll.statues %= S.insert (x, y)

          else do let is_peaceful = T.isInfixOf "peaceful" monname
                  ll.monsters.at (x, y)._Just.isPeaceful .= Just is_peaceful
                  ll.statues %= S.delete (x, y)

        ll.cells.ix (x, y).cellFeature %= \old_feature ->
          if old_feature == Just InkyBlackness || old_feature == Nothing
            then logTrace ("Inferring " <> show (x, y) <> " to be item pile floor.") $ Just ItemPileFloor
            else old_feature

      _ -> return ()

  when more $ send " "

cleanup :: T.Text -> T.Text
cleanup (T.strip -> txt) =
  let p = go txt
   in if T.isSuffixOf "--More--" p
        then T.dropEnd 8 p
        else p
 where
  go txt =
    let nxt = T.replace "  " " " txt
     in if nxt /= txt
           then go nxt
           else nxt

