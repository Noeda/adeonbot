{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Bot.NetHack.MonadAI
  ( AIF(..)
  , AI
  , MonadAI(..)
  , Listener
  , matchf
  , matchfirst
  , currentScreen
  , repeatUntilFalse
  , getScreenLine
  , send )
  where

import Bot.NetHack.ScreenPattern
import Control.Monad.Free
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.Text as T
import Prelude hiding ( getLine )
import Terminal.Screen

data AIF f
  = GetCurrentScreenState !(ScreenState -> Int -> Int -> f)
  | Send !B.ByteString f
  deriving ( Functor )

type AI a = Free AIF a

type Listener = [(ScreenPattern, AI ())]

class Monad m => MonadAI m where
  liftAI :: AI a -> m a

instance MonadAI (Free AIF) where
  liftAI = id
  {-# INLINE liftAI #-}

instance MonadAI m => MonadAI (StateT s m) where
  liftAI action = lift $ liftAI action
  {-# INLINE liftAI #-}

send :: MonadAI m => B.ByteString -> m ()
send bs = liftAI $ liftF $ Send bs ()

-- | Returns current screen state and cursor coordinates.
currentScreen :: MonadAI m => m (ScreenState, Int, Int)
currentScreen = liftAI $ liftF $ GetCurrentScreenState (\ss x y -> (ss, x, y))

matchf :: (Match match, MonadAI m) => ScreenPattern -> m match
matchf pattern = do
  (ss, _, _) <- currentScreen
  return $ match ss pattern

matchfirst :: MonadAI m => [(ScreenPattern, m ())] -> m Bool
matchfirst ((pattern, payload):rest) = do
  r <- matchf pattern
  if r
    then payload >> return True
    else matchfirst rest
matchfirst [] = return False

getScreenLine :: MonadAI m => Int -> m T.Text
getScreenLine row = do
  (ss, _, _) <- currentScreen
  return $ fst $ getLine row ss

repeatUntilFalse :: Monad m => m Bool -> m ()
repeatUntilFalse thing = do
  x <- thing
  if x
    then repeatUntilFalse thing
    else return ()

