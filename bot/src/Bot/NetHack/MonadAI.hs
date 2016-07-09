{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Bot.NetHack.MonadAI
  ( AIF(..)
  , AI
  , AIT
  , WAI()
  , runWAI
  , toWAI
  , AbortAI()
  , runAbortAI
  , runAbortAI_
  , MonadAI(..)
  , MonadWAI(..)
  , matchf
  , matchfirst
  , repeatUntilFalse
  , getScreenLine
  , module Control.Applicative )
  where

import Bot.NetHack.ScreenPattern
import Bot.NetHack.WorldState
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free.Church
import qualified Data.ByteString as B
import qualified Data.Text as T
import Prelude hiding ( getLine )
import Terminal.Screen

data AIF f
  = GetCurrentScreenState !(ScreenState -> Int -> Int -> f)
  | Send !B.ByteString f
  deriving ( Functor )

type AI a = AIT Identity a
type AIT m a = FT AIF m a

class Monad m => MonadAI m where
  send :: B.ByteString -> m ()
  currentScreen :: m (ScreenState, Int, Int)

instance MonadAI (FT AIF m) where
  send bs = liftF $ Send bs ()
  currentScreen = liftF $ GetCurrentScreenState (\ss x y -> (ss, x, y))

instance MonadAI m => MonadAI (StateT s m) where
  send = lift . send
  currentScreen = lift currentScreen

-- | Like `AI` but can exit early with its `Alternative` instance.
newtype AbortAI m a = AbortAI (ExceptT () m a)
  deriving ( Functor, Applicative, Monad )

instance MonadWAI m => MonadWAI (AbortAI m) where
  askWorldState = AbortAI $ lift $ askWorldState
  askMessages = AbortAI $ lift $ askMessages
  modWorld fun = AbortAI $ lift $ modWorld fun

instance MonadAI m => MonadAI (AbortAI m) where
  send bs = AbortAI $ lift $ send bs
  currentScreen = AbortAI $ lift $ currentScreen

instance Monad m => Alternative (AbortAI m) where
  empty = AbortAI $ throwE ()
  (AbortAI a1) <|> (AbortAI a2) = AbortAI $ a1 <|> a2

runAbortAI :: MonadAI m => AbortAI m a -> m (Maybe a)
runAbortAI (AbortAI exc) =
  runExceptT exc >>= \case
    Left () -> return Nothing
    Right ok -> return $ Just ok

runAbortAI_ :: MonadAI m => AbortAI m () -> m ()
runAbortAI_ ai = void $ runAbortAI ai

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

newtype WAI m a = WAI (FT AIF (StateT (WorldState, [T.Text]) m) a)
  deriving ( Functor, Applicative, Monad )

class MonadAI m => MonadWAI m where
  askWorldState :: m WorldState
  askMessages :: m [T.Text]
  modWorld :: (WorldState -> WorldState) -> m ()

instance Monad m => MonadWAI (WAI m) where
  askWorldState = WAI $ lift $ do
    (w, _) <- get
    return w

  askMessages = WAI $ lift $ do
    (_, m) <- get
    return m

  modWorld fun = WAI $ lift $ _1 %= fun

instance MonadAI (WAI m) where
  send bs = WAI $ liftF $ Send bs ()
  currentScreen = WAI $ liftF $ GetCurrentScreenState (\ss x y -> (ss, x, y))

runWAI :: MonadAI m => WAI m a -> FT AIF (StateT (WorldState, [T.Text]) m) a
runWAI (WAI ff) = ff

toWAI :: FT AIF (StateT (WorldState, [T.Text]) m) a -> WAI m a
toWAI = WAI

