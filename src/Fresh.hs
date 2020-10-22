{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Fresh where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Trans.State.Strict (liftListen, liftPass)
import Control.Monad.Writer
import Control.Monad.Reader
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import LLVM.AST


class MonadFresh m where
  next :: String -> m Integer

uniqueName :: (Functor m, MonadFresh m) => String -> m Name
uniqueName name = Name . toShort . pack . (name ++) . show <$> next name

fresh :: (Functor m, MonadFresh m) => m Name
fresh = UnName . fromIntegral <$> next ""


newtype FreshT m a =
  FreshT { freshState :: StateT (Map String Integer) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

evalFreshT :: Monad m => FreshT m a -> Map String Integer -> m a
evalFreshT = evalStateT . freshState

runFreshT :: Monad m => FreshT m a -> Map String Integer -> m (a, Map String Integer)
runFreshT = runStateT . freshState


type Fresh = FreshT Identity

evalFresh :: FreshT Identity a -> Map String Integer -> a
evalFresh = evalState . freshState


instance Monad m => MonadFresh (FreshT m) where
  next name = FreshT $ do
    count <- gets $ fromMaybe 0 . Map.lookup name
    modify $ Map.insert name (count + 1)
    return count


instance (MonadFresh m, Monad m) => MonadFresh (StateT s m) where
  next = lift . next

instance MonadState s m => MonadState s (FreshT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter w m => MonadWriter w (FreshT m) where
  writer = lift . writer
  listen = FreshT . liftListen listen . freshState
  pass = FreshT . liftPass pass . freshState

instance MonadReader r m => MonadReader r (FreshT m) where
  ask = lift ask
  local f = FreshT . (mapStateT . local) f . freshState

-- TODO: Add these to a test suite
-- a :: FreshT (State Int) String
-- a = return "Hi"
--
-- b :: FreshT (State Int) Name
-- b = a >>= uniqueName
--
-- test :: Name
-- test = flip evalState 24 . flip evalFreshT (Map.fromList []) $ b
--
-- c :: StateT Int Fresh Name
-- c = uniqueName "Var"
--
-- test2 :: Name
-- test2 = flip evalFresh (Map.fromList []) . flip evalStateT 42 $ c
