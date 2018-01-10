{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fresh where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Identity
import Control.Monad.State


class MonadFresh m where
  uniqueName :: String -> m String

fresh :: MonadFresh m => m String
fresh = uniqueName ""


newtype FreshT m a =
  FreshT { freshState :: StateT (Map String Int) m a }
  deriving (Functor, Applicative, Monad, MonadState (Map String Int))

evalFreshT :: Monad m => FreshT m a -> Map String Int -> m a
evalFreshT m s = flip evalStateT s . freshState $ m


type Fresh = FreshT Identity

evalFresh :: FreshT Identity a -> Map String Int -> a
evalFresh m s = flip evalState s . freshState $ m


instance Monad m => MonadFresh (FreshT m) where
  uniqueName name = do
    count <- gets $ fromMaybe 0 . Map.lookup name
    modify $ Map.insert name (count + 1)
    return $ name ++ show count


instance (MonadFresh m, Monad m) => MonadFresh (StateT s m) where
  uniqueName = lift . uniqueName


a :: FreshT (State Int) String
a = return "Hi"

b :: FreshT (State Int) String
b = a >>= uniqueName

test :: String
test = flip evalState 24 . flip evalFreshT (Map.fromList []) $ b

c :: StateT Int Fresh String
c = uniqueName "Var"

test2 :: String
test2 = flip evalFresh (Map.fromList []) . flip evalStateT 42 $ c
