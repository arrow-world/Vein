{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vein.Syntax.Resolver where

import qualified Vein.Syntax.AST as AST
import qualified Vein.Core.Module as Module
import qualified Vein.Core.Module as M
import qualified Vein.Core.Lambda.Expr as E
import Vein.Util.Allocator (AllocatorT , allocate , allocateWith)
import Vein.Util.Counter (CounterT , count)

import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT, mapReaderT, local, MonadReader)
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                , MonadError
                                                )
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (MonadState , StateT , runStateT)
import Numeric.Natural (Natural)
import Data.Hashable (Hashable)
import qualified Data.Map.Lazy as Map
import           Data.Bitraversable             ( bitraverse )
import qualified Data.HashMap.Lazy as HashMap
import           Data.Maybe                     ( isJust )
import qualified Data.HashSet as HashSet
import           Data.List                      ( elemIndex )


data Scope v =
    Scope
      { decls :: HashSet.HashSet v
      , currentModule :: M.QN
      , parent :: Scope v
      , cons :: HashMap.HashMap v Con
      , args :: [v]
      }
  | NilScope
    deriving (Eq,Show)

type Scope' = Scope MatchVar

type MatchVar = Either Natural Module.QN

inScope :: (Eq v , Hashable v) => v -> Scope v -> Bool
inScope name = \case
  Scope decls mod parent ctors args -> HashSet.member name decls || inScope name parent
  NilScope -> False


resolve name = do
  scope <- ask
  fqn <- toFQN name

  if inScope name scope then
    ResolverMonad $ lift $ lift $ allocateWith (const count) $ allocate fqn
  else
    throwError $ NotInScope name scope

tryResolve name = do
  scope <- ask
  fqn <- toFQN name

  if inScope name scope then
    Just <$> ResolverMonad (lift $ lift $ allocateWith (const count) $ allocate fqn)
  else
    return Nothing


dbi :: MatchVar -> Scope' -> Maybe Natural
dbi name scope =
    maybe (fmap (fromIntegral (length $ args scope) +) $ dbi name $ parent scope) (Just . fromIntegral) i
  where
    i = elemIndex name $ args scope

tryResolveArg :: Monad m => MatchVar -> ResolverMonadT m (Maybe Natural)
tryResolveArg name = asks $ dbi name

resolveArg :: Monad m => MatchVar -> ResolverMonadT m Natural
resolveArg name = do
  scope <- ask
  maybe (throwError $ ArgNotInScope name scope) return $ dbi name scope


localScope :: MonadReader Scope' m => M.QN -> HashSet.HashSet MatchVar -> HashMap.HashMap MatchVar Con -> [MatchVar] -> m a -> m a
localScope relName declNames ctors args m = do
  fqn <- toFQN $ Right relName
  let Right fqn' = fqn
  local (\parent -> Scope (declNames <> HashMap.keysSet ctors <> HashSet.fromList args) fqn' parent ctors args) m


newtype ResolverMonadT m a = ResolverMonad (ReaderT Scope' (ExceptT Error (StateT (Map.Map Id MatchVar) (CounterT m))) a)
  deriving (Functor , Applicative , Monad , MonadState (Map.Map Id MatchVar) , MonadError Error , MonadReader Scope')

data Error =
    NotInScope MatchVar (Scope MatchVar)
  | ConNotInScope MatchVar (Scope MatchVar)
  | ArgNotInScope MatchVar (Scope MatchVar)
  deriving (Eq,Show)


type Id = Natural


toFQN :: MonadReader Scope' m => MatchVar -> m MatchVar
toFQN name = asks $ \scope -> either Left (Right . concatQN (currentModule scope)) name

concatQN :: M.QN -> M.QN -> M.QN
concatQN (M.QN ns) (M.QN ns') = M.QN $ ns ++ ns'

emptyQN = M.QN []


resolveCon :: MonadReader Scope' m => MonadError Error m => MatchVar -> m Con
resolveCon name = do
  con <- HashMap.lookup <$> toFQN name <*> asks cons
  maybe (throwError =<< asks (ConNotInScope name)) return con

data Con = Con { datatype :: E.Const , conId :: Natural }
    deriving (Eq,Show)