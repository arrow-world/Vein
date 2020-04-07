{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vein.Syntax.Resolver where

import qualified Vein.Syntax.AST as AST
import qualified Vein.Core.Module as Module
import qualified Vein.Core.Lambda.Expr as E
import Vein.Util.Allocator (AllocatorT , allocate , allocateWith)
import Vein.Util.Counter (Counter , count)

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


data Scope v =
    Scope
      { decls :: HashSet.HashSet v
      , currentModule :: v
      , parent :: Scope v
      , cons :: HashMap.HashMap v Con
      }
  | NilScope
    deriving (Eq,Show)

type Scope' = Scope Module.QN

inScope :: (Eq v , Hashable v) => v -> Scope v -> Bool
inScope name = \case
  Scope decls mod parent ctors -> HashSet.member name decls || inScope name parent
  NilScope -> False


resolve name = do
  scope <- ask
  fqn <- toFQN name

  if inScope name scope then
    ResolverMonad $ lift $ lift $ allocateWith (const $ count) $ allocate fqn
  else
    throwError $ NotInScope name scope


localScope :: MonadReader Scope' m => Module.QN -> HashSet.HashSet Module.QN -> HashMap.HashMap Module.QN Con -> m a -> m a
localScope relName declNames ctors m = do
  fqn <- toFQN relName
  local (\parent -> Scope (declNames <> HashMap.keysSet ctors) fqn parent ctors) m


newtype ResolverMonad a = ResolverMonad (ReaderT Scope' (ExceptT Error (StateT (Map.Map Id Module.QN) Counter)) a)
  deriving (Functor , Applicative , Monad , MonadState (Map.Map Id Module.QN) , MonadError Error , MonadReader Scope')

data Error =
    NotInScope Module.QN (Scope Module.QN)
  | ConNotInScope Module.QN (Scope Module.QN)
  deriving (Eq,Show)


type Id = Natural


toFQN :: MonadReader Scope' m => Module.QN -> m Module.QN
toFQN name = asks $ \scope -> concatQN (currentModule scope) name

concatQN :: Module.QN -> Module.QN -> Module.QN
concatQN (Module.QN ns) (Module.QN ns') = Module.QN $ ns ++ ns'

emptyQN = Module.QN []


resolveCon :: MonadReader Scope' m => MonadError Error m => Module.QN -> m Con
resolveCon name = do
  con <- HashMap.lookup <$> toFQN name <*> asks cons
  maybe (throwError =<< asks (ConNotInScope name)) return con

data Con = Con { datatype :: E.Const , conId :: Natural }
    deriving (Eq,Show)