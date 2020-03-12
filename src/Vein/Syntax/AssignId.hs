module Vein.Syntax.AssignId where

import qualified Vein.Syntax.AST as AST
import Vein.Util.Allocator (AllocatorT , allocate , allocateWith)
import Vein.Util.Counter (Counter , count)

import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT, mapReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT)
import Numeric.Natural (Natural)
import qualified Data.Map.Lazy as Map

type Id = Natural

assignVarId :: AST.LocatedExprF' r v -> StateT (Map.Map Id v) Counter (AST.LocatedExprF' r Id)
assignVarId e = allocateWith (const count) $ traverse allocate e