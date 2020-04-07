{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Core.Lambda.Desugar where

import qualified Vein.Core.Lambda.Expr as E
import qualified Vein.Core.Lambda.TypeCheck as TC
import qualified Vein.Syntax.AST as AST
import qualified Vein.Syntax.Lexer as L
import qualified Vein.Syntax.Parser as P
import qualified Vein.Core.Module as M
import qualified Vein.Syntax.Resolver as R

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Data.Fix ( Fix(..) )
import Control.Monad.State ( MonadState , get , put , StateT , runState )
import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT, mapReaderT, local, MonadReader)
import           Control.Monad.Except           ( MonadError , throwError )
import           Control.Applicative            ( liftA2 )
import Data.Default (Default(..))
import           Data.Foldable                  ( foldlM
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad                  ( (<=<) )
import Numeric.Natural (Natural)

desugar :: AST.Env M.QN AST.LocatedExpr -> DesugarMonad (TC.GlobalEnv Ann)
desugar (AST.Env defs anns) =
  TC.GlobalEnv <$> HashMap.traverseWithKey desugarItem undefined <*> traverse desugarAnn anns

desugarItem = undefined

desugarAnn :: AST.Annotation M.QN AST.LocatedExpr -> DesugarMonad (TC.TypedExpr Ann)
desugarAnn = \case
  AST.TypeAnnotation e t -> Fix <$> liftA2 TC.typed (desugarExpr e) (desugarExpr t)

type Expr = TC.ExprFWith Ann (TC.TypedExpr Ann)

desugarExpr :: AST.LocatedExpr -> DesugarMonad Expr
desugarExpr (Fix (AST.LocatedExprF e span)) = do
    let ann = spanToAnn span

    case e of
      AST.EApp f (AST.Located _ (p:ps)) -> do
          f <- desugarExprWithType f
          p <- traverse desugarParam p
          ps <- traverse (traverse desugarParam) ps
          foldlM (\f' p' -> app <$> (Fix . (f' `TC.typed`) <$> metaVar) <*> return p') (app f p) ps
        where
          app f' (AST.Located l p') = case p' of
            AST.Param e -> eExprFWith (spanToAnn $ span `P.composeSpan` l) $ E.App f' e
      
      AST.EUnaryOpF (AST.Located l op) e -> do
          op' <- resolve $ AST.uopToName op
          eExprFWith ann <$> (E.App <$> withType (eExprFWith annOp $ E.Const op') <*> desugarExprWithType e)
        where annOp = spanToAnn l
      
      AST.EBinaryOpF (AST.Located l op) e1 e2 -> do
          let f = either desugarExprWithType (withType <=< fmap (eExprFWith (spanToAnn l) . E.Const) . resolve) $ AST.bopToExpr' op
          eExprFWith ann <$> app2
            f
            (desugarExprWithType e1)
            (desugarExprWithType e2)
        where annOp = spanToAnn l
              app2 e1' e2' e3'  = E.App <$> (withType . eExprFWith (spanToAnn $ span `P.composeSpan` l) =<< (E.App <$> e1' <*> e2')) <*> e3'
      
      AST.ELiteralF l -> return $ eExprFWith ann $ E.Primitive $ E.Literal l

      AST.ELetInF decls e -> do
        env <- unwrapParsedEnv $ decls
        let declNames = AST.mapModuleMap (HashSet.fromList . map fst) $ AST.envModule env

        liftRToD $ R.localScope R.emptyQN declNames undefined undefined

eExprFWith ann = TC.ExprFWith ann . TC.EExpr

toDestructuringAssignment :: AST.Env M.QN AST.LocatedExpr -> (AST.LocatedExpr , AST.LocatedExpr)
toDestructuringAssignment decls = undefined

fnBindings :: AST.Located [(AST.Located [AST.Located (AST.Param AST.LocatedExpr)] , AST.LocatedExpr)] -> DesugarMonad Expr
fnBindings (AST.Located l clauses) =
  if all ((== nArgs) . length . fst) clauses then
    eExprFWith (spanToAnn l) <$> E.Lam <$> undefined undefined
  else
    throwError $ MismatchNumOfArgs `at` l
  where nArgs = length $ fst $ head clauses

desugarClauses :: [AST.Located (AST.Clause AST.LocatedExpr)] -> DesugarMonad [E.Clause (TC.TypedPat Ann) (TC.TypedExpr Ann)]
desugarClauses = traverse desugarClause

desugarClause :: AST.Located (AST.Clause AST.LocatedExpr) -> DesugarMonad (E.Clause (TC.TypedPat Ann) (TC.TypedExpr Ann))
desugarClause = undefined

desugarPat :: AST.LocatedExpr -> DesugarMonad Pat
desugarPat (Fix (AST.LocatedExprF c l)) = case c of
  AST.EApp (Fix (AST.LocatedExprF (AST.EVar fName) lF)) xs -> do
    R.Con datatype conId <- resolveCon fName
    patFWith . E.PatCon datatype conId <$> traverse desugarPatParamWithType (AST.unLocated xs)

  AST.EUnaryOpF op e -> desugarPat' $ uncurry AST.EApp $ uopToApp op e

  AST.EBinaryOpF op e1 e2 -> desugarPat' $ uncurry AST.EApp $ bopToApp $ AST.Located l (op,e1,e2)

  AST.ELiteralF lit -> return $ patFWith $ E.PatLit lit

  AST.EListF (AST.Located _ es) -> patFWith . E.PatList <$> traverse desugarPatWithType es

  AST.ETupleF (AST.Located _ es) -> patFWith . E.PatTuple <$> traverse desugarPatWithType es

  AST.EPlaceholder -> return $ patFWith E.PatWildcard

  _ -> throwError $ NotAllowedPattern `at` l

  where
    desugarPat' e = desugarPat $ Fix $ AST.LocatedExprF e l
    patFWith = E.PatFWith $ spanToAnn l

desugarPatWithType :: AST.LocatedExpr -> DesugarMonad (TC.TypedPat Ann)
desugarPatWithType e = Fix <$> liftA2 TC.typedPat (desugarPat e) metaVar

desugarPatParamWithType :: AST.Located (AST.Param AST.LocatedExpr) -> DesugarMonad (TC.TypedPat Ann)
desugarPatParamWithType (AST.Located l p) = case p of
  AST.Param e -> desugarPatWithType e
  AST.ParamImplicit _ -> throwError $ ImplicitArgPatternNotImplemented `at` l

type Pat = E.PatFWith Ann (TC.TypedPat Ann)


uopToApp :: AST.Located AST.UnaryOp -> AST.LocatedExpr -> (AST.LocatedExpr , AST.Located [AST.Located (AST.Param AST.LocatedExpr)])
uopToApp (AST.Located lop op) e =
  ( Fix $ AST.LocatedExprF (AST.EVar $ AST.uopToName op) lop
  , AST.Located le [AST.Located le $ AST.Param e]
  )
  where AST.LocatedExprF _ le = unFix e

bopToApp :: AST.Located (AST.Located (AST.BinaryOp AST.LocatedExpr), AST.LocatedExpr , AST.LocatedExpr)
              -> (AST.LocatedExpr , AST.Located [AST.Located (AST.Param AST.LocatedExpr)])
bopToApp (AST.Located l (AST.Located lop op , e1 , e2)) = 
  ( case AST.bopToExpr' op of {
      Left op' -> op';
      Right op' -> Fix $ AST.LocatedExprF (AST.EVar op') lop;
    }
  , AST.Located l [AST.Located le1 $ AST.Param e1 , AST.Located le2 $ AST.Param e2]
  )
  where
    AST.LocatedExprF _ le1 = unFix e1
    AST.LocatedExprF _ le2 = unFix e2

multiParamLam :: [AST.Located (AST.Param AST.LocatedExpr)] -> AST.Located Expr -> DesugarMonad Expr
multiParamLam params (AST.Located lb body) =
  foldlM (\e p@(AST.Located lp _) -> TC.ExprFWith (spanToAnn $ P.composeSpan lp lb) . TC.EExpr <$> lam p e) body params

lam :: AST.Located (AST.Param AST.LocatedExpr) -> Expr -> DesugarMonad (E.ExprF (TC.TypedPat Ann) (TC.TypedExpr Ann))
lam (AST.Located l param) e =
  E.Lam <$> (E.Abs <$> (Fix . (`TC.typed` TC.univ) . (TC.ExprFWith $ spanToAnn l) . TC.EMetaVar <$> genMetaVar) <*> withType e)

unwrapParsedEnv :: AST.Located (AST.ParsedEnv M.QN AST.LocatedExpr) -> DesugarMonad (AST.Env M.QN AST.LocatedExpr)
unwrapParsedEnv (AST.Located l (AST.ParsedEnv env)) = either (throwError . (`at` l) . EnvParseError) return env

desugarParam :: AST.Param AST.LocatedExpr -> DesugarMonad (AST.Param (TC.TypedExpr Ann))
desugarParam = traverse desugarExprWithType

desugarExprWithType :: AST.LocatedExpr -> DesugarMonad (TC.TypedExpr Ann)
desugarExprWithType e = Fix <$> liftA2 TC.typed (desugarExpr e) metaVar

withType :: TC.ExprFWith Ann (TC.TypedExpr Ann) -> DesugarMonad (TC.TypedExpr Ann)
withType e = Fix . (e `TC.typed`) <$> metaVar

locToAnn :: (a -> TC.ExprF (TC.TypedPat Ann) r) -> AST.Located a -> TC.ExprFWith Ann r
locToAnn f (AST.Located l e) = TC.ExprFWith (spanToAnn l) (f e)


newtype DesugarMonad a = DesugarMonad (StateT TC.MetaVar (ExceptT (AST.Located Error) R.ResolverMonad) a)
  deriving (Functor , Applicative , Monad , MonadState TC.MetaVar , MonadError (AST.Located Error) , MonadReader R.Scope')

genMetaVar :: DesugarMonad TC.MetaVar
genMetaVar = DesugarMonad $ do
  v@(TC.MetaVar n) <- get
  put $ TC.MetaVar $ n + 1
  return v

metaVar :: DesugarMonad (TC.ExprFWith Ann (TC.TypedExpr Ann))
metaVar = TC.expr . TC.EMetaVar <$> genMetaVar

liftRToD :: R.ResolverMonad a -> DesugarMonad a
liftRToD = DesugarMonad . lift . lift

resolve = liftRToD . R.resolve
resolveCon = liftRToD . R.resolveCon

type Ann = HashSet.HashSet L.Span
emptyAnn = HashSet.empty

instance Default (HashSet.HashSet a) where
  def = HashSet.empty

spanToAnn :: Maybe L.Span -> Ann
spanToAnn = maybe emptyAnn HashSet.singleton

data Error =
    EnvParseError [AST.ParseError M.QN AST.LocatedExpr]
  | MismatchNumOfArgs
  | NotAllowedPattern
  | ImplicitArgPatternNotImplemented
  deriving (Eq,Show)

at :: a -> Maybe L.Span -> AST.Located a
at = flip AST.Located