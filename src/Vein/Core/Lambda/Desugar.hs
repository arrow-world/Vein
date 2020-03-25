{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Core.Lambda.Desugar where

import qualified Vein.Core.Lambda.Expr as E
import qualified Vein.Core.Lambda.TypeCheck as TC
import qualified Vein.Syntax.AST as AST
import qualified Vein.Syntax.Lexer as L
import qualified Vein.Syntax.Parser as P
import qualified Vein.Core.Module as M

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Data.Fix ( Fix(..) )
import Control.Monad.State ( MonadState , get , put , StateT , runState )
import           Control.Monad.Except           ( MonadError , throwError )
import           Control.Applicative            ( liftA2 )
import Data.Default (Default(..))
import           Data.Foldable                  ( foldlM )

desugar :: AST.Env AST.LocatedExpr -> DesugarMonad (TC.GlobalEnv Ann)
desugar (AST.Env defs anns) =
  TC.GlobalEnv <$> HashMap.traverseWithKey desugarItem defs <*> traverse desugarAnn anns

desugarItem = undefined

desugarAnn :: AST.Annotation AST.LocatedExpr -> DesugarMonad (TC.TypedExpr Ann)
desugarAnn = \case
  AST.TypeAnnotation e t -> Fix <$> liftA2 TC.typed (desugarExpr e) (desugarExpr t)

desugarExpr :: AST.LocatedExpr -> DesugarMonad (TC.ExprFWith Ann (TC.TypedExpr Ann))
desugarExpr (Fix (AST.LocatedExprF e span)) = do
    e' <- traverse desugarExprWithType e
    let ann = spanToAnn span
    let eExprFWith ann = TC.ExprFWith ann . TC.EExpr

    case e' of
      AST.EApp f (AST.Located _ (p:ps)) ->
          foldlM (\f' p' -> app <$> (Fix . (f' `TC.typed`) <$> metaVar) <*> return p') (app f p) ps
        where
          app f' (AST.Located l p') = case p' of
            AST.Param e -> eExprFWith (spanToAnn $ span `P.composeSpan` l) $ E.App f' e
      
      AST.EUnaryOpF (AST.Located l op) e ->
          eExprFWith ann <$> (E.App <$> withType (eExprFWith annOp $ E.Const $ AST.uopToName op) <*> pure e)
        where annOp = spanToAnn l
      
      AST.EBinaryOpF (AST.Located l op) e1 e2 ->
          eExprFWith ann <$> app2
            (either return (withType . eExprFWith (spanToAnn l) . E.Const) $ AST.bopToExpr' op)
            (return e1)
            (return e2)
        where annOp = spanToAnn l
              app2 e1' e2' e3'  = E.App <$> (withType . eExprFWith (spanToAnn $ span `P.composeSpan` l) =<< (E.App <$> e1' <*> e2')) <*> e3'
      
      AST.ELiteralF l -> return $ eExprFWith ann $ E.Primitive $ E.Literal l

      AST.ELetInF decls e -> undefined


toDestructuringAssignment :: AST.Env AST.LocatedExpr -> (AST.LocatedExpr , AST.LocatedExpr)
toDestructuringAssignment decls = undefined

unwrapParsedEnv :: AST.ParsedEnv AST.LocatedExpr -> DesugarMonad (AST.Env AST.LocatedExpr)
unwrapParsedEnv (AST.ParsedEnv env) = either (throwError . EnvParseError) return env


desugarExprWithType :: AST.LocatedExpr -> DesugarMonad (TC.TypedExpr Ann)
desugarExprWithType e = Fix <$> liftA2 TC.typed (desugarExpr e) metaVar

withType :: TC.ExprFWith Ann (TC.TypedExpr Ann) -> DesugarMonad (TC.TypedExpr Ann)
withType e = Fix . (e `TC.typed`) <$> metaVar

locToAnn :: (a -> TC.ExprF r) -> AST.Located a -> TC.ExprFWith Ann r
locToAnn f (AST.Located l e) = TC.ExprFWith (spanToAnn l) (f e)


newtype DesugarMonad a = DesugarMonad (StateT TC.MetaVar (Either DesugarError) a)
  deriving (Functor , Applicative , Monad , MonadState TC.MetaVar , MonadError DesugarError)

genMetaVar :: DesugarMonad TC.MetaVar
genMetaVar = DesugarMonad $ do
  v@(TC.MetaVar n) <- get
  put $ TC.MetaVar $ n + 1
  return v

metaVar :: DesugarMonad (TC.ExprFWith Ann (TC.TypedExpr Ann))
metaVar = TC.expr . TC.EMetaVar <$> genMetaVar


type Ann = HashSet.HashSet L.Span
emptyAnn = HashSet.empty

instance Default (HashSet.HashSet a) where
  def = HashSet.empty

spanToAnn :: Maybe L.Span -> Ann
spanToAnn = maybe emptyAnn HashSet.singleton

data DesugarError =
    EnvParseError [AST.ParseError AST.LocatedExpr]
  deriving (Eq,Show)