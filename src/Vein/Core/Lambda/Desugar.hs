{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Core.Lambda.Desugar where

import qualified Vein.Core.Lambda.Expr as E
import qualified Vein.Core.Lambda.TypeCheck as TC
import qualified Vein.Syntax.AST as AST
import qualified Vein.Syntax.Lexer as L
import qualified Vein.Syntax.Parser as P
import qualified Vein.Core.Module as M
import qualified Vein.Syntax.Resolver as R

import Vein.Util.Counter (Counter)
import Vein.Syntax.Resolver (MatchVar)

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import           Data.Fix                       ( Fix(..)
                                                , cata
                                                )
import Control.Monad.State ( MonadState , get , put , StateT , runState )
import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT, mapReaderT, local, MonadReader)
import           Control.Monad.Except           ( MonadError , throwError , ExceptT , throwError )
import           Control.Applicative            ( liftA2
                                                , Const(Const)
                                                )
import Data.Default (Default(..))
import           Data.Foldable                  ( foldlM
                                                , fold
                                                )
import           Data.Functor.Const             ( getConst )
import           Data.Functor.Identity          ( Identity(Identity)
                                                , runIdentity
                                                )
import           Data.Bitraversable             ( bitraverse
                                                , bisequence
                                                , bimapM
                                                )
import           Data.Bifunctor                 ( first
                                                , bimap
                                                )
import           Data.Maybe                     ( isJust )
import           Data.List                      ( uncons
                                                , groupBy
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad                  ( (<=<)
                                                , replicateM
                                                , join
                                                )
import Numeric.Natural (Natural)
import           Safe                           ( headMay )
import           Control.Monad.Trans.Maybe      ( MaybeT
                                                , runMaybeT
                                                , MaybeT(MaybeT)
                                                )

desugar :: AST.Env M.QN AST.LocatedExpr -> DesugarMonad (TC.GlobalEnv Ann)
desugar (AST.Env defs anns) =
  TC.GlobalEnv <$> HashMap.traverseWithKey desugarItem undefined <*> traverse desugarAnn anns

desugarItem = undefined

desugarAnn :: AST.Annotation M.QN AST.LocatedExpr -> DesugarMonad (TC.TypedExpr Ann)
desugarAnn = \case
  AST.TypeAnnotation e t -> Fix <$> liftA2 TC.typed (desugarExpr $ undefined e) (desugarExpr $ undefined t)

type Expr = TC.ExprFWith Ann (TC.TypedExpr Ann)

desugarExpr :: Monad m => AST.LocatedExpr' MatchVar -> DesugarMonadT m Expr
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
          op' <- resolveConst $ Right $ AST.uopToName op
          eExprFWith ann <$> (E.App <$> withType (eExprFWith annOp $ E.Const op') <*> desugarExprWithType e)
        where annOp = spanToAnn l
      
      AST.EBinaryOpF (AST.Located l op) e1 e2 -> do
          let f = either desugarExprWithType (withType <=< fmap (eExprFWith (spanToAnn l) . E.Const) . resolveConst . Right) $ AST.bopToExpr' op
          eExprFWith ann <$> app2
            f
            (desugarExprWithType e1)
            (desugarExprWithType e2)
        where annOp = spanToAnn l
              app2 e1' e2' e3'  = E.App <$> (withType . eExprFWith (spanToAnn $ span `P.composeSpan` l) =<< (E.App <$> e1' <*> e2')) <*> e3'
      
      AST.ELiteralF l -> return $ eExprFWith ann $ E.Primitive $ E.Literal l

      AST.ETyping body t -> eExprFWith ann <$> (E.Typing <$> desugarExprWithType body <*> desugarExprWithType t)

      AST.ELetInF decls body -> desugarLetIn' decls body
      AST.EWhereF body decls -> desugarLetIn' decls body

      AST.ECaseOfF body (AST.Located lClauses clauses) -> do
        body' <- desugarExprWithType body
        return $ eExprFWith ann $ E.Case body' undefined undefined
      
      AST.EListF es -> eExprFWith ann . E.Primitive . E.List <$> mapM desugarExprWithType (AST.unLocated es)

      AST.ETupleF es -> eExprFWith ann . E.Primitive . E.Tuple <$> mapM desugarExprWithType (AST.unLocated es)

      AST.ELamF pat body -> undefined
      AST.EArrowF params body -> undefined

      AST.EHole -> TC.ExprFWith ann . TC.EMetaVar <$> genMetaVar

      AST.EPlaceholder -> throwError $ NotAllowedPattern `at` span

      AST.EVar v -> resolve ann v

    where
      desugarLetIn' decls body = do
        decls' <- unwrapParsedEnv decls
        desugarLetIn decls' body

eExprFWith ann = TC.ExprFWith ann . TC.EExpr

desugarLetIn :: Monad m => AST.Env MatchVar (AST.LocatedExpr' MatchVar) -> AST.LocatedExpr' MatchVar -> DesugarMonadT m Expr
desugarLetIn decls e = R.localScope R.emptyQN (declNames decls) HashMap.empty [] undefined

declNames :: AST.Env MatchVar (AST.LocatedExpr' MatchVar) -> HashSet.HashSet MatchVar
declNames decls = AST.mapModuleMap (HashSet.fromList . map fst) $ AST.envModule decls

toDestructuringAssignment :: AST.Located (AST.Env MatchVar (AST.LocatedExpr' MatchVar)) -> DesugarMonad (Expr,Expr)
toDestructuringAssignment (AST.Located l decls) = do
    decls' <- AST.mapModuleMap (mapM $ mapM $ fnBindings . AST.Located l . coerceConst) $ AST.envModule decls
    bimapM (fmap mkTuple . mapM (withType <=< resolve (spanToAnn l))) (return . mkTuple) $ unzip decls'
  where
    coerceConst (AST.ItemDef (AST.DefConst c)) = c
    mkTuple = eExprFWith emptyAnn . E.Primitive . E.Tuple


fnBindings :: AST.Located [(AST.Located [AST.Located (AST.Param (AST.LocatedExpr' MatchVar))] , AST.LocatedExpr' MatchVar)]
                -> DesugarMonad (TC.TypedExpr Ann)
fnBindings (AST.Located l clauses) =
  if all ((== nArgs) . length . fst) clauses then do
    typeofArgs <- replicateM nArgs $ Fix . (`TC.typed` TC.univ) <$> metaVar

    args <- mapM (withType . eExprFWith ann . E.Var) $ reverse [0 .. fromIntegral $ nArgs-1]

    body <- E.Case <$> withType (eExprFWith ann $ E.Primitive $ E.Tuple args) <*> mapM desugarClause' clauses <*> undefined

    multiLam (zip typeofArgs $ repeat ann) =<< withType (eExprFWith ann body)
  else
    throwError $ MismatchNumOfArgs `at` l

  where
    nArgs = length $ fst $ head clauses

    ann = spanToAnn l

    desugarClause' (AST.Located lParams params , e) = undefined


match ::  [MatchVar] -> [([AST.LocatedExpr' MatchVar] , AST.LocatedExpr' MatchVar)] -> AST.LocatedExpr' MatchVar
            -> DesugarMonadT Counter (TC.TypedExpr Ann , [E.Alt (TC.TypedExpr Ann)] , TC.TypedExpr Ann)
match (u:us) qs def =
    case mapMay_headPat coerceVar of
      -- The Variable Rule
      Just qs' -> match us (map ( \((v,ps),e') -> (ps , subst u v e') ) qs') def

      Nothing -> mapMayM_headPat coerceCon >>= \case
        -- The Constructor Rule
        Just qs' -> do
            u' <- withType =<< desugarMatchVar u
            def' <- desugarExprWithType def
            return (u' , alts , def')
          where alts = undefined
                groupByCon = groupBy eqCon qs'
                eqCon x y = con x == con y
                con = fst . fst . fst
        
        Nothing -> undefined

  where
    mapMayM_headPat f = runMaybeT $ mapM (firstM $ (firstM f =<<) . hoistMaybe . uncons) qs
    mapMay_headPat f = runIdentity $ mapMayM_headPat $ hoistMaybe . f
    firstM f = bimapM f return

    hoistMaybe :: Applicative f => Maybe a -> MaybeT f a
    hoistMaybe = MaybeT . pure

    coerceVar (Fix (AST.LocatedExprF (AST.EVar v) _)) = Just v
    coerceVar _ = Nothing

    coerceCon :: AST.LocatedExpr' MatchVar -> MaybeT (DesugarMonadT m) (R.Con , [AST.LocatedExpr' MatchVar])
    coerceCon e = undefined

    subst :: Eq v => v -> v -> AST.LocatedExpr' v -> AST.LocatedExpr' v
    subst v v' = cata $ Fix . first (\u -> if u == v then v' else u)

-- The Empty Rule
match [] qs e = undefined


desugarMatchVar :: MatchVar -> DesugarMonadT m Expr
desugarMatchVar mv = undefined


multiLam :: [(TC.TypedExpr Ann , Ann)] -> TC.TypedExpr Ann -> DesugarMonad (TC.TypedExpr Ann)
multiLam ts e = foldlM (\e' (t,ann) -> withType $ eExprFWith ann $ E.Lam $ E.Abs t e') e ts

{-

mkCase :: AST.LocatedExpr -> AST.Located [AST.Located (AST.Clause AST.LocatedExpr)] -> DesugarMonad (E.ExprF (TC.TypedPat Ann) (TC.TypedExpr Ann))
mkCase e clauses = E.Case <$> desugarExprWithType e <*> desugarClauses (AST.unLocated clauses)

desugarClauses :: [AST.Located (AST.Clause AST.LocatedExpr)] -> DesugarMonad [E.Clause (TC.TypedPat Ann) (TC.TypedExpr Ann)]
desugarClauses = traverse desugarClause

desugarClause :: AST.Located (AST.Clause AST.LocatedExpr) -> DesugarMonad (E.Clause (TC.TypedPat Ann) (TC.TypedExpr Ann))
desugarClause (AST.Located _ (AST.Clause pat e)) = do
  pat' <- desugarPatWithType pat
  fvs <- undefined <$> freeVars pat
  e' <- R.localScope R.emptyQN HashSet.empty HashMap.empty fvs $ desugarExprWithType e
  return $ E.Clause pat' e'

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
desugarPatWithType e = patWithType =<< desugarPat e

patWithType :: E.PatFWith Ann (TC.TypedPat Ann) -> DesugarMonad (TC.TypedPat Ann)
patWithType pat = Fix . (pat `TC.typedPat`) <$> metaVar

desugarPatParamWithType :: AST.Located (AST.Param AST.LocatedExpr) -> DesugarMonad (TC.TypedPat Ann)
desugarPatParamWithType (AST.Located l p) = case p of
  AST.Param e -> desugarPatWithType e
  AST.ParamImplicit _ -> throwError $ ImplicitArgPatternNotImplemented `at` l

type Pat = E.PatFWith Ann (TC.TypedPat Ann)
-}


uopToApp :: AST.Located AST.UnaryOp -> AST.LocatedExpr' MatchVar
              -> (AST.LocatedExpr' MatchVar , AST.Located [AST.Located (AST.Param (AST.LocatedExpr' MatchVar))])
uopToApp (AST.Located lop op) e =
  ( Fix $ AST.LocatedExprF (AST.EVar $ Right $ AST.uopToName op) lop
  , AST.Located le [AST.Located le $ AST.Param e]
  )
  where AST.LocatedExprF _ le = unFix e

bopToApp :: AST.Located (AST.Located (AST.BinaryOp (AST.LocatedExpr' MatchVar)), AST.LocatedExpr' MatchVar , AST.LocatedExpr' MatchVar)
              -> (AST.LocatedExpr' MatchVar , AST.Located [AST.Located (AST.Param (AST.LocatedExpr' MatchVar))])
bopToApp (AST.Located l (AST.Located lop op , e1 , e2)) = 
  ( case AST.bopToExpr' op of {
      Left op' -> op';
      Right op' -> Fix $ AST.LocatedExprF (AST.EVar $ Right op') lop;
    }
  , AST.Located l [AST.Located le1 $ AST.Param e1 , AST.Located le2 $ AST.Param e2]
  )
  where
    AST.LocatedExprF _ le1 = unFix e1
    AST.LocatedExprF _ le2 = unFix e2

multiParamLam :: [AST.Located (AST.Param (AST.LocatedExpr' MatchVar))] -> AST.Located Expr -> DesugarMonad Expr
multiParamLam params (AST.Located lb body) =
  foldlM (\e p@(AST.Located lp _) -> TC.ExprFWith (spanToAnn $ P.composeSpan lp lb) . TC.EExpr <$> lam p e) body params

lam :: AST.Located (AST.Param (AST.LocatedExpr' MatchVar)) -> Expr -> DesugarMonad (E.ExprF (TC.TypedExpr Ann))
lam (AST.Located l param) e =
  E.Lam <$> (E.Abs <$> (Fix . (`TC.typed` TC.univ) . TC.ExprFWith (spanToAnn l) . TC.EMetaVar <$> genMetaVar) <*> withType e)

unwrapParsedEnv :: Monad m => AST.Located (AST.ParsedEnv MatchVar (AST.LocatedExpr' MatchVar))
                                -> DesugarMonadT m (AST.Env MatchVar (AST.LocatedExpr' MatchVar))
unwrapParsedEnv (AST.Located l (AST.ParsedEnv env)) = either (throwError . (`at` l) . EnvParseError) return env

desugarParam :: Monad m => AST.Param (AST.LocatedExpr' MatchVar) -> DesugarMonadT m (AST.Param (TC.TypedExpr Ann))
desugarParam = traverse desugarExprWithType

desugarExprWithType :: Monad m => (AST.LocatedExpr' MatchVar) -> DesugarMonadT m (TC.TypedExpr Ann)
desugarExprWithType e = Fix <$> liftA2 TC.typed (desugarExpr e) metaVar

withType :: Monad m => TC.ExprFWith Ann (TC.TypedExpr Ann) -> DesugarMonadT m (TC.TypedExpr Ann)
withType e = Fix . (e `TC.typed`) <$> metaVar

locToAnn :: (a -> TC.ExprF r) -> AST.Located a -> TC.ExprFWith Ann r
locToAnn f (AST.Located l e) = TC.ExprFWith (spanToAnn l) (f e)


traverseExprV' :: (MatchVar -> DesugarMonad a) -> AST.LocatedExpr' MatchVar -> DesugarMonad (AST.LocatedExpr' a)
traverseExprV' f = fmap runIdentity . traverseExprV (fmap Identity . f)

freeVars :: AST.LocatedExpr' MatchVar -> DesugarMonad (HashSet.HashSet MatchVar)
freeVars = foldMapMExprV $ \v -> maybe (HashSet.singleton v) (const HashSet.empty) <$> tryResolve v

traverseExprV :: Applicative f => (MatchVar -> DesugarMonad (f a)) -> AST.LocatedExpr' MatchVar -> DesugarMonad (f (AST.LocatedExpr' a))
traverseExprV f (Fix (AST.LocatedExprF e l)) = fmap (Fix . flip AST.LocatedExprF l) <$> case e of
    AST.EVar v -> fmap AST.EVar <$> f v

    AST.ELetInF decls body -> traverseLetIn decls body
    AST.EWhereF body decls -> traverseLetIn decls body

    AST.ECaseOfF body clauses -> do
      body' <- traverseExprV f body
      clauses' <- traverse' (traverse' $ traverse' traverseClause) clauses
      return $ AST.ECaseOfF <$> body' <*> clauses'

    AST.ELamF pat body -> do
      fvs <- freeVars pat
      pat' <- traverseExprV f pat
      body' <- traverseInnerScope fvs body
      return $ AST.ELamF <$> pat' <*> body'
    
    AST.EArrowF params body -> do
      (params',body') <- go params body HashSet.empty
      return $ AST.EArrowF <$> params' <*> body'

      where
        go [] body' fvs = do
          body'' <- traverseInnerScope fvs body'
          return (pure [] , body'')

        go (param:params') body' fvs = do
          fvs' <- (fvs <>) <$> freeVars (unParam $ AST.unLocated param)
          param' <- traverse' (traverse' $ traverseInnerScope fvs') param
          (params'' , body'') <- go params' body' fvs'
          return (liftA2 (:) param' params'' , body'')

    _ -> bisequence <$> bitraverse f (traverseExprV f) e
  
  where
    traverseLetIn decls@(AST.Located lDecls _) body = do
      decls' <- unwrapParsedEnv decls
      body' <- traverseInnerScope (declNames decls') body
    
      mod' <- fmap bisequence . bitraverse f (traverseExprV f) $ AST.envModule decls'
      anns <- sequence' $ fmap bisequence . bitraverse f (traverseExprV f) <$> AST.envAnnotations decls'
      let env' = AST.Env <$> mod' <*> anns

      return $ AST.ELetInF <$> (AST.Located lDecls . AST.ParsedEnv . Right <$> env') <*> body'


    traverseInnerScope decls body = R.localScope R.emptyQN decls HashMap.empty [] (traverseExprV f body)
    traverseInnerScope' decls args body = R.localScope R.emptyQN decls HashMap.empty args (traverseExprV f body)
    

    traverseClause (AST.Clause pat body) = do
      pat' <- traverseExprV f pat
      body' <- join $ traverseInnerScope <$> freeVars pat <*> pure body
      return $ AST.Clause <$> pat' <*> body'


    fmap' :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
    fmap' f = fmap (fmap f)

    traverse' :: (Applicative f , Applicative g , Traversable t) => (a -> f (g b)) -> t a -> f (g (t b))
    traverse' f = fmap (traverse id) . traverse f

    sequence' :: (Applicative f , Applicative g , Traversable t) => t (f (g a)) -> f (g (t a))
    sequence' = traverse' id

foldMapMExprV :: Monoid m => (MatchVar -> DesugarMonad m) -> (AST.LocatedExpr' MatchVar) -> DesugarMonad m
foldMapMExprV f = fmap getConst . traverseExprV (fmap Const . f)


unParam :: AST.Param a -> a
unParam (AST.Param x) = x
unParam (AST.ParamImplicit x) = x


newtype DesugarMonadT m a = DesugarMonadT (StateT TC.MetaVar (ExceptT (AST.Located Error) (R.ResolverMonadT m)) a)
  deriving (Functor , Applicative , Monad , MonadState TC.MetaVar , MonadError (AST.Located Error) , MonadReader R.Scope')

type DesugarMonad = DesugarMonadT Identity

genMetaVar :: Monad m => DesugarMonadT m TC.MetaVar
genMetaVar = DesugarMonadT $ do
  v@(TC.MetaVar n) <- get
  put $ TC.MetaVar $ n + 1
  return v

metaVar :: Monad m => DesugarMonadT m (TC.ExprFWith Ann (TC.TypedExpr Ann))
metaVar = TC.expr . TC.EMetaVar <$> genMetaVar

liftRToD :: Monad m => R.ResolverMonadT m a -> DesugarMonadT m a
liftRToD = DesugarMonadT . lift . lift

resolve ann v = 
  tryResolveArg v >>= maybe
    (eExprFWith ann . E.Const <$> resolveConst v)
    (return . eExprFWith ann . E.Var)

resolveConst :: Monad m => MatchVar -> DesugarMonadT m Natural
resolveConst = liftRToD . R.resolve

resolveCon :: Monad m => MatchVar -> DesugarMonadT m R.Con
resolveCon = liftRToD . R.resolveCon

resolveArg :: Monad m => MatchVar -> DesugarMonadT m Natural
resolveArg = liftRToD . R.resolveArg

tryResolve = liftRToD . R.tryResolve

tryResolveArg :: Monad m => MatchVar -> DesugarMonadT m (Maybe Natural)
tryResolveArg = liftRToD . R.tryResolveArg

type Ann = HashSet.HashSet L.Span
emptyAnn = HashSet.empty

instance Default (HashSet.HashSet a) where
  def = HashSet.empty

spanToAnn :: Maybe L.Span -> Ann
spanToAnn = maybe emptyAnn HashSet.singleton

data Error =
    EnvParseError [AST.ParseError MatchVar (AST.LocatedExpr' MatchVar)]
  | MismatchNumOfArgs
  | NotAllowedPattern
  | ImplicitArgPatternNotImplemented
  | NotAllowedExpr
  deriving (Eq,Show)

at :: a -> Maybe L.Span -> AST.Located a
at = flip AST.Located