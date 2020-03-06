{-# LANGUAGE DeriveFunctor #-}

module Vein.Syntax.AST where

import qualified Vein.Syntax.Lexer as L
import Vein.Syntax.Lexer (Span(..))
import Vein.Core.Module as Module

import Numeric.Natural (Natural)
import Data.Fix (Fix(..))

data Top e = Top
  { definitions :: [Module.QNamed (Definition e)]
  , annotations :: [Annotation e]
  }
  deriving (Eq,Show,Functor)

data Statement e =
    Def (Module.QNamed (Definition e))
  | Ann (Annotation e)
  deriving (Eq,Show,Functor)

data Definition e =
    DefData (Datatype e)
  | DefTypeclass (Typeclass e)
  | DefInstance (Instance e)
  | DefConst (Located [Located (Param e)]) e
  deriving (Eq,Show,Functor)

data Annotation e =
    TypeAnnotation e e
  deriving (Eq,Show,Functor)

data Prop e =
    PropEq (Located Module.QN) (Located [Located (Param e)]) e
  | PropTypeAnnotation e e
  deriving (Eq,Show,Functor)

data Clause e = Clause e e
  deriving (Eq,Show,Functor)

data Datatype e =
    GADT (Located [Located (Param e)]) (Located [Located (Prop e)])
  | ADT (Located [Located (Param e)]) [Constructor e]
  deriving (Eq,Show,Functor)

data Typeclass e = Typeclass (Located [Located (Param e)]) (Located [Located (Prop e)])
  deriving (Eq,Show,Functor)

data Instance e = Instance (Located [Located (Param e)]) (Located [Located (Prop e)])
  deriving (Eq,Show,Functor)

data Constructor e = Constructor (Located Module.QN) (Located [Located (Param e)])
  deriving (Eq,Show,Functor)

data Param e =
    Param e
  | ParamImplicit e
  deriving (Eq,Show,Functor)

data ExprF r =
    EApp r (Located [Located (Param r)])
  | EUnaryOpF UnaryOp r
  | EBinaryOpF (BinaryOp r) r r
  | ELiteralF Literal
  | ELetInF (Located [Located (Prop r)]) r
  | EWhereF r (Located [Located (Prop r)])
  | ECaseOfF r (Located [Located (Clause r)])
  | EMatchF (Located [Located (Clause r)])
  | EListF (Located [r])
  | ETupleF (Located [r])
  | ELamF r r
  | EArrowF [Located (Param r)] r
  | EDo (Located [Located (Stmt r)])
  | EHole
  | EPlaceholder
  | EVar Module.QN
  deriving (Eq,Show,Functor)

data LocatedExprF r = LocatedExprF { leExprF :: ExprF r , leSpan :: Maybe Span }
  deriving (Eq,Show,Functor)

type LocatedExpr = Fix LocatedExprF

data Located a = Located { lSpan :: Maybe Span , unLocated :: a }
  deriving (Eq,Show,Functor)

data UnaryOp =
    Inverse
  deriving (Eq,Show)

data BinaryOp e =
    Arrow
  | App
  | AppImplicit
  | Plus
  | Minus
  | Times
  | Div
  | Typing
  | AppRight
  | Compose
  | ComposeRight
  | Infixated e
  deriving (Eq,Show,Functor)

data Literal =
    LNat L.Base Natural
  | LFP L.Base L.FloatingPoint
  | LStr String
  | LChar Char
  deriving (Eq,Show)

data Stmt e =
    Stmt e
  | StmtAssign e e
  deriving (Eq,Show,Functor)
