{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Syntax.PrettyPrinter where

import Vein.Syntax.Lexer as L
import Vein.Syntax.Parser as P

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Text (Text)
import Data.Fix (Fix(..))

instance Pretty e => Pretty (P.Top e) where
  pretty = stmts . P.definitions


instance Pretty r => Pretty (P.ExprF r) where
  pretty = \case
    P.EApp f xs -> pretty f <+> (align $ vsep $ map pretty xs)
    P.EUnaryOpF op e -> pretty op <+> pretty e
    P.ELiteralF l -> pretty l
    P.ELetInF ps e -> "let" <+> pretty ps <+> softnest ("in" <+> pretty e)
    P.EWhereF e ps -> pretty e <+> softnest ("where" <+> pretty ps)
    P.ECaseOfF e cs -> "case" <+> pretty e <+> softnest ("of" <+> pretty cs)
    P.EMatchF cs -> "match" <+> softnest (pretty cs)
    P.EListF es -> list $ map pretty $ P.unLocated es
    P.ETupleF es -> tupled $ map pretty $ P.unLocated es
    P.ELamF e1 e2 -> "\\" <> pretty e1 <+> "->" <+> pretty e2 
    P.EArrowF es e -> hsep (map ((<+> "->") . pretty) es) <+> pretty e
    P.EDo ss -> "do {" <+> stmts (unLocated ss) <+> "}"
    P.EHole -> "?"
    P.EPlaceholder -> "_"
    P.EVar name -> pretty name

    P.EBinaryOpF op e1 e2 -> parens $ case op of
        P.Arrow         -> stdLayout "->"
        P.Plus          -> stdLayout "+"
        P.Minus         -> stdLayout "-"
        P.Times         -> stdLayout "×"
        P.Div           -> stdLayout "/"
        P.Typing        -> stdLayout ":"
        P.AppRight      -> stdLayout "$"
        P.Compose       -> stdLayout "."
        P.ComposeRight  -> stdLayout ">>"
        P.Infixated e   -> stdLayout $ "`" <> pretty e <> "`"

        P.App         -> e1' <+> e2'
        P.AppImplicit -> e1' <+> braces e2'
      where
        e1' = pretty e1
        e2' = pretty e2
        stdLayout op' = e1' <+> op' <+> e2'

instance Pretty e => Pretty (P.LocatedExprF e) where
  pretty = pretty . leExprF

instance Pretty P.LocatedExpr where
  pretty = pretty . unFix

instance Pretty e => Pretty (P.Param e) where
  pretty = \case
    Param e -> pretty e
    ParamImplicit e -> braces $ pretty e

instance Pretty a => Pretty (P.Located a) where
  pretty = pretty . P.unLocated

instance {-# OVERLAPPING #-} Pretty e => Pretty [Located (Prop e)] where
  pretty = stmts

instance Pretty e => Pretty (P.Prop e) where
  pretty = \case
    P.PropEq l r -> pretty l <+> "=" <+> pretty r
    P.PropTypeAnnotation l r -> pretty l <+> ":" <+> pretty r
--  P.PropEqWithTypeAnnotation tl tr el er -> pretty tl <+> ":" <+> pretty tr <> ";" <+> pretty el <+> "=" <+> pretty er

instance Pretty P.Literal where
  pretty = \case
    P.LNat base n -> pretty base <> pretty n
    P.LFP base fp -> pretty base <> pretty fp
    P.LStr s -> pretty $ show s
    P.LChar c -> pretty $ show c

instance Pretty L.Base where
  pretty = \case
    L.Decimal -> "0d"
    L.Hex     -> "0x"
    L.Binary  -> "0b"

instance Pretty L.FloatingPoint where
  pretty = \case

instance Pretty L.QN where
  pretty = \case
    L.QN s -> pretty s
    L.QNCons s r -> pretty s <> "." <> pretty r

instance Pretty P.UnaryOp where
  pretty = \case
    P.Inverse -> "~"

instance Pretty e => Pretty (P.Clause e) where
  pretty (P.Clause pat e) = pretty pat <+> "->" <+> pretty e

instance Pretty e => Pretty (P.Stmt e) where
  pretty = \case
    P.Stmt e -> pretty e
    P.StmtAssign l r -> pretty l <+> "<-" <+> pretty r


instance Pretty e => Pretty (P.Definition e) where
  pretty = \case
    P.DefData d -> pretty d
    P.DefTypeclass d -> pretty d
    P.DefInstance d -> pretty d
    P.DefConst p -> pretty p

instance Pretty e => Pretty (P.Datatype e) where
  pretty = ("data" <+>) . \case
    P.GADT e params props -> prettyExprParamsProps e params props
    P.ADT e cs -> pretty e <+> "="
      <+> group ( flatAlt (line <> "  ") "" <> align (encloseSep (flatAlt "  " "") "" (flatAlt "| " " | ") $ map pretty cs) )

instance Pretty e => Pretty (P.Constructor e) where
  pretty (P.Constructor e) = pretty e

instance Pretty e => Pretty (P.Typeclass e) where
  pretty (P.Typeclass e params props) = "typeclass" <+> prettyExprParamsProps e params props

instance Pretty e => Pretty (P.Instance e) where
  pretty (P.Instance e params props) = "instance" <+> prettyExprParamsProps e params props


softnest = (softline <>) . nest 2

stmts :: Pretty a => [a] -> Doc ann
stmts = vsep . map ( (<> ";") . pretty )

prettyExprParamsProps :: Pretty e => e -> P.Located [P.Located (P.Param e)] -> P.Located [P.Located (P.Prop e)] -> Doc ann
prettyExprParamsProps e params props =
  pretty e <+> (align $ vsep $ map pretty $ unLocated params) <+> "{" <> line <> indent 2 (stmts $ unLocated props) <> line <> "}"