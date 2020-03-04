{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Syntax.PrettyPrinter where

import Vein.Syntax.Lexer as L
import Vein.Syntax.Parser as P
import Vein.Syntax.AST as AST

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Text (Text)
import Data.Fix (Fix(..))

instance Pretty e => Pretty (AST.Top e) where
  pretty = stmts . AST.definitions


instance Pretty r => Pretty (AST.ExprF r) where
  pretty = \case
    AST.EApp f xs -> pretty f <+> (align $ vsep $ map pretty xs)
    AST.EUnaryOpF op e -> pretty op <+> pretty e
    AST.ELiteralF l -> pretty l
    AST.ELetInF ps e -> "let" <+> pretty ps <+> softnest ("in" <+> pretty e)
    AST.EWhereF e ps -> pretty e <+> softnest ("where" <+> pretty ps)
    AST.ECaseOfF e cs -> "case" <+> pretty e <+> softnest ("of" <+> pretty cs)
    AST.EMatchF cs -> "match" <+> softnest (pretty cs)
    AST.EListF es -> list $ map pretty $ AST.unLocated es
    AST.ETupleF es -> tupled $ map pretty $ AST.unLocated es
    AST.ELamF e1 e2 -> "\\" <> pretty e1 <+> "->" <+> pretty e2 
    AST.EArrowF es e -> hsep (map ((<+> "->") . pretty) es) <+> pretty e
    AST.EDo ss -> "do {" <+> stmts (unLocated ss) <+> "}"
    AST.EHole -> "?"
    AST.EPlaceholder -> "_"
    AST.EVar name -> pretty name

    AST.EBinaryOpF op e1 e2 -> parens $ case op of
        AST.Arrow         -> stdLayout "->"
        AST.Plus          -> stdLayout "+"
        AST.Minus         -> stdLayout "-"
        AST.Times         -> stdLayout "×"
        AST.Div           -> stdLayout "/"
        AST.Typing        -> stdLayout ":"
        AST.AppRight      -> stdLayout "$"
        AST.Compose       -> stdLayout "."
        AST.ComposeRight  -> stdLayout ">>"
        AST.Infixated e   -> stdLayout $ "`" <> pretty e <> "`"

        AST.App         -> e1' <+> e2'
        AST.AppImplicit -> e1' <+> braces e2'
      where
        e1' = pretty e1
        e2' = pretty e2
        stdLayout op' = e1' <+> op' <+> e2'

instance Pretty e => Pretty (AST.LocatedExprF e) where
  pretty = pretty . leExprF

instance Pretty AST.LocatedExpr where
  pretty = pretty . unFix

instance Pretty e => Pretty (AST.Param e) where
  pretty = \case
    Param e -> pretty e
    ParamImplicit e -> braces $ pretty e

instance Pretty a => Pretty (AST.Located a) where
  pretty = pretty . AST.unLocated

instance {-# OVERLAPPING #-} Pretty e => Pretty [Located (Prop e)] where
  pretty = stmts

instance Pretty e => Pretty (AST.Prop e) where
  pretty = \case
    AST.PropEq l r -> pretty l <+> "=" <+> pretty r
    AST.PropTypeAnnotation l r -> pretty l <+> ":" <+> pretty r
--  AST.PropEqWithTypeAnnotation tl tr el er -> pretty tl <+> ":" <+> pretty tr <> ";" <+> pretty el <+> "=" <+> pretty er

instance Pretty AST.Literal where
  pretty = \case
    AST.LNat base n -> pretty base <> pretty n
    AST.LFP base fp -> pretty base <> pretty fp
    AST.LStr s -> pretty $ show s
    AST.LChar c -> pretty $ show c

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

instance Pretty AST.UnaryOp where
  pretty = \case
    AST.Inverse -> "~"

instance Pretty e => Pretty (AST.Clause e) where
  pretty (AST.Clause pat e) = pretty pat <+> "->" <+> pretty e

instance Pretty e => Pretty (AST.Stmt e) where
  pretty = \case
    AST.Stmt e -> pretty e
    AST.StmtAssign l r -> pretty l <+> "<-" <+> pretty r


instance Pretty e => Pretty (AST.Definition e) where
  pretty = \case
    AST.DefData d -> pretty d
    AST.DefTypeclass d -> pretty d
    AST.DefInstance d -> pretty d
    AST.DefConst p -> pretty p

instance Pretty e => Pretty (AST.Datatype e) where
  pretty = ("data" <+>) . \case
    AST.GADT e params props -> prettyExprParamsProps e params props
    AST.ADT e cs -> pretty e <+> "="
      <+> group ( flatAlt (line <> "  ") "" <> align (encloseSep (flatAlt "  " "") "" (flatAlt "| " " | ") $ map pretty cs) )

instance Pretty e => Pretty (AST.Constructor e) where
  pretty (AST.Constructor e) = pretty e

instance Pretty e => Pretty (AST.Typeclass e) where
  pretty (AST.Typeclass e params props) = "typeclass" <+> prettyExprParamsProps e params props

instance Pretty e => Pretty (AST.Instance e) where
  pretty (AST.Instance e params props) = "instance" <+> prettyExprParamsProps e params props


softnest = (softline <>) . nest 2

stmts :: Pretty a => [a] -> Doc ann
stmts = vsep . map ( (<> ";") . pretty )

prettyExprParamsProps :: Pretty e => e -> AST.Located [AST.Located (AST.Param e)] -> AST.Located [AST.Located (AST.Prop e)] -> Doc ann
prettyExprParamsProps e params props =
  pretty e <+> (align $ vsep $ map pretty $ unLocated params) <+> "{" <> line <> indent 2 (stmts $ unLocated props) <> line <> "}"