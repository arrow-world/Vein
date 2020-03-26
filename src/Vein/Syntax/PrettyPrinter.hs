{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Syntax.PrettyPrinter where

import Vein.Syntax.Lexer as L
import Vein.Syntax.Parser as P
import Vein.Syntax.AST as AST
import qualified Vein.Core.Module as Module

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Text (Text)
import Data.Fix (Fix(..))
import qualified Data.HashMap.Lazy as HashMap

{-
instance Pretty e => Pretty (AST.Top e) where
  pretty = stmts . AST.definitions
-}

instance (Pretty e , Pretty v) => Pretty (AST.Statement v e) where
  pretty = \case
    AST.Def name d -> "<Def>" <+> align (pretty $ Named name d)
    AST.Ann a -> "<Ann>" <+> align (pretty a)

instance (Pretty e , Pretty v) => Pretty (AST.Annotation v e) where
  pretty = \case
    AST.TypeAnnotation l r -> pretty l <+> ":" <+> pretty r
    AST.DeclInstance (AST.Instance name params props) -> "instance" <+> prettyExprParamsProps name params props


instance (Pretty e , Pretty v) => Pretty (AST.Env v e) where
  pretty (Env mod anns) =
    "module:" <> line <> indent 2 (align $ stmts $ map (uncurry Named) $ AST.moduleMapToList mod) <> line <>
    "annotations:" <> line <> indent 2 (align $ stmts anns)

instance (Pretty e , Pretty v) => Pretty (AST.ParseError v e) where
  pretty = \case
    AST.MultipleDecl name p i -> "MultipleDecl" <+> pretty p <+> pretty (Named name i)
    AST.MultipleDeclS name d i -> "MultipleDecl" <+> pretty (Named name d) <+> pretty (Named name i)
    AST.NotAllowedExpr e -> "NotAllowedExpr" <+> pretty e

instance (Pretty e , Pretty v) => Pretty (Named v (AST.Item v e)) where
  pretty (Named name i) = case i of
    AST.ItemDef d -> "<ItemDef>" <+> align (pretty $ Named name d)
    AST.ItemEnv e -> "<ItemEnv>" <+> align (pretty e)


instance Pretty r => Pretty (AST.ExprF r) where
  pretty = \case
    AST.EApp f xs -> pretty f <+> (align $ vsep $ map pretty $ unLocated xs)
    AST.EUnaryOpF op e -> pretty op <+> pretty e
    AST.ELiteralF l -> pretty l
    AST.ELetInF ps e -> "let" <+> prettyParsedEnv (unLocated ps) <+> softnest ("in" <+> pretty e)
    AST.EWhereF e ps -> pretty e <+> softnest ("where" <+> prettyParsedEnv (unLocated ps))
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

    AST.EBinaryOpF op e1 e2 -> parens $ case unLocated op of
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

instance {-# OVERLAPPING #-} (Pretty e , Pretty v) => Pretty [Located (Prop v e)] where
  pretty = stmts

instance (Pretty e , Pretty v) => Pretty (AST.Prop v e) where
  pretty = \case
    AST.PropEq name params r -> prettyNameParams (unLocated name) params <+> "=" <+> pretty r
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

instance Pretty AST.UnaryOp where
  pretty = \case
    AST.Inverse -> "~"

instance Pretty e => Pretty (AST.Clause e) where
  pretty (AST.Clause pat e) = pretty pat <+> "->" <+> pretty e

instance Pretty e => Pretty (AST.Stmt e) where
  pretty = \case
    AST.Stmt e -> pretty e
    AST.StmtAssign l r -> pretty l <+> "<-" <+> pretty r

data Named v e = Named v e
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance (Pretty e , Pretty v) => Pretty (Named v (AST.Definition v e)) where
  pretty (Named name def) = case def of
      AST.DefData d -> ("data" <+>) $ case d of
        AST.GADT params props -> prettyExprParamsProps name params props
        AST.ADT params cs -> prettyNameParams name params <+> "="
          <+> group ( flatAlt (line <> "  ") "" <> align (encloseSep (flatAlt "  " "") "" (flatAlt "| " " | ") $ map pretty cs) )

      AST.DefTypeclass (AST.Typeclass params props) -> "typeclass" <+> prettyExprParamsProps name params props

      AST.DefConst cs -> vsep $ map (\(params,p) -> prettyNameParams name params <+> "=" <+> pretty p) cs

instance (Pretty e , Pretty v) => Pretty (AST.Constructor v e) where
  pretty (AST.Constructor name params) = prettyNameParams (unLocated name) params

instance Pretty Module.Name where
  pretty (Module.Name t) = pretty t

instance Pretty Module.QN where
  pretty (Module.QN t) = encloseSep mempty mempty "." $ map pretty t

softnest = (softline <>) . nest 2

stmts :: Pretty a => [a] -> Doc ann
stmts = vsep . map ( (<> ";") . pretty )

prettyNameParams :: (Pretty e , Pretty v) => v -> AST.Located [AST.Located (AST.Param e)] -> Doc ann
prettyNameParams name params =
  pretty name <+> (align $ hsep $ map (parens . pretty) $ unLocated params)

prettyExprParamsProps :: (Pretty e , Pretty v) => v -> AST.Located [AST.Located (AST.Param e)] -> AST.Located (ParsedEnv v e) -> Doc ann
prettyExprParamsProps e params props =
  prettyNameParams e params <+> "{" <> line <> indent 2 (prettyParsedEnv $ unLocated props) <> line <> "}"

prettyParsedEnv :: (Pretty e , Pretty v) => AST.ParsedEnv v e -> Doc ann
prettyParsedEnv (AST.ParsedEnv env) = either (("<parse error>" <+>) . pretty) pretty env