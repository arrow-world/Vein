{
{-# LANGUAGE TupleSections #-}

module Vein.Syntax.Lexer where

import Numeric.Natural (Natural)
import qualified Numeric as Numeric
import Data.List (head,drop,splitAt)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)
}

%wrapper "posn"

$digit = [0-9]
$alphabet = [a-zA-Z]
$sign = [\+\-]

@alnum = $alphabet | $digit | [\_\']
@fqn = (@alnum\.)*@alnum
@special = []
@char = @special | .

tokens :-
  $white+               ;
  "--" .* $             ;
  "{-" .* "-}"          ;
  $digit+               { locate $ TNat . (Decimal,) . readDec }
  "0x" $digit+          { locate $ TNat . (Hex,) . readHex . drop 2 }
  "0b" $digit+          { locate $ TNat . (Binary,) . readBin . drop 2 }
  \' @char \'           { locate $ TChar . readChar . init . tail }
  data                  { locate $ const $ TKeyword Data }
  let                   { locate $ const $ TKeyword Let }
  in                    { locate $ const $ TKeyword In }
  case                  { locate $ const $ TKeyword Case }
  of                    { locate $ const $ TKeyword Of }
  match                 { locate $ const $ TKeyword Match }
  where                 { locate $ const $ TKeyword Where }
  import                { locate $ const $ TKeyword Import }
  module                { locate $ const $ TKeyword Module }
  typeclass             { locate $ const $ TKeyword Typeclass }
  @fqn                  { locate $ TFQN . readFQN }
  "="                   { locate $ const $ TSymbol Def }
  "=="                  { locate $ const $ TSymbol Eq }
  "/="                  { locate $ const $ TSymbol Neq }
  "<"                   { locate $ const $ TSymbol LessThan }
  ">"                   { locate $ const $ TSymbol GreaterThan }
  "<=" | "≦"            { locate $ const $ TSymbol LessThanEq }
  ">=" | "≧"            { locate $ const $ TSymbol GreaterThanEq }
  "+"                   { locate $ const $ TSymbol Plus }
  "-"                   { locate $ const $ TSymbol Minus }
  "><" | "×"            { locate $ const $ TSymbol Times }
  "/"                   { locate $ const $ TSymbol Div }
  "~"                   { locate $ const $ TSymbol Inverse }
  "->"                  { locate $ const $ TSymbol Arrow }
  "\\"                  { locate $ const $ TSymbol Lambda }
  "*"                   { locate $ const $ TSymbol Asterisk }
  "^"                   { locate $ const $ TSymbol Power }
  "_"                   { locate $ const $ TSymbol Hole }
  ":"                   { locate $ const $ TSymbol Typing }
  "."                   { locate $ const $ TSymbol Compose }
  "$"                   { locate $ const $ TSymbol AppRight }
  "!"                   { locate $ const $ TSymbol LiftFunctor }
  "("                   { locate $ const $ TParen Round LeftParen }
  ")"                   { locate $ const $ TParen Round RightParen }
  "{"                   { locate $ const $ TParen Curly LeftParen }
  "}"                   { locate $ const $ TParen Curly RightParen }
  "["                   { locate $ const $ TParen Square LeftParen }
  "]"                   { locate $ const $ TParen Square RightParen }
  ","                   { locate $ const $ TSeparator Comma }
  ";"                   { locate $ const $ TSeparator Semicolon }
  "|"                   { locate $ const $ TSeparator VerticalBar }

{
data Token =
    TNat (Base , Natural)
  | TFP (Base , FloatingPoint)
  | TStr String
  | TChar Char
  | TParen ParenSort ParenLR
  | TKeyword Keyword
  | TSymbol Symbol
  | TFQN FQN 
  | TSeparator Separator
  deriving (Eq,Show)

type LocatedToken = (Token , AlexPosn)

locate :: (a -> Token) -> AlexPosn -> a -> LocatedToken
locate f loc x = (f x , loc)

data FQN =
    FQN String
  | FQNCons String FQN
  deriving (Eq,Show)

readFQN :: String -> FQN
readFQN fqn =
  let (s:ss) = splitOn "." fqn in
    foldr FQNCons (FQN s) ss


data FloatingPoint =
    FPPoint Natural Natural
  | FPExp { fpSign :: Sign , fpExp :: Natural , fpMag :: Natural }
  deriving (Eq,Show)

data Sign = SPlus | SMinus deriving (Eq,Show)

data Base = Decimal | Hex | Binary deriving (Eq,Show)

data ParenSort = Round | Curly | Square deriving (Eq,Show)
data ParenLR = LeftParen | RightParen deriving (Eq,Show)

data Keyword = Data | Let | In | Case | Of | Match | Where | Module | Import | Typeclass
  deriving (Eq,Show)

data Symbol =
    Def | Eq | Neq | LessThan | GreaterThan | LessThanEq | GreaterThanEq
  | Plus | Minus | Times | Div | Inverse | Arrow | Lambda
  | Asterisk | Power | Hole | Typing | Compose | AppRight | LiftFunctor | ComposeRight
  | UserDef String
  deriving (Eq,Show)

data Separator = Comma | Semicolon | VerticalBar deriving (Eq,Show)

readBin :: Integral a => String -> a
readBin = fst . head . Numeric.readInt 2 (`elem` "01") digitToInt

readDec :: Integral a => String -> a
readDec = fst . head . Numeric.readDec

readHex :: Integral a => String -> a
readHex = fst . head . Numeric.readHex

readChar :: String -> Char
readChar s = head s
}