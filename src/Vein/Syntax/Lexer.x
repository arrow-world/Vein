{
{-# LANGUAGE TupleSections #-}

module Vein.Syntax.Lexer where

import Numeric.Natural (Natural)
import qualified Numeric as Numeric
import Data.List (head,drop,splitAt)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)
}

%wrapper "monad"

$digit = [0-9]
$alphabet = [a-zA-Z]
$sign = [\+\-]

@letter = $alphabet | $digit | \'
@name = @letter (@letter | \_)* | \_ (@letter | \_)+
@fqn = (@name \.)* @name

@escape = \\ [\'\"\\abnfrtv0]
@char = @escape | [^\'\"\\]

tokens :-
  $white+               ;
  "--" .* $             ;
  "{-" .* "-}"          ;
  $digit+               { hook $ TNat Decimal . readDec }
  "0x" $digit+          { hook $ TNat Hex . readHex . drop 2 }
  "0b" $digit+          { hook $ TNat Binary . readBin . drop 2 }
  \' @char \'           { hook $ TChar . readChar . init . tail }
  \" @char* \"          { hook $ TStr . readStr . init . tail }
  data                  { hook $ const $ TKeyword Data }
  let                   { hook $ const $ TKeyword Let }
  in                    { hook $ const $ TKeyword In }
  case                  { hook $ const $ TKeyword Case }
  of                    { hook $ const $ TKeyword Of }
  match                 { hook $ const $ TKeyword Match }
  where                 { hook $ const $ TKeyword Where }
  import                { hook $ const $ TKeyword Import }
  module                { hook $ const $ TKeyword Module }
  typeclass             { hook $ const $ TKeyword Typeclass }
  instance              { hook $ const $ TKeyword Instance }
  do                    { hook $ const $ TKeyword Do }
  @fqn                  { hook $ TQN . readQN }
  "="                   { hook $ const $ TSymbol Def }
  "=="                  { hook $ const $ TSymbol Eq }
  "/="                  { hook $ const $ TSymbol Neq }
  "<"                   { hook $ const $ TSymbol LessThan }
  ">"                   { hook $ const $ TSymbol GreaterThan }
  "<=" | "≦"            { hook $ const $ TSymbol LessThanEq }
  ">=" | "≧"            { hook $ const $ TSymbol GreaterThanEq }
  "+"                   { hook $ const $ TSymbol Plus }
  "-"                   { hook $ const $ TSymbol Minus }
  "><" | "×"            { hook $ const $ TSymbol Times }
  "/"                   { hook $ const $ TSymbol Div }
  "~"                   { hook $ const $ TSymbol Inverse }
  "->"                  { hook $ const $ TSymbol Arrow }
  "\\"                  { hook $ const $ TSymbol Lambda }
  "*"                   { hook $ const $ TSymbol Asterisk }
  "^"                   { hook $ const $ TSymbol Power }
  "?"                   { hook $ const $ TSymbol Hole }
  "_"                   { hook $ const $ TSymbol Placeholder }
  ":"                   { hook $ const $ TSymbol Typing }
  "."                   { hook $ const $ TSymbol Compose }
  "$"                   { hook $ const $ TSymbol AppRight }
  "!"                   { hook $ const $ TSymbol LiftFunctor }
  "<-"                  { hook $ const $ TSymbol Assign }
  "("                   { hook $ const $ TParen Round LeftParen }
  ")"                   { hook $ const $ TParen Round RightParen }
  "{"                   { hook $ const $ TParen Curly LeftParen }
  "}"                   { hook $ const $ TParen Curly RightParen }
  "["                   { hook $ const $ TParen Square LeftParen }
  "]"                   { hook $ const $ TParen Square RightParen }
  ","                   { hook $ const $ TSeparator Comma }
  ";"                   { hook $ const $ TSeparator Semicolon }
  "|"                   { hook $ const $ TSeparator VerticalBar }

{
data Token =
    TNat Base Natural
  | TFP Base FloatingPoint
  | TStr String
  | TChar Char
  | TParen ParenSort ParenLR
  | TKeyword Keyword
  | TSymbol Symbol
  | TQN QN
  | TSeparator Separator
  | TEof
  deriving (Eq,Show)

data Span = Span { spanBegin :: AlexPosn , spanEnd :: Maybe Natural }
  deriving (Eq,Show)

spanLength :: Span -> Maybe Natural
spanLength (Span posn end) = do
  end' <- end
  return $ end' - (absAlexPosn posn)

absAlexPosn :: AlexPosn -> Natural
absAlexPosn (AlexPn n _ _) = fromIntegral n

type LocatedToken = (Token , Maybe Span)

hook :: (String -> Token) -> AlexInput -> Int -> Alex LocatedToken
hook f (pos,_,_,input) len =
    return ( f s , Just $ Span pos $ Just $ absAlexPosn pos + fromIntegral len )
  where
    s = take len input

data QN =
    QN String
  | QNCons String QN
  deriving (Eq,Show)

readQN :: String -> QN
readQN = toQN . splitOn "."
  where
    toQN [s] = QN s
    toQN (s:ss) = QNCons s (toQN ss)


data FloatingPoint =
    FPPoint Natural Natural
  | FPExp { fpSign :: Sign , fpExp :: Natural , fpMag :: Natural }
  deriving (Eq,Show)

data Sign = SPlus | SMinus deriving (Eq,Show)

data Base = Decimal | Hex | Binary deriving (Eq,Show)

data ParenSort = Round | Curly | Square deriving (Eq,Show)
data ParenLR = LeftParen | RightParen deriving (Eq,Show)

data Keyword = Data | Let | In | Case | Of | Match | Where | Module | Import | Typeclass | Instance | Do
  deriving (Eq,Show)

data Symbol =
    Def | Eq | Neq | LessThan | GreaterThan | LessThanEq | GreaterThanEq
  | Plus | Minus | Times | Div | Inverse | Arrow | Lambda
  | Asterisk | Power | Hole | Typing | Compose | AppRight | LiftFunctor | ComposeRight | Assign | Placeholder
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
readChar = fst . consumeChar

readStr :: String -> String
readStr [] = []
readStr s =
  let (c,s') = consumeChar s in
    c : readStr s'

consumeChar :: String -> (Char,String)
consumeChar s = case s of
  '\\' : c : s -> (c',s)
    where
      c' = case c of
        'a' -> '\a'
        'b' -> '\b'
        'n' -> '\n'
        'f' -> '\f'
        'r' -> '\r'
        't' -> '\t'
        'v' -> '\v'
        '0' -> '\0'
        '\'' -> '\''
        '\"' -> '\"'
        '\\' -> '\\'
  c : s -> (c,s)

alexEOF = return (TEof,Nothing)
}