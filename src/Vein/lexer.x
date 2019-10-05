{
module Vein.Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$sign = [\+\-]
@name = $alpha [$alpha $digit \_ \']*

tokens :-
  $white+                         ;
  "--".* 	                        ;
  let                             { \s -> Let }
  in                              { \s -> In }
  $sign? $digit+                  { \s -> Int (read s) }
  @name(\.@name)*                 { \s -> Ident s }

{
  data Token =
    Let
  | In
  | Ident [String]
  | Int Int
  deriving (Eq, Show)

  main = do
    s <- getContents
    let toks = alexScanTokens s
    print toks
}
