{
module Lexer
  ( Token(..)
  , lexer
  ) where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  $white+                         ;
  "--".*                          ;

  let                             { \s -> TokenLet }
  in                              { \s -> TokenIn }
  \=                              { \s -> TokenEq }
  \+                              { \s -> TokenPlus }
  \-                              { \s -> TokenMinus }
  \*                              { \s -> TokenTimes }
  \/                              { \s -> TokenDiv }
  \(                              { \s -> TokenLParen }
  \)                              { \s -> TokenRParen }

  $digit+                         { \s -> TokenInt (read s) }
  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s }


{
-- Each action has type :: String -> Token


-- The token type:
data Token
  = TokenLet
  | TokenIn
  | TokenVar String
  | TokenInt Int
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenLParen
  | TokenRParen
  deriving (Eq,Show)


-- Lexer wrapper function
lexer :: String -> [Token]
lexer = alexScanTokens

}
