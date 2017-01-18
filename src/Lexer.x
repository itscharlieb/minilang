{
module Lexer
  ( Token(..)
  , TokenClass(..)
  , lexer
  ) where
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters


-- All token actions have type ( AlexPosn -> String -> Token )
tokens :-
  $white+                         ;
  "--".*                          ;

  let                             { \p _ -> Token p TokenLet }
  in                              { \p _ -> Token p TokenIn }
  \=                              { \p _ -> Token p TokenEq }
  \+                              { \p _ -> Token p TokenPlus }
  \-                              { \p _ -> Token p TokenMinus }
  \*                              { \p _ -> Token p TokenTimes }
  \/                              { \p _ -> Token p TokenDiv }
  \(                              { \p _ -> Token p TokenLParen }
  \)                              { \p _ -> Token p TokenRParen }

  $digit+                         { \p s -> Token p $ TokenInt (read s) }
  $alpha [$alpha $digit \_ \’]*   { \p s -> Token p $ TokenVar s }


{

-- Token includes source code position and a token class
data Token = Token AlexPosn TokenClass
  deriving (Eq,Show)

-- Each action has type :: String -> TokenClass -> Token
data TokenClass
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
