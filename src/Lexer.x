{
module Lexer
  ( Token(..)
  , TokenClass(..)
  , lexer
  ) where
}

%wrapper "posn"

$digit = [0-9] -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$symbol = [\~ \# \$ \^ \& \* \- \+ \/ \` \> \< \= \_ \| \' \; \: \{ \} \[ \] \( \)]
$escaped = [\a \b \f \n \r \t \v \" \\ \@ \! \? \. \,]
$quotable = [$digit $alpha $symbol $escaped]


-- All token actions have type ( AlexPosn -> String -> Token )
tokens :-
  $white+                         ;
  "//".*                          ;

  var                             { \p _ -> Token p TokenVar }
  while                           { \p _ -> Token p TokenWhile }
  do                              { \p _ -> Token p TokenDo }
  done                            { \p _ -> Token p TokenDone }
  if                              { \p _ -> Token p TokenIf }
  then                            { \p _ -> Token p TokenThen }
  else                            { \p _ -> Token p TokenElse }
  endif                           { \p _ -> Token p TokenEndif }
  float                           { \p _ -> Token p TokenFloatType }
  int                             { \p _ -> Token p TokenIntType }
  string                          { \p _ -> Token p TokenStringType }
  print                           { \p _ -> Token p TokenPrint }
  read                            { \p _ -> Token p TokenRead }

  \:                              { \p _ -> Token p TokenColon }
  \;                              { \p _ -> Token p TokenSemicolon }

  \=                              { \p _ -> Token p TokenEq }
  \+                              { \p _ -> Token p TokenPlus }
  \-                              { \p _ -> Token p TokenMinus }
  \*                              { \p _ -> Token p TokenTimes }
  \/                              { \p _ -> Token p TokenDiv }
  \(                              { \p _ -> Token p TokenLParen }
  \)                              { \p _ -> Token p TokenRParen }

  $digit+                         { \p s -> Token p $ TokenIntVal (read s) }

  0 | ([1-9] $digit*)
    "." $digit+                   { \p s -> Token p $ TokenFloatVal (read s) }

  \" ( $quotable # \")* \"        { \p s -> Token p $ TokenStringVal s }

  $alpha [$alpha $digit \_ \’]*   { \p s -> Token p $ TokenId s }


{

-- Token includes source code position and a token class
data Token = Token AlexPosn TokenClass
  deriving (Eq,Show)

-- Each action has type :: String -> TokenClass -> Token
data TokenClass
  = TokenVar
  | TokenId String
  | TokenFloatType
  | TokenFloatVal Float
  | TokenIntType
  | TokenIntVal Int
  | TokenStringType
  | TokenStringVal String
  | TokenWhile
  | TokenDo
  | TokenDone
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenEndif
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenLParen
  | TokenRParen
  | TokenSemicolon
  | TokenColon
  | TokenRead
  | TokenPrint
  deriving (Eq,Show)

-- Lexer wrapper function
lexer :: String -> [Token]
lexer = alexScanTokens


main :: IO ()
main = do
  program <- getLine
  print $ lexer program

}
