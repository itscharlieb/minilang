{
module Compiler.Lexer
  ( Token(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where


import Prelude hiding (lex)
import Control.Monad ( liftM )

}

%wrapper "monadUserState"

$digit = [0-9] -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$symbol = [\~ \# \$ \^ \& \* \- \+ \/ \` \> \< \= \_ \| \' \; \: \{ \} \[ \] \( \)]
$escaped = [\" a b f n r t v \\]
$graphic = [$digit $alpha $white $symbol]


-- All token actions have type ( AlexPosn -> String -> Token )
tokens :-
  $white+                         ;
  "//".*                          ;

  var                             { lex' TokenVar }
  while                           { lex' TokenWhile }
  do                              { lex' TokenDo }
  done                            { lex' TokenDone }
  if                              { lex' TokenIf }
  then                            { lex' TokenThen }
  else                            { lex' TokenElse }
  endif                           { lex' TokenEndif }
  float                           { lex' TokenFloatType }
  int                             { lex' TokenIntType }
  string                          { lex' TokenStringType }
  print                           { lex' TokenPrint }
  read                            { lex' TokenRead }

  \:                              { lex' TokenColon }
  \;                              { lex' TokenSemicolon }

  \=                              { lex' TokenEq }
  \+                              { lex' TokenPlus }
  \-                              { lex' TokenMinus }
  \*                              { lex' TokenTimes }
  \/                              { lex' TokenDiv }
  \(                              { lex' TokenLParen }
  \)                              { lex' TokenRParen }

  0|[1-9][0-9]*                   { lex (TokenIntVal . read) }

  (0|([1-9][0-9]*))\.[0-9]*       { lex (TokenFloatVal . read) }

  \"($graphic|(\\$escaped))*\"    { lex TokenStringVal }

  [$alpha \_]
    [$alpha $digit \_ \’]*        { lex TokenId }


{

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }


alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"


getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState


setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState


-- Token includes source code position and a token class
data Token = Token AlexPosn TokenClass
  deriving (Eq, Show)


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
  | TokenEOF
  deriving (Eq,Show)


-- Required by Alex spec
alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF


-- Unfortunately, we have to extract the matching bit of string ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex cons = \(p, _, _, s) i -> return $ Token p (cons (take i s))


-- For constructing tokens that do not depend on the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const


-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
      alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
      alexSetInput inp'
      alexMonadScan'
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len


-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)


-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

}
