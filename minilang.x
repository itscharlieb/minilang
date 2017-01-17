{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  $white+                         ;
  "--".*                          ;

  var                             { \s -> Var }
  while                           { \s -> While }
  do                              { \s -> Do }
  done                            { \s -> Done }
  if                              { \s -> If }
  then                            { \s -> Then }
  else                            { \s -> Else }
  endif                           { \s -> EndIf }
  print                           { \s -> Print }
  read                            { \s -> Read }
  int                             { \s -> Int }
  float                           { \s -> Float }
  string                          { \s -> String }

  \;                              { \s -> Semicolon }
  \:                              { \s -> DeclarationColon }

  $digit+                         { \s -> IntVal (read s) }
  $digit+.$digit                  { \s -> FloatVal (read s) }
  \".*\"                          { \s -> StringVal s }

  [$alpha \_] [$alpha $digit \_]* { \s -> Identifier s }
  [\= \+ \- \* \/ \( \)]          { \s -> Sym (head s) }


{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    Var
  | While
  | Do
  | Done
  | If
  | Then
  | Else
  | EndIf
  | Print
  | Read
  | Semicolon
  | DeclarationColon
  | Int
  | Float
  | String
  | IntVal Int
  | FloatVal Float
  | StringVal String
  | Identifier String
  | Sym Char
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
