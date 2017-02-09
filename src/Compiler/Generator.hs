module Compiler.Generator where


import Compiler.Language
import Compiler.TypeChecker


--
generate :: TProgram -> String
generate (TProgram dclrs stmts) = unlines
  [ "#include <stdio.h>"
  , stringRepeat
  , "int main() {"
  , generateDclrs dclrs
  , generateStmts stmts
  , "}"
  ]


stringRepeat :: String
stringRepeat = unlines
  [ "char* string_repeat(char* s, int count) {"
    , "char* ret = malloc(strlen(s) * count);"
    , "strcpy(ret, s);"
    , "while (--count > 0) {"
      , "strcat(ret, s);"
    , "}"
  , "}"
  ]


--
generateDclrs :: [TDclr] -> String
generateDclrs dclrs = unlines $ map generateDclr dclrs


--
generateDclr :: TDclr -> String
generateDclr (TDclr name TInt) = "int " ++ name ++ ";"
generateDclr (TDclr name TFloat) = "float " ++ name ++ ";"
generateDclr (TDclr name TString) = "char* " ++ name ++ ";"


--
generateStmts :: [TStmt] -> String
generateStmts stmts = unlines $ map generateStmt stmts


--
generateStmt :: TStmt -> String
generateStmt (TPrint e) =
  "printf(\"%s\", "
    ++ generateExp e
    ++ ");"
generateStmt (TRead _) = "scanf();"
generateStmt (TAssign name e) = name ++ " = " ++ generateExp e ++ ";"
generateStmt (TWhile e stmts) =
  "while (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts ++ "}"
generateStmt (TIf e stmts []) =
  "if (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts ++ "}"
generateStmt (TIf e stmts1 stmts2) =
  "if (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts1
    ++ "} else if {\n" ++ generateStmts stmts2 ++ "}"
generateStmt (TExp e) = generateExp e


--
generateExp :: TExp -> String
generateExp (TNegate e, _) = "- " ++ generateExp e
generateExp (TPlus e1 e2, _) =
  generateExp e1 ++ " + " ++ generateExp e2
generateExp (TMinus e1 e2, _) =
  generateExp e1 ++ " - " ++ generateExp e2
generateExp (TTimes e1 e2, _) =
  "(" ++ generateExp e1 ++ " * " ++ generateExp e2 ++ ")"
generateExp (TDiv e1 e2, _) =
  "(" ++ generateExp e1 ++ " / " ++ generateExp e2 ++ ")"
generateExp (TIntId i, _) = show i
generateExp (TFloatId f, _) = show f
generateExp (TStringId s, _) = s
generateExp (TId name, _) = name
