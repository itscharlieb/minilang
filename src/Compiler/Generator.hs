module Compiler.Generator where


import Compiler.Language
import Compiler.TypeChecker


--
generate :: TProgram -> String
generate (TProgram dclrs stmts) = unlines
  [ "#include <stdio.h>"
  , "#include <stdlib.h>"
  , "#include <string.h>"
  , stringRepeat
  , stringConcat
  , "int main() {"
  , generateDclrs dclrs
  , generateStmts stmts
  , "}"
  ]

--
generateDclrs :: [TDclr] -> String
generateDclrs dclrs = unlines $ map generateDclr dclrs


--
generateDclr :: TDclr -> String
generateDclr (TDclr name TInt) = "int " ++ name ++ " = 0;"
generateDclr (TDclr name TFloat) = "float " ++ name ++ " = 0.0;"
generateDclr (TDclr name TString) = "char* " ++ name ++ " = \"\";"


--
generateStmts :: [TStmt] -> String
generateStmts stmts = unlines $ map generateStmt stmts


--
generateStmt :: TStmt -> String
generateStmt (TPrint e@(_, TInt)) = "printf(\"%d\", " ++ generateExp e ++ ");"
generateStmt (TPrint e@(_, TFloat)) = "printf(\"%f\", " ++ generateExp e ++ ");"
generateStmt (TPrint e@(_, TString)) = "printf(\"%s\", " ++ generateExp e ++ ");"

-- TODO fix invalide missing case statement (see note in type checker)
generateStmt (TRead (TId name, TInt)) = "scanf(\"%d\", &" ++ name ++ ");"
generateStmt (TRead (TId name, TFloat)) = "scanf(\"%f\", &" ++ name ++ ");"
generateStmt (TRead (TId name, TString)) = "scanf(\"%s\", " ++ name ++ ");"

generateStmt (TAssign name e) = name ++ " = " ++ generateExp e ++ ";"
generateStmt (TWhile e stmts) =
  "while (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts ++ "}"
generateStmt (TIf e stmts []) =
  "if (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts ++ "}"
generateStmt (TIf e stmts1 stmts2) =
  "if (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts1
    ++ "} else if {\n" ++ generateStmts stmts2 ++ "}"


--
generateExp :: TExp -> String
generateExp (TNegate e, _) = "-" ++ generateExp e
generateExp (TPlus e1 e2, TString) = generateStrConcat e1 e2
generateExp (TPlus e1 e2, _) =
  generateExp e1 ++ " + " ++ generateExp e2
generateExp (TMinus e1 e2, _) =
  generateExp e1 ++ " - " ++ generateExp e2
generateExp (TTimes e1 e2, TString) = generateStrRepeat e1 e2
generateExp (TTimes e1 e2, _) =
  "(" ++ generateExp e1 ++ " * " ++ generateExp e2 ++ ")"
generateExp (TDiv e1 e2, _) =
  "(" ++ generateExp e1 ++ " / " ++ generateExp e2 ++ ")"
generateExp (TIntVal i, _) = show i
generateExp (TFloatVal f, _) = show f
generateExp (TStringVal s, _) = s
generateExp (TId name, _) = name


generateStrConcat :: TExp -> TExp -> String
generateStrConcat e1 e2 =
  "string_concat(" ++ generateExp e1 ++ ", " ++ generateExp e2 ++ ")"


generateStrRepeat :: TExp -> TExp -> String
generateStrRepeat e1 e2 =
  "string_repeat(" ++ generateExp e1 ++ ", " ++ generateExp e2 ++ ")"


stringConcat :: String
stringConcat = unlines
  [ "char* string_concat(char* s1, char* s2) {"
    , "char* buf = malloc(strlen(s2) + strlen(s2));"
    , "snprintf(buf, sizeof buf, \"%s%s\", s1, s2);"
    , "return buf;"
  , "}"
  ]


stringRepeat :: String
stringRepeat = unlines
  [ "char* string_repeat(char* s, int count) {"
    , "char* buf = malloc(strlen(s) * count);"
    , "strcpy(buf, s);"
    , "while (--count > 0) {"
      , "strcat(buf, s);"
    , "}"
    , "return buf;"
  , "}"
  ]
