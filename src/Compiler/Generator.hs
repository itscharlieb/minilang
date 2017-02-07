module Compiler.Generator where


import Compiler.Language


--
generate :: Program -> String
generate (Program dclrs stmts) =
  "#include <stdio.h>\n\n"
    ++ "int main() {\n"
    ++ generateDclrs dclrs
    ++ generateStmts stmts
    ++ "}"


--
generateDclrs :: [Dclr] -> String
generateDclrs dclrs = unlines $ map generateDclr dclrs


--
generateDclr :: Dclr -> String
generateDclr (Dclr name TInt) = "int " ++ name ++ ";"
generateDclr (Dclr name TFloat) = "float " ++ name ++ ";"
generateDclr (Dclr name TString) = "char* " ++ name ++ ";"


--
generateStmts :: [Stmt] -> String
generateStmts stmts = unlines $ map generateStmt stmts


--
generateStmt :: Stmt -> String
generateStmt (Print e) =
  "printf(\"%s\", "
    ++ generateExp e
    ++ ");"
-- TODO
generateStmt (Read name) = "scanf();"
generateStmt (Assign name e) = name ++ " = " ++ generateExp e ++ ";"
generateStmt (While e stmts) =
  "while (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts ++ "}"
generateStmt (If e stmts []) =
  "if (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts ++ "}"
generateStmt (If e stmts1 stmts2) =
  "if (" ++ generateExp e ++ ") {\n" ++ generateStmts stmts1
    ++ "} else if {\n" ++ generateStmts stmts2 ++ "}"
generateStmt (Exp e) = generateExp e


--
generateExp :: Exp -> String
generateExp (Negate e) = "- " ++ generateExp e
generateExp (Plus e1 e2) =
  generateExp e1 ++ " + " ++ generateExp e2
generateExp (Minus e1 e2) =
  generateExp e1 ++ " - " ++ generateExp e2
generateExp (Times e1 e2) =
  "(" ++ generateExp e1 ++ " * " ++ generateExp e2 ++ ")"
generateExp (Div e1 e2) =
  "(" ++ generateExp e1 ++ " / " ++ generateExp e2 ++ ")"
generateExp (Int i) = show i
generateExp (Float f) = show f
generateExp (String s) = s
generateExp (Id name) = name
