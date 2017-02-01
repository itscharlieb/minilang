module Compiler.TypeChecker
  ( typeCheck ) where


import Compiler.Language
import Compiler.SymbolTable


--
typeCheck :: Program -> Maybe String
typeCheck (Program dclrs stmts) =
  case buildTable dclrs of
    Left dclrsErrorMsg -> Just dclrsErrorMsg
    Right table -> validateStatements stmts table


--
validateStatements :: [Stmt] -> SymbolTable -> Maybe String
validateStatements [] _ = Nothing
validateStatements (stmt:stmts) table =
  case validateStatement stmt table of
    Just errorMsg -> Just errorMsg
    Nothing -> validateStatements stmts table


--
validateStatement :: Stmt -> SymbolTable -> Maybe String
validateStatement (Print e) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> Nothing
validateStatement (Read name) table =
  case get table name of
    Nothing -> Just $ "Name " ++ name ++ " hasn't been declared"
    Just _ -> Nothing
validateStatement (Assign name e) table =
  case get table name of
    Nothing -> Just $ "Name " ++ name ++ " hasn't been declared"
    Just (_, nameType) -> case validateExpression e table of
      Left errorMsg -> Just errorMsg
      Right expType -> if nameType == expType then Nothing
        else Just $ "Id " ++ name ++ " has type " ++ show nameType ++ " and expression "
          ++ show e ++ " has type " ++ show expType
validateStatement (While e stmts) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> validateStatements stmts table
validateStatement (If e stmts) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> validateStatements stmts table
validateStatement (IfElse e stmts1 stmts2) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> case validateStatements stmts1 table of
      Just errorMsg -> Just errorMsg
      Nothing -> validateStatements stmts2 table
validateStatement (Exp e) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> Nothing


--
validateExpression :: Exp -> SymbolTable -> Either String Type
validateExpression (Negate e) table =
  case validateExpression e table of
    Left errorMsg -> Left errorMsg
    Right type' -> case type' of
      StringType -> Left "Unary negation on String type not permitted"
      _ -> Right type'
validateExpression (Plus e1 e2) table = validateStandardBinaryOp e1 e2 table
validateExpression (Minus e1 e2) table = validateStandardBinaryOp e1 e2 table
validateExpression (Times e1 e2) table =
  case (validateExpression e1 table, validateExpression e2 table) of
    (Left errorMsg, _) -> Left errorMsg
    (_, Left errorMsg) -> Left errorMsg
    (Right t1, Right t2) -> case (t1, t2) of
      (IntType, IntType) -> Right IntType
      (IntType, FloatType) -> Right FloatType
      (IntType, StringType) -> Right StringType
      (FloatType, IntType) -> Right FloatType
      (FloatType, FloatType) -> Right FloatType
      (StringType, IntType) -> Right StringType
      (_, _) -> Left $ "Binary operation on types " ++ show t1
        ++ " and " ++ show t2 ++ " is not defined"
validateExpression (Div e1 e2) table = validateStandardBinaryOp e1 e2 table
validateExpression (Int _) _ = Right IntType
validateExpression (Float _) _ = Right FloatType
validateExpression (String _) _ = Right StringType
validateExpression (Id name) table =
  case get table name of
    Nothing -> Left $ "Name " ++ name ++ " hasn't been declared"
    Just (_, t) -> Right t


--
validateStandardBinaryOp :: Exp -> Exp -> SymbolTable -> Either String Type
validateStandardBinaryOp e1 e2 table =
  case (validateExpression e1 table, validateExpression e2 table) of
    (Left errorMsg, _) -> Left errorMsg
    (_, Left errorMsg) -> Left errorMsg
    (Right t1, Right t2) -> case (t1, t2) of
      (IntType, IntType) -> Right IntType
      (FloatType, FloatType) -> Right FloatType
      (IntType, FloatType) -> Right FloatType
      (FloatType, IntType) -> Right FloatType
      (StringType, StringType) -> Right StringType
      _ -> Left $ "Binary operation doesn't permit types "
        ++ show t1 ++ " and " ++ show t2


--
buildTable :: [Dclr] -> Either String SymbolTable
buildTable dclrs = buildTable' dclrs empty where
  buildTable' [] table = Right table
  buildTable' (dclr:dclrs') table =
    case insert' table dclr of
      Left errorMsg -> Left errorMsg
      Right table' -> buildTable' dclrs' table'


--
insert' :: SymbolTable -> Dclr -> Either String SymbolTable
insert' table (IntId name) = insert table (name, IntType)
insert' table (FloatId name) = insert table (name, FloatType)
insert' table (StringId name) = insert table (name, StringType)