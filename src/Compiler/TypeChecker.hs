module Compiler.TypeChecker
  ( typeCheck ) where


import Compiler.Language
import Compiler.SymbolTable


data TypeError
  = TypeMismatch
    { expected :: PrimType
    , found :: PrimType
    }
  | InvalidType
    { found :: PrimType }
  | MissingDeclaration
    { name :: String }
  | DuplicateDeclaration
    { name :: String }
  deriving (Eq)


instance Show TypeError where
  show (TypeMismatch {expected = e,  found = f}) =
    concat ["Expected Type = ", show e, ". Found Type = ", show f]
  show (InvalidType {found = f}) =
    "Invalid Type = " ++ show f
  show (MissingDeclaration {name = n}) =
    "Missing Declaration for " ++ show n
  show (DuplicateDeclaration {name = n}) =
    "Duplication Declaration for " ++ show n


--
typeCheck :: Program -> Maybe TypeError
typeCheck (Program dclrs stmts) =
  case buildTable dclrs of
    Left dclrsErrorMsg -> Just dclrsErrorMsg
    Right table -> validateStatements stmts table


--
validateStatements :: [Stmt] -> SymbolTable -> Maybe TypeError
validateStatements [] _ = Nothing
validateStatements (stmt:stmts) table =
  case validateStatement stmt table of
    Just errorMsg -> Just errorMsg
    Nothing -> validateStatements stmts table


--
validateStatement :: Stmt -> SymbolTable -> Maybe TypeError
validateStatement (Print e) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> Nothing
validateStatement (Read name) table =
  case get table name of
    Nothing -> Just $ MissingDeclaration name
    Just _ -> Nothing
validateStatement (Assign name e) table =
  case get table name of
    Nothing -> Just $ MissingDeclaration name
    Just (_, nameType) -> case validateExpression e table of
      Left errorMsg -> Just errorMsg
      Right expType -> if nameType == expType then Nothing
        else Just $ TypeMismatch nameType expType
validateStatement (While e stmts) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> validateStatements stmts table
validateStatement (If e stmts1 stmts2) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right t -> case t of
      TInt -> case validateStatements stmts1 table of
        Just errorMsg -> Just errorMsg
        Nothing -> validateStatements stmts2 table
      _ -> Just $ TypeMismatch TInt t
validateStatement (Exp e) table =
  case validateExpression e table of
    Left errorMsg -> Just errorMsg
    Right _ -> Nothing


--
validateExpression :: Exp -> SymbolTable -> Either TypeError PrimType
validateExpression (Negate e) table =
  case validateExpression e table of
    Left errorMsg -> Left errorMsg
    Right type' -> case type' of
      TString -> Left $ InvalidType TString
      _ -> Right type'
validateExpression (Plus e1 e2) table = validateStandardBinaryOp e1 e2 table
validateExpression (Minus e1 e2) table = validateStandardBinaryOp e1 e2 table
validateExpression (Times e1 e2) table =
  case (validateExpression e1 table, validateExpression e2 table) of
    (Left errorMsg, _) -> Left errorMsg
    (_, Left errorMsg) -> Left errorMsg
    (Right t1, Right t2) -> case (t1, t2) of
      (TInt, TInt) -> Right TInt
      (TInt, TFloat) -> Right TFloat
      (TInt, TString) -> Right TString
      (TFloat, TInt) -> Right TFloat
      (TFloat, TFloat) -> Right TFloat
      (TString, TInt) -> Right TString
      (_, _) -> Left $ TypeMismatch t1 t2
validateExpression (Div e1 e2) table = validateStandardBinaryOp e1 e2 table
validateExpression (Int _) _ = Right TInt
validateExpression (Float _) _ = Right TFloat
validateExpression (String _) _ = Right TString
validateExpression (Id name) table =
  case get table name of
    Nothing -> Left $ MissingDeclaration name
    Just (_, t) -> Right t


--
validateStandardBinaryOp :: Exp -> Exp -> SymbolTable -> Either TypeError PrimType
validateStandardBinaryOp e1 e2 table =
  case (validateExpression e1 table, validateExpression e2 table) of
    (Left errorMsg, _) -> Left errorMsg
    (_, Left errorMsg) -> Left errorMsg
    (Right t1, Right t2) -> case (t1, t2) of
      (TInt, TInt) -> Right TInt
      (TFloat, TFloat) -> Right TFloat
      (TInt, TFloat) -> Right TFloat
      (TFloat, TInt) -> Right TFloat
      (TString, TString) -> Right TString
      _ -> Left $ TypeMismatch t1 t2


--
buildTable :: [Dclr] -> Either TypeError SymbolTable
buildTable dclrs = buildTable' dclrs empty where
  buildTable' [] table = Right table
  buildTable' (dclr:dclrs') table =
    case insert' table dclr of
      Left errorMsg -> Left errorMsg
      Right table' -> buildTable' dclrs' table'


--
insert' :: SymbolTable -> Dclr -> Either TypeError SymbolTable
insert' table (Dclr name primType) =
  case insert table (name, primType) of
    Left _ -> Left $ DuplicateDeclaration name
    Right table' -> Right table'
