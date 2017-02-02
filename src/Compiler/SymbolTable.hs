module Compiler.SymbolTable
  ( SymbolTable
  , Symbol
  , Type(..)
  , empty
  , insert
  , get
  ) where


import qualified Data.Map as Map


-- symbol table is a list of name->type mappings
type SymbolTable = Map.Map String Type


-- Symbol type
type Symbol = (String, Type)


-- variable types
data Type
  = IntType
  | FloatType
  | StringType
  deriving (Show, Eq)


--
empty :: SymbolTable
empty = Map.empty


--
insert :: SymbolTable -> Symbol -> Either String SymbolTable
insert table (name, t) =
  case Map.member name table of
    True -> Left $ "Symbol " ++ name ++ " already exists"
    False -> Right $ Map.insert name t table


--
get :: SymbolTable -> String -> Maybe Symbol
get table name = case Map.lookup name table of
  Nothing -> Nothing
  Just t -> Just (name, t)
