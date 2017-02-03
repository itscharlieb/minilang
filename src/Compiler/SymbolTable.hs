module Compiler.SymbolTable where


import Compiler.Language
import qualified Data.Map as Map


-- symbol table is a list of name->type mappings
type SymbolTable = Map.Map String PrimType


-- Symbol type
type Symbol = (String, PrimType)


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
