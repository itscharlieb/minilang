{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where


import Compiler
import System.Environment ( getArgs )
import System.FilePath
import System.Exit


--
main :: IO ()
main = do
  args <- getArgs

  -- Parse Program
  case args of
    []  -> error "expected 1 argument"
    [filePath] -> compile filePath
    _ -> error "expected max. 1 argument"


--
compile :: FilePath -> IO ()
compile fp = do
  text <- readFile fp

  case parse fp text of
    Left parseError -> do
      print parseError
      exitFailure
    Right program -> do
      writeFile prettyFile (pretty program)
      case typeCheck program of
        Left (typeError, symbolTable) -> do
          writeFile symbolFile (show symbolTable)
          print typeError
          exitFailure
        Right typedProgram ->
          writeFile cFile $ generate typedProgram
      where
        prettyFile = replaceExtension fp "pretty.min"
        symbolFile = replaceExtension fp "symbol.txt"
        cFile = replaceExtension fp ".c"
