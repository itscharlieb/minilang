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
  print args

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
    Left errorMsg -> do
      putStrLn errorMsg
      exitFailure
    Right program -> do
      let prettyFile = replaceExtension fp "pretty.min" in
        writeFile prettyFile (pretty program)
      let symbolFile = replaceExtension fp "symbol.txt" in
        writeFile symbolFile $ show $ typeCheck program
      let cFile = replaceExtension fp ".c" in
        writeFile cFile (generate program)
