{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

----------------------------------------------------------------------------
import Compiler
import System.Environment ( getArgs )
----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  print args
  result <- case args of
              []  -> error "expected 1 argument"
              [file] -> do
                program <- readFile file
                return $ parse program
              _   -> error "expected max. 1 argument"
  putStrLn $ "VALID: " ++ show result
