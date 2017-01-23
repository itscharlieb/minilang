{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

----------------------------------------------------------------------------
import qualified Parser
-- import qualified Language
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
                return $ Parser.parse program
              _   -> error "expected max. 1 argument"
  print result
  -- either putStrLn (print . eval []) result
