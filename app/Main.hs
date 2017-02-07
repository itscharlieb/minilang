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

  -- Parse Program
  program <- case args of
    []  -> error "expected 1 argument"
    [file] -> do
      text <- readFile file
      return $ parse file text
    _ -> error "expected max. 1 argument"

  --
  case program of
    Left errorMsg -> do
      putStrLn errorMsg
    Right program -> do
      putStrLn "============================="
      putStrLn "------------PRETTY-----------"
      putStr $ "VALID:\n" ++ pretty program
      putStrLn "----------TYPE CHECK---------"
      putStr $ show $ typeCheck program
      putStrLn "\n----------GENERATED----------"
      putStr $ generate program
      putStrLn "\n------------DONE-------------"
      putStrLn "=============================\n"
