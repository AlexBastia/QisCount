module Main (main) where

import Program.Parser

import System.Environment
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  mainAct args

mainAct :: [String] -> IO ()
mainAct [] = do
  putStrLn "Please give a valid file path."

mainAct args = do
  let filepath = unwords args
  input <- readFile filepath
  case parseProgram filepath input of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right out -> print out
