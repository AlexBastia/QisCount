module Main (main) where

import Program.Parser
import Logic.GenVC
import Solver.Solver

import System.Environment
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  mainAct args

mainAct :: [String] -> IO ()
mainAct [] = do
  putStrLn "Please give a valid file path."

mainAct [filepath] = do
  input <- readFile filepath
  case parseProgram filepath input of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right out -> do
          let vcs = genVCs out
          results <- sequence $ verifyVCs vcs
          print results

mainAct _ = putStrLn "Too many arguments."