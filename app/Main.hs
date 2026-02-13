{-| 
Module      : Main
Description : Determines validity of a given Hoare triple.

Coordinates the parser, the VC generator and the solver in order to output the total validity (or not) of 
  the given triple.
-}

module Main (main) where

import Program.Parser
import Solver.GenVC
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
          sequence_ $ verifyVCs vcs
          putStrLn "end" 

mainAct _ = putStrLn "Too many arguments."