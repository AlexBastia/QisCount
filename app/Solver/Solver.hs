module Solver.Solver where

import Logic.Syntax

import Data.SBV

solve :: BExpr -> BExpr -> IO ThmResult
solve p q = prove $ makeImp p q

makeImp :: BExpr -> BExpr -> SBool
makeImp p q = evalBExp p .=> evalBExp q

evalBExp :: BExpr -> SBool
evalBExp = undefined

evalAExp :: AExpr -> SInteger
evalAExp = undefined

type FQ = FQFname  Integer

evalQExp :: QExp -> 