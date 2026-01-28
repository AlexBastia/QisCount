module Solver.Solver where

import Logic.Syntax

import Data.SBV

verify :: BExpr -> BExpr -> IO ThmResult
verify p q = prove $ makeImp p q

makeImp :: BExpr -> BExpr -> SBool
makeImp p q = evalBExp p .=> evalBExp q

evalBExp :: BExpr -> SBool
evalBExp (BoolLit k) = undefined
evalBExp (BIde ide) = undefined
evalBExp (Comparison op a b) = undefined
evalBExp (Not a) = undefined
evalBExp (Logic.Syntax.And a b) = undefined
evalBExp (Or a b) = undefined

evalAExp :: AExpr -> SInteger
evalAExp (NumLit k) = undefined
evalAExp (AIde ide) = undefined
evalAExp (BinOp op a b) = undefined
evalAExp (MonOp op a) = undefined
evalAExp (QOp op qc par) = undefined

data SymbGateName
  = H | X | CX | RX | MEAS

type Gate = (SymbGateName, Integer, Integer)

data SQC = SQC
  { sGates :: SList Gate
  , sQb :: SInteger
  , sCb :: SInteger
  }

evalQExpr :: QExpr -> SQC
evalQExpr (QC qb cb) = undefined
evalQExpr (QIde ide) = undefined
evalQExpr (Compose q1 q2) = undefined
evalQExpr (QF (APP g) qc qbs) = undefined
evalQExpr (QF MES qc qbs) = undefined
evalQExpr (QF ADQ qc qbs) = undefined
evalQExpr (QF ADB qc qbs) = undefined
