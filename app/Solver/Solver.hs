module Solver.Solver where

import Logic.Syntax

import Data.SBV
import qualified Data.SBV.List as L
import qualified Data.SBV.Tuple as T

verify :: BExpr -> BExpr -> IO ThmResult
verify p q = prove $ makeImp p q

makeImp :: BExpr -> BExpr -> Symbolic SBool
makeImp p q = do
  hp <- evalBExp p
  th <- evalBExp q
  return $ hp .=> th

evalBExp :: BExpr -> Symbolic SBool
evalBExp (BoolLit k) = undefined
evalBExp (BIde ide) = undefined
evalBExp (Comparison op a b) = undefined
evalBExp (Not a) = undefined
evalBExp (Logic.Syntax.And a b) = undefined
evalBExp (Or a b) = undefined

evalAExp :: AExpr -> Symbolic SInteger
evalAExp (NumLit k) = undefined
evalAExp (AIde ide) = undefined
evalAExp (BinOp op a b) = undefined
evalAExp (MonOp op a) = undefined
evalAExp (QOp op qc par) = undefined

-- Quantum Expressions

data SymbGateName
  = H | X | CX | RX | MEAS

type Gate = (SymbGateName, Integer, Integer)

data SQC = SQC
  { sGates :: SList Gate
  , sQb :: SInteger
  , sCb :: SInteger
  }

sQC :: String -> Symbolic SQC
sQC ide = do
  qb <- sInteger (ide ++ "_qb")
  cb <- sInteger (ide ++ "_cb")
  gates <- sList (ide ++ "_gates")
  return $ SQC gates qb cb

evalQExpr :: QExpr -> Symbolic SQC
evalQExpr (QC qb cb) = do
  sqb <- evalAExp qb
  scb <- evalAExp cb
  return $ SQC { sGates = L.nil :: SList Gate, sQb = sqb, sCb = scb}
evalQExpr (QIde ide) = sQC ide
evalQExpr (Compose q1 q2) = do
  sq1 <- evalQExpr q1
  sq2 <- evalQExpr q2
  return $ SQC (sGates sq1 L.++ sGates sq2) (sQb sq1) (sCb sq1)

-- evalQExpr (QF _ _ []) = fail

-- Siccome ho implementato i gate nella logica e qui in modo diverso devo tradurre
evalQExpr (QF (APP g) qc qbs) = do
  sq <- evalQExpr qc
  h <- evalAExp (head qbs)
  unused <- evalAExp 0
  let sOp = literal Solver.Solver.H
  let sG = T.tuple (sOp, h, unused)
  return $ SQC (sG L..: sGates sq) (sQb sq) (sCb sq)

evalQExpr (QF MES qc qbs) = do
  sq <- evalQExpr qc
  return $ SQC ((MEAS (head qbs) (second qbs)) L..: sGates sq) (sQb sq) (sCb sq)

evalQExpr (QF ADQ qc a) = do
  sq <- evalQExpr qc
  sa <- evalAExp (head a)
  return $ SQC (sGates sq) (sQb sq + sa) (sCb sq)

evalQExpr (QF ADB qc a) = do
  sq <- evalQExpr qc
  sa <- evalAExp (head a)
  return $ SQC (sGates sq) (sQb sq) (sCb sq + sa)
  
