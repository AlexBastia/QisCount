{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Solver.Solver where

import Logic.Syntax

import Data.SBV
import qualified Data.SBV.List as L
import qualified Data.SBV.Tuple as T

-- Quantum Expressions

data SymbGateName
  = H | X | CX | RX | MEAS
mkSymbolic [''SymbGateName]

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
  sqb <- evalAExpr qb
  scb <- evalAExpr cb
  return $ SQC { sGates = L.nil :: SList Gate, sQb = sqb, sCb = scb}

evalQExpr (QIde ide) = sQC ide

evalQExpr (Compose q1 q2) = do
  sq1 <- evalQExpr q1
  sq2 <- evalQExpr q2
  return $ SQC (sGates sq1 L.++ sGates sq2) (sQb sq1) (sCb sq1)

evalQExpr (QF1 n qc b) = do
  sq <- evalQExpr qc
  let sOpName = literal $ matchGate (M n)
  sb <- evalAExpr b
  let sG = T.tuple (sOpName, sb, 0)
  return $ SQC (sG L..: sGates sq) (sQb sq) (sCb sq)

evalQExpr (QF2 n qc b1 b2) = do
  sq <- evalQExpr qc
  let sOpName = literal $ matchGate (B n)
  sb1 <- evalAExpr b1
  sb2 <- evalAExpr b2
  let sG = T.tuple (sOpName, sb1, sb2)
  return $ SQC (sG L..: sGates sq) (sQb sq) (sCb sq)

matchGate :: QFname -> SymbGateName
matchGate (M Logic.Syntax.H) = Solver.Solver.H
matchGate (M Logic.Syntax.X) = Solver.Solver.X
matchGate (M _) = undefined

matchGate (B Logic.Syntax.CX) = Solver.Solver.CX
matchGate (B Logic.Syntax.RX) = Solver.Solver.RX
matchGate (B Logic.Syntax.MES) = Solver.Solver.MEAS

-- Boolean Expressions

evalBExpr :: BExpr -> Symbolic SBool
evalBExpr (BoolLit k) = return $ literal k
evalBExpr (BIde ide) = sBool ide
evalBExpr (Comparison op a b) = do
  sa <- evalAExpr a
  sb <- evalAExpr b
  case op of
    Eq -> return (sa .== sb)
    Ne -> return (sa ./= sb)
    Ge -> return (sa .>= sb)
    Le -> return (sa .<= sb)
    Gt -> return (sa .> sb)
    Lt -> return (sa .< sb)
evalBExpr (Not a) = do
  sa <- evalBExpr a
  return (sNot sa)
evalBExpr (Logic.Syntax.And a b) = do
  sa <- evalBExpr a
  sb <- evalBExpr b
  return (sa .&& sb)
evalBExpr (Or a b) = do
  sa <- evalBExpr a
  sb <- evalBExpr b
  return (sa .|| sb)

-- Arithmetic Expression

evalAExpr :: AExpr -> Symbolic SInteger
evalAExpr (NumLit k) = return $ literal k
evalAExpr (AIde ide) = sInteger ide
evalAExpr (BinOp op a b) = do
  sa <- evalAExpr a
  sb <- evalAExpr b
  case op of
    Add -> return (sa + sb)
    Sub -> return (sa - sb)
    Mult -> return (sa * sb)
    Div -> undefined -- Do we really only want Ints?
evalAExpr (MonOp op a) = do
  sa <- evalAExpr a
  case op of
    Neg -> return (-sa)
    Pos -> return sa
evalAExpr (QOp op qc par) = do
  sqc <- evalQExpr qc
  -- spar <- map par evalAExpr per ora non li uso
  case op of
    Wid -> return (sQb sqc + sCb sqc)
    GC -> return (L.length (sGates sqc))
    Dep -> undefined -- Mi sa che e' meglio lasciarlo uninterpreted


-- Main Solver

makeImp :: BExpr -> BExpr -> Symbolic SBool
makeImp p q = do
  hp <- evalBExpr p
  th <- evalBExpr q
  return $ hp .=> th

verify :: BExpr -> BExpr -> IO ThmResult
verify p q = prove $ makeImp p q
