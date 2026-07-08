{-|
Module      : Solver.Solver
Description : Proves the validity of a Logic BExpr

Evaluates the semantic value of a Logic expression using SBV and a store to maintain variable name
and type coherence.
The symbolic version of the expression is then passed to an SMT solver (in this case 'Z3') and the result
of the proof attempt is returned.
-}

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Solver.Solver where

import Expression.Syntax

import Control.Monad.State
import Data.SBV
import qualified Data.SBV.List as L
import qualified Data.SBV.Tuple as T
import qualified Data.Map as M

--------------------------------
-- * Symbolic Quantum Circuit --
--------------------------------

-- TODO: any way to avoid redifining gate names??
-- | Note that the `measure` operation also counts as a quantum gate.
data GateName
  = H | X | CNOT | SWAP | CR | MEAS
mkSymbolic [''GateName]

{-| To simplify the translation into a symbolic value, every gate
has two integer parameters. In the case that only one is needed,
only the first is used and the second is set to 0. -}
type Gate = (GateName, Integer, Integer)
data SQC = SQC
  { sGates :: SList Gate
  , sQb :: SInteger
  , sCb :: SInteger
  } deriving (Show)

-- | Creates a new symbolic Quantum Circuit.
sQC :: String -> Symbolic SQC
sQC ide = do
  qb <- sInteger (ide ++ "_qb")
  cb <- sInteger (ide ++ "_cb")
  gates <- sList (ide ++ "_gates")
  return $ SQC gates qb cb


----------------------
-- * Variable Store --
----------------------

-- | Symbolic version of the three types of possible variables.
data SymVar
  = VBool SBool
  | VInt  SInteger
  | VQC   SQC
  deriving (Show)

-- | Map from variable names to symbolic type.
type Env = M.Map String SymVar

{-| SBV Symbolic monad wrapped in State monad. Allows access to the store
while evaluating expressions. -}
type EvalM a = StateT Env Symbolic a


---------------------------
-- * Semantic Evaluation --
---------------------------

-- 1. Quantum Expressions
getSymQC :: String -> EvalM SQC
getSymQC ide = do
    env <- get
    case M.lookup ide env of
        Just (VQC s) -> return s
        Just _ -> error $ "Type Mismatch: Variable " ++ ide ++ " is not a Quantum Circuit."
        Nothing -> do
            s <- lift $ sQC ide
            put $ M.insert ide (VQC s) env
            return s

evalQExpr :: QExpr -> EvalM SQC

evalQExpr (QC qb cb) = do
  sqb <- evalAExpr qb
  scb <- evalAExpr cb
  return $ SQC { sGates = L.nil :: SList Gate, sQb = sqb, sCb = scb}

evalQExpr (QIde ide) = getSymQC ide

evalQExpr (Compose q1 q2) = do
  sq1 <- evalQExpr q1
  sq2 <- evalQExpr q2
  return $ SQC (sGates sq1 L.++ sGates sq2) (sQb sq1 `smax` sQb sq2) (sCb sq1 `smax` sCb sq2)

evalQExpr (QF1 ADQ qc a) = do
  sq <- evalQExpr qc
  sa <- evalAExpr a
  return $ SQC (sGates sq) (sQb sq + sa) (sCb sq)

evalQExpr (QF1 ADB qc a) = do
  sq <- evalQExpr qc
  sa <- evalAExpr a
  return $ SQC (sGates sq) (sQb sq) (sCb sq + sa)

evalQExpr (QF1 n qc b) = do
  sq <- evalQExpr qc
  let sOpName = matchGate (M n)
  sb <- evalAExpr b
  let noSecondOperand = 0
  let sg = T.tuple (sOpName, sb, noSecondOperand)
  return $ SQC (sg L..: sGates sq) (sQb sq) (sCb sq)

evalQExpr (QF2 MES qc _ _) = do
  evalQExpr qc

evalQExpr (QF2 n qc b1 b2) = do
  sq <- evalQExpr qc
  let sOpName = matchGate (B n)
  sb1 <- evalAExpr b1
  sb2 <- evalAExpr b2
  let sg = T.tuple (sOpName, sb1, sb2)
  return $ SQC (sg L..: sGates sq) (sQb sq) (sCb sq)

-- | Returns corresponding symbolic version of a Logic Quantum Gate.
matchGate :: QFname -> SGateName
matchGate (M Expression.Syntax.H) = sH
matchGate (M Expression.Syntax.X) = sX
matchGate (M Expression.Syntax.ADQ) = error "ADQ is not a quantum gate"
matchGate (M Expression.Syntax.ADB) = error "ADB is not a quantum gate"

matchGate (B Expression.Syntax.CR) = sCR
matchGate (B Expression.Syntax.SWAP) = sSWAP
matchGate (B Expression.Syntax.CNOT) = sCNOT
matchGate (B Expression.Syntax.MES) = sMEAS

-- 2. Boolean Expressions
getSymBool :: String -> EvalM SBool
getSymBool ide = do
    env <- get
    case M.lookup ide env of
        Just (VBool s) -> return s
        Just _ -> error $ "Type Mismatch: Variable " ++ ide ++ " is not a Boolean."
        Nothing -> do
            s <- lift $ sBool ide
            put $ M.insert ide (VBool s) env
            return s

evalBExpr :: BExpr -> EvalM SBool
evalBExpr (BoolLit k) = return $ literal k
evalBExpr (BIde ide) = getSymBool ide
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
evalBExpr (Expression.Syntax.And a b) = do
  sa <- evalBExpr a
  sb <- evalBExpr b
  return (sa .&& sb)
evalBExpr (Or a b) = do
  sa <- evalBExpr a
  sb <- evalBExpr b
  return (sa .|| sb)
evalBExpr (Imp a b) = do
  sa <- evalBExpr a
  sb <- evalBExpr b
  return (sa .=> sb)

-- 3. Arithmetic Expression
getSymInt :: String -> EvalM SInteger
getSymInt ide = do
    env <- get
    case M.lookup ide env of
        Just (VInt s) -> return s
        Just _        -> error $ "Type Mismatch: " ++ ide ++ " is not an Integer."
        Nothing -> do
            s <- lift $ sInteger ide
            put $ M.insert ide (VInt s) env
            return s

evalAExpr :: AExpr -> EvalM SInteger
evalAExpr (NumLit k) = return $ literal k
evalAExpr (AIde ide) = getSymInt ide
evalAExpr (BinOp op a b) = do
  sa <- evalAExpr a
  sb <- evalAExpr b
  case op of
    Add -> return (sa + sb)
    Sub -> return (sa - sb)
    Mult -> return (sa * sb)
    Mod -> return (sa `sMod` sb)
    Div -> return (sa `sDiv` sb)  -- TODO: if sb is 0, the result is 0; is this an issue?
evalAExpr (MonOp op a) = do
  sa <- evalAExpr a
  case op of
    Neg -> return (-sa)
    Pos -> return sa
evalAExpr (QOp Dep qc [qb]) = do
  sqc <- evalQExpr qc
  sqb <- evalAExpr qb
  return $ calcDepth (sGates sqc) sqb
evalAExpr (QOp Dep _ _) = error "Wrong amount of arguments passed to 'depth'."
evalAExpr (QOp op qc _) = do
  sqc <- evalQExpr qc
  case op of
    Wid -> return (sQb sqc + sCb sqc)
    GC -> return (L.length (sGates sqc))

-- | Calculates the depth of a given qbit of a circuit.
calcDepth :: SList Gate -> SInteger -> SInteger
calcDepth gates qb = finalDepth
  where
      initialDepths :: SArray Integer Integer
      initialDepths = constArray 0

      updateDepths :: SArray Integer Integer -> SBV Gate -> SArray Integer Integer
      updateDepths currentAcc sgate =
          let
              (op, q1, q2) = T.untuple sgate

              d1 = readArray currentAcc q1
              d2 = readArray currentAcc q2

              isTwoQubit = op .== sSWAP  .|| op .== sCR .|| op .== sCNOT

              maxInputDepth = ite isTwoQubit (smax d1 d2) d1
              newGateDepth  = maxInputDepth + 1

              acc1 = writeArray currentAcc q1 newGateDepth
              acc2 = writeArray acc1 q2 newGateDepth
          in
              ite isTwoQubit acc2 acc1

      finalDepthsArray :: SArray Integer Integer
      finalDepthsArray = L.foldl updateDepths initialDepths gates

      finalDepth = readArray finalDepthsArray qb


-------------------
-- * Main Solver --
-------------------

{-| Evaluates and proves the given Logic Boolean Expressions.
-}
verifyVC :: BExpr -> IO ThmResult
verifyVC be = do
  putStrLn $ "VC: " ++ show be
  prove $ evalVC be

evalVC :: BExpr -> Symbolic SBool
evalVC be = do
  (res, _finalEnv) <- runStateT (evalBExpr be) M.empty
  return res