module AssertLogic.Syntax where

import Program.QuantumGates
import Expression.Syntax (ArithmeticFormula, BooleanFormula)

type Assertion = BooleanFormula ExtendedAExpr

data QOpExt
    =  Wid ExtendedQExpr | GC ExtendedQExpr | Dep ExtendedQExpr ExtendedAExpr
    deriving(Show)

type ExtendedAExpr = ArithmeticFormula QOpExt

data ExtendedQExpr
    = QC ExtendedAExpr ExtendedAExpr
    | QIde String
    | UnitOp ExtendedQExpr (QuantumGate ExtendedAExpr)
    | Measure ExtendedQExpr ExtendedAExpr ExtendedAExpr
    | Compose ExtendedQExpr ExtendedQExpr
    | AddQ ExtendedQExpr ExtendedAExpr
    | AddB ExtendedQExpr ExtendedAExpr

instance Show ExtendedQExpr where
    show (QC a b) = "qc(" ++ show a ++ ", " ++ show b ++ ")"
    show (QIde i) = show i
    show (UnitOp q (QG1 n a)) = show n ++ "(" ++ show q ++ ", " ++ show a ++ ")"
    show (UnitOp q (QG2 n a b)) = show n ++ "(" ++ show q ++ ", " ++ show a ++ ", " ++ show b ++ ")"
    show (Measure q a b) = "measure(" ++ show q ++ ", " ++ show a ++ ", " ++ show b ++ ")"
    show (Compose q1 q2) = "compose(" ++ show q1 ++ ", " ++ show q2 ++ ")"
    show (AddQ q a) = "addQ(" ++ show q ++ ", " ++ show a ++ ")"
    show (AddB q a) = "addB(" ++ show q ++ ", " ++ show a ++ ")"