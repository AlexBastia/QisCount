{-| 
Module      : Logic.Syntax
Description : Types for expressions.

Defines the AST created from the parsing of expressions (boolean, arithmetic, quantum circuits). 
-}

module Expression.Syntax where

import Data.Void (Void)

data Expr =
    BE BExpr| AE AExpr | QE QExpr
    deriving (Show)

-- Boolean Value
type BExpr = BooleanFormula AExpr

data BooleanFormula a
    = BoolLit Bool
    | BIde String
    | Comparison COp a a
    | And (BooleanFormula a) (BooleanFormula a)
    | Or (BooleanFormula a) (BooleanFormula a)
    | Imp (BooleanFormula a) (BooleanFormula a)
    | Not (BooleanFormula a)

instance Show a => Show (BooleanFormula a) where
    show (BoolLit b) = show b
    show (BIde i) = show i
    show (Comparison op a b) = show a ++ show op ++ show b
    show (And a b) = show a ++ " && " ++ show b
    show (Or a b) = show a ++ " || " ++ show b
    show (Imp a b) = show a ++ " => " ++ show b
    show (Not a) = "!" ++ show a

data COp
    = Eq | Ne | Gt | Lt | Ge | Le

instance Show COp where
    show Eq = " == "
    show Ne = " != "
    show Gt = " > "
    show Lt = " < "
    show Ge = " >= "
    show Le = " <= "

-- Integer Value
type AExpr = ArithmeticFormula Void

data ArithmeticFormula ext
    = NumLit Integer
    | AIde String
    | BinOp Op2 (ArithmeticFormula ext) (ArithmeticFormula ext) 
    | MonOp Op1 (ArithmeticFormula ext)
    | Ext ext

instance Show ext => Show (ArithmeticFormula ext) where
    show (NumLit a) = show a
    show (AIde i) = show i
    show (BinOp op a b) = show a ++ show op ++ show b
    show (MonOp op a) = show op ++ show a
    show (Ext e) = show e

data Op2
    = Add | Sub | Mult | Div | Mod

instance Show Op2 where
    show Add = " + "
    show Sub = " - "
    show Mult = " * "
    show Div = " / "
    show Mod = " % "

data Op1
    = Neg | Pos

instance Show Op1 where
    show Neg = "-"
    show Pos = "+"

-- QuantumCircuit Value
data QExpr
    = QC AExpr AExpr
    | QIde String
instance Show QExpr where
    show (QC a b) = "qc(" ++ show a ++ ", " ++ show b ++ ")"
    show (QIde ide) = show ide