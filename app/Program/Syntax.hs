{-|
Module: Program.Syntax
Description: Proto-Qiskit syntax types

Defines the AST used in the parsing of the program (excluding logica formule) and the 
describes the format of a complete Hoare triple.
-}

module Program.Syntax where

import Expression.Syntax (AExpr, BExpr, QExpr, QF1name, QF2name)

-- Boolean and Integer Expressions
type Vname = String
data Type = 
  TyBool | TyInt | TyQC
  deriving (Show, Eq)

data Expr =
    BE BExpr | AE AExpr | QE QExpr
    deriving (Show)

rwords :: [String]
rwords = ["if", "then", "else", "while", "True", "False", "QuantumCircuit"]

-- Commands
data Command
    = Assign Vname Expr
    | Seq [Command]
    | If BExpr Command (Maybe Command)
    | While BExpr BExpr Command
    | UnitOp Vname Gate
    | Measure Vname AExpr AExpr
    | Compose Vname QExpr
    | AddQ Vname AExpr
    | AddB Vname AExpr
    deriving (Show)

data Gate
    = MonGate QF1name AExpr
    | BinGate QF2name AExpr AExpr
    deriving (Show)

data HoareTriple = HoareTriple
    { htP :: BExpr
    , htC :: Command
    , htQ :: BExpr
    }
    deriving(Show)