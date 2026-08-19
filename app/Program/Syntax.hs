{-|
Module: Program.Syntax
Description: Proto-Qiskit syntax types

Defines the AST used in the parsing of the program (excluding logica formule) and the 
describes the format of a complete Hoare triple.
-}

module Program.Syntax where

import Expression.Syntax (Expr, AExpr, BExpr, QExpr)
import Program.QuantumGates
import AssertLogic.Syntax (Assertion)

data Type = 
  TyBool | TyInt | TyQC
  deriving (Show, Eq)

data HoareTriple = HoareTriple
    { htP :: Assertion
    , htC :: Command
    , htQ :: Assertion
    }
    deriving(Show)

-- Commands
type Vname = String

data Command
    = Assign Vname Expr
    | Seq [Command]
    | If BExpr Command (Maybe Command)
    | While BExpr BExpr Command
    | UnitOp Vname (QuantumGate AExpr)
    | Measure Vname AExpr AExpr
    | Compose Vname QExpr
    | AddQ Vname AExpr
    | AddB Vname AExpr
    deriving (Show)