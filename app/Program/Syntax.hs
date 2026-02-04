module Program.Syntax where

import qualified Logic.Syntax as L

-- Boolean and Integer Expressions
type Vname = String
data Type = 
  TyBool | TyInt | TyQC
  deriving (Show, Eq)

data Expr
    = BoolLit Bool
    | NumLit Integer
    | Ide Vname Type
    | BinOp Op2 Expr Expr 
    | MonOp Op1 Expr
    | QC Expr Expr
    deriving (Show)

data Op2
    = Add | Sub | Mult | Div | And | Or | Eq | Ne | Lt | Gt | Ge | Le
    deriving (Show)

data Op1
    = Not | Neg | Pos
    deriving (Show)

rwords :: [String]
rwords = ["if", "then", "else", "while", "True", "False", "QuantumCircuit"]

-- Commands
data Command
    = Assign Vname Expr Type
    | Seq [Command]
    | If Expr Command (Maybe Command)
    | While Expr L.BExpr Command
    | UnitOp Vname Gate
    | Measure Vname Expr Expr
    | Compose Vname Expr
    | AddQ Vname Expr
    | AddB Vname Expr
    deriving (Show)

data Gate
    = MonGate Gate1 Expr
    | BinGate Gate2 Expr Expr
    deriving (Show)

data Gate1
    = H | X
    deriving (Show)

data Gate2
    = CX | RX
    deriving (Show)

data HoareTriple = HoareTriple
    { htP :: L.BExpr
    , htC :: Command
    , htQ :: L.BExpr
    }
    deriving(Show)