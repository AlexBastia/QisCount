module Syntax where

-- Boolean and Integer Expressions
type Vname = String

data Expr
    = BoolLit Bool
    | NumLit Integer
    | Ide Vname
    | BinOp Op2 Expr Expr 
    | MonOp Op1 Expr
    | QC Expr Expr
    deriving (Show, Eq)

data Op2
    = Add | Sub | Mult | Div | And | Or | Eq | Lt | Gt | Ge | Le
    deriving (Show, Eq)

data Op1
    = Not | Neg | Pos
    deriving (Show, Eq)

rwords :: [String]
rwords = ["if", "then", "else", "while", "True", "False", "QuantumCircuit"]

-- Commands
data Command
    = Assign Vname Expr
    | Seq [Command]
    | If Expr Command (Maybe Command)
    | While Expr Command
    | UnitOp Vname Gate [Expr]
    | Measure Vname Expr Expr
    | Comp Vname Expr
    | AddQ Vname Expr
    | AddB Vname Expr
    deriving (Show, Eq)
data Gate
    = H | CX | X | RX
    deriving (Show, Eq)