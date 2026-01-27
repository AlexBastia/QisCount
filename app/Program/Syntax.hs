module Program.Syntax where

import qualified Logic.Syntax as L

-- Boolean and Integer Expressions
type Vname = String

data Expr
    = BoolLit Bool
    | NumLit Integer
    | Ide Vname
    | BinOp Op2 Expr Expr 
    | MonOp Op1 Expr
    | QC [Gate] Expr Expr
    deriving (Show, Eq)

data Op2
    = Add | Sub | Mult | Div | And | Or | Eq | Ne | Lt | Gt | Ge | Le
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
    | While Expr L.BExpr Command
    | UnitOp Vname Gate
    | Measure Vname Expr Expr
    | Compose Vname Expr
    | AddQ Vname Expr
    | AddB Vname Expr
    deriving (Show, Eq)

data Gate
    = MonGate Gate1 Expr
    | BinGate Gate2 Expr Expr
    deriving (Show, Eq)

data Gate1
    = H | X
    deriving (Show, Eq)

data Gate2
    = CX | RX
    deriving (Show, Eq)

data HoareTriple = HoareTriple
    { p :: L.BExpr
    , c :: Command
    , q :: L.BExpr
    }
    deriving(Show)