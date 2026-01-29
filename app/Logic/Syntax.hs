module Logic.Syntax where

-- Boolean Value
data BExpr
    = BoolLit Bool
    | BIde String
    | Comparison COp AExpr AExpr -- In future could be defined on Circ type as well
    | And BExpr BExpr
    | Or BExpr BExpr
    | Not BExpr
    deriving(Show, Eq)

data COp
    = Eq | Ne | Gt | Lt | Ge | Le
    deriving(Show, Eq)

-- Integer Value
data AExpr
    = NumLit Integer
    | AIde String
    | BinOp Op2 AExpr AExpr 
    | MonOp Op1 AExpr
    | QOp QOpName QExpr [AExpr]
    deriving(Show, Eq)

data Op2
    = Add | Sub | Mult | Div 
    deriving(Show, Eq)

data Op1
    = Neg | Pos
    deriving(Show, Eq)

data QOpName
    = Wid | GC | Dep
    deriving(Show, Eq)

-- QuantumCircuit Value
data QExpr
    = QC AExpr AExpr
    | QIde String
    | Compose QExpr QExpr
    | QF1 QF1name QExpr AExpr
    | QF2 QF2name QExpr AExpr AExpr
    deriving(Show, Eq)

data QF1name
    = H | X | ADQ | ADB
    deriving(Show, Eq)

data QF2name
    = CX | RX | MES
    deriving(Show, Eq)

data QFname
    = M QF1name | B QF2name