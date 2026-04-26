{-| 
Module      : Logic.Syntax
Description : Types for expressions.

Defines the AST created from the parsing of expressions (boolean, arithmetic, quantum circuits). 
-}

module Expression.Syntax where

-- Boolean Value
data BExpr
    = BoolLit Bool
    | BIde String
    | Comparison COp AExpr AExpr
    | And BExpr BExpr
    | Or BExpr BExpr
    | Imp BExpr BExpr
    | Not BExpr

instance Show BExpr where
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
data AExpr
    = NumLit Integer
    | AIde String
    | BinOp Op2 AExpr AExpr 
    | MonOp Op1 AExpr
    | QOp QOpName QExpr [AExpr]

instance Show AExpr where
    show (NumLit a) = show a
    show (AIde i) = show i
    show (BinOp op a b) = show a ++ show op ++ show b
    show (MonOp op a) = show op ++ show a
    show (QOp op q as) = show op ++ "(" ++ show q ++ ", " ++ show as ++ ")"

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

data QOpName
    = Wid | GC | Dep
    deriving(Show)

-- QuantumCircuit Value
data QExpr
    = QC AExpr AExpr
    | QIde String
    | Compose QExpr QExpr
    | QF1 QF1name QExpr AExpr
    | QF2 QF2name QExpr AExpr AExpr

instance Show QExpr where
    show (QC a b) = "qc(" ++ show a ++ ", " ++ show b ++ ")"
    show (QIde i) = show i
    show (Compose a b) = show a ++ " ++ " ++ show b
    show (QF1 op q a) = show op ++ "(" ++ show q ++ ", " ++ show a ++ ")" 
    show (QF2 op q a b) = show op ++ "(" ++ show q ++ ", " ++ show a ++ ", " ++ show b ++ ")" 

data QF1name
    = H | X | ADQ | ADB
    deriving(Show, Eq, Enum, Ord)

data QF2name
    = CX | RX | CNOT | MES
    deriving(Show, Eq, Enum, Ord)

data QFname
    = M QF1name | B QF2name
    deriving(Show)