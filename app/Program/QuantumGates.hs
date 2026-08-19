module Program.QuantumGates where

data QuantumGate a
    = QG1 QG1name a
    | QG2 QG2name a a
    deriving (Show)

data QG1name
    = H | X
    deriving(Show, Eq, Enum, Ord)

data QG2name
    = SWAP | CR | CNOT
    deriving(Show, Eq, Enum, Ord)

data QGname
    = G1 QG1name | G2 QG2name
    deriving(Show)