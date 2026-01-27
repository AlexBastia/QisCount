[n > 0]
{
qc <- QuantumCircuit(3,3);
x <- 0;
while x != n: [gatecount(qc) == x]
{
    qc.H(3);
    x <- x + 1;
}
qc.CX(1,1);
}
[gatecount(qc) < 2 * n]

{
p = Comparison Gt (AIde "n") (NumLit 0), n .> 0 .=> gatecount(([], 3, 3)) .== 0
Comparison Eq (QOp GC (QC [] (NumLit 3) (NumLit 3)) []) (NumLit 0)
c = Seq [
    Assign "qc" (QC [] (NumLit 3) (NumLit 3)),
    Assign "x" (NumLit 0),
    While (BinOp Ne (Ide "x") (Ide "n")) (Comparison Eq (QOp GC (QIde "qc") []) (AIde "x")) (
        Seq [
            UnitOp "qc" (MonGate H (NumLit 3)),
            Assign "x" (BinOp Add (Ide "x") (NumLit 1))]
        ),
    UnitOp "qc" (BinGate CX (NumLit 1) (NumLit 1))
    ],
And (Comparison Eq (AIde "x") (AIde "n")) (Comparison Eq (QOp GC (QIde "qc") []) (AIde "x"))
Comparison Lt (QOp GC (QIde (QF (APP CX) (QIde "qc") [NumLit 1, NumLit 1])) []) (BinOp Mult (NumLit 2) (AIde "n"))

x .== n .&& gatecount(qc) .== x .=> gatecount(qc) .< 2 * n:while

q = Comparison Lt (QOp GC (QIde "qc") []) (BinOp Mult (NumLit 2) (AIde "n"))
}