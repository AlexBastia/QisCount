[n > 0]
{
qc <- QuantumCircuit(3, 3);
qc.H(0);
qc.CX(0, 1);
qc.H(0);
i <- 0;
while i != n: [gatecount(qc) == 3 + i && n > 0]
{
    i <- i + 1;
    qc.H(0);
}
}
[gatecount(qc) <= (3 * (n + 2))]