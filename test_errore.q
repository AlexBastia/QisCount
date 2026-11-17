[n > 0]
{
    qc <- QuantumCircuit(n, 0);
    qc.X(1);
    qc.X(1);
    qc.X(1);
    qc.CR(1, 2);
    qc.X(2);
}
[depth(qc, 1) == 4]