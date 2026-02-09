[True]
{
    x <- 2 - 1;
    qc <- QuantumCircuit(3,3);
    qc.H(x);
    qc.H(2);
}
[depth(qc, 1) == 1]