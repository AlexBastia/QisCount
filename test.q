[n > 0]
{
qc <- QuantumCircuit(1,1);
qc.H(1);
qc.measure(1,1);
}
[gatecount(qc) == 1]