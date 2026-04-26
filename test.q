[True]
{
qc <- QuantumCircuit(2, 0);
qc.CNOT(0,1)[q(1) == 1, q(0) == 0];
}
[gatecount(qc) == 0]