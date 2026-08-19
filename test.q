[i > 2 && i < 7]
{
  qc <- QuantumCircuit(2, 2);
  qc.H(0);
  qc.CNOT(0,1);
  if i % 2 == 0:
  {
    qc.H(1);
  }
  qc.measure(0,0)
}
[gatecount(qc) == 3]