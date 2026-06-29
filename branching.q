[n > 0]
{
  qc <- QuantumCircuit(n, 0);
  i <- 0;
  while i != n: [gatecount(qc) == i/2 && 0 <= i && i <= n]
  {
    if i % 2 == 1:
    {
      qc.CNOT(i, i + 1);
    }
    i <- i + 1;
  }
}
[gatecount(qc) == n/2]