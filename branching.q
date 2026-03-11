[n > 0]
{
  qc <- QuantumCircuit(n, 0);
  i <- 1;
  while i != n: [gatecount(qc) == i - 1 && 1 <= i && i <= n]
  {
    if i % 2 == 1:
    {
      qc.CNOT(i, i + 1);
    }
    else:
    {
      qc.CX(i, i + 1);
    }
    i <- i + 1;
  }
}
[gatecount(qc) == n - 1]