[n > 0]
{
  qc <- QuantumCircuit(n, 0);
  i <- 1;
  while i != n + 1: [gatecount(qc) == i - 1 && 1 <= i && i <= n + 1]
  {
    qc.H(i);
    i <- i + 1;
  }
}
[gatecount(qc) == n]