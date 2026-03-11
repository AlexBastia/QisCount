[n > 0]
{
  qc <- QuantumCircuit(n, 0);
  i <- 1;
  while i != n + 1: [gatecount(qc) == (i - 1) * n && 1 <= i && i <= n + 1]
  {
    j <- 1;
    while j != n + 1: [gatecount(qc) == (i - 1) * n + j - 1 && 1 <= i && i <= n && 1 <= j && j <= n + 1]
    {
      if (i + j) % 2 == 0:
      {
        qc.CX(i, j);
      }
      else:
      {
        qc.CX(i, j);
      }
      j <- j + 1;
    }
    i <- i + 1;
  }
}
[gatecount(qc) == n * n]