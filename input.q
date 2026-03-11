[True]
{
  i <- 0;
  qc <- QuantumCircuit(n, 0);
  while i != n: [gatecount(qc) == i*n]
  {
    j <- 0;
    while j != n: [gatecount(qc) == j + (i * n)]
    {
      qc.H(i);
      j <- j + 1;
    }
    i <- i + 1;
  }
}
[gatecount(qc) == n * n]
