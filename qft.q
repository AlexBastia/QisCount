[n > 0]
{
qc <- QuantumCircuit(n, 0);
i <- 1;
while i != n + 1: [gatecount(qc) == (i-1)*(2*n-i+2)/2 && 1 <= i && i <= n + 1]
{
  qc.H(i);
  j <- i + 1;
  while j != n + 1: [gatecount(qc) == (i-1)*(2*n-i+2)/2+j-i && 1 <= i && i <= n && i + 1 <= j && j <= n + 1]
  {
    qc.RX(i, j);  // Should be a UROT gate
    j <- j + 1;
  }
  i <- i + 1;
}
}
[gatecount(qc) <= n * n ]
