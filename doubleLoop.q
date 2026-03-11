[n > 0 && n % 2 == 0]
{
  qc <- QuantumCircuit(n, 0);
  
  // First Loop: Prepare isolated Bell pairs
  i <- 1;
  while i != n + 1: [n > 0 && n % 2 == 0 && gatecount(qc) == i - 1 && 1 <= i && i <= n + 1 && i % 2 == 1]
  {
    qc.H(i);
    qc.CNOT(i, i + 1);
    i <- i + 2;
  }
  
  // Second Loop: Entanglement Swapping sequence
  j <- 2;
  while j != n: [n > 0 && n % 2 == 0 && gatecount(qc) == n + j - 2 && 2 <= j && j <= n && j % 2 == 0]
  {
    qc.CNOT(j, j + 1);
    qc.H(j);
    j <- j + 2;
  }
}
[gatecount(qc) == 2 * n - 2]