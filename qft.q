[n > 0]
{
qc <- QuantumCircuit(n, 0);
i <- 1;
// Instead of using closed-form invariants i could define a recursive (uninterpreted?) function that 
// the Solver can use to reason inductilvy
while i != n + 1: [gatecount(qc) == (i-1)*(2*n-i+2)/2 && 1 <= i && i <= n + 1]
{
  qc.H(i);
  j <- i + 1;
  while j != n + 1: [gatecount(qc) == (i-1)*(2*n-i+2)/2+j-i && 1 <= i && i <= n && i + 1 <= j && j <= n + 1]
  {
    qc.RX(i, j);  // Giusto per esempio, in realta' usa gate UROT
    j <- j + 1;
  }
  i <- i + 1;
}
// Non posso chiedere se E un numero naturale a che soddisfa perche' viene usato solo il perogni
}
[gatecount(qc) <= n * n ]
