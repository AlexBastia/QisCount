[True]
{
   qc <- QuantumCircuit(3, 3);
   qc.H(0);
   qc.CX(0, 1);
   qc.H(0);
   qc.X(2);
   qc.CX(1,2);
   qc.measure(0,0);
   qc.measure(1,1);
}
[gatecount(qc) == 7]