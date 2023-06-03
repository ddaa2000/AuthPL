/*
Wrong authentication in application
Should return:
A1->A2; 
A1->A3; A1->A2; 
A2->A3; A1->A3; A1->A2; 
(lambda x:Nat. x) : Nat -> Nat
unit : Unit
/Users/ddaa/files/programing/DPPL/fullref-auth/test1.f:6.6:
unexpected auth in appliaction
*/

Root: A1 -> A2;
Root: A1 -> A3;
Root: A2 -> A3;
A1: lambda x:Nat$A1 down. x;
A1: (lambda x:Unit$A2 down. x) unit $A1 up;
A1: (lambda x:Unit$A1 down. x) unit $A2 up;