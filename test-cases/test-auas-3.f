/*
A1->A2; 
A1->A3; A1->A2; 
A2->A3; A1->A3; A1->A2; 
/Users/ddaa/files/programing/DPPL/fullref-auth/test-auas-3.f:14.49:
no auth on auas
*/

Root: A1 -> A2;
Root: A1 -> A3;
Root: A2 -> A3;
A2: f2 = lambda x:Unit$A3 down. (unit $A1 down) auas A3 down;
A3: f2 unit $A3 down;