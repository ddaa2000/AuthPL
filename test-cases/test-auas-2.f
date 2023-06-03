/*
Test auas with wrong use of up type
Should return:
A1->A2; 
A1->A3; A1->A2; 
A2->A3; A1->A3; A1->A2; 
/Users/ddaa/files/programing/DPPL/fullref-auth/test-auas-2.f:15.47:
no auth on auas
*/

Root: A1 -> A2;
Root: A1 -> A3;
Root: A2 -> A3;
A1: f1 = lambda x:Unit$A2 down. (unit $A1 up) auas A2 down;
A2: f2 = lambda x:Unit$A3 down. (f1 unit $A2 down) auas A3 down;
A3: f2 unit $A3 down;