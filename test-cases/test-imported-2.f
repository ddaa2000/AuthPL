/*
Test imported funciton with insufficient auth
Should return:
A1->A2; 
A1->A3; A1->A2; 
A2->A3; A1->A3; A1->A2; 
f1 : Unit -> Unit
/Users/ddaa/files/programing/DPPL/fullref-auth/test-imported-2.f:15.5:
no authentication to use outer element
*/

Root: A1 -> A2;
Root: A1 -> A3;
Root: A2 -> A3;
A1: f1 = lambda x:Unit$A1 down. x;
A2: f1 unit $A2 down;