/*
Test imported funciton with sufficient auth
Should return:
A1->A2; 
A1->A3; A1->A2; 
A2->A3; A1->A3; A1->A2; 
f1 : Unit -> Unit
f2 : Unit -> Unit
unit : Unit
*/

Root: A1 -> A2;
Root: A1 -> A3;
Root: A2 -> A3;
A1: f1 = lambda x:Unit$A2 down. x;
A2: f2 = lambda x:Unit$A3 down. (f1 unit $A2 down) auas A3 down;
A3: f2 unit $A3 down;