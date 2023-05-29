f = ref (lambda a:Nat. lambda b:Nat. a);
plus = lambda a:Nat. lambda b:Nat. if (iszero a) then b else ((!f) (pred a) (succ b));
f := plus;
plus 10 105;
plus 0 1;
plus 0 0;
plus 2 0;