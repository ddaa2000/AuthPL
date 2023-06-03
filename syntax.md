# General Syntax
## Command Modifications:
Perset declaration command:
```
# Perset A1 has higher authority than A2
Root: A1 -> A2 #Perset names need to be upper case

# Circles are not allowed, including self-circles
Root: A2 -> A3
Root: A3 -> A2 # wrong!

Root: A1 -> A1 # wrong!

```

Binding (definition) command:
Add perset before the command to mark the perset of the user:
```
A1: f1 = lambda x:Unit$A1 down. x;
```

Evaluation (use) command:
Add perset before the command to mark the perset of the user:
```
A1: (lambda x:Unit$A1 down. x) unit $A1 down;
```

## Term Modifications:
Authority representations:
```
#atomic authorities:
A1 up
A1 down

#arrow authorities:
(A1 up -> A2 up)

#compound authorities:
(A1 up)A2 down
```

Abstraction:
Add authority restriction to variables in abstractions:
```
# use $ to mark authority
A1: f1 = lambda x:Unit$A1 down. x;
```

Consts:
Add authority restriction to consts:
```
0 $A1 down
unit $A2 up
1 $A1 up #which is the short-term of succ(0 $A1 up)
```

## Supported Terms:
- abstractions
- applications
- ref-related
- zero
- unit
- True
- False
- as
- auas
- require

其他的暂时懒得写了，先这样吧😂

