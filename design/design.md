# Hints

- `-- comment` is a comment, it doesn't touch any rules
- `..` represents atmospheres of ranges :joy:
- `a === b` represents that if `a` is valid then `b` is valid too

<!--TODO
# The core language rules
-->

# The exression rules
The notation of programs :notebook:  
All expression are represented as this.

<!--TODO
- the if syntax will be represented as the case syntax of the core lang
-->

```
lowerAscChar <- "a" / .. / "z"
upperAscChar <- "A" / .. / "Z"

--

atomicVal <- natVal / boolVal / unitVal
natVal    <- "0" / "1" / "2" / ..
boolVal   <- "True" / "False"
unitVal   <- "Unit"

-- Non atomic terms
expr <- atomicVal
      / syntax
      / lambda
      / "(" expr ")"

identifier <- !lowerAscChar (lowerAscChar / upperAscChar)* -- variables
lambda <- expr
        / identifier
        / "\" identifier ":" type "." expr -- lambda abstractions
        / lambda expr                      -- function applications

if <- "if" expr "then" expr "else" expr
syntax <- if

--

type <- atomicType
      / type "->" type -- function types (lambda caluculus types)
atomicType <- "Nat" / "Bool" / "Unit"
```


# The typing rules
The type checker :notes:  
All valid expression is represented as this.  
All invalid terms cannot be an expression in here.

```
-- Atomic typing
x is natVal
===
x : Nat


x is boolVal
===
x : Bool

x is unitVal
===
x : Unit

x : Bool, y : T, z : T
===
if x then y else z : T

x : T, y : U
===
\x.y : T -> U
```


# The convertion rules
The preprocessor.  
This converts expressions to a representation of the core language :sunny:

```
```


# The evaluation rules
The executor.  
This converts expressions to expressions.

```
```
