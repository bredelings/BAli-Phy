# Changeable Types

Currently our interpreter treats all cells as possibly changeable.
Cells in that machine can be either constant, modifiable, or changeable.
However, if we know that cells can't possibly be changeable, then we
should be able to run them in a faster interpreter.
Therefore, we would like to be able to determine when computations are
not changeable.

## Data types
One issue is that we need to express whether data types are changeable.
If we have a changeable list, then it could be that the spine of the list
is changeable or fixed, and separately the values in the list could be
changeable or pure.  We can write this like [a@p]@q, where the p says
whether the elements are changeable, and the q says whether the choice
of constructors is changeable.


### Changeability of data types
We might try to write [a@changeable]@changeable as `Changeable [Changeable a]`.
However, this could be interpreted as a cell that changeably evaluates to
`[Changeable a]`.  Instead, we want the `[a]` fields of a changeable list to
also be changeable.

One way to handle this is to say that `data List a` translates to 

    data List@a a@p = Nil | Cons a@p (List@q a@p)

following the rule that any recursive occurrences of `List` in the field
types get the same changeability.
If we have mutually recursive types `X` and `Y`, then we can infer their taint
together:

    data X@p a@q b@r = NoY | YesY (Y@s a@p b@r)
    data Y@s a@t b@u = NoX | YesX (X@p b@u a@t)

This means that any `X` fields contained in `X` through `Y` share the same
changeability.  Any types that jointly reference each other are already 
grouped in to SCCs to determine their kind, IIRC.

## Determining 

If we have a function such as 

    take 0 xs     = []
    take n []     = []
    take n (x:xs) = take (n-1) xs
    
The depending on whether the `n` is changeable, and whether the 

Then we should be able to figure out how to handle if the `n` is changable,
and if the 

## Effects

We might represent functions that can do effective computations using
(->{e}).  So we would have (->{changeable}) and (->{pure}) with the latter being
equivalent to plan (->).

## Extended Core

One idea is that we add an extended `read x as y in E` or `read F as y in E` to Core.
The idea is that it converts `x :: Changeable a` to `y :: a` while also
recording a USE or FORCE edge.  So possibly I need both
  * `use x as y` and 
  * `force x as y`
Another idea is to do this with case statements where the object is changeable,
so that in

    case E of x {alts}

then x would have type `a` if `E :: Changeable a`.

### Should read take expression?

It looks like `read x+y as z in body` would record a use on x and y.

However, if we forbid operations from taking changeable arguments the we would
need `read x as x' in read y as y' in case x' + y' of z in body`.

So the answer is... no?  The operations are `use` and `force` so the can only
take... heap variables???

### Handling builtin operations
If we require that builtin operations cannot take changeable arguments then we
must convert `op x y` to someting like

    use x as x' in use y as y' in op x' y'
    
That is good because it then directly expresses the use statements, and the use
statements are required to change the type, so we can just insert an arbitrary
number.

### Optimization
The goal would be to allow transformations.  For example, if all case
alternatives perform the same `read`, then we should be able to lift it out of
the case alternatives.  However, since `read` is an effectful computation we
need to be careful about moving it, and careful about moving things across it.

#### Factorial
If we have `factorial n`, then right now we would have 5 different (*) nodes
and all of them would depend on `n`.  We should be able to transform this to
do a single read of `n`, and then perform a pure computation on `n', like`

    factorial n  ==>  read n as n' in factorial n'
    
This should have fewer reads.

### Expressing dependencies and stem
If we have `read x as x' in read y as y' in body`, how do we decide how much to group
together?

If we have e.g. `case x+y z _ -> case a+b c _ -> a+c, how do we express whether the
computation continues on IN THE SAME CELL after the x+y calculation?
