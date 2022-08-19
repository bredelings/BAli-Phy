% Adding a C++ function

To add a "builtin" C++ operation to bali-phy's Haskell code, you need to do two things:
1.. *write* the code for function in a C++ file.
2. *declare* the C++ builtin function in a Haskell module.
The second step is necessary to make the C++ function visible in Haskell.

## Declaring a C++ builtin in Haskell

A builtin is declared via the following syntax:

``` Haskell
foreign import bpcall "module_name:cpp_func_name" haskell_name :: Type
```

For example, the Haskell function `poisson_density` is declared with the following line from [haskell/Distributions.hs](https://github.com/bredelings/BAli-Phy/blob/master/haskell/Distributions.hs):

``` Haskell
foreign import bpcall "Distribution:poisson_density" poisson_density :: Double -> Int -> LogDouble
```

The quoted string specifies the loadable module that contains the function and the C++ function name.
Since this function is in the module "Distribution", its source code goes in [src/builtins/Distribution.cc](https://github.com/bredelings/BAli-Phy/blob/master/src/builtins/Distribution.cc).

The C++ function name is obtained by adding `builtin_function_` in front of `poisson_density`.
So the C++ function will be called `builtin_function_poisson_density`.

The rest of the declaration specifies the Haskell name (`poisson_density`) and the type (`Double -> Int -> Double`).

## Writing a builtin in C++

The C++ function for a builtin must be defined in one of the C++ files in the [src/builtins](https://github.com/bredelings/BAli-Phy/blob/master/src/builtins) directory, and the function name must begin with `builtin_function_`.  The function must also be declared `extern "C"` (to avoid name mangling).

For example, the poisson density function is written in [src/builtins/Distribution.cc](https://github.com/bredelings/BAli-Phy/blob/master/src/builtins/Distribution.cc) as follows:

``` C++
extern "C" closure builtin_function_poisson_density(OperationArgs& Args)
{
    double mu = Args.evaluate(0).as_double();
    int n = Args.evaluate(1).as_int();
  
    return { poisson_pdf(mu,n) };
}
```

Input:

* The function takes a single `OperationArgs& Args` argument.
* The `n`th argument is fetched by calling `Args.evaluate(n)`, and is of type `expression_ref` ([src/computation/expression/expression_ref.H](https://github.com/bredelings/BAli-Phy/blob/master/src/computation/expression/expression_ref.H))
* The `expression_ref` can be converted to `int`, `double`, or `log_double_t` using the methods `.as_int()`, `.as_double()` and `.as_log_double()`.

Output:

* The function returns a `closure` object ([src/computation/closure.H](https://github.com/bredelings/BAli-Phy/blob/master/src/computation/closure.H))
* A closure can be created from a `double` or `int`.  Here an explicit conversion is invoked by surrouding a `log_double_t` with curly braces.

# Types

## `log_double_t`
This is a positive real number represented in terms of its logarithm.  Operators have been defined so that you can multiply, add, subtract, and divide this type.

## `Object`
All C++ objects are accessed from Haskell inherit from this type.

## `expression_ref`
An expression ref is basically either an atomic value or an Object followed by a list of `expression_ref`s

See [src/computation/expression/expression_ref.H](https://github.com/bredelings/BAli-Phy/blob/master/src/computation/expression/expression_ref.H)

## `closure`
A closure is an `expression_ref` with an environment.

See [src/computation/closure.H](https://github.com/bredelings/BAli-Phy/blob/master/src/computation/closure.H)

