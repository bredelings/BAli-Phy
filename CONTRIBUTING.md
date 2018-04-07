# Contributing to BAli-Phy

We are excited to see what you will contribute!

## Patches

Patches should be sent through a github PR ("pull request").  This causes the CI tests to run for the patch.

C++ code should be valid C++14.

## Adding functionality to bali-phy

### Adding a builtin

To add a "builtin" C++ operation to bali-phy's Haskell code, you must add the C++ code for the operation to one of the C++ files in the `src/builtins/` directory.  You must then declare the builtin in one of the Haskell files in the `modules/` directory.

#### Declaring a builtin in Haskell

A builtin is declared via the following syntax:

``` Haskell
builtin haskell_name number_of_arguments "c++ name" "module name";
```

For example, the Haskell function `poisson_density` is declared with the following line from `modules/Distributions.hs`:

``` Haskell
builtin poisson_density 2 "poisson_density" "Distribution";
```

The first two arguments specify the Haskell name (`poisson_density`) and the number of arguments in Haskell (`2`).  The C++ function name is derived from the third argument (`poisson_density`) by adding `builtin_function_` in front.  So the C++ function will be called `builtin_function_poisson_density`.  The last argument specifies which loadable module contains the C++ function.  Since this function is in the module "Distribution", its source code goes in `src/builtins/Distribution.cc`.

#### Writing a builtin in C++

The C++ function for a builtin must be defined in one of the C++ files in the `src/builtins/` directory, and the function name must begin with `builtin_function_`.  The function must also be declared `extern "C"` (to avoid name mangling).

For example, the poisson density function is written in `src/builtins/Distribution.cc` as follows:

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
* The `n`th argument is fetched by calling `Args.evaluate(n)`, and is of type `expression_ref` (`src/computation/expression/expression_ref.H`)
* The `expression_ref` can be converted to `double` or `int` using the methods `.as_double()` and `.as_int()`.

Output:
* The function returns a `closure` object (`src/computation/closure.H`).
* A closure can be created from a `double` or `int`.  Here an explicit conversion is invoked by surrouding a `double` with curly braces.

## Adding a distribution

Distributions are defined in `modules/Distributions.hs`.


