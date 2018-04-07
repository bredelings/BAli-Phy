% Guide to development in BAli-Phy

# Getting started

## Fork the repo

Go to the github page for bali-phy here: [https://github.com/BAli-Phy/](https://github.com/BAli-Phy).  Then click in the "Fork" button.  This will create a copy of the repo under your own account.  You should then clone your own version of the repo:

``` sh
git clone git@github.com:your-username/BAli-Phy.git
```

The name `origin` in your local repo will then refer to your modified version of BAli-Phy.  To refer to the official upstream version, create a new remote called `upstream`:

``` sh
cd BAli-Phy/
git remote add upstream git@github.com:bredelings/BAli-Phy.git
git remote -v
```

## Repo overview

These directories contain code that affects how `bali-phy` runs:

[modules/](https://github.com/bredelings/BAli-Phy/blob/master/modules)
: Haskell code

[src/](https://github.com/bredelings/BAli-Phy/blob/master/src)
: C++14 code

[functions/](https://github.com/bredelings/BAli-Phy/blob/master/functions)
: JSON definitions of functions for the command-line interface

[help/](https://github.com/bredelings/BAli-Phy/blob/master/help)
: Help files

These directories contain documentation and examples:

[doc/](https://github.com/bredelings/BAli-Phy/blob/master/doc/)
: Documentation

[doc/man/](https://github.com/bredelings/BAli-Phy/blob/master/doc/man/)
: Markdown files for generating UNIX manual pages

[examples/sequences/](https://github.com/bredelings/BAli-Phy/blob/master/examples/sequences/)
: Example sequences

[examples/models/](https://github.com/bredelings/BAli-Phy/blob/master/examples/models/)
: Example files for running graphical models.

## Contributions

We are excited to see what you will contribute!

The way to submit patches is:

1. First develop changes in your own repo.
1. Send a [pull request](https://help.github.com/articles/about-pull-requests/) through github.
1. CI tests will run automatically on the on suggested  changes.
1. We will review the changes.
1. If accepted, changes will be merged to the master branch.

# Adding functionality to bali-phy

## Adding a Haskell function

Haskell functions are defined in the Haskell modules under [modules/](https://github.com/bredelings/BAli-Phy/blob/master/modules/).  For example, the function `min` is defined in `Prelude.hs` as follows:

``` Haskell
min x y = if (x <= y) then x else y;
```

To add a Haskell function, you simply need to define a function in one of these modules.  However, be aware that the Haskell parser in bali-phy is not very advanced, and so you will need to specify `{`, `}`, and `;` in places where they are optional in standard Haskell.

## Adding a C++ function

To add a "builtin" C++ operation to bali-phy's Haskell code, you must add the C++ code for the operation to one of the C++ files in the [src/builtins/](https://github.com/bredelings/BAli-Phy/blob/master/src/builtins) directory.  You must then declare the builtin in one of the Haskell files in the [modules/](https://github.com/bredelings/BAli-Phy/blob/master/modules/) directory.

### Declaring a builtin in Haskell

A builtin is declared via the following syntax:

``` Haskell
builtin haskell_name number_of_arguments "c++ name" "module name";
```

For example, the Haskell function `poisson_density` is declared with the following line from [modules/Distributions.hs](https://github.com/bredelings/BAli-Phy/blob/master/modules/Distributions.hs):

``` Haskell
builtin poisson_density 2 "poisson_density" "Distribution";
```

The first two arguments specify the Haskell name (`poisson_density`) and the number of arguments in Haskell (`2`).  The C++ function name is derived from the third argument (`poisson_density`) by adding `builtin_function_` in front.  So the C++ function will be called `builtin_function_poisson_density`.  The last argument specifies which loadable module contains the C++ function.  Since this function is in the module "Distribution", its source code goes in [src/builtins/Distribution.cc](https://github.com/bredelings/BAli-Phy/blob/master/src/builtins/Distribution.cc).

### Writing a builtin in C++

The C++ function for a builtin must be defined in one of the C++ files in the [src/builtins](https://github.com/bredelings/BAli-Phy/blob/master/src/builtins) directory, and the function name must begin with `builtin_function_`.  The function must also be declared `extern "C"` (to avoid name mangling).

For example, the poisson density function is written in [src/builtins/Distirbution.cc](https://github.com/bredelings/BAli-Phy/blob/master/src/builtins/Distribution.cc) as follows:

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
* The `expression_ref` can be converted to `double` or `int` using the methods `.as_double()` and `.as_int()`.

Output:

* The function returns a `closure` object ([src/computation/closure.H](https://github.com/bredelings/BAli-Phy/blob/master/src/computation/closure.H))
* A closure can be created from a `double` or `int`.  Here an explicit conversion is invoked by surrouding a `double` with curly braces.

## Adding a distribution

Distributions are defined in [modules/Distributions.hs](https://github.com/bredelings/BAli-Phy/blob/master/modules/Distributions.hs).

For a distribution, you need to add a function that constructs a ProbDensity object.

``` Haskell
name parameters = ProbDensity (density parameters) (quantile parameters) (sample parameters) (range parameters);
```

For example, the Normal distribution is defined as:
``` Haskell
builtin normal_density 3 "normal_density" "Distribution";
builtin normal_quantile 3 "normal_quantile" "Distribution";
builtin builtin_sample_normal 2 "sample_normal" "Distribution";
sample_normal m s = Random (IOAction2 builtin_sample_normal m s);
normal m s = ProbDensity (normal_density m s) (normal_quantile m s) (sample_normal m s) realLine;
```

### Density
Note that the normal density takes 3 arguments, so that `(normal_density m s)` is a function of the third argument.

### Quantile
If the function is univariate, you can define
a quantile function that takes a probability and returns the value of
x such that the cdf up to x is p.
``` Haskell
dist_quantile args p = x
```
If the function is not univariate, or you don't have a quantile functon, set the quantile function to `(no_quantile "distribution name")`.  This will later change to use polymorphism, where only 1-dimensional functions will have a quantile attribute.

### Sample
The `(dist_sample parameters)` function returns an object in the Random monad.
To construct a random sample from a C++ procedure, use one of the following patterns, depending on how many arguments your sampling routine takes:
``` Haskell
sample_dist arg1 = Random (IOAction1 builtin_sample_dist arg1);
sample_dist arg1 arg2 = Random (IOAction2 builtin_sample_dist arg1 arg2);
sample_dist arg1 arg2 arg3 = Random (IOAction3 builtin_sample_dist arg1 arg2 arg3);
```
The procedure can also call other actions in the Random monad, where executing a distribution has the semantics of sampling from the distribution.  For example, here we sample from the distribution `(dist2 args)` and transform the result.
``` Haskell
sample_dist args = do { x <- dist2 args; return (f x);}
```

### Range

Ranges for real numbers are:

* above x
* below x
* between x y
* realLine

Ranges for Integers are:

 * integer_above i
 * integer_below i
 * integer_between i j

In each case, the range includes the bounds.  For example, `(integer_above 0)` includes `0`, and `(integer_between 0 1)` includes `0` and `1`.

Ranges for Booleans are:

* TrueFalseRange

Ranges for simplices are

* Simplex n sum

where `n` is the number of dimensions, and `sum` is the sum of the values (usually `1.0`).

	 

	 

