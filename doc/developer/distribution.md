% Add a probability distributon to BAli-Phy

Each probability distribution is defined in a module for that distribution.  For example, the normal distribution is defined in `Probability.Distribution.Normal`, which is located at [haskell/Probability/Distribution/Normal.hs](https://github.com/bredelings/BAli-Phy/blob/master/haskell/Probability/Distribution/Normal.hs).  

To make a new distribution `mydist`, you should follow these steps, using the normal distribution as a template:

1. create a new Haskell module `Probability.Distribution.MyDist` in `haskell/Probability/Distribution/Mydist.hs` using `Normal.hs` as a template.
2. create a new c++ function `builtin_function_mydist_density` in `src/builtins/Distribution.cc` using `builtin_function_normal_density` as a template.
3. create a new c++ function `builtin_function_sample_mydist` in `src/builtins/Distribution.cc` using `builtin_function_sample_normal` as a template.
4. create a new JSON binding in `bindings/distributions/mydist.json` using `bindings/distribution/normal.json` as a template.

## Making Haskell model that defines the distribution

For a distribution, you need to add a function that constructs a Distribution object.

``` Haskell
name parameters = Distribution (densities parameters) (quantile parameters) (sample parameters) (range parameters)
```
For example, the Normal distribution is defined as:
``` Haskell
builtin normal_density 3 "normal_density" "Distribution"
builtin normal_quantile 3 "normal_quantile" "Distribution"
builtin builtin_sample_normal 3 "sample_normal" "Distribution"
sample_normal m s = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction (\state -> (state, builtin_sample_normal m s state)))
normal m s = Distribution (\x -> [normal_density m s x]) (normal_quantile m s) (sample_normal m s) realLine
```

### densities

The first argument to `Distribution` is a function that returns a list of terms which yield the density when multiplied together.  In this case there is just one term.

A `normal_density` function takes an extra argument after the distribution parameters.  For example, the normal density takes 3 arguments, so that `(normal_density m s)` is a function of the third argument.

### quantile
A quantile function takes an extra argument after the distribution parameters.  For example, the normal quantile takes 3 arguments, so that `(normal_quantile m s)` is a function of the third argument.  The extra argument should have type `double`, and ranges from 0 to 1.

If the function is not univariate, or does not have a quantile functon, set the quantile function to `(no_quantile "distribution name")`.  This will later change to use polymorphism, where only 1-dimensional functions will have a quantile attribute.


## C++ sample

To construct a random sample from a C++ procedure, access the `n`th parameter via `Args.evaluate_(n)` (with an underscore) instead of `Args.evaluate(n)`.
For example:
``` C++
extern "C" closure builtin_function_sample_normal(OperationArgs& Args)
{
    double a1 = Args.evaluate_(0).as_double();
    double a2 = Args.evaluate_(1).as_double();
  
    Args.make_changeable();

    return { gaussian(a1, a2) };
}
```

## C++ density

A density function should return type `log_double_t`.

