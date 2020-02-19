% Add a probability distributon to BAli-Phy

Distributions are defined in [haskell/Distributions.hs](https://github.com/bredelings/BAli-Phy/blob/master/modules/Distributions.hs).

## Making a ProbDensity object

For a distribution, you need to add a function that constructs a ProbDensity object.

``` Haskell
name parameters = ProbDensity (densities parameters) (quantile parameters) (sample parameters) (range parameters)
```
Here `densities` returns a list of terms which yield the density when multiplied together.

For example, the Normal distribution is defined as:
``` Haskell
builtin normal_density 3 "normal_density" "Distribution"
builtin normal_quantile 3 "normal_quantile" "Distribution"
builtin builtin_sample_normal 2 "sample_normal" "Distribution"
sample_normal m s = Random (IOAction2 builtin_sample_normal m s)
normal m s = ProbDensity (\x -> [normal_density m s x]) (normal_quantile m s) (sample_normal m s) realLine
```
## Defining the density

A density function takes an extra argument after the distribution parameters.  For example, the normal density takes 3 arguments, so that `(normal_density m s)` is a function of the third argument.

A density function should return type `log_double_t`.

### Quantile
A quantile function takes an extra argument after the distribution parameters.  For example, the normal quantile takes 3 arguments, so that `(normal_quantile m s)` is a function of the third argument.  The extra argument should have type `double`, and ranges from 0 to 1.

If the function is not univariate, or does not have a quantile functon, set the quantile function to `(no_quantile "distribution name")`.  This will later change to use polymorphism, where only 1-dimensional functions will have a quantile attribute.

## Sample

To construct a random sample from a C++ procedure, access the `n`th parameter via `Args.evaluate_(n)` (with an underscore) instead of `Args.evaluate(n)`.
For example:
``` C++
extern "C" closure builtin_function_sample_normal(OperationArgs& Args)
{
    double a1 = Args.evaluate_(0).as_double();
    double a2 = Args.evaluate_(1).as_double();
  
    return { gaussian(a1, a2) };
}
```

Then use one of the following patterns, depending on how many arguments your sampling routine takes:
``` Haskell
sample_dist arg1 = Random (IOAction1 builtin_sample_dist arg1)
sample_dist arg1 arg2 = Random (IOAction2 builtin_sample_dist arg1 arg2)
sample_dist arg1 arg2 arg3 = Random (IOAction3 builtin_sample_dist arg1 arg2 arg3)
```
For example:
``` Haskell
builtin builtin_sample_normal 2 "sample_normal" "Distribution"
sample_normal m s = Random (IOAction2 builtin_sample_normal m s)
```

The `(dist_sample parameters)` function returns an object in the Random monad, where executing a distribution has the semantics of sampling from the distribution.  The sampling procedure can also call other actions in the Random monad. For example, here we sample from the distribution `(dist2 args)` and transform the result.
``` Haskell
sample_dist args = do x <- dist2 args
                      return (f x)
```

## Range

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

