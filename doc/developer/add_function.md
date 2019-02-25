% Adding a function to BAli-Phy

In BAli-Phy, models and distributions are implemented as functions.  This makes sense since models and distributions take parameters, just like functions do.
To add a function, we must first write a function in Haskell.  Then we must put a JSON file in the `bindings` directory so that the Haskell function becomes visible from the command-line.

## Adding a function: Haskell

Haskell functions are defined in Haskell _modules_ in the [modules/](https://github.com/bredelings/BAli-Phy/blob/master/modules/) directory.  
The special module called `Prelude` is automatically imported, so that functions defined in the Prelude are visible by default.
Any function added to the Prelude (`modules/Prelude.hs`) will be visible by default.

For example, you could add the function `square` to the Prelude:

``` Haskell
square x = x * x
```

This function will then be visible to other Haskell functions, but will not yet be visible from the command line.

## Using a function from the command line

To make a Haskell function accessible from the command line, you must
add a JSON file to the directory `bindings/`. For example, to add our
new function `square` to the command line interface, we could add a
file called `bindings/functions/square.json` containing the text:
``` json
{
    "name": "square",
    "result_type": "Double",
    "call": "Prelude.square[x]",
    "args": [
        {
            "arg_name": "x",
            "arg_type": "Double",
        }
    ]
}
```
* The `name` property determines the name that is visible on the command line.
* The `result_type` property specifies the type of object returned by the function.
* The `call` property specifies which Haskell function to call, and which arguments to pass.
* The `args` array specifies the name and type for each argument.

When you reinstall BAli-Phy, this file will land in the bindings
directory, and you will be able to run `bali-phy help square`.

See additional explanation (forthcoming) for writing help for functions.

See additional explanation on writing default arguments or priors for functions.