% Adding a function to BAli-Phy

Since BAli-Phy models are written in Haskell, you can define functions
inside your own model file.  However, some functions are used frequently
enough to add them to BAli-Phy itself instead of including them
separately in multiple different model files.

Adding a function to BAli-Phy involves adding it to one of the Haskell
modules that comes with BAli-Phy so that it can be used from multiple
different model files.  To make the function visible from the 
command line as well, you will need to add a JSON file to the `bindings`
directory that describes how to access the Haskell function.

## Haskell modules

In order to add a function to BAli-Phy you need to add it to a module.
For example, if you add the function to the module
`SModel.Nucleotides` then you could access it by adding
```Haskell
import SModel.Nucleotides
```
to the beginning of your model file.

The Haskell file for the module `SModel.Nucleotides` is located at
`haskell/SModel/Nucleotides.hs`. (See the [haskell/](https://github.com/bredelings/BAli-Phy/blob/master/haskell/) 
directory on github.) The module file begins with a 
module declaration:

```Haskell
module SModel.Nucleotides where
```

We can then write a function inside this module.  For example, in the module
`SModel.Nucleotides`, we have the function `hky85`:
```Haskell
hky85 k    pi a = gtr a (hky85_sym k a) pi
```
This function defines the `hky85` model as a function of the
transition-transversion ratio (`k`), the equilibrium frequencies
(`pi`) and the DNA or RNA alphabet (`a`).


## Creating a new module

Let's create a new module called `Bio.MyModule` that
contains a function for squaring numbers.  First we create a new
module at `haskell/Bio/MyModule.hs`.  Module names must begin with
an upper-case letter.  After the first letter, lower-case letters,
numbers, and `_` are allowed. 

Then we add the following to the file:
```Haskell
module Bio.MyModule where

square x = x * x
```

After we rerun `ninja install`, the function `square` module will be
accessible to model files that import `Bio.MyModule`.

## Using a function from the command line

To make a Haskell function accessible from the command line, you must
add a JSON file to the directory `bindings/`. For example, to add our
a function `square` in the module `Bio.MyModule` to the command line
interface, we could add a file called `bindings/functions/square.json`
containing the text: 
``` json
{
    "name": "square",
    "result_type": "Double",
    "import": ["Bio.MyModule"],
    "call": "square(x)",
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
* The `import` property gives a list of modules that need to be imported to find the function.
* The `call` property specifies which Haskell function to call, and which arguments to pass.
* The `args` array specifies the name and type for each argument.

When you reinstall BAli-Phy, this file will land in the bindings
directory, and you will be able to run `bali-phy help square`.

See additional explanation (forthcoming) for writing help for functions.

See additional explanation on writing default arguments or priors for functions.