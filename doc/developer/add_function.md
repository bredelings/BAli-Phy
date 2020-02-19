% Adding a function to BAli-Phy

You can add a function to your probabilistic model file using Haskell
syntax.  For example, we can define a function called `square` that
squares its input as follows:
``` Haskell
square x = x * x
```

However, you might want to make the function part of the BAli-Phy
system so that it can be used in multiple different model files.  This
can be done by adding it to one of the built in Haskell modules, or by
creating a new module to contain it.

Adding a Haskell function to a module does not automatically make the
function visible from the command line for use in the `--smodel`
command and other commands.  To make the function visible from the
command line, you will need to add a JSON file to the `bindings`
directory that describes how to access the Haskell function.

## Creating a new built in module

Suppose you want to create a new module called `Bio.MyModule`.  You
would do this by creating a Haskell file that begins with
```Haskell
module Bio.MyModule where

```
In order for BAli-Phy to find this file, you then need to place it at
`haskell/Bio/MyModule.hs`. (See the [haskell/](https://github.com/bredelings/BAli-Phy/blob/master/haskell/)
directory on github.)

Note that Haskell module names need to conform to a certain rules.
Module names names must begin with an upper-case letter.  After the
first letter, lower-case letters, numbers, and `_` are allowed.

## Adding a function to a builtin module

First locate the Haskell file for the module that you want to modify.
For example, the module `SModel.Nucleotides` is located as
`haskell/SModel/Nucleotides.hs`.

Then write a function inside this module.  For example, in the module
`SModel.Nucleotides`, we have the function `hky85`:
```Haskell
hky85 k    pi a = gtr a (hky85_sym k a) pi
```
This function defines the `hky85` model as a function of the
transition-transversion ratio (`k`), the equilibrium frequencies
(`pi`) and the DNA or RNA alphabet (`a`).

Note that other Haskell modules (including your model file) need to
_import_ the module in order for its functions to be
accessible.  To import the `hky85` function, the command `import
SModel.Nucleotides` would do the trick.

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
    "call": "square[x]",
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