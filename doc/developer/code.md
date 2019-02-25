% BAli-Phy architecture and code layout

## BAli-Phy architecture

In order to add functionality to BAli-Phy, it is important to understand the BAli-Phy architecture.  This architecture is divided into several levels. 

![Architecture diagram](architecture.png)

In BAli-Phy, distributions, functions, and models are all implemented as functions.  To add functionality to the standard BAli-Phy interface, you need to add a Haskell function (green), and a binding (blue) that makes that function visible. BAli-Phy does not use a standard Haskell compiler, but instead implements its own Haskell interpreter that has special functionality.

If you are using the generic graphical modeling language, you do can use the Haskell function directly, and do not need to add a binding.

In some cases you might want to implement your function in C++ (yellow) and then call it from Haskell.

## Source Code

These directories contain code that affects how `bali-phy` runs:
![Code layout](code-layout.png)

[modules/](https://github.com/bredelings/BAli-Phy/blob/master/modules)
: Haskell code

[src/](https://github.com/bredelings/BAli-Phy/blob/master/src)
: C++17 code

[functions/](https://github.com/bredelings/BAli-Phy/blob/master/functions)
: JSON definitions of functions for the command-line interface

## Documentation and examples

[help/](https://github.com/bredelings/BAli-Phy/blob/master/help)
: Help files

[doc/](https://github.com/bredelings/BAli-Phy/blob/master/doc/)
: Documentation

[doc/man/](https://github.com/bredelings/BAli-Phy/blob/master/doc/man/)
: Markdown files for generating UNIX manual pages

[examples/sequences/](https://github.com/bredelings/BAli-Phy/blob/master/examples/sequences/)
: Example sequences

[examples/models/](https://github.com/bredelings/BAli-Phy/blob/master/examples/models/)
: Example files for running graphical models.

