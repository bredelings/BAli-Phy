% Using a function from the command line

## To make a Haskell function accessible from the command line, you must add a JSON file to the directory `bindings/` that registers the Haskell function.
For example, the file `bindings/models/hky85.json` allows the user to specify (for example) `-S hky85[kappa=2]` as a substitution model.
The JSON looks like this:

``` json
{
    "name": "hky85",
    "title": "The Hasegawa-Kishino-Yano (1985) nucleotide rate matrix",
    "result_type": "ExchangeModel[a]",
    "constraints": ["Nucleotides[a]"],
    "citation":{"type": "article",
                "title": "Dating of the human-ape splitting by a molecular clock of mitochondrial DNA",
                "year": "1985",
                "author": [{"name": "Hasegawa, Masami"}, {"name": "Kishino, Hirohisa"}, {"name": "Yano, Taka-aki"}],
                "journal": {"name": "Journal of molecular evolution", "volume": "22", 
                "identifier": [{"type":"doi","id":"10.1007/BF02101694"}]
               },
    "call": "SModel.Nucleotides.hky85[kappa,SModel.Frequency.frequencies_from_dict[a,pi],a]",
    "args": [
        {
            "arg_name": "kappa",
            "arg_type": "Double",
            "default_value": "~log_normal[log[2],0.25]",
            "description": "Transition\/transversion ratio"
        },
        {
            "arg_name": "pi",
            "arg_type": "List[Pair[String,Double]]",
            "default_value": "~dirichlet_on[letters[@a],1]",
            "description": "Letter frequencies"
        },
        {
            "arg_name": "a",
            "arg_type": "a",
            "default_value": "getAlphabet",
            "description": "The alphabet"
        }
    ],
    "description":"The HKY85 model",
    "extract": "all"
}
```

The fields are defined as follows:

`name`
: The name of this function on the command line.

`call`
: The Haskell function to call, and the order of the arguments to pass.

`result_type`
: The result type of the function.

`args`
: The the list of named arguments

`args.arg_name`
: The name of each argument

`args.arg_type`
: The type of each argument

`args.default_value`
: A value for the argument if not specified (optional).

`args.description`
: A short phrase describing the argument (optional).

`title`
: A title for the function (optional).

`description`
: A longer description of the function (optional).

