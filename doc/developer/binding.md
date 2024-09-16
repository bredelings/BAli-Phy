% Using a function from the command line

To make a Haskell function accessible from the command line, you must add a JSON file to the directory `bindings/` that registers the Haskell function.

For example, the file `bindings/models/hky85.json` allows the user to specify (for example) `-S hky85(kappa=2)` as a substitution model.
The JSON looks like this:

``` json
{
    "name": "hky85",
    "title": "The Hasegawa-Kishino-Yano (1985) nucleotide rate matrix",
    "deprecated-synonyms": ["HKY","hky","HKY85"],
    "result_type": "RevCTMC<a>",
    "constraints": ["Nucleotides<a>"],
    "citation":{"type": "article",
		"title": "Dating of the human-ape splitting by a molecular clock of mitochondrial DNA",
		"year": "1985",
		"author": [{"name": "Hasegawa, Masami"}, {"name": "Kishino, Hirohisa"}, {"name": "Yano, Taka-aki"}],
		"journal": {"name": "Journal of molecular evolution", "volume": "22", "number": "2", "pages": "160--174"},
		"identifier": [{"type":"doi","id":"10.1007/BF02101694"}]
	       },
    "call": "hky85'(@a,@kappa,@pi)",
    "import": ["SModel"],
    "args": [
        {
            "arg_name": "kappa",
            "arg_type": "Double",
            "default_value": "~logNormal(log(2),0.25)",
            "description": "Transition\/transversion ratio"
        },
        {
            "arg_name": "pi",
            "arg_type": "List<(String,Double)>",
            "default_value": "~symmetric_dirichlet_on(letters(@a),1)",
	    "description": "Letter frequencies"
        },
        {
            "arg_name": "a",
            "arg_type": "a",
	    "default_value": "get_state(alphabet)",
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

`import`
: The Haskell modules to import so that the function `hky85` is in scope.

`result_type`
: The result type of the function.

`args` 
:   The list of named arguments

    `arg_name`
    : The name of each argument

    `arg_type`
    : The type of each argument

    `default_value`
    : A value for the argument if not specified (optional).

    `description`
    : A short phrase describing the argument (optional).

`title`
: A title for the function (optional).

`description`
: A longer description of the function (optional).

