# Command line language feature assessment.

The command line language evolved from the ability to specify models like `gtr`
for the command line.  However, as it has gained expressive power, it has moved
towards a fuller language with more familiar syntax than Haskell (i.e. using
parenthesis instead of space for function application) that also has built-in
support for random sampling.  This positions it as something more like RevBayes,
LinguaPhylo, PhyloSpec, and TreePPL.

However, it has some unique features that need to be assessed.  There features
generally exist to make model-specification ergonomic, and sometimes extremely
terse.  For example, default arguments that may be random allow writing `gtr`
while relying on default priors.  Likewise, the attempt to thread the alphabet
through the specification of substitution models allows writing `gtr + x3`
without specifying the alphabet multiple times -- and while making the whole
thing a function of the alphabet.  Implicit type conversion means that
it is possibly to write `gtr +> Rates.gamma` without writing `unit_mixture(gtr) +> rates.gamma`.

However, when attempting to provide a syntax for graphical models, some of
these features can be awkward.  Thus this document is an attempt to explore
what special features of the command-line language complicate the language
how they attempt to make the language more ergonomic, whether there are 
alternative ways to achieve the ergonomicness that work better, and whether
a language comparable to RevBayes or PhyloSpec needs these kind of ergonomic
features.  How can we remove problematic features without breaking simple
idiomatic usage of bali-phy?  Suppose we split this language into two forms,
one that works betteras a generic model-description language, and one that
(at least temporarily) retain some of the features that don't fit so well in
such a language?

## Special features of the command-line language

### Implicit type conversion

During typechecking, if an expression does not unify with the required type,
the typechecker tries to rewrite the AST by inserting a conversion function.
The implemented conversions include `Int` to `Double`, `ExchangeModel<a>` to
`CTMC<a>` via `f`, `CTMC<a>` to `DiscreteDist<CTMC<a>>` via `unit_mixture`,
lists of weighted pairs to `DiscreteDist<a>` via `discrete`,
`DiscreteDist<a>` to `Distribution<b>` via `convertDiscrete`, and
`DiscreteDist<CTMC<a>>` to `MultiMixtureModel<a>` via `multiMixtureModel`.

This lets users write short expressions whose inferred type is close to, but
not exactly, the type required by the surrounding model.

### Default arguments, including implicit priors

Binding JSON files can give each rule argument a `default_value`.  When a
call omits that argument, the typechecker inserts the default expression and
typechecks it as if it had been supplied.  These default values may themselves
be random expressions, so a compact model name like `gtr` can implicitly stand
for a model with sampled parameters and priors.

Default expressions can reference other arguments with `@arg`.  Code
generation orders defaulted arguments by these dependencies and rejects
dependency cycles.

### Threading of the alphabet through

Binding JSON files can also give an argument an `alphabet` expression.  The
typechecker typechecks that expression and temporarily makes its type available
as the `alphabet` state while checking the argument value.  Code generation
evaluates the alphabet expression and makes it available to subexpressions that
use `get_state(alphabet)`.

This is what allows a higher-level expression to pass alphabet information
through a chain of substitution model combinators without the user spelling out
the alphabet at each step.

### get_state

`get_state(name)` is parsed as a special AST node rather than an ordinary
function call.  Typechecking looks up `name` in the current model state and
unifies its type with the required type.  Code generation either uses an
existing state variable or records the state as a used state so that the
compiled model can be turned into a function of that state.

This provides implicit access to contextual values such as `alphabet`,
`tree`, or `branch_categories`.

### Functions that perform actions

Rules can set `"perform": true` in their binding JSON.  Code generation then
treats the call as producing a monadic action rather than a pure value, using
`perform` statements and carrying action-ness through enclosing expressions.

This means that an expression can look like an ordinary model expression while
actually sequencing sampling, observation, logging, or other effects.

### Named arguments

The parser accepts both positional arguments and named arguments.  After
parsing, positional arguments for rule-backed calls are rewritten to named
arguments using the argument order in the rule.  Named arguments are rejected
for calls to local function variables or for functions that have no binding
rule.

This gives the command-line language convenient positional syntax while still
using named arguments internally for defaults, dependency tracking, and code
generation.

### Bare rule names as nullary calls

If an identifier is not a local variable but matches a rule name, the
typechecker treats it as a zero-argument call to that rule.  This lets users
write `gtr` instead of `gtr()`, relying on defaults for any omitted arguments.

### Sample sugar

The grammar supports `~expr`, `x ~ expr`, and `arg ~ expr` as sampling syntax.
These forms are represented as a dedicated `Sample` AST node.  Typechecking
uses the rule-backed `sample` function to assign types, but the dedicated node
is preserved for pretty-printing, extraction, and display.

### `+>` stacking and `_` placeholders

The parser treats `lhs +> rhs` specially.  The right-hand side must be a
function name or function call.  If the call contains the placeholder `_`, the
left-hand side replaces that placeholder.  Otherwise the left-hand side is
prepended as the first positional argument.

This supports terse composition such as passing a submodel into a model
transformer without writing the full nested call.

### Automatic logging and extraction metadata

Rules can set `no_log`, `extract`, and `computed` in their binding JSON.  The
typechecker attaches logging/extraction metadata to typed AST nodes, and code
generation uses it to decide which random or computed values should be bound
and logged.  Extraction also rewrites selected subterms into separately named
model components for display and reporting.

### Local `let`, lambdas, and function definitions

The parser supports `where { ... }` declarations, lambda syntax of the form
`| pattern : expr |`, tuple patterns, and declaration-side function definitions.
Function definitions are converted into lambda expressions during parsing.

These features make the command-line language closer to a small expression
language than a simple model-name DSL.

### Operators as function calls

Arithmetic, comparison, boolean, and symbolic operators are parsed into normal
function calls such as `+`, `-`, `*`, `/`, `&&`, and `==`.  They are then
resolved through the same rule/scope/typechecking machinery as named
functions.

I'm thinking of the fact that |w: gtr +> x3 +> dNdS(w)| is an action,
because it really samples w from the (implicit) prior, and then returns a
pure function.

What other things?



## Possible extensions

For a high-level model-description language,

### Function declarations

Probably functions need to specific whether they are random sampling
procedures or not?

How would such a function declaration work?
