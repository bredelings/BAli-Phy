# Command-Line Model `ptree` Invariants

This documents the `ptree` shapes used by the current command-line model
language.  These shapes are the compatibility contract for the planned
`ModelExpr` AST conversion.

## Unannotated Expressions

- Null `ptree` values are used only as parser compatibility placeholders for
  missing positional arguments.
- Integers, doubles, and booleans are represented as scalar `ptree` values with
  no children.
- String constants are represented as scalar strings that include the quote
  markers, for example `"abc"`.
- Variables and function names are represented as unquoted string values.
- Argument references are represented as unquoted string values beginning with
  `@`, for example `@x`.
- The wildcard placeholder is the unquoted string `_`.
- Function calls are represented as a string function name with children.  Each
  child key is the argument name; an empty key means a positional argument.
- `List` is a string value whose children are the list elements.  List element
  keys are ignored and are normally empty.
- `Tuple` is a string value whose children are tuple elements.  Tuple element
  keys are ignored and are normally empty.  A valid tuple has at least two
  elements.
- `!let` has children `decls` and `body`.  The `decls` child is a declaration
  block and the `body` child is an expression.
- A declaration block has string value `!Decls` and children whose keys are
  binding names and whose values are expressions.
- `function` represents a lambda and has exactly two children: the pattern and
  the body.
- `sample` represents `~expr` and has exactly one expression child.
- `get_state` represents state lookup and has exactly one string-valued child
  containing the state name.

Malformed `!let`, `function`, `sample`, `get_state`, one-element tuples, and
null placeholders outside parser compatibility should be rejected by the new
converters instead of preserved.

## Annotated Expressions

After typechecking, expression nodes are wrapped in annotation records:

- `value` contains the underlying expression shape.
- `type` contains the inferred type, currently still a `ptree`.
- `used_args` contains the set of argument names used by the expression.
- `no_log` suppresses logging for the node.
- `extract` names extracted model terms.

The following fields are currently stored on argument child values, but they are
argument-edge metadata rather than expression-node metadata:

- `is_default_value`
- `alphabet`
- `suppress_default`

The `ModelExpr` compatibility layer should move those fields to `ModelArg`.
When list and tuple elements carry old always-false edge metadata, conversion
may ignore it because those children are expression children, not function
arguments.
