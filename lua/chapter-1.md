To start the interpreter, use `lua`

To run commands from a file, use `lua [filename].lua`

A chunk is a sequence of commands. A chunk may be a single statement, or it may
be composed of a mix of statements and function definitions.

Lua needs no separator between consecutive statements, but a `;` can be used.

To start an interactive session after running a given chunk, use 
`lua -i [file].lua`.

Use `dofile` to run a chunk from an interactive session.

Identifiers in Lua can be any string of letters, digits and underscores, not
beginning with a digit. Example
```
i
jj
i10
_ij
```

Reserved words
```
and		break	do		else		elseif
end		false	for		function	if
in		local	nil		not			or
repeat	return	then	true		until
while
```
Identifiers are case sensitive.

Comments start with `--` and run until the end of the line.

Block comments start with `--[[` and run until next `]]`. A common trick is to
use `--[[` and `--]]` for comments so that blocks can be uncommented by adding a
single `-` at the beginning.

Global variables need no declarations. Non-initialized variables are initialized
to `nil`.

To delete a global variable, just assign `nil` to it. Lua behaves as if the
variable had never been used. A global variable is existent iff it has a non-nil
value.

Some common options
|	Option		|			Description					|
|	-----		|			-----------					|
|	`-e`		|	Execute script from command line	|
|	`-l`		|	Load a library						|
|	`_PROMPT`	|	Prompt for interacting				|

In interactive mode, value of any expression can be printed by writing a line
that starts with an equal sign followed by the expression.
```
> = math.sin(30)	--> 0.14
> a = 3
> = a				--> 3
```

Set the `LUA_INIT` environment variable to either `@filename` to make `lua` run
the contents of `filename` before beginning, or to a chunk. If there is no `@`,
it simply tries to execute its contents as a command.

Command line arguments can be accessed via the global variable `arg`. Preceding
options go to negative indices as they appear before the script.
