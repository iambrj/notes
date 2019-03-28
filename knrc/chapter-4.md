If return type of a function is ommited, int is assumed.

A function is implicitly declared by its first appearance in an expression.

If a name that has not been previously decflared occurs in an expression and is
followed by a left parenthesis, it is declared by context to be a function name,
the function is assumed to return an int, and nothing is assumed about its
arguments.

By default, external variables and functions have the propertry that all
references to them by the same name, even from functions compiled separately,
are references to the same thing.

Any function may access an external variable by referring to it by name, if the
name has been declared somehow.

The scope of a name is the part of the program within which the name can be
used.

A declaration announces the properties of a variable; a definition also causes
storage to be set aside.

There must be only one definition of an external variable among all files that
make up the source program; other files may contain `extern` declarations to
access it.

The static declaration, applied to an external variable or function, limits the
scope of that object to the rest of the source file being compiled.

`register` variables are to be placed in machine registers, which may result in
smaller and faster programs.

The `register` declaration can only be applied to automatic variables and to the
formal parameters of a function.

Excess `register` declarations are harmless since the word `register` is ignored
for excess or disallowed declarations.

Automatic variables, including formal parameters, also hide external variables
and functions of the same name.

In the absence of explicit initilization, external and static variables are
guaranteed to be initializzed to zero; automatic and register variables have
undefined (i.e. garbage) initial values.

The name in a #define has the same form as a variable name; the replacement text
is arbitary. Long definiions may be continued onto several lines by placing a
`\` at the end of each line. Substitutions are made only for tokens, and do not
take place within quoted strings.

Macros avoid the run-time overhead of a function call.

The preprocessor operator ## provides a way to concatenate actual arguments
during macro expansion.
