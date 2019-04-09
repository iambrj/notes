If the function has one single argument and that argument is either a string
literal or a table constructor, then the parantheses are optional

When calling a function, there is no difference between functions defined in Lua
and functions defined in C.

## Syntax
```
function <function-name> (<parameters>)
	<body>
	return <values>
end
```

Extra arguments are thrown away; extra parameters get `nil`.

Functions return multiple values by listing them all after the `return` keyword.
```
function alpha(meh)
	<do something>
	return a, b, c
end
```
## Return values
* Lua adjusts number of results returned - extra return values are discarded and
missing values are set to `nil`

* When a function is called as a statement, Lua discards all results from the
function.

* When a function is called as an expression, Lua keeps only the first result.

* All results are returned only when the call is the last (or the only) expression
in a list of expressions.

To force a call to return exactly one result by enclosing it in an extra pair of
parantheses.

`unpack` receives an array and returns as results all elements from the array,
starting from index 1
```
a, b = unpack{10,20,30} -- a=10, b=20, 30 discarded
```

To call a function `f` with variable arguments in an array a, write
```
f(unpack(a))
```

Use three dots `...` in the parameter list indicate that the function accepts a
variable number of arguments. All its arguments are collected as varargs
internally. 

Lua uses positional arguments by defualt, to use named arguments define a
structure containing all arguments to be passed and pass this structure.
```
rename{old="temp.lua", new="temp1.lua"}
function os.rename(arg)
	return os.rename(arg.old, arg.new)
end
```
