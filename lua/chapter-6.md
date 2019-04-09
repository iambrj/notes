Functions in Lua are first-class values with proper lexical scoping.
* first-class = can be used just another variable
* lexical scoping = functions can access variables of their enclosing functions

```
function <name> (<parameters>)
	<body>
	return
end
```
is syntactic sugar for
```
<name> = function (<parameters>)	<body>	return end
```
i.e. the Lambda.

A closure is a function plus all it needs to access non local variabels
correctly. Used in callback functions.

Functions can be scoped using `local` keyword. They are then visible only in the
block in which they are defined.

Syntactic sugar for local functions is also available
```
local function <name> (<params>) -- note use of local keyword
	<body>
end
```

Note that recursive functions must use forward declarations
```
local function <name> (<parameters>) -- note use of local
	<body>
end
```
for mutually recursive functions
```
local f, g -- forward declarations
function g ()
	<some code> f() <some code>
end
function f ()
	<some code> g() <some code>
end
```
A tail call happens when a function calls another as its last action.
