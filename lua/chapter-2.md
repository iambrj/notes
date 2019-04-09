There are no type definitions in Lua; each value carries its own type.

The eight basic types in Lua are: `nil`, `boolean`, `number`, `string`,
`userdata`, `function`, `thread`, and `table`.

The `type` function gives the type name of a given value as a `string`.

Variables have no predefined types; any variable may contain values of any type.

`nil` is a type with a single value, `nil`, whose main property is to be
different from any other value. It represents the absence of a value.

`boolean` has two values, `false` and `true`. Note that zero and the empty
string in conditionals are `true` in Lua. Only `false` and `nil` evaluate to
false.

The `number` type represents double-precision floating-point numbers. Lua has no
integer type. Examples
```
4	0.4		4.75e-3		0.3e12		5e+20
```

The `string` type represents a sequence of characters. Lua also allows for
binary data to be stored in strings. Strings are immutable. They are subject to
automatic memory management, like all other objects. Literal strings are
delimited by either matching single or double quotes.
```
a = "a line"
b = 'another line'
```
Lua also supports the following escape sequences
|	Escape sequence		|		Description		|
|	--------------		|		-----------		|
|	`\a`				|	bell				|
|	`\b`				|	backspace			|
|	`\f`				|	formfeed			|
|	`\n`				|	newline				|
|	`\r`				|	carriage return		|
|	`\v`				|	vertical tab		|
|	`\t`				|	horizontal tab		|
|	`\v`				|	backslash			|
|	`\\`				|	bell				|
|	`\"`				|	bell				|
|	`\'`				|	bell				|

Literals may also be delimited using `[[` and `]]`. Example
```
page = [[
	<html>
	<head>
	brj rocks!
	</head>
	<body>
	<h1>
	Not really... :(
	</h1>
	</body>
	</html>
]]
```
Lua ignores first character when it is a newline and escape sequences when using
bracket syntax.

Use some `=` signs between first `[` and second `[` (example `[===[`) to
distinguish them from other strings, comments etc. Note that both the enclosing
double brackets must have the same number of `=` in them.

Lua automatically converts strings to numbers and vice versa.

`..` is the concatenation operator. Example
```
print(10 .. 20)		-->		1020
```
Note usage of spaces to distinguish concatenation operation from decimal point.

Use `tonumber` and `tostring` for conversions between datatypes.

Length of a string can be obtained using the prefix `#` operator. Example
```
a = "hello"
print(#a)				-->	5
print(#"good\0bye")		-->	8
```

The `table` type implements associative arrays. Tables in Lua are objects.
Tables can be accessed only via references. They are created using the
constructor expression, `{}`. Example
```
a = {}
k = "x"
a[k] = 10
print(a["x"])	-->	10
```

A table is always anonymous. A table without any references is automatically
deleted by the garbage collector.

`a.name` is the syntactic sugar for `a["name"]`

The length operator `#` applied to a array/list returns the last index of the
array/list.

Use `maxn(a)` to get the maximum positive index of the list/array `a` when it
has holes in it.

Functions are first class values in Lua.

The `userdata` type allows arbitary C data to be stored in Lua variables.
