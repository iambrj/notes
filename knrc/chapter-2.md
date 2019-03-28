Identifiers are made up of letters and digits; the first character must be a
letter. Underscore counts as a letter.

Strings may be concatenated at compile time
```
"hello," " world"
```
is equivalent to 
```
"hello, world"
```

Conversion rules:-
* If either operand is long double, convert the other to long double
* Otherwise, if either operand is double, convert the other to double
* Otherwise, if either operand is float, convert the other to float
* Otherwise, conver char and short to int
* Then, if either oerand is long, conver the other to long

Increment and decrement operators can only be applied to variables; an
expression like `(i + j)++` is illegal.

## Precedence and Associativity of operators
|				Operators				|	Associativity	|
|				---------				|	-------------	|
| `() [] -> .`							|	left to right	|
| `! ~ ++ -- + _ * & (type) sizeof`		|	right to left	|
| `* / %`								|	left to right	|
| `+ - `								|	left to right	|
| `<< >>`								|	left to right	|
| `< <= > >=`							|	left to right	|
| `== !=`								|	left to right	|
| `&`									|	left to right	|
| `^`									|	left to right	|
| `|`									|	left to right	|
| `&&`									|	left to right	|
| `||`									|	left to right	|
| `?:`									|	right to left	|
| `= += -= *= /= %= &= ^= |= <<= >>=`	|	right to left	|
| `,`									| 	left to right	|
