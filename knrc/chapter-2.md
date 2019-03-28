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

## Sizes and ranges for various types
|		Type			|			Size			|							Range								|
|		----			|			----			|							-----								|
|		char			|			1 byte			|						-128 to 127								|
|		short			|			2 bytes			|						-32768 to 32767							|
|		int				|			2/4 bytes		|						-32768 to 32767 / -2M to +2M			|
|		long			|			4 bytes			|						-2M to +2M								|
|		float			|			4 bytes			|				-3.4E+38 to +3.4E+38 (6 decimal places)			|
|		double			|			8 bytes			|				-1.7E+308 to +1.7E+308 (15 decimal places)		|
|		long double		|			10 bytes		|				-3.4E+4932 to +3.4E+4932 (19 decimal places)	|

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
| () [] -> .							|	left to right	|
| ! ~ ++ -- + _ * & (type) sizeof		|	right to left	|
| * / %								|	left to right	|
| + - 								|	left to right	|
| << >>								|	left to right	|
| < <= > >=							|	left to right	|
| == !=								|	left to right	|
| &									|	left to right	|
| ^									|	left to right	|
| \|									|	left to right	|
| &&									|	left to right	|
| \|\|									|	left to right	|
| ?:									|	right to left	|
| = += -= *= /= %= &= ^= |= <<= >>=	|	right to left	|
| ,									| 	left to right	|
