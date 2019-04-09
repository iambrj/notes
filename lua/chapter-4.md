Lua allows multiple assignment, where a list of values is assigned to a list of
varibles in one step.
```
a, b = 10, 2 * x
```
Here, `a` gets `10` and `b` gets `2 * x`

In a multiple assignment, Lua first evaluates all the values and only then
executes the assignments. Extra variables are assgined `nil`.

Local variables are created using the `local` statement
```
local i = 1
```

Local variables are limited to the block/chunk where they are declared.

`do` blcoks are useful for finer control over scope
```
do
	local a2 = 2 * a
	local d = (b ^ 2 - 4 * a * c)^(1 / 2)
	x1 = (-b + d)/a2
end -- scope of a2 and d ends here
print(x1) -- works fine, x1 is global
```
Local variables are faster to access than global variables.

## Control Structures
Lua provides `if`, `while`, `repeat` and `for`. All control structures must be
explicitly terminated - `if`, `for` and `while` with an `end`; and `repeat` with
`until.
### if-elseif-then-end
```
if op == "+" then
	r = a + b
elseif op == "-" then
	r = a - b
elseif op == "*" then
	r = a * b
elseif op == "/" then
	r = a / b
else
	error("invalid operation")
end -- note use of end
```
Lua has no switch statement.

### while-do-end
```
while a[i] do
	print(a[i])
	i = i + 1
end
```
### repeat-until
```
repeat
	line = os.read()
until line ~= ""
print(line)
```
### for
`for` is of two types - numeric `for` and generic `for`
#### numeric for
```
for var=exp1,exp2,exp3 do
	<something>
end
```
`<something>` is executed with `var` set to `exp1`, `exp2` etc.
All expressions are evaluated once, before the loop starts. The control variable
is `local`.

Use a `break` to end a `for` before its normal termination - changing `var`
leads to unpredictable results.

#### generic for
The generic `for` tarverses all values returned by an iterator function:
```
for i,v in ipairs(a) do print(v) end
```

For syntactic reasons, a `break` or `return` can appear only as the last
statement of a block. For inserting them in between, use explicit `do-end`
blocks.
