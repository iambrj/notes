Expressions include numeric constatns, string literals, variables, unary and
binary operations, function calls, function definitions and table constructors.

`x % 1` gives fractional part of `x`.

Lua uses `~=` instead of `!=` for inequality.

Lua compares tables, useradata and functions by reference, that is, two such
values are consdered equal only if they point to the very same object.

Lua compares strings in alphabetical order (i.e. "2" < "15" is `false`)

### Precedence table (higher to lower)
```
^
not    #    -
*    /    %
+    -
..
<    >    <=    >=    ~=    ==
and
or
```

All binary operators are left associative, except for `^` and `..`

When in doube, always use explicit parentheses.

Constructor are expressions that create and initialize tables.

The simplest constructor, `{}`, creates an empty table.

Constructors are also used to initialize arrays/lists/sequences. Note that Lua
uses 1 based indexes.

```
day = {"Sunday", "Monday", "Tuesday", ......}
print(day[1])    --> Sunday
```

To remove something, set it to `nil`
```
day[1] = nil
```

Indexes can be explicitly stated
```
opnames = {["+"] = "add", ["-"] = "sub", ["*"] = "mul", ["/"] = "div"}
```
