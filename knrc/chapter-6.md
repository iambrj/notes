A structure member or tag and an ordinary (i.e., non-member) variable can have
the name without conflict, since they can always be distinguished by context.

Structures may not be compared. A structure may be initialized by a list of
constant member values; an automatic structure may also be initialized by an
assignment.

It is not necessary to enclose initializer for each "row" or structure in braces
as in,
```
struct key {
	char *word;
	int count;
} keytab[] = {
	"auto", 0,
	"break", 0,
	...
};
```
and
```
	...
	{ "auto", 0 },
	{ "break", 0},
	...
```
are syntactically equivalent. However, the latter is easier to read.

The expression `sizeof object` and `sizeof typename` yeild an integer equal to
the size of the specified object or typename.

Always error check on values returned by strdup, malloc etc

Alignment requirements can generally be satisfied easily, at the cost of some
wasted space, by ensuring that the allocator always returns a pointer that meets
*all* alignment restrictions.

The type being declared in a `typedef` appears in the position of a variable 
name, not right after the word `typedef`. Syntactically, `typedef` is like
storage classes extern, static etc.

A `typedef` declaration does not create a new type in any sense; it merely adds
a new name for some existing type. In effect, it is like `#define`, except that
since it is interpreted by the compiler, it can cope with textual substitutions
that are beyond the capabilities of the poreprocessor. For example
```
	typedef int (*PFI)(char *, char*)
```
creates the type `PFI`, for "pointer to function (of two char * arguments)
returning `int`", which can be used in contexts like
```
	PFI strcmp, numcmp;
```

Two main purposes for `typedef`s are
* To parameterize a program against portability problems
* To provide better documentation for a program

A *union* is a variable that may hold (at different times) objects of different
types and sizes, with the compiler keeping track of size and alignment
requirements. Unions are variants.

Union syntax:
```
union u_tag {
	int ival;
	float fval;
	char *sval;
} u;
```
The variable u will be large enough to hold the largest of the three types; the
specific size is implementation dependent. The results are implementation
dependent if something is stored as one type and extracted as another.

A *bit-field*, or *field* for short, is a set of adjacent bits within a single
implementation-defined storage unit that we will call a "word".

Whether a field may overlap a word boundary is implementation defined. Fields
need not be named; unnamed fields (a colon and width only) are used for padding.
The special width 0 may be used to force alighment at the next word boundary.

Fields are assigned left to right on some machines and right to left on others
(little endian vs big endian). They are not arrays, and they do not have
addresses, so the &operator cannot be applied to them.
