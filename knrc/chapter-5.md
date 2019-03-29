A pointer is constrained to point to a particular kind of object: every pointer
points to a specific datatype (except for the void pointer, which is used to
hold any type of pointer but cannot be dereferenced itself.)

Pointer arguments enable a function to access and change objects in the function
that called it.

An array-and-index expression is equvialent to one written as a pointer and
offset.

There is one difference between an array name and a pointer that must be kept in
mind - a pointer is a variable, so pa = a and pa++ are legal. But an array name
is not a variable; constructions like a = pa and a++ are illegal.

When an array name is passed to a function, the function can at its convenience
believe that it has been handed either an array or a pointer, and manipulate it
accordingly.

If one is sure that the elements exits, it is also possible to index backwards
in an array; p[-1], p[-2] and so on are syntactically legal, and refer to
elements that immediately precede p[0].

C guarantees that zero is never a valid address for data, so a return value of
zero can be used to signal an abnormal event.

Pointer and integers are not interchangeable, with zero being the sole
exception: the constant zero may be assigned to a pointer, and a pointer may be
compared with the constant zero.

The valid pointer operations are assignment of pointer of the same type, adding
or subtracting a pointer and an integer, subtracting or comparing two pointers
to members of the same array, and assigning or comparing to zero. All other
pointer arithmetic is illegal.

Only the first dimension (subscript) of an array is free; all the others have to
be specified.

In function pointers, the `&` operand is not necessary, in the same way that it
is not needed before an array name.

Any pointer can be case to void\* and back again without loss of information.
