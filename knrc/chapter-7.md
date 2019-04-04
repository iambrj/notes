## I/O
A text stream consists of a sequence of lines; each line ends with a newline
character. If the system doesn't operate that way, the library does whatever is
necessary to make it appear as if it does (convert carriage return to newline
etc)

Programs are oblivious to I/O redirection; in particular, strings like
"\<infile" are not included in the command-line arguments in argv.

`printf` works as follows
```
	int printf(char *format, arg1, arg2, ...)
```
The format string contains two types of objects: ordinary characters, which are
copied to the output stream, and conversiono specifications, each of which
causes conversion and printing of the next successive argument to printf. Each
conversion specification begins with a % and ends with a conversion character.
Between them, there may be, in order:
* A minus sign, which specifies left adjustment of the converted argument
* A number that specifies the minimum field width. If necessary it will be
  padded on the left (or right, if minus sign is specified) to make up field
width.
* A period, which separates the field width from precision
* A number that specifies the precision, the maximum number of characters to be
  printed from the string, or the number of digits after the decimal point of a
  floating-point value, or the minimum number of digits for an integer.
* A `h` if the integer is to be printed as a `short`, or `l` if as a `long`.
Example: `:%-15.10s:	:hello, wor     :`
### Basic printf conversions

|		Character		|		Argument type; printed as		|
|	----------------	|	--------------------------------	|
|	`d`, `i`			|	`int`; decimal number				|
|	`o`					|	`int`; unsigned octal number (without leading zero)		|
|	`x`, `X`			|	`int`; unsigned hexadecimal number (without a leading `0x` or `0X`), using `abcdef` or `ABCDEF` for 10,..., 15	|
|	`u`					|	`int`; unsigned decimal number		|
|	`c`					|	`int`; single character				|
|	`s`					|	`char *`; print characters from the string until a `'\0'` or the number of characters given by the precision	|
|	`f`					|	`double`;[--]m.dddddd, where the number of d's is given by the precision (default 6)	|
|	`e`, `E`			|	`double`;[--]m.dddddde(E)+(-)xx, where the number of d's is given by the precision (default 6)	|
|	`g`, `G`			|	`double`; use `%e` or `%E` if the exponent is less than -4 or greater than or equal to the precision; otherwise use %f. Trailing zeros and a trailing decimal point are not printed.	|
|	`p`					|	`void *`; pointer (implementation-dependent representation)	|
|	`%`					|	no argument is converted; print a %	|

### Basic scanf conversions

|		Character		|		Input Data: Argument type		|
|	----------------	|	---------------------------------	|
|	`d`					|	decimal integer; `int *`			|
|	`i`					|	integer; `int *`. The integer may be in octal (leading `0`) or hexadecimal (leading `0x` or `0X`)	|
|	`o`					|	octal integer (without leading `0`	|
|	`u`					|	unsigned decimal integer; `unsigned int *`	|
|	`x`					|	hexadecimal integer (without leading `0x` or `0X`); `int *`	|
|	`c`					|	characters; `char *`. To read the next non-whitespace character, use `%1s`				|
|	`s`					|	character string; `char *`	|
|	`e`,`f`,`g`			|	floating-point number wiht optional sign, optional decimal point and optional exponent; `float *`	|
|	`%`					|	literal `%`; no assignment is made	|

## Variable-length Argument lists
Use `#include <stdarg.h>` for handling variable-length argument lists.

The type `va_list` is used to declare a variable that will refer to each
argument in turn (say, `ap`). The macro `va_start` initializes `ap` to point to
the first unnamed argument. It must be called once before `ap` is used. There
must be atleast one named argument; the final named argument is used by
`va_start` to get started. Each call of va_arg returns one argument and steps
`ap` to the next argument; va_arg uses a type name to determine what type to
return and how big a step to take. Finally, va_end does whatever cleanup is
necessary. Example:
```
va_list ap;
va_start(ap, [first-arg-name]);
va_arg()
```

## Files

A file pointer points to a structure that contains information about the file,
such as the location of a buffer, the current character position in the buffer,
whether the file is being read or written, and whether errors and EOF have
occured. It is declared in `stdio.h`

Some systems distinguish between text and binary files; for the latter, a "b"
must be appended to the mode string.

If a file that does not exist is opened for writing or appending, it is created
if possible. Opening an existing file for writing causes the old contents to be
discarded, while opening for appending preserves them. Trying to read a file
that does no exist is an error.

Use `getc` and `putc` to read/write characters to a stream
```
int getc(FILE *fp)
int putc(int c, FILE *fp)
```
Like `getchar` and `putchar`, `getc` and `putc` may also be implemented as
macros for efficiency.

Syntax for handling files:
```
int fopen(FILE *fp, "bwar")
int fclose(FILE *fp)
```

_Never_ use `fgets` and `fputs` to read input linewise as they do not detect 
`NULL`s. Use `getline` instead.

The function `system(char *s)` executes the command contained in the character
string `s`, then resumes the execution of the current program.

The pointer returned by `malloc` or `calloc` has the proper alignment for the
object in question, but it must be cast into the appropriate type.

`free(p)` frees the space pointed to by `p`, where `p` was originally obtained
by a call to `malloc` or `calloc`. There are no restrictions on the order in
which space is freed, but it is a ghastly error to free something not obtained
by calling `calloc` or `malloc`.

The function `rand()` computes a sequence of pseudo-random integers in the range
zero to RAND_MAX, which is defined in `stdlib.h`
