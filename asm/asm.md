# Various general purpose registers available for x86 are

`rax` - 64 bit accumulator: Used in I/O and arithmetic instructions

`ebx` - 32 bit base register: Used for indexing

`cx` - 16 bit count register: Used for count in looping

`dl` - 8 bit data register: Also used for I/O

`ip` - 16 bit Instruction Pointer: Points to next instruction

`sp` - 16 bit Stack Pointer: Provides offset value within program stack

`bp` - 16 bit Base Pointer: Used to get parameter location in functions

`si` - 16 bit Source Index: Source index for string operations

`di` - 16 bit Destination Index: Destination index for string operations

`cs` - 16 bit Code Segment: Contains starting address of code segment

`ds` - 16 bit Data Segment: Contains strating address of data segment

`ss` - 16 bit Stack Segment: Contains starting address of stack segment

# Flags

`OF` - Overflow Flag: Indicates overflow of higher-order bit of data after a signed arithmetic operation

`DF` - Direction Flag: If set, strings are processed right-to-left

`IF` - Interrupt Flag: If set, enables interrupts

`TF` - Trap Flag: If set, processor enters single-step execution

`SF` - Sign Flag: Set if result of arithmetic operator negative

`ZF` - Zero Flag: Set if result of arithmetic operator zero

`AF` - Auxillary carry Flag: Contains carry from bit-3 to bit-4

`PF` - Parity Flag: Set if odd number of 1-bits

`CF` - Carry Flag: Contains carry from higher-order bit after arithmetic operation. It also contains last bit of shift or rotate

# Using Linux system calls
| eax | Name | ebx | ecx | edx | esx | edi |
| --- | ---- | --- | --- | --- | --- | --- |
|  1  | sys_exit | int |	 |	   |	 |		|
|  2  | sys_fork | struct pt_regs	|	|	|	|	|
|  3  | sys_read | unsigned int	| char * | size_t |	|	|
|  4  | sys_write | unsinged int | const char * | size_t |	|	|
|  5  | sys_open | const char *	| int | int	|	|	|
|  6  | sys_close | unsigned int |	|	|	|	|

# Addressing modes

## Register Addressing
In this addressing scheme, operands are registers
```
MOV DX,	TAX_RATE	; Register in first operand
MOV COUNT, CX		; Register in second operand
MOV EAX, EBX		; Both operands are registers
```
## Immediate Addressing
In this addressing scheme, operands are constans or expressions
```
BYTE_VALUE DB 150	; A byte value is defined
WORD_VALUE DW 300	; A word value is defined
ADD BYTE_VALUE, 65	; An immediate operand 65 is added
MOV AX, 45H			; Immediate constant 45H is transferred
```

## Direct Memory Addressing
In this addressing scheme, operands are specified as memory address using data segment.
```
ADD BYTE_VALUE, DL	; Adds register to memory location
MOV BX, WORD_VALUE	; Moves value from memory location to register
```
This is further divided into two types
### Direct-Offset Addressing
This addressing scheme uses arithmetic operators to modify an address
```
BYTE_TABLE DB 14, 15, 22, 45		; Table of bytes
WORD_TABLE DW 134, 135, 564, 123	; Table of words
MOV CL, BYTE_TABLE[2]				; Get 3rd element of BYTE_TABLE
MOV CL, BYTE_TABLE + 2				; Get 3rd element of BYTE_TABLE
MOV CX, WORD_TABLE[3]				; Get 4th element of WORD_TABLE
MOV CX, WORD_TABLE + 3				; Get 4th element of WORD_TABLE
```
### Indirect-Memory Addressing
This addressing scheme uses registers to refer to memory locations
```
MY_TABLE TIMES 10 DW 0	; Allocate 10 words each initialized to 0
MOV EBX, [MY_TABLE]		; Effective address of MY_TABLE in EBX
MOV [EBX], 110			; MY_TABLE[0] = 110
ADD EBX, 2				; EBX += 2
MOV [EBX], 123			; MY_TABLE[1] = 123
```
#### Type Specifiers
| Type specifier | Bytes addressed |
| -------------- | --------------- |
|	BYTE		|		1			|
|	WORD		|		2			|
|	DWORD		|		4			|
|	QWORD		|		8			|
|	TBYTE		|		10			|

## Allocating memory for variables
|	Keyword		|	Bytes allocated	|
| ------------  |	---------------	|
|	DB			|	1				|
|	DW			|	2				|
|	DD			|	4				|
|	DQ			|	8				|
|	DT			|	10				|

Example:
```
section .text

	global _start

_start:
   mov	edx,1		  ;message length
   mov	ecx,choice    ;message to write
   mov	ebx,1		  ;file descriptor (stdout)
   mov	eax,4		  ;system call number (sys_write)
   int	0x80		  ;call kernel

   mov	eax,1		  ;system call number (sys_exit)
   int	0x80		  ;call kernel

section .data
choice DB 'y'
	
```

## Allocating space for uninitialized data
|	Keyword	|	Bytes allocated	|
|	-------	|	---------------	|
|	RESB	|		1			|
|	RESW	|		2			|
|	RESD	|		4			|
|	RESQ	|		8			|
|	REST	|		10			|

_NOTE_: Use `times` directive to for multiple instantiations for the same value
```
marks TIMES 9 DW 0
```

Use `equ` directive to declare immutable numeric constants using syntax `CONSTANT_NAME equ EXPRESSION`
```
totalStudents equ 45
mov ax, totalStudents	; totalStudents = 45
```

Use `%assign` directive to declare mutable numeric constants using syntax `%assign CONSTANT_NAME EXPRESSION`

Use `%define` directive to declare mutable numeric/string constants using syntax `%define PTR [EBP + 4]`
