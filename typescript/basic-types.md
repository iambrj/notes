# Boolean
```
let isDone: boolean = false;
```
# Number
```
let decimal: number = 6;
let hex: number = 0xf00d;
let binary: number = 0b101;
let octal: number = 0o744;
```
# String
```
let color: string = "blue";
color = 'red';
```
# Template strings
```
let fullName: string = `Bharathi Ramana Joshi`;
let age: number = 91;
let sentence = `Hello, my name is ${ fullName }.
I'll be ${ age + 1 } years old next month`;
```
# Array
```
let num_list: number[] = [1, 2, 3];
let num_list: Array<number> = [1, 2, 3];
```
# Tuple
```
let x: [string, number];
x = ["hello", 10]
```
# Enum
```
enum Color {Red, Green, Blue} // Indexed from 0 by default
let c: Color = Color.Green
```
# Any
Supertype of all types (like `Top`). Used as a placeholder for values of unknown types.
```
let notSure: any = 4;
notSure = "Maybestring instead";
notSure = false;
```
# Void
Absence of a type, for instance functions that do not return anything
```
function printMessage(message: string): void {
	console.log(message);
}
```

Variables of type `void` can only take the values `null` and `undefined`.
```
let unusable: void = undefined;
unusable = null; // OK if `--strictNullChecks` is not given
```
# Null and Undefined
`null` and `undefined` are subtypes of all other types
```
let u: undefined = undefined;
let n: null = null;
```
If `--strictNullChecks` is set (good practice) `null` is assignable to `any` and
`null` and `undefined` is assignable to `any`, `undefined` and `void`
# Never
Subtype of all types (like `Bottom`). Uses for values that never occur (for
instance functions that never return, throw exceptions etc)
```
function infiniteLoop(): never {
	while(true) {
	}
}
```
# Object
`object` is used for terms of non-primite types (not
`number`/`string`/`boolean`/`symbol`/`null`/`undefined`)
```
declare function create(o: object | null): void;

create({prop: 0}); // ok
create(null) // ok

create(1) // type error
```
# Type Assertions
```
let someValue: any = "this is a string";
let strLength: number = (<string>someValue).length; // not allowed with jsx
let strLength: number = (someValue as string).length; // allowed with jsx
```

