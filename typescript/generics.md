* Use `<>` syntax for creating generics
```
function identity<T>(arg: T): T { // T is called a type variable
	return arg;
}

// Explicit typing
let output = identity<string>("myString");

// Type inference
let output = identity("myString");

// Generic type parameter in the type
let myIdentity: <U>(arg: U) => U = identity;

// Generic type as a call signature of an object literal type
let myIdentity: {<T>(arg: T): T} = identity;
```
* The type of generic functions is just like those of non-generic functions,
	with the type parameters listed first.
```
// Generic interface
interface GenericIdentityFn {
	<T>(arg: T): T;
}

function identity<T>(arg: T): T {
	return arg;
}

let myIdentity: GenericIdentityFn = identity;
```
# Generic classes
* Generic classes have a generic type parameter list in angle brackets.
```
class GenericNumber<T> {
	zeroValue: T;
	add: (x: T, y: T) => T;
}

let myGenericNumber = new GenericNumber<number>();
myGenericNumber.zeroValue = 0;
myGenericNumber.add = function(x, y) { return x + y; };
```
* Generic classes are only generic over their instance side rather than their
	static side, so when working with classes, static members can not use the
	class's type parameter.
# Generic Constraints
* Use interfaces to express constraints
```
interface Lengthwise {
	length: number;
}

function loggingIdentity<T extends Lengthwise>(arg: T): T {
	console.log(arg.length);
	return arg;
}

loggingIdentity(3); // Error, no .length property

loggingIdentity({length: 3, value: 3}); // Works
```
# Type Parameters in Generic Constraints
```
function getProperty<T, K extends keyof T>(obj: T, key: k) {
	return obj[key]; // Constraint to make sure we don't try accessing
					 // properties that dont exist
}
```

# Class Types in Generics
TODO: read up about factories and read this section again
