* An interface is a syntactical contract that an entity should conform to. In
  other words, an interface defines the syntax that any entity must adhere to.
  Interfaces define properties, methods, and events, which are the members of
  the interface.

```
interface SquareConfig {
	color?: string; // optional argument
	readonly width?: number; // optional nonwritable argument
}

function createSquare(config: SquareConfig): {color: string; area: number} {
	let newSquare = {color: "white", area: 100};

	if(config.color) {
		newSquare.color = config.color;
	}

	if(config.width) {
		newSquare.area = config.width * config.width;
	}

	return newSquare;
}
```
* Variables use `const` whereas properties use `readonly`
* Add string index signature to tell `tsc` that an interface may have extra
  properties.
```
interface SquareConfig {
	color?: string;
	width?: number;
	[propName: string]: any; // extra allowed, with atleast one of above
}
```
# Function Types
Syntax
```
interface <typeName> {
	(arg1: type1, arg2: type2, ...) : <returnType>;
}
```
Example
```
interface SearchFunc {
	(source: string, subString: string): boolean;
}

let mySearch: SearchFunc;
mySearch = function(source: string, subString: string) {
	let result = source.search(subString);
	return result > -1;
}
```
# Indexable Types
Syntax
```
interface <typeName> {

	[index: <key type>]: <val type>
}
```
Example
```
interface StringArray {
    [index: number]: string;
}

let myArray: StringArray;
myArray = ["Bob", "Fred"];

let myStr: string = myArray[0];
```
* Making index signatures `readonly` prevents assignment after definition
```
interface ReadonlyStringArray {
	readonly[index: number]: string;
}
let myArray: ReadonlyStringArray = ["Alice", "Bob"];
myArray[2] = "Mallory"; // error!
```
# Class Types
* Classes *implement* interfaces, for example
```
interface ClockInterface {
	currentTime: Date;
	setTime(d: Date): void;
}

class Clock implements ClockInterface {
	currentTime: Date = new Date();
	setTime(d: Date): void {
		this.currentTime = d;
	}
	constructor(h: number, m: number) { }
}
```
TODO: read up rest of the section after reading the chapter on classes.

# Extending Interfaces
* Interfaces can be extended to include more elements, example
```
interface Shape {
	color: string;
}

interface PenStroke {
	penWidth: number;
}

interface Square extends Shape, PenStroke {
	sideLength: number;
}

let square = {} as Square;
square.color = "blue"
square.sideLength = 5
square.penWidth = 1.2
```

# Hybrid Types
TODO: read up this section after reading the chapter on functions
# Interfaces extending classes
TODO: read up this section after reading the chapter on classes
