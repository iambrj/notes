# Classes
```
class Greeter {
	greeting: string;
	constructor(message: string) {
		this.greeting = message;
	}
	greet(): string {
		return "Hello, " + this.greeting;
	}
}

let greeter = new Greeter("world");
```
# Inheritance
```
class Animal {
	move(distanceInMeters: number = 0) {
		console.log(`Animal moved ${distanceInMeters}m.`)
	}	
}

class Dog extends Animal {
	bark() {
		console.log("Woof! Woof!")
	}
}

const dog = new Dog();
dog.bark();
dog.move(10);
dog.bark();
```
* Each derived class that contains a constructor function *must* call `super()`
	which will execute the constructor of the base class and this call to
	`super()` must happen *before* accessing any property using `this`.
```
class Animal {
	name: string;
	constructor(theName: string) {
		this.name = theName;
	}

	move(distanceInMeters: number = 0) {
		console.log(`${this.name} moved ${distanceInMeters}m.`);
	}
}

class Snake extends Animal {
	constructor(name: string) {
		super(name);
	}

	move(distanceInMeters = 5) {
		console.log("Slithering...");
		super.move(distanceInMeters);
	}
}

class Horse extends Animal {
	constructor(name: string) {
		super(name);
	}

	move(distanceInMeters = 45) {
		console.log("Galloping...");
		super.move(distanceInMeters);
	}
}

let sam = new Snake("Sammmy the Python");
let tom: Animal = new Horse("Tommy the Palomino");

sam.move();
tom.move(34); // calls Horse.move, not Animal.move (Java-like polymorphism)
```
* TypeScript has a structural type system - if the types all members are
	compatible, then the types themselves are compatible. Also if a type has a
	`private`/`protected` member, then the other must have a
	`private`/`protected` member that originated in the same declaration (for
	example, via inheritance) for them to be compatible.

```
class Animal {
	private name: string;
	constructor(theName: string) {
		this.name = theName;
	}
}

class Rhino extends Animal {
	constructor() { super("Rhino"); }
}

class Employee {
	private name: string;
	constructor(theName: string) {
		this.name = theName;
	}
}

let animal = new Animal("Goat");
let rhino = new Rhino();
let employee = new Employee("Bob");

animal = rhino; // ok, public parts match and private member originated in same declaration
animal = employee; // Error! Although public parts match, private member not originated in the same declaration
```
# Protected
* `protected` members behave like `private` members, except they can be accessed
	from derived classes.
* If a constructor is marked `protected`, the class cannot be instantiated
	outside of its containng classs, but can be extended.
```
class Person {
	protected name: string;
	protected constructor(name: string) {
		this.name = name;
	}
}

class Employee extends Person {
	private department: string;

	constructor(name: string, department: string) {
		super(name);
		this.department = department;
	}
}

let howard = new Employee("Howard", "Sales"); // Ok
let john = new Person("John");
```
* `readonly` members must be initialized at their declaration or in the
	constructor.
# Parameter properties
* Members can be created and initialized in place by using appropriate accessors
	(`public`/`private`/`readonly`) in constructor.
```
// Circle has two members, center (a readonly member) and radius (a private
member)
class Circle {
	constructor(readonly center: number, private radius: number) {
	}
}
```
# Static Properties
* Members can be declared static for all instances of an object using `static`
	keyword. To access a static member, append the member name to class name
```
class Grid {
	static origin = {x: 0, y: 0};
	calculateDistanceFromOrigin(point: {x: number; y: number;}) {
		let xDist = (point.x - Grid.origin.x);
		let yDist = (point.y - Grid.origin.y);
		return Math.sqrt(xDist * xDist + yDist * yDist) / this.scale;
	}
	constructor(public scale: number);
}

let grid1 = new Grid(1.0);
let grid2 = new Grid(5.0);

grid1.calculateDistanceFromOrigin({x: 10, y: 10});
```
# Abstract Classes
* Abstract classes are base classes from which other classes can be derived, but
	may not be instantiated directly. `abstract` keyword is used to define an
	abstract class.
```
abstract class Animal {
	abstract makeSound(): void;
	move(): void {
	}
}
```
