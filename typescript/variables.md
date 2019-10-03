* Variables are declared using `let` keyword
* Variables are lexical-scoped
* Variables cannot be used outside their scope or before they are declared
	("temporal dead zone")
* Re-declarations are illegal
# const
Variables declared with `const` cannot be changed. However, their internal state
is still modifiable.
```
const kitty = {
	name: "Aurora",
	numLives: 9,
}

// Error
kitty  = {
	name: "Danielle",
	numLives: 10,
}

// Accepted
kitty.name = "Cat";
kitty.numLives--;
```
# Destructring
Destructring is pattern matching.
```
let [first, ...rest] = [1, 2, 3, 4];
let [,second, ,fourth] = [1, 2, 3, 4]
console.log(first) // 1
console.log(rest) // [2, 3, 4]
cosole.log(second) // 2
```
# Property renaming
```
let { a: newName1, b: newName2} = o;
```
is same as
```
let newName1 = o.a;
let newName2 = o.b;
```
