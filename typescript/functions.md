* Functions can access variables outside their scope. A function is said to
	*capture* such variables.
```
let z: number = 100;

let my Add:(x: number, y: number) => number =
	function addToZ(x: number, y: number): number { return x + y + z; };
```

# Optional & Default Parameters
* Append `?` to a parameter name to make it optional. Use `if` to check if an
	optional parameter has been passed. Optional parameters must follow required
	parameters.
* Append `=` followed by a value to assign a default value to a parameter.
	Default-initialized parameters that come after all required parameters are
	treated as optional. If a user wishes to omit a default parameter in
	between, they should explicitly pass `undefined`.
# Rest Parameters
* Rest parameters are treated as boundless number of optional parameters.
```
function buildName(firstName: string, ...restOfName: string[]) {
    return firstName + " " + restOfName.join(" ");
}

let buildNameFun: (fname: string, ...rest: string[]) => string = buildName;
```
# `this`
TODO: read up on JavaScript's `this` and then come back
# Function overloading
* Supply multiple function types for the same function as a list of overloads
	and use `typeof` operator to check the type of a parameter.
```
let suits = ["hearts", "spades", "clubs", "diamonds"];

function pickCard(x: {suit: string; card: number;}[]): number;
function pickCard(x: number): {suit: string; card: number; };
function pickCard(x) : any {
	if(typeof x == "object") {
		let pickedCard = Math.floor(Math.random)() * x.length);
		return pickedCard;
	}

	else if(typeof x == "number") {
		let pickedSuit = Math.floor(x / 13);
		return {suit: suits[pickedSuit], card: x % 13};
	}
}

let myDeck = [{ suit: "diamonds", card: 2 }, { suit: "spades", card: 10 }, { suit: "hearts", card: 4 }];
let pickedCard1 = myDeck[pickCard(myDeck)];
alert("card: " + pickedCard1.card + " of " + pickedCard1.suit);

let pickedCard2 = pickCard(15);
alert("card: " + pickedCard2.card + " of " + pickedCard2.suit);
```
