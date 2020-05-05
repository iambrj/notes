# Document Object Model
* `document` - refers to entire document, has `head` and `body` properties
* `document.documentElement` - refers to object representing `<html>` tag; root
    of the DOM tree
* `foo.childNodes` - refers to `NodeList` of child nodes of node `foo`
* `foo.firstChild` - refers to first child of node `foo` (or `null`)
* `foo.lastChild` - refers to last child of node `foo` (or `null`)
* `foo.previousSibling`, `foo.nextSibling` - refer to adjacent nodes (or `null`)
* `foo.children` - refers to only element children
* `foo.getElementsByTagName("bar")` - refers to an array-like object of
    elements with the tag name `bar` in the subtree rooted at `foo`
* `foo.getElementById("bar")` - refers to the object with id `bar` in the
    subtree rooted at `foo`
* `foo.getElementsByClassName("bar")` - refers to an array-like object of
    elements with the class name `bar` in the subtree rooted at `foo`
* `foo.appendChild(bar)` - append `bar` to end of `foo`'s `childNodes`
* `foo.insertBefore(newBar, oldBar)` - insert `newBar` before `oldBar` in
    subtree rooted at `foo`
* `foo.replaceChild(newBar, oldBar)` - replace `oldBar` by `newBar` in subtree
    rooted at `foo`
* `document.createTextNode("foo")` - returns a text node with text `foo` that
    can be inserted
    into the document
* `document.createElement("foo")` - returns an element with tag `foo`
* `foo.getAttribute("bar")` - returns value of attribute `bar` for element `foo`
* `foo.setAttribute("name", "value")` - set `name` attribute of `foo` element to
    `bar`
* `foo.offsetWidth`/`foo.offsetHeight` - returns width/height of element `foo`
* `foo.clientWidth`/`foo.clientHeight` - returns width/height of element `foo`
    ignoring borders
* `foo.getBoundingClientRect` - returns `{top, bottom, left, right}` indicating
    pixel properties of `foo`
* `foo.pageXOffset`/`foo.pageYOffset` - returns scroll position
* `foo.style` - refers to object that has properties for all possible style
    properties (hyphenated names are replaced with camelCase names)
* `foo.querySelectorAll("bar")` - returns a `NodeList` containing all elements that
    match `bar` (non live version). Use `querySelector` (without the `All`) to
    get the first element that matches or `null` (if none match)
* `window` - object for registering handlers on entire window
* `foo.addEventListener("baz", bar)` - call function `bar` for event `baz` on
    element `foo`
* `foo.removeEventListener("baz", bar)` - remove function `bar` for event `baz` on
    element `foo`
* Event object - holds additional information about the event, for example
```
let button = document.querySelector("button");
button.addEventListener("mousedown", event => {
    if (event.button == 0) {
        console.log("Left button");
    } else if (event.button == 1) {
        console.log("Middle button");
    } else if (event.button == 2) {
        console.log("Right button");
    });
```
* `event.type` - stores string identifying event
* `foo.stopPropogation()` - stop event propogation to parent elements
* `foo.target` - refers to node where event originated
* `keyup`, `keydown` - events received on key presses
* `event.ctrlKey`/`event.shiftKey`/`event.altKey` etc - check whether key is
    being held down
