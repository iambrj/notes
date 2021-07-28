---
author: Bharathi Ramana Joshi
title: 'Notes on "Clean Code"'
---

# Clean Code

- LeBlanc's law: Later equals never.
- What if you are the doctor and the patient demands that you stop all the
    silly hand-washing in preparation for surgery because it is taking too much
    time?
- Just because we can recognize a good painting from a bad one does not mean we
    know how to paint a good painting (painting = writing code).
- Bad code tempts the mess to grow --- when others change bad code, they make it
    worse.
- If it hath not tests, it be unclean.
- Clean code is that which is left by someone who cares deeply about the craft.
- Each method/class should do *one* thing *well*.
- ***No*** duplication.
- Ward Cunningham:
> You know you are working on clean code when each routine you read turns out to
> be pretty much what you expected. You can call it beautiful code when the code
> also makes it look like the language was made for the problem.
- The designer makes it look ridiculously simple like all exceptional designs.
- Making it easier to read actually makes it easier to write.
- Leave the campground cleaner than you found it.

# Meaningful names

## Intention-revealing names

- Implicity: the degree to which the context is explicit in the code.
- Avoid disinformation (e.g. `accounts` instead of `accountsList`).
- Inconsistent spellings is disinformation.
- Beware of names that wary in small ways.

## Make meaningful distinctions

- Number series naming (`a1`, `a2`, ...) isn't disinformative, but
    noninformative.
- Random renaming to satisfy compiler (e.g. scope conflicts) is ***not***
    a meaningful distinction.
- Distinguish names in such a way that the reader knows what the differences
    offer.
## Use pronounceable names

- Programming is a social activity.

## Use searchable names

- The length of a name should correspond to the size of its scope.

## Avoid encodings

## Avoid mental mapping

- Programmers are smart people and like to show off their smarts by
  demonstrating their mental juggling abilities.

## Class names are nouns

## Method names are verbs

## Don't be cute

- Choose clarity over entertainment value.
- Say what you mean, mean what you say.

## Pick one word per concept

- Have a consistent lexicon (e.g. use `getXXX` across all classes, instead of
    `getXXX` for one, `retrieveXXX` for another etc).

## Don't pun

- Using same term for two different ideas is a pun.
- Author not academic, you should make yourself clear it's not the scholar's job
    to dig the meaning out of the paper.

## Use solution domain names

## Use problem domain names

- Separating solution and problem domain concepts is part of the job of a good
    programmer and designer.

## Add meaningful context

- Most names are not meaningful in and of themselves.

## Don't add gratuitous context

# Functions

## Small!

- ~20 lines in the ***worst*** case.
- ***Never*** more than one indent level.
> Functions should do one thing. They should do it well. They should do it only.
- A function is doing more than One Thing if you can extract another function
    from it with a name that is not merely a restatement of its implementation.
- Functions that do One Thing cannot be reasonably divided into sections.

## One level of abstraction per function

## Reading code from top to bottom

- A program is a set of TO paragraphs, each of which is describing the current
    level of abstraction and referencing subsequent TO paragraphs at the next
    level down.

## Bury the switches

## Use descriptive names

- A long descriptive name is better than a short enigmatic name.
- A long descriptive name is better than a long descriptive comment.

## Function arguments

- Ideal number of arguments are no arguments.
- 0 > 1 > 2 > 3. > 3 => special justification.

## Flag arguments

- ***UGLY***
- Indicates function does more than One Thing.

## Dyadic functions

- Makes us ignore arguments, and bugs hide in parts of code we ignore!

## Argument objects

- Classes are *for* grouping related objects together!

## Argument lists

## Verbs and keywords

- Function name and argument must form a verb/noun pair.

## Have no side effects

- Functions promise to do One Thing, but side effects mean doing hidden things!
- Side effects introduce temporal coupling.
- If a function must change the state of something, have it change the state of
    its owning object.

## Command Query Separation

- Function should either do something, or answer something but not both!

## Exceptions over error codes

## Extract try/catch blocks

## Error handling is One Thing

## Don't repeat yourself

- Duplication may be the root of all evil in software.

## Dijkstra's Structured Programming

- Every function, and every block within a function, should have one entry and
    one exit.

# Comments

- Kernighan:
> Don't comment bad code --- rewrite it.
- The proper use of comments is to compensate for our failure to express ourself
    in code.
- Comments are always failures.
- The older a comment is, and the farther away it is from the code it describes,
    the more likely it is to be just plain wrong.
- Inaccurate comments are worse than no comments at all.
- Truth can only be found in one place: the code.

## Comments do not make up for bad code

## Explain yourself in code

## Good Comments

- The only truly good comment is the one that doesn't need to be written.

### Legal comments

### Informative comments

### Explanation of Intent

### Misc.

- TODOs are not an explanation to leave bad code.
- Warning comments.
- Amplification comments.
- Don't use a comment when a variable or a function can be used.
- Avoid position markers.

## Bad comments

- Mumbling.
- Misleading comments.
- Mandated comments.
- Journal comments.
- Noise comments.
- Scary noise.
- Closing brace comments.
- Commented out source code.
- Nonlocal information.
- Too much information.
- Inobvious connection.
- Function headers.

# Formatting

- Newspaper metaphor
- Vertical openness: use empty lines judiciously
- Concepts that are closely related should be kept vertically close to each
    other.
- Variables should be declared as close to their usage as possible.
- Instance variables should be declared at the top of the class.
- Dependent functions should be vertically close.
- Conceptual affinity is inversely proportional to vertical distance.
- Vertical Ordering: Functions should go from high level to low level.
- Horizontal limit: reader should never have to scroll to the right (120 at most).
- Horizontal openness and density.
- If there are long lists that need to be aligned, the problem is the length of
    the lists, not the lack of alignment.
- Indentation.
- Dummy scopes: avoid them, worst case put the semicolon in its own line.

# Objects and Data Structures

- Hiding implementation is not about putting a bunch of getters and setters, but
    about exposing abstract interfaces that allow its users to manipulate the
    essence of data.
- We don't what to expose the details of our data, rather we want to express our
    data in abstract terms.
- Dichotomy between objects and data structures: procedural code (data
    structures based code) makes it easy to add new functions without changing
    the existing data structure. OO code makes it easy to add new classes
    without changing existing functions.
- Conversely, procedural code makes it hard to add new data structures (because
    all the functions must change), OO code makes it hard to add new functions
    because all the classes must change.
- Objects expose behaviour and hide data.
- TLDR: do you need protection from functions, or from types?
- Law of Demeter: talk to friends, not to strangers. A method `f` of a class `C`
    should only call methods of:
    1. `C`.
    2. An object created by `f`.
    3. An object passed as an argument to `f`.
    4. An object held in an instance variable of `C`.
In particular, `f` should ***not*** invoke methods on objects returned by any of
the above allowed functions.

## Train wreck
## Hybrids
## Data Transfer Objects (DTOs)
- Classes with only public variables and no member methods.
## Active Records

# Error Handling

- Error handling is important, but if it obscures logic, it's wrong.

## Use exceptions rather than return codes

## Write `try-catch-finally` first

## Provide context with exceptions

- Informative error messages.

## Define exception classes by caller's needs

## Define the normal flow

## Don't return null

## Don't pass null
