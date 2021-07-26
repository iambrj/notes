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
