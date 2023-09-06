---
fontfamily: mathpazo
title: "Notes on Oleg's Reconciling Abstraction with High Performance"
author: 'iambrj'
date: '06-09-2023'
header-includes:
  - \hypersetup{colorlinks=true, urlcolor=blue}
---
- The promise: we can have high performance code AND still write it using
  high-level abstractions; i.e. abstraction without guilt.
- MetaOCaml is purely generative: it can generate code, but not break apart and
  examine it.

# First Steps
- Wrapping OCaml code in brackets, `.< code >.`, produces a *code value*. It is
  just like any other value -- it can be bound to variables, passed as
  arguments, and returned from functions.
- Wrapping OCaml code in brackets quotes the code, annotating it as computed
  later.
- As a first approximation, a code value can be thought of as an OCaml AST that
  other OCaml code can work with.
- An expression `e` of type `t` when quoted using brackets produces a code value
  of type `t code`.
- The other annotation MetaOCaml introduces is *escape* `.~`, which can only be
  used inside brackets. Escape an expression `.~e` unquotes `e`.
- `.~e` says that `e` is computed now but produces the code to run later ("now"
  meaning the point where the escape is used and "later" meaning the surrounding
  brackets).
- We can abstract over the escape expression:
```
let genTimes10 x = .<.~x * 10>.;;
=> val genTimes10 : int code -> int code = <fun>
```
i.e. `genTimes10` is a function that produces a code value, i.e. `genTimes10` is
a *code generator*. `genTimes10` takes a code value as an argument too.
- The code inside brackets is also type checked. The type-checker rejects code
  that *may* produce ill-typed code. A well-typed generator generates only
  well-typed code.
- Computation is divided into (any number of) *stages*. A *now stage*, for
  values which are known and can be computed and a *later stage*, for values
  which are not known and computation involving them is deferred to the future.
- CSP: Cross-stage Persistence. Functions that persist across changes, i.e.
  functions that are defined now and can always be used in the future.
- `Runcode.run: x code -> x` can be used to run any generated code. It invokes
  the compiler, performs linking, runs the resulting program, and returns the
  value of the program. It must be emphasized that *run invokes the compiler at
  runtime*.
- Two key takeaways:
    1. A code value can represent open code.
    2. MetaOCaml can be used for both runtime specialization (i.e. specializing
       a function for frequently used  inputs for example) and offline
       generation of specialized library code.
TODO: try to re-read the recap section and meditate on it to understand better
what is being said.

# Misc
```
eval $(opam env --switch=4.14.1+BER)
```
