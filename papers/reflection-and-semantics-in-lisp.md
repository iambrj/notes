---
title: 'Notes on the paper "Reflection and Semantics in Lisp"'
author: Bharathi Ramana Joshi
---

- Procedural reflection: architecture to support self-directed reasoning in a
    serial programming language
- Examples of reflection: macros, debuggers, run-time compilers, dynamic
    optimizers, types, comments
- Reflection requirements
    1. More to reflection than self-reference --- need an embedded theory to
       make sense of what is being to referred to
    2. Needs to be a connection between the embedded theory and the embedding
       system (causal connection --- with causality going both ways)
    3. Vantage point, far enough to have itself in focus and yet close enough to
       see the important details. E.g. debugger needs to ensure it's own stack
       frames, variables, etc don't affect the program's stack frames, variables
       etc.
- Structural field: semantic representations for programs
- Processor: interpreter
- Evaluation = Simplification + Designation
- Metastructural: structure designating another structure
- Computational model = Structural field + Processor
- "S" signifies "D"/ "D" is the signification of "S" (S is the syntactic object
    and D is the semantic object mapped by the interpretation function)
- Declarative import (significance outside effects) vs procedural consequence
  (effects it had on the system). E.g. NIL stands for falsehood (declarative)
  which is not captured by the procedural consequence (appended to list gives list
  itself etc).
- Three objects:
    1. Objects and events of the world in which the computational process is
       embedded (numbers, sets, cars, tables). Designations.
    2. Internal elements of the computer (data structures, execution sequences,
       program representations). Structural elements. Structures.
    3. Communication elements (strings of input, output images on terminal).
       Notations.
- Example:
    1. In Lisp, notations are the strings derivable from the grammar, structures
       are the cons cells on the heap and designations are the results of
       `eval`.
- Relationships between these objects
    1. O: notations and structures (internalisation, O^-1 being externalisation)
    2. Psi: processes and behaviours the structural field elements engender
       (psychology, relationships between symbols in the head).
    3. Phi: structures and designations (philosophy, relationships between
       symbols in the head and external world).
- Examples:
    1. Phi maps the mental token for T.S. Eliot to the poet himself but (compose
    Phi O) maps the public name "T.S. Eliot" to the poet.
    2. Phi maps the numeral 3 to the abstract concept of the number 3.
    3. The function computed by Lisp's `eval` is Psi, 
- Phi(structure) = declarative import, designation of a structure
- Psi(structure) = procedural consequence
- Constraints lisp obeys
    1. forall structures s, if Phi(s) is a structure
                            then Psi(s) = Phi(s)
                            else Phi(Psi(s)) = Phi(s)
    2. forall structures s, Phi(Psi(s)) = Phi(s) AND normal-form(Psi(s))
- A structure is in normal form if
    1. It is context independent (same declarative and procedural import
       independent of use context).
    2. It is side-effect free (processing of structure will have no effect on
       structural field, processor and world).
    3. It is stable (it must normalize to itself in all contexts, i.e. Psi is
       idempotent).
- `(up struct)`: designates the normal-form designator of the designation of
    `struct`; designates what `struct` normalizes to
- `(down struct)`: designates the designation of the designation of `struct`
- `up`, `down` and psi, phi
    1. `phi(normalise) = psi`
    2. `phi(down . normalise) = phi` (when `down` is defined, as it is a partial function)
