---
author: Bharathi Ramana Joshi
title: 'Notes : Stanford intrologic textbook'
---

- Simple sentences/proposition constants/logical constants : statements (no
  logic variables allowed). E.g. "It rained on Monday".
- Compound sentences : simple sentences + logical operations {negation, conj,
    disj, implies, bicond}
- To resolve ambiguity, look at both sides of operand and associate operand with
    the operator with higher precedence. E.g. `~p and q` == `(~p) and q`, since `~`
    preceeds `and`. We abstract over simple sentences and just use symbols to
    represent them (like `p` and `q` above).
- Assignment : mapping each simple sentence to a boolean
# Properties
- Sentence is
    + Valid : true for all assignments
    + Unsatisfiable : false for all assignments
    + Contingent : true for some, false for other assignments
- Evaluation : given assignments for occurring simple sentences, find value of
  sentence
- Satisfaction : given sentence, find assignments for occurring simple sentences
    to make the sentence true
# Relationships
- Logical equivalence : X and Y are logically equivalent if they both have the
    same truth values for the same assignments. Logical equivalence for
    propositional logic supports substitutability
- Logical entailment : {X} logically entails Y if all the assignments that make every
  sentence in {X} true, also make Y true. E.g. `{p, q}` logically entails `p or
  q`.
- Weird consequence of above definition : an unsatisfiable (set of) sentence(s)
  logically entials *everything*. The weirdness comes from the fact that
  quantification over empty set can satisfy arbitrary propositions.
- Logical consistency : X is logically consistent with {Y} if there is some
    assignment that makes X and every sentence in {Y} true. E.g. `p or ~q` is
    logically consistent with `{q, p and q}` via the assignment `{p -> #t, q
    ->#t}`.
# Connections between the two
- Equivalence theorem : X and Y are logically equivalent iff the sentence `X <=>
    Y` is valid
- Deduction theorem : {X} logically entails Y iff `(and X) => Y` is valid
- Unsatisfiability theorem : {X} logically entails Y iff {X} U (~Y) is
    unsatisfiable
- Consistency theorem : X is logically consistent iff `(and Y) and X` is
    satisfiable
# Sound vs complete
- Sound : iff every provable conclusion is logically entailed
- Complete : iff every logical conclusion is provable
- Hilbert system/style proofs : every statement is either derived or an axiom
- Fitch system/style proofs : subproofs, conditional (sub)proofs
# Propositional Resolution
- Rule of inference for propositional logic
