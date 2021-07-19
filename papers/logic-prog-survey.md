---
title: 'Notes on the paper "Complexity and Expressive Power of Logic Programming"'
author: Bharathi Ramana Joshi
---

- Term:
    + Logic variables (`X`, `Y`, `Z`, ...).
    + Propositional constants (`a`, `b`, `c`, ...).
    + Functions, which take terms corresponding to its arity; i.e.
      `f(t1,...,tn)` is a term if `f` is an n-ary function symbol and `ti` are
      all terms.
- A term is ground if it has no logic variables.
- Herbrand universe of $L$, denoted by $U_L$ is the set of all ground terms that
    can be formed with functions and constants in $L$.
- Atom: formula `p(t1,...,tn)` where `p` is a predicate symbol of arity `n` and
  all of `ti` are terms
- Herbrand base of $L$, denoted by $B_L$, is the set of all ground atoms that
    can be formed with predicates from $L$ and terms from $U_L$.
- Horn clause: `Head <- Body`, which means `Body => Head`.
- Horn clause: `A0 <- A1,...,Am`, each `Ai` is an atom.
- A Horn clause of the form `A0 <-` is a fact, and a ground fact if `A0` is
    ground.
