---
title: 'Notes on the paper "Partial Computation of Programs"'
author: Bharathi Ramana Joshi
---

- Partial computation: specializing a general program based on its operating
  environment into a more efficient program.
- f_k (projecting program f at k): substitute value of k in program f and do all
  possible computations based upon value k.
- [E1, result of partial computation = result of total computation]
  f_k(u) = f(k, u), where f_k is the projection of f at k.
- [E2, projection machine/partial computer/partial evaluator] alpha(f, k) = f_k.
- Difference is that f_k is the actual result you end up with, but alpha(f, k)
    is the algorithm being applied.
- Substituting following in E1
    1. f = alpha
    2. k = f
    3. u = k
    i.e. alpha_f(k) = alpha(f, k)
    (Intuition: we want the programmer to only specify the program f, and what
    is known(k)/unknown(u) must be inferred).
    and using E2, we have
    [E3, basic equation of partial computation] alpha_f(k) = f_k.
- Applications of partial computation
    1. Theorem proving THEOREM-PROVER(AXIOMS, THEOREM): partially compute the
       theorem prover for the given set of axioms.
    2. Pattern matching PATTERN-MATCHER(PATTERN, TEXT): partially compute the
       pattern matcher for common patterns.
    3. Syntax analyzer SYNTAX-ANALYZER(GRAMMAR, TEXT): partially compute the
       syntax analyzer for grammar.
    4. Compiler generation COGEN()
- What is an interpreter? Interpreter(Program, Data)
- What is a compiler? Compiler(Program), i.e. translates a program from source
    language to target language.
- Furthermore, when C(P) is run on D the result is same as I(P, D), i.e.
    C(P)(D) = I(P, D)
- It is less efficient to repeatedly compute I(P, D) with P fixed and D varying.
    Thus, we specialize I to P i.e. I_P and end up with an object program for P.
- [E4, First equation of partial computation] I_P = C(P), i.e. I_P can be regarded
    as the object program of P.
- alpha(I, P) takes too long to be practical as alpha is usually complicated.
    Thus alpha, which takes I and P and produces I_P, cannot be regarded as a
    compiler for I. However alpha itself is a program with two data, thus we can
    specialize alpha for a given I to end up with:
  [E5, Second equation of partial computation] alpha_I(P) = I_P.
- Give me an interpreter, and with a "good" partial evaluator I can produce a
    compiler using alpha(alpha, I).
- In E3, substitute
    1. f = alpha
    2. k = I
  [E6, Third equation of partial computation] alpha_alpha(I) = alpha_I.
- Since alpha_I is a compiler, alpha_alpha(I) is a compiler-compiler as it
    generates alpha_I from an interpreter I.
- [E7, Fourth equation of partial computation] alpha_alpha(f)(k) = f_k.

# Questions
- What does it mean for a partial evaluator to be "good"? Is it computing
  alpha(alpha, I) reasonably fast?
- If the reason we don't consider alpha to be a compiler, given I and P, is
  because alpha "is a complicated program and computation of alpha(I, P) takes
  a long time" [page 266], how come we can compute alpha(alpha, I)? Is it a
  matter of how alpha is implemented, i.e. alpha is slow for arbitrary inputs
  I and P, but fast enough for inputs alpha and I?
