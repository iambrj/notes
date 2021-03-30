---
author : Bharathi Ramana Joshi
title : "Notes on : Software Design for Flexibility"
---

# Flexibility in Nature and in Design

- Additive programming : one should not modify an existing program, but be able
    to add to it to implement new functionality
- To suppress noise, make the outputs of one phase small and well defined than
    the acceptable inputs of the next phase. "Be conservative in what you do, be
    liberal in what you accept from others."
- Long term costs are additive rather than multiplicative
- Generic dispatch : have distinct response procedures for disjoint sets of
    inputs and dispatch the appropriate procedure
- Degeneracy : have multiple ways to compute something, which can be combined or
    modulated as needed
- Parti : an abstract plan for the computations to be performed
- Postel's law : be conservative in what you do, be liberal in what you accept
    from others

# Domain-Specific Languages

- Use general parts by configuring them dynamically to environment changes
- Combinators : set of primitive parts + set of means of combining parts such
    that combined parts have same interface as primitive parts
- Advantages of organizing a system around combinators
    + Parts can be arbitrarily mixed and matched, all combinations yield valid
        programs
    + Context in which part appears does not change the part's behaviour
    + Extensible since new parts don't affect existing parts
- 
