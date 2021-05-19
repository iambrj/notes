---
author: Bharathi Ramana Joshi
title: Notes on Essentials of Compilation
---

# Registers and Calling Conventions

- Spilling : pushing register values onto stack
- Caller-saved registers : registers that caller saves before call to function,
    allowing the function to reuse these registers
- Callee-saved registers : registers that callee restores before returning,
    allowing the caller to reuse these registers
- For variables not in use during a function call, try in order
    1. Put it in caller saved register
    2. Put it in callee saved register
    3. Spill it onto the stack
- Graph coloring for register allocation
    1. Vertices are variables
    2. Edges indicate when to variables interfere (i.e. are needed at the same
       time)
