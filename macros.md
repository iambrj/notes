---
author: Bharathi Ramana Joshi
title: Notes on using Racket macros
---

- Just as `lambda`s are functions from values to values, macros are functions
    from syntax to syntax.
- Since `let` can be macro-ized as follows
  ```racket
  (let ([x v]) body) -> ((lambda (x) body) v)
  ```
  we can use `syntax-rules` to define the above macro
  ```racket
  (define-syntax my-let-1       
      (syntax-rules ()
  ; whenever a s-expression with my-let-1 after an open paerns is encountered
          [(my-let-1 (var val) body) ; var, val, body are syntactic variables
           ((lambda (var) body) val)]))
  ```
  We can generalize this to any number of bindings as follows
  ```racket
  (define-syntax my-let-2
      (syntax-rules ()
          [(my-let-1 ((var val) ...) body)
           ; unzipping happens for free!
           ((lambda (var ...) body) val ...)]))
  ```
- The functional nature of macros is laid bare when `syntax-case` is used:
    ```racket
    (define-syntax (my-let-3 x)     
        (syntax-case x ()             
            [(my-let-3 (var val) body)  
            ; Can perform any arbitrary computation, just needs to evaluate to a
            ; syntactic object
             #'((lambda (var) body) val)]))

    ```
 - Guards can be used to enforce syntax:
    ```racket
    (define-syntax (my-let-4 x)     
        (syntax-case x ()             
            [(my-let-4 (var val) body)  
             ; var should be an identifier, throw syntax error otherwise
             (identifier? var)
             #'((lambda (var) body) val)]))
    ```
- Macros can do with their arguments:
    + Use its value
    + Turn it into a datum using quote
    + Extend the environment the argument is evaluated in
    + It can decide whether or not to evaluate it
- Simple macros are defined using `define-syntax-rule`:
    ```racket
    (define-syntax-rule (assert x)
        ; 'x grabs the syntactical piece without evaluating it
        (unless x (error "Assertion violated: " 'x)))
    ```
- Macros are evaluated "outer-most" first, unlike procedures which are evaluated
    "inner-most" first.
- Hygiene: distinguishing identifiers introduced by a macro and identifiers in
    macro arguments.
- Transforming macros into procedures is often useful since this lets us test
    procedures, e.g.:
    ```racket
    (define-syntax-rule (andlet1 var e1 e2)       
        (let ([var e1])                             
            (if var e2 #f)))                          

    (define-syntax-rule (andlet1-th var e1 e2)    
        (andlet-fun (lambda () e1) (lambda (x) e2)))
    ```
    In such transformations, expressions become thunks and extending scope
    becomes procedures with arguments (beautiful connection to let being a macro
    over lambda!).

# Lessons to keep in mind
1. A macro template must contain at most one reference to an expression
   argument.
2. Minimize the code introduced by a macro, use helper procedures to implement
   complex dynamic behaviour.
3. Not every macro that can be written needs to be written.
