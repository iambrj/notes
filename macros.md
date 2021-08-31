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
