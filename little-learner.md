# Chapter 1
- Parameter vs argument: in y = w * x + b, (w, b) are parameters and y is said
  to be parameterized over theta = (w, b). x is said be an argument.
- A parameterized function is one that takes its argument before it takes its
  parameters

# Chapter 2
- Rank of a tensor: tells us how deeply nested its elements are. Number of left
  square brackets before the first element.
- Shape of a tensor: 
- Tensor invariants
  + Each element has the same shape
  + The rank of a tensor is equal to the length of its shape

# Interlude 1
- [Pointwise] Extension: getting a scalar function to work on tensors of
  arbitrary rank
- sum1: folds a rank 1 tensor using +
- sum: descends into a rank m tensor until it hits a rank 1 tensor which it then
  sum1's to produce a rank (m - 1) tensor

# Chapter 3
- Expectant function: function that expects a dataset
- Objective function: function that expects a theta
- Rate of change of loss: change in loss / change in theta
- Learning rate (alpha) : how fastly are you updating theta
- Rate of change of loss is different for different theta

# Chapter 4
- A gradient is a way of understanding the rate of change of a parameterized
  function with respect to all its parameters
- The `del` operator produces the gradient loss for each parameter given an
  objective function with resepct to a theta (using automatic differentiation)
- l2-loss:
```
(define l2-loss
 (lambda (target)
  (lambda (xs ys)
   (lambda (theta)
    (let ([pred-ys ((target xs) theta)])
     (sum (sqr (- ys pred-ys))))))))
```
- Revision function:
```scheme
(define revise
 (lambda (f revs acc)
  (cond
   [(zero? revs) acc]
   [else (revise f (sub1 revs) (f acc))])))
```
- Gradient descent:
```scheme
(define gradient-descent
 (lambda (obj theta)
  (let ([f (lambda (Theta)
            (map (lambda (p g)
                  (- p (* alpha g))
                  Theta
                  (del obj Theta)))])
        (revise f revs theta)))))
```

