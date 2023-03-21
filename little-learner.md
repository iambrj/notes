# Chapter 1
- Parameter vs argument: in y = w * x + b, (w, b) are parameters and y is said
  to be parameterized over theta = (w, b). x is said be an argument.
- A parameterized function is one that takes its argument before it takes its
  parameters

# Chapter 2
- Rank of a tensor: tells us how deeply nested its elements are. Number of left
  square brackets before the first element.
- Shape of a tensor: cross product specifying size of each dimension
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
- Target function: function that is being fitted
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

# Interlude 2
- 

# Chapter 5
- Devanagari "besan laddoo" what a lovely surprise!
- The expectant function can be generalized to any nonlinear function

# Interlude 3
- 

# Chapter 6
- Since datasets tend to be huge, running gradient descent on entire the dataset
  tends to be too slow. Instead, gradient descent is run in batches, where each
  batch is a randomly sampled subset of the dataset.
```scheme
(define samples
 (lambda (n s)
  (sampled n s (list))))

(define sampled
 (lambda (n i a)
  (cond
   [(zero? i) a]
   [else (sampled n (sub1 i) (cons (random n) a))])))
```
- We can use sampling to turn an objective function into a sampling objective
  function
```scheme
(define sampling-obj
 (lambda (expectant xs ys)
  (let ([n (tsize xs)])
   (lambda (theta)
    (let ([b (samples n batch-size)])
     ((expectant (trefs xs b) (trefs ys b)) theta))))))
```
- For example, using the above definition, gradient descent can now be invoked
  as follows
```scheme
(with-hypers
 ([alpha 0.001]
  [batch-size 4])
 (gradient-descent (sampling-obj (l2-loss target) xs ys) theta))
```

# Chapter 7
- Ultimate gradient descent:
```scheme
(define gradient-descent
 (lambda (inflate deflate update)
  (lambda (obj theta)
   (let ([f (lambda (Theta)
             (map update Theta (del obj (map deflate Theta))))])
    (map deflate (revise f revs (map inflate theta)))))))
```

# Chapter 8
- Momentum gradient descent: incorporate previous update into current update
- Analogy for intuition: baton race, current update should carry momentum from
  previous updates
```scheme
(define momentum-i
 (lambda (p)
  (list p (zeros p))))

(define momentum-d
 (lambda (P)
  (listref P 0)))

(define momentum-u
 (lambda (P g)
  (let ([v (- (* mu (listref P 1)) (* alpha g))])
   (list (+ (listref P 0) v) v))))

(define momentum-gradient-descent
 (gradient-descent momentum-i momentum-d momentum-u))
```
- Intuition behind why it works?

# Interlude 4
- The smooth operator (is shape polymorphic)
```scheme
(define smooth
 (lambda (decay-rate average g)
  (+ (* decay-rate average) (* (- 1 decay-rate) g))))
```

# Chapter 9
- Root Mean Squared Prop (rmsProp) gradient descent
- Modifier intuition: rate at which the fraction of gradient used at each
  revision decreases should be less than the rate at which the gradient
  decreases
- 
```scheme
(define rms-u
 (lambda (P g)
  (let ([r (smooth beta (listref P 1) (sqr g))])
   (let ([D (sqrt r)])
    (let ([alpha (/ alpha (+ D epsilon))])
     (list (- (lisref P 0) (* alpha g)) r))))))
```
