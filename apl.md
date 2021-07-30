```
author: Bharathi Ramana Joshi
title: Notes on APL
```

- Depth (`≡`): layers of nesting, or length of largest layer (prepended with -
    if uneven array).
- Shape (`⍴`): exact lengths of each dimension.
- Rank: number of dimensions, i.e. shape of shape!
- Important identity: `X ≡ ⍴ (X ⍴ A)`, i.e. the shape of an array is always the
    same after reshaping.
- Empty shape: ` ⍬`(numbers)/`''`(character).
- Light bulb `⍝` for comments (since the illuminate us).
- Scalar functions: functions that penetrate the arrays, all the way down to
    simple scalars before application.
- Scalar functions applied dyadically throw errors if argument shapes don't
    match up.
- Scalar extension
- Mixed function: works with larger structures of its arguments.
- Dynamic functions (`dfns`): expressions enclosed in `{}`, left argument `⍺` and
    right argument `⍵`.
- Use "self" `∇` for recursive definitions, can rename functions without having
  to change the body.
- Tacit function: APL statement that does not have data on the right
- Two-train/atop:
    ```
    (f g) X ←→ f (g X)
    ```
- Dyadic trains:
    ```
    X (f g) Y  ←→  f (X g Y)
    ```
    and,
    ```
    X (f g h) Y  ←→  (X f Y) g (X h Y)
    ```
- Tree-trains/forks:
    ```
    (f g h) X ←→ (f X) g (h X)
    ```
    and,
    ```
    (A g h) X ←→ A g (h X)
    ```
