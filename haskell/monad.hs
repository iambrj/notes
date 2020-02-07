data TrivialMonad a = TrivialWrapper a deriving Show

{- Wrap a value -}
return :: a -> TrivialMonad a
return x = TrivialWrapper x

{- Apply a function to a wrapped value -}
fmap :: (a -> b) -> TrivialMonad a -> TrivialMonad b
fmap f (TrivialWrapper x) = TrivialWrapper (f x)

{- Apply a function after unwrapping a wrapped value -}
bind :: TrivialMonad a -> (a -> TrivialMonad b) -> TrivialMonad b
bind (TrivialWrapper x) f = f x

{- Exercise 1 -}
{- g x (TrivialWrapper y) = TrivialWrapper (x + y) -}
g :: Int -> TrivialMonad Int -> TrivialMonad Int
{-g x y = Main.fmap (+ x) y-}
g x y = bind y (Main.return . (+ x))


{- Exercise 2 -}
{- h (TrivialMonad x) (TrivialMonad y) = TrivialMonad (x + y)-}
{- h :: TrivialMonad Int -> TrivialMonad Int -> TrivialMonad Int -}
h x y = bind x (\x -> g x y)
