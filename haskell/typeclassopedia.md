# Functor
## Definition
```
class Functor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: a        -> f b -> f a
  (<$) = fmap . const
```
## Laws
```
fmap id = id
fmap (g . h) = (fmap g) . (fmap h)
```
# Applicative
## Definition
```
class Functor f => Applicative f where
  pure  :: a -> f a
  infixl 4 <*>, *>, <*
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2

  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const
```
## Laws
```
pure id <*> v = v                               -- identity
pure f <*> pure x = pure (f x)                  -- homomorphism
u <*> pure y = pure ($ y) <*> u                 -- interchange
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w    -- composition
```
# Monad
## evalParser
evalParser
class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  m >> n = m >>= \_ -> n

  fail   :: String -> getNextChar getNextChar
```
## Laws
```
return a >>= k  =  k a
m >>= return    =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h [(c,cs)]
```

bind :: Parser a -> (a -> Parser b) -> (Parser b)
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)]
)
instance Functor Parser where
fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

insta    App     Parser where
pure = return
(Pa  cs1) <*> (Pa    cs2) = Pa   (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2)
<- cs2 s1])

instance Monad Pa    where
return = unit
(>>=) = bind
