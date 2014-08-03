module Kleisli where

import Prelude hiding (id, (>>), pred, ($), sequence)
import Nat

class Category cat where
    id :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
    idK :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)

infixr 0 $, *$, +$

($) :: (a -> b) -> a -> b
f $ a = (const a >> f) ()

(*$) :: Kleisli m => (a -> m b) -> m a -> m b
f *$ a = (const a *> f) ()

(+$) :: Kleisli m => (a -> b) -> m a -> m b
f +$ a = (const a +> f) ()

lift1 :: Kleisli m => (a -> b) -> m a -> m b
lift1 f a = (const a +> f) ()

($$) :: Kleisli m => m (a -> b) -> m a -> m b
mf $$ ma = (+$ ma) *$ mf

lift2 :: Kleisli m => (a -> b -> c) -> m a -> m b -> m c
{-
lift2 f a b = (f +$ a) $$ b
lift2 f a b = (+$ b) *$ (f +$ a)
-}
lift2 f a b = lift1 f a $$ b

lift3 :: Kleisli m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f a b c = (f +$ a) $$ b $$ c

instance Category (->) where
    id = \x -> x
    f >> g = \x -> g (f x)

instance Kleisli Maybe where
    idK = Just
    f *> g = \a -> case f a of
                     Nothing -> Nothing
                     Just b -> g b

instance Kleisli [] where
    idK = \a -> [a]
    f *> g = f >> map g >> concat

pred :: Nat -> Maybe Nat
pred Zero = Nothing
pred (Succ a) = Just a

beside' :: Nat -> Maybe (Nat, Nat)
beside' = pred +> \a -> (a, a + 2)

sequence :: Kleisli m => [m a] -> m [a]
sequence = foldr (lift2 (:)) (idK [])

mapK :: Kleisli m => (a -> m b) -> [a] -> m [b]
mapK f = sequence . map f
