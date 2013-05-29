
-- Geklaut aus bifunctors-3.2.0.1


module EFA.Utility.Bifunctor where


class Bifunctor p where

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  {-# INLINE bimap #-}


  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  {-# INLINE first #-}


  second :: (b -> c) -> p a b -> p a c
  second = bimap id
  {-# INLINE second #-}

instance Bifunctor (,) where
  bimap f g ~(a, b) = (f a, g b)
  {-# INLINE bimap #-}


instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)
  {-# INLINE bimap #-}