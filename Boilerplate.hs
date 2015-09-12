instance Functor M where
  fmap f xs = xs >>= return . f

instance Applicative M where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f
