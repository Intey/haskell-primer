
data Department a = Department String a

instance Functor Department where
  fmap f (Department n a) = Department n (f a)

instance Applicative Department where
  pure = Department "default"
  (<*>) (Department _ f) s@(Department na a) = Department na (f a)

instance Monad Department where
  return = pure
  (Department na a) >>= f = f a


main :: IO ()
main = putStrLn "Not implemented"
