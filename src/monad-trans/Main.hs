import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad ( ap, liftM, MonadPlus(..), guard )
import Control.Monad.Trans ( MonadTrans(..), MonadIO (liftIO) )
import Data.Char ( isNumber, isAlpha, isPunctuation )

{-| 
  Monad Trasformer that wraps monad value in Maybe 
-}
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do 
                maybe_value <- runMaybeT x
                case maybe_value of
                  Nothing -> return Nothing
                  Just a -> runMaybeT $ f a


instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap


instance Monad m => Functor (MaybeT m) where
    fmap = liftM


instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just


{-
    Maybe has instance for this two typeclasses. 
    So, it's good to implement them, but not necessary...
    while you not want to use "Control.Monad.guard"
-}

instance Monad m => Alternative (MaybeT m) where
    empty   = MaybeT $ return Nothing
    x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> runMaybeT y
                               Just _     -> return maybe_value
-- instance Monad m => MonadPlus (MaybeT m) where 
--     mzero = empty
--     mplus = (<|>)


isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

{-
========================================= direct approach ==========================================
-}
getPassphrase' :: IO (Maybe String)
getPassphrase' = do 
    s <- getLine
    if isValid s then return $ Just s
                else return Nothing


askPassphrase' :: IO ()
askPassphrase' = do 
    putStrLn "Insert your new passphrase:"
    maybe_value <- getPassphrase'
    case maybe_value of
        Just value -> do putStrLn "Storing in database..."  -- do stuff
        Nothing -> putStrLn "Passphrase invalid."

{- 
======================================== with transformers =========================================
-}
getPassphrase :: MaybeT IO String
getPassphrase = do 
    s <- lift getLine
    guard (isValid s) -- функция guard определена для класса Alternative.
    return s
    
{-| 
  Ask password from user and do nothing
-}
askPassphrase :: MaybeT IO ()
askPassphrase = do 
    lift $ putStrLn "Insert your new passphrase:"
    value <- getPassphrase
    lift $ putStrLn "some actions..."

main :: IO ()
main = do
  v <- runMaybeT askPassphrase
  pure ()
