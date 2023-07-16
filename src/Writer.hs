module Writer (Writer, (>>=), return, fmap, (<*>), pure, runWriter, tell) where

data Writer a = Writer a String

instance Monad Writer where
    return x = Writer x ""
    (Writer x s) >>= f = let (Writer y s') = f x in Writer y (s ++ s')

instance Functor Writer where
    fmap f (Writer x s) = Writer (f x) s

instance Applicative Writer where
    pure = return 
    (<*>) mf mx = do
        f <- mf
        f <$> mx

runWriter :: Writer a -> (a, String)
runWriter (Writer x s) = (x, s)

tell :: String -> Writer ()
tell s = Writer () s

