module ParsComb (
    Parser (..), nap,
    headP, predP, getP, 
    prefixP, validateP,
    tracedGetP, tracedPredP
) where

import Control.Applicative ((<|>), (<*>), Alternative, empty)
import Data.Bifunctor (first, second)
import Control.Monad.Zip
import Data.List (uncons, stripPrefix)
import Data.Tuple (swap)

import Debug.Trace (trace)

data Parser m i o = Parser { runP :: [i] -> m ([i], o) }

instance Functor m => Functor (Parser m i) where
    fmap :: (o -> o') -> Parser m i o -> Parser m i o'
    fmap f p = Parser $ (fmap $ fmap f) . runP p

instance Monad m => Applicative (Parser m i) where
    pure :: o -> Parser m i o
    pure o = Parser $ pure . (, o)
    (<*>) :: Parser m i (o -> o') -> Parser m i o -> Parser m i o'
    p <*> q = Parser $ \i -> do
        (i', f) <- runP p i
        second f <$> runP q i'

instance (Monad m, Alternative m) => Alternative (Parser m i) where
    empty :: Parser m i o
    empty = Parser $ const empty
    (<|>) :: Parser m i o -> Parser m i o -> Parser m i o
    p <|> q = Parser $ \i -> runP p i <|> runP q i

instance Monad m => Monad (Parser m i) where
    (>>=) :: Parser m i o -> (o -> Parser m i u) -> Parser m i u
    p >>= f = Parser $ \i -> second f <$> runP p i >>= \(i', q) -> runP q i'

instance Monad m => MonadZip (Parser m i) where
    mzip :: Parser m i o -> Parser m i u -> Parser m i (o, u)
    mzip p q = (,) <$> p <*> q

nap :: (m ([i], o) -> n ([i], o)) -> Parser m i o -> Parser n i o
nap t p = Parser $ t . runP p

validateP :: Monad m => (o -> m o) -> Parser m i o -> Parser m i o
validateP v p = Parser $ \i -> do
    (rest, o) <- runP p i
    o' <- v o
    return (rest, o')

headP :: Parser Maybe i i
headP = Parser $ (fmap swap) . uncons

predP :: (i -> Bool) -> Parser Maybe i i
predP f = Parser $ \i -> do
    (o, rest) <- uncons i
    if f o then return (rest, o) else empty

getP :: Eq i => i -> Parser Maybe i i
getP i = predP (i ==)

tracedPredP :: Show i => String -> (i -> Bool) -> Parser Maybe i i
tracedPredP msg f = Parser $ \i -> do
    trace (msg ++ show (take 5 i)) $ Just ()
    (o, rest) <- uncons i
    if f o then return (rest, o) else empty

tracedGetP :: (Show i, Eq i) => String -> i -> Parser Maybe i i
tracedGetP msg i = tracedPredP msg (i ==)

prefixP :: Eq i => [i] -> Parser Maybe i [i]
prefixP s = Parser $ (fmap (, s)) . stripPrefix s