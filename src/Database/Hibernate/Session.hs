module Database.Hibernate.Session
(
   session
  ,Session
  ,SessionT
)
where

import Control.Applicative
import Control.Monad (ap)

type Session s = SessionT s IO

newtype SessionT s m a = SessionT { runSessionT :: s -> m (a,s) }

session :: Monad m => (s -> (a, s)) -> SessionT s m a
session f = SessionT (return . f)

instance (Functor m) => Functor (SessionT s m) where
    fmap f m = SessionT $ \ s ->
        fmap (\(a, s') -> (f a, s')) $ runSessionT m s

instance (Functor m, Monad m) => Applicative (SessionT s m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (SessionT s m) where
    return a = session $ \s -> (a, s)
    m >>= k  = SessionT $ \s -> do
        (a, s') <- runSessionT m s
        runSessionT (k a) s'
    fail str = SessionT $ \_ -> fail str

newtype SessionDriver a = SessionDriver {
        save :: a -> a
 }