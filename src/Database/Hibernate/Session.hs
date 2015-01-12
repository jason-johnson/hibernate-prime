module Database.Hibernate.Session
(
   session
  ,Session
  ,SessionT
  ,genericSessionDriver
  ,runSessionT
  ,save
)
where

import Control.Applicative
import Control.Monad (ap, mzero, mplus, MonadPlus)
import Control.Arrow (first)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Fix (mfix, MonadFix)

type Session = SessionT IO

-- SessionT transformer

newtype SessionT m a = SessionT { runSessionT :: SessionDriver -> m (a, SessionDriver) }

session :: Monad m => (SessionDriver -> (a, SessionDriver)) -> SessionT m a
session f = SessionT (return . f)

instance (Functor f) => Functor (SessionT f) where
    fmap f m = SessionT $ \sd -> first f <$> runSessionT m sd

instance (Functor m, Monad m) => Applicative (SessionT m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (SessionT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (SessionT m) where
    return a = session $ \sd -> (a, sd)
    m >>= k  = SessionT $ \sd -> do
        (a, sd') <- runSessionT m sd
        runSessionT (k a) sd'
    fail str = SessionT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (SessionT m) where
    mzero       = SessionT $ const mzero
    m `mplus` n = SessionT $ \sd -> runSessionT m sd `mplus` runSessionT n sd

instance (MonadFix m) => MonadFix (SessionT m) where
    mfix f = SessionT $ \sd -> mfix $ \(a, _) -> runSessionT (f a) sd

instance MonadTrans (SessionT) where
    lift m = SessionT $ \sd -> do
        a <- m
        return (a, sd)

instance (MonadIO m) => MonadIO (SessionT m) where
    liftIO = lift . liftIO

-- SessionDriver

newtype SessionDriver = SessionDriver {
        driverSave :: IO ()
 }
 
instance Show (SessionDriver) where       -- TODO: Remove this later
  show = const "SESSION-DRIVER"
 
genericSessionDriver :: SessionDriver
genericSessionDriver = SessionDriver $ putStrLn "Saved!"

-- Session commands

save :: MonadIO m => a -> SessionT m a
save x = SessionT $ \sd -> do
  liftIO $ driverSave sd
  return (x, sd)