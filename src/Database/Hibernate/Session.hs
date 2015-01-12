{-# LANGUAGE RankNTypes #-}
module Database.Hibernate.Session
(
   session
  ,Session
  ,SessionT
  ,genericSessionDriver
  ,runSession
  ,save
  ,HibernateFieldType(..)
  ,HibernateField(..)
  ,HibernateTable(..)
  ,Hibernatable(..)
)
where

import Control.Applicative
import Control.Monad (ap, mzero, mplus, MonadPlus, liftM)
import Control.Arrow (first)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Fix (mfix, MonadFix)

type Session = SessionT IO

-- SessionT transformer

newtype SessionT m a = SessionT { runSessionT :: SessionDriver -> m (a, SessionDriver) }

runSession :: Monad m => SessionT m a -> SessionDriver -> m a
runSession m sd = liftM fst $ runSessionT m sd

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

-- hibernatable

data HibernateFieldType =
    HibernateIntField Int
  | HibernateCharField Char
  | HibernateStringField String     -- TODO: This stuff should probably go in Types.hs, but maybe not, have to think about it
  deriving (Show)

data HibernateField = HibernateField { hfType :: HibernateFieldType, hfName :: String }
  deriving (Show)

data HibernateTable = HibernateTable { htName :: String, htFields :: [HibernateField] }
  deriving (Show)

class Hibernatable a where
  derive :: a -> HibernateTable

-- SessionDriver

newtype SessionDriver = SessionDriver {
        driverSave :: Hibernatable a => a -> IO a
 }
 
instance Show (SessionDriver) where       -- TODO: Remove this later
  show = const "SESSION-DRIVER"
 
genericSessionDriver :: SessionDriver
genericSessionDriver = SessionDriver $ \x -> do
  putStrLn "Saving..."
  print . derive $ x
  putStrLn "Done!"
  return x

-- Session commands

save :: (MonadIO m, Hibernatable a) => a -> SessionT m a
save x = SessionT $ \sd -> do
  x' <- liftIO $ driverSave sd x
  return (x', sd)