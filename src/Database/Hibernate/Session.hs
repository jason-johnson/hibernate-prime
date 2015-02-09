{-# LANGUAGE RankNTypes #-}
module Database.Hibernate.Session
(
   session
  ,Session
  ,SessionT
  ,genericSessionDriver
  ,runSession
  ,save
  ,update
  ,set
  ,modify
)
where

import Database.Hibernate.Driver (Driver(..), genericSessionDriver)
import Database.Hibernate.Driver.Command
import Database.Hibernate.Meta
import Control.Applicative
import Control.Monad (ap, mzero, mplus, MonadPlus, liftM)
import Control.Arrow (first)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Fix (mfix, MonadFix)

-- SessionT transformer

type Session = SessionT IO

newtype SessionT m a = SessionT { runSessionT :: Driver -> m (a, Driver) }

runSession :: Monad m => SessionT m a -> Driver -> m a
runSession m sd = liftM fst $ runSessionT m sd

session :: Monad m => (Driver -> (a, Driver)) -> SessionT m a
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

-- Session commands

save :: (MonadIO m, TableMetaData a) => a -> SessionT m a
save x = SessionT $ \sd -> do
  _ <- liftIO . driverSave sd $ buildCommands                         -- TODO: this will return a response that has the key in it, which must be put in x (which will require a type family to do probably)
  return (x, sd)
  where
    buildCommands = SaveEntry (TableInfo (tableName x) "") $ mapColumns x tosd
    tosd n = StoreColumnData (FieldInfo n)
 
update :: (MonadIO m, TableMetaData a) => a -> ((a, UpdateEntry) -> (a, UpdateEntry)) -> SessionT m a
update x f = SessionT $ \sd -> do
  _ <- liftIO . driverUpdate sd $ ut                                  -- TODO: this will return a response that has the key in it, which must be put in x' (which will require a type family to do probably)
  return (x', sd)
  where
    (x', ut) = f (x, UpdateEntry ti [])
    ti = TableInfo (tableName x) (schemaName x)

setLens :: ColumnMetaData c => c -> ColType c -> Table c -> Table c
setLens c v = head . lens c ((: []) . const v)                      -- NOTE: We use list as Identity here so we don't have to pull in the package Identity is in, but the idea is the same

getLens :: ColumnMetaData c => c -> Table c -> ColType c
getLens c = getConst . lens c Const

set :: ColumnMetaData c => c -> ColType c -> (Table c, UpdateEntry) -> (Table c, UpdateEntry)
set c v (x, uc) = (setLens c v x, tl' uc)
  where                          
    tl' (UpdateEntry ti ccs) = UpdateEntry ti (cc : ccs)
    cc = StoreColumnData (FieldInfo $ columnName c) $ toFieldData c v

modify :: ColumnMetaData c => c -> (ColType c -> ColType c) -> (Table c, UpdateEntry) -> (Table c, UpdateEntry)
modify c f (x, uc) = (x', tl' uc)
  where
    l' = head . lens c ((: []) . f)                            -- NOTE: We use list as Identity here so we don't have to pull in the package Identity is in, but the idea is the same
    x' = l' x
    tl' (UpdateEntry ti ccs) = UpdateEntry ti (cc : ccs)
    cc = StoreColumnData (FieldInfo $ columnName c) $ toFieldData c $ getLens c x'