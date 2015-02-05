{-# LANGUAGE RankNTypes, TypeFamilies #-}
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
  ,ColumnMetaData(..)
)
where

import Database.Hibernate.Driver (Driver, driverSave, driverUpdate, genericSessionDriver)
import Database.Hibernate.Serialization
import Database.Hibernate.Driver.Command
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

save :: (MonadIO m, Serializable a) => a -> SessionT m a
save x = SessionT $ \sd -> do
  _ <- liftIO . driverSave sd . buildCommands . dehydrate $ x           -- TODO: this will return a response that has the key in it, which must be put in x (which will require a type family to do probably)
  return (x, sd)
  where
    buildCommands (RowData tableName fieldData _) = SaveTable (TableInfo tableName "") . map buildColumn $ fieldData
    buildColumn (FieldData colName fieldData) = StoreColumnData (FieldInfo colName) . buildColumValue $ fieldData
    buildColumValue (BoolFieldData val) = BoolData val
    buildColumValue (Int16FieldData val) = IntData . fromIntegral $ val
    buildColumValue (IntFieldData val) = IntData val
    buildColumValue (CharFieldData val) = CharData val
    buildColumValue (StringFieldData val) = StringData val
    buildColumValue (NullableFieldData val) = NullableData . fmap buildColumValue $ val
 
update :: (MonadIO m, Serializable a) => a -> ((a, UpdateTable) -> (a, UpdateTable)) -> SessionT m a
update x f = SessionT $ \sd -> do
  _ <- liftIO . driverUpdate sd $ ut                                  -- TODO: this will return a response that has the key in it, which must be put in x' (which will require a type family to do probably)
  return (x', sd)
  where
    (x', ut) = f (x, UpdateTable ti [])           -- f needs to build the update commands at the same time as updating the actual structure.  We need a lens plus something with table info
    ti = tableInfo . dehydrate $ x
    tableInfo (RowData tableName _ _) = TableInfo tableName ""

set :: ColumnMetaData c => c -> ColType c -> (Table c, UpdateTable) -> (Table c, UpdateTable)
set c v (x, uc) = (l' x, tl' uc)
  where
    l' = head . lens c ((: []) . const v)
    tl' (UpdateTable ti ccs) = UpdateTable ti (cc : ccs)
    cc = StoreColumnData (FieldInfo $ columnName c) $ toFieldData c v

class ColumnMetaData c where
  type Table c
  type ColType c
  columnName :: c -> String
  lens :: Functor f => c -> (ColType c -> f (ColType c)) -> Table c -> f (Table c)
  toFieldData :: c -> ColType c -> Database.Hibernate.Driver.Command.FieldData