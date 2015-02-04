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
  ,update'
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

-- UpdateContext

newtype UpdateContext a = UpdateContext { runUpdateContext :: UpdateTable -> (a, UpdateTable) }

instance Functor UpdateContext where
    fmap f m = UpdateContext $ first f <$> runUpdateContext m

instance Applicative UpdateContext where
    pure = return
    (<*>) = ap

instance Monad UpdateContext where
    return a = UpdateContext $ \ut -> (a, ut)
    m >>= k = UpdateContext $ \ ut ->
      let (x, ut') = runUpdateContext m ut
      in runUpdateContext (k x) ut'

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

-- NOTE: This signature looks like it could work but it means f functions have to be fused with >>=   (which is probably correct as they cannot be a (.) compose)
update :: (MonadIO m, Serializable a) => UpdateContext a -> a -> SessionT m a
update f x = SessionT $ \sd -> do
  _ <- liftIO . driverUpdate sd $ ut                                  -- TODO: this will return a response that has the key in it, which must be put in x' (which will require a type family to do probably)
  return (x', sd)
  where
    (x', ut) = runUpdateContext f $ UpdateTable ti []           -- f needs to build the update commands at the same time as updating the actual structure.  We need a lens plus something with table info
    ti = tableInfo . dehydrate $ x
    tableInfo (RowData tableName _ _) = TableInfo tableName ""

-- NOTE: With this signature we would have to first apply (_f _x) and then simply do an SQL:    update _x set <field> = <field value>, ... where id = <id>      where the ... is all remaining fields in the record 
update' :: (MonadIO m, Serializable a) => (a -> a) -> a -> SessionT m a
update' _f _x = undefined