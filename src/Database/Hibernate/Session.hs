{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
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
  ,fetch
  ,fetchAll
  ,(~==)
  ,(<.>)
  ,join'
  ,join''
  ,eq'
  ,fetch'
)
where

import Database.Hibernate.Driver (Driver(..), genericSessionDriver)
import Database.Hibernate.Driver.Command
import Database.Hibernate.Meta
import Control.Applicative
import Control.Monad (ap, mzero, mplus, MonadPlus, liftM)
import Control.Arrow (first, (***), (&&&))
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

modLens :: ColumnMetaData c => c -> (ColType c -> ColType c) -> Table c -> Table c
modLens c f = head . lens c ((: []) . f)                      -- NOTE: We use list as Identity here so we don't have to pull in the package Identity is in, but the idea is the same

modLens' :: ColumnMetaData c => c -> (ColType c -> ColType c) -> Table c -> (ColType c, Table c)
modLens' c f = lens c (f &&& f)                               -- TODO: this is assuming that calling the function on the data twice is more efficient than using getLens to get it back out again.  Is that likely?

setLens :: ColumnMetaData c => c -> ColType c -> Table c -> Table c
setLens c v = modLens c (const v)

--getLens :: ColumnMetaData c => c -> Table c -> ColType c
--getLens c = getConst . lens c Const

set :: PrimativeColumnMetaData c => c -> ColType c -> (Table c, UpdateEntry) -> (Table c, UpdateEntry)
set c v = setLens c v *** g
  where                          
    g (UpdateEntry ti ccs) = UpdateEntry ti (cc : ccs)
    cc = StoreColumnData (FieldInfo $ columnName c) $ toFieldData c v

modify :: PrimativeColumnMetaData c => c -> (ColType c -> ColType c) -> (Table c, UpdateEntry) -> (Table c, UpdateEntry)
modify c f (x, uc) = (x', g uc)
-- modify c f = first (modLens c f) >>> fst &&& g                 -- alternative using arrows that removes the need to have x' subexpression. g would have to be modified to take (x, ut) and cc to take x
-- modify c f = first (modLens c f) >>> g                         -- and this variant where g also returns an unmodified x with ut (UPDATE: g ((v,x), ut) = (x, UpdateEntry ti (cc v: ccs)))
  where
    (v, x') = modLens' c f x
    g (UpdateEntry ti ccs) = UpdateEntry ti (cc : ccs)
    cc = StoreColumnData (FieldInfo $ columnName c) $ toFieldData c v

fetch :: forall a m. (MonadIO m, TableMetaData a) => (a -> FetchEntries -> FetchEntries) -> SessionT m [a]
fetch f = SessionT $ \sd -> do
  FetchedResponse _ rows <- liftIO . driverFetch sd $ fe
  return (map toTable rows, sd)
  where
    fe = f (undefined :: a) $ FetchEntries ti []                                     -- TODO: f probably needs to take (undefined :: a) as well to make sure only columns from the correct table can be used
    ti = TableInfo (tableName (undefined :: a)) (schemaName (undefined :: a))
    toTable = foldColumns . map tocd
    tocd (RetreivedColumnData (FieldInfo fn) fd) = (fn, fd)

fetchAll :: (MonadIO m, TableMetaData a) => SessionT m [a]
fetchAll = fetch $ const id

-- newtype WriterArrow w a b c = WriterArrow (a b (c, w))     -- TODO: Look up the writer monad

-- TODO: this needs to be converted to compose somehow, I think there must be a way to do this with arrows.  See alt defs of modify above for potential ideas
-- TODO: but a monad might make more sense, since that might let cl be a normal value and cr be the (m b)....
(<.>) :: (ColumnMetaData a, ColumnMetaData b) => (b, FetchEntries) -> a -> (ComposeColumn b a, FetchEntries)
(cl, FetchEntries ti exprs) <.> cr = (c, FetchEntries ti (expr:exprs))
  where
    expr = JoinExp { parentTableInfo = pti, childTableInfo = cti, foreignKeyInfo = fi cl, childFieldNameInfo = fi cr }
    pti = TableInfo "parent" ""
    cti = TableInfo "child" ""
    fi :: ColumnMetaData c => c -> FieldInfo
    fi = FieldInfo . columnName
    c = ComposeColumn cl cr

join' :: (ColumnMetaData a, ColumnMetaData b) => b -> a -> (ComposeColumn b a, FetchEntries)
join' cl = (<.>) (cl, FetchEntries (TableInfo "dummy" "") [])

-- NOTE: If we change fetch to take the first column and "lift" it, then take another function then it could roll the whole expression up and be reactivated by the terminator (eq)
join'' :: (ColumnMetaData r, ColumnMetaData l) => (FetchEntries -> (l, FetchEntries)) -> r -> FetchEntries -> (ComposeColumn l r, FetchEntries)
join'' cle cr fe =
  let
    (cl, FetchEntries ti exprs) = cle fe
    expr = JoinExp { parentTableInfo = pti, childTableInfo = cti, foreignKeyInfo = fi cl, childFieldNameInfo = fi cr }
    pti = TableInfo "parent" ""
    cti = TableInfo "child" ""
    fi :: ColumnMetaData c => c -> FieldInfo
    fi = FieldInfo . columnName
    c = ComposeColumn cl cr
  in (c, FetchEntries ti (expr:exprs))

eq' :: PrimativeColumnMetaData c => (FetchEntries -> (c, FetchEntries)) -> ColType c -> Table c -> FetchEntries -> FetchEntries     -- NOTE: The Table c entry is just to keep the table in the type
eq' ce v _ fe =
  let
    (c, FetchEntries ti exprs) = ce fe
    expr = EqExp (FieldInfo . columnName $ c) (toFieldData c v)
  in FetchEntries ti (expr:exprs)

(~==) :: PrimativeColumnMetaData c => (c, FetchEntries) -> ColType c -> FetchEntries
(c, FetchEntries ti exprs) ~== v = FetchEntries ti (expr:exprs)
  where
    expr = EqExp (FieldInfo . columnName $ c) (toFieldData c v)