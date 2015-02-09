{-# LANGUAGE RankNTypes #-}
module Database.Hibernate.Driver
(
   Driver(..)
  ,genericSessionDriver
)
where

import Database.Hibernate.Driver.Command
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intercalate)

data Driver = Driver {
         driverSave   :: SaveEntry -> IO StoredResponse
        ,driverUpdate :: UpdateEntry -> IO UpdatedResponse
 }
 
instance Show (Driver) where       -- TODO: Remove this later
  show = const "SESSION-DRIVER"

genericSessionDriver :: Driver
genericSessionDriver = Driver save update
  where
    save (SaveEntry ti@(TableInfo tableName _) rows) = do
      insertStmt tableName . toListPair $ rows
      return . StoredResponse ti . NativeSerialKeyData $ 1

    insertStmt tName xs = putStrLn $ "insert into " ++ tName ++ "(" ++ (intercalate ", " . fst) xs ++ ") values(" ++ (intercalate ", " . snd) xs ++ ")"
    
    update (UpdateEntry ti@(TableInfo tableName _) rows) = do
      updateStmt tableName . toPairsList $ rows
      return . UpdatedResponse ti . NativeSerialKeyData $ 1
    
    updateStmt tName xs = putStrLn $ "update " ++ tName ++ " " ++ (intercalate ", " . map (\(f,v) -> "set " ++ f ++ " = " ++ v) $ xs)
    
    mapCC f (StoreColumnData (FieldInfo colName) colData) = f colName colData
    
    toMaybeString (BoolData val) = justShow val
    toMaybeString (IntData val) = justShow val
    toMaybeString (CharData val) = justShow val
    toMaybeString (StringData val) = justShow val
    toMaybeString (NullableData mcd) = maybe Nothing toMaybeString mcd
    
    justShow :: Show a => a -> Maybe String
    justShow = Just . show
    
    accP (n, cd) (ns, cds) = (n : ns, cd : cds)
    toListPair = foldr accP ([], []) . mapMaybe (mapCC go)
      where
        go n x = go' n $ toMaybeString x
        go' n = fmap (\x -> (n, x))
    toPairsList = map $ mapCC go
      where go n = fmap (\x -> (n, fromMaybe "NULL" x)) toMaybeString