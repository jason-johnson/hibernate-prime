{-# LANGUAGE RankNTypes #-}
module Database.Hibernate.Driver
(
   Driver(..)
  ,genericSessionDriver
)
where

import Database.Hibernate.Driver.Command
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

newtype Driver = Driver {
        driverSave :: TableCommand -> IO TableCommandResponse
 }
 
instance Show (Driver) where       -- TODO: Remove this later
  show = const "SESSION-DRIVER"

genericSessionDriver :: Driver
genericSessionDriver = Driver save
  where
    save (StoreTable tableName rows) = do
      build tableName rows
      return . Stored tableName . (:[]) . StoredRow . NativeSerialKeyData $ 1
    interpretColCommand (StoreColumnData colName colData) = colData2tup colName colData
    colData2tup n (BoolData val) = Just (n, show val)
    colData2tup n (IntData val) = Just (n, show val)
    colData2tup n (CharData val) = Just (n, show val)
    colData2tup n (StringData val) = Just (n, show val)
    colData2tup n (NullableData mcd) = maybe Nothing (colData2tup n) mcd
    build tn = insertStmt tn . foldr mkIns ([], []) . mapMaybe interpretColCommand
    mkIns (n, cd) (n', cd') = (n : n', cd : cd')
    insertStmt table x = putStrLn $ "insert into " ++ table ++ "(" ++ (intercalate ", " . fst) x ++ ") values(" ++ (intercalate ", " . snd) x ++ ")"