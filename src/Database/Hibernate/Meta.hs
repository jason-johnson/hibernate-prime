{-# LANGUAGE TypeFamilies #-}
module Database.Hibernate.Meta
(
   ColumnMetaData(..)
  ,PrimativeColumnMetaData(..)
  ,TableMetaData(..)
)
where

import Database.Hibernate.Driver.Command
import Control.Applicative (Const(..))

class ColumnMetaData c where
  type Table c
  type ColType c
  columnName :: c -> String
  lens :: Functor f => c -> (ColType c -> f (ColType c)) -> Table c -> f (Table c)

class ColumnMetaData c => PrimativeColumnMetaData c where
  toFieldData :: c -> ColType c -> FieldData
  getFieldData :: c -> Table c -> FieldData
  getFieldData c t = toFieldData c $ getConst . lens c Const $ t

class TableMetaData t where
  tableName :: t -> String
  schemaName :: t -> String
  schemaName _ = ""
  mapColumns :: t -> (String -> FieldData -> a) -> [a]
  foldColumns :: [(String, FieldData)] -> t
  {-# MINIMAL tableName, mapColumns, foldColumns #-}