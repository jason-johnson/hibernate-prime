{-# LANGUAGE TypeFamilies #-}
module Database.Hibernate.Meta
(
   ColumnMetaData(..)
  ,PrimativeColumnMetaData(..)
  ,TableMetaData(..)
  ,ComposeColumn(..)
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

data ComposeColumn l r = ComposeColumn l r

instance (ColumnMetaData l, ColumnMetaData r, ColType l ~ Table r) => ColumnMetaData (ComposeColumn l r) where
  type Table (ComposeColumn l r) = Table l
  type ColType (ComposeColumn l r) = ColType r
  columnName (ComposeColumn _ r) = columnName r
  lens (ComposeColumn l r) = lens l . lens r

instance (ColumnMetaData l, PrimativeColumnMetaData r, ColType l ~ Table r) => PrimativeColumnMetaData (ComposeColumn l r) where
  toFieldData (ComposeColumn _ r) = toFieldData r

class TableMetaData t where
  tableName :: t -> String
  schemaName :: t -> String
  schemaName _ = ""
  mapColumns :: t -> (String -> FieldData -> a) -> [a]
  foldColumns :: [(String, FieldData)] -> t
  {-# MINIMAL tableName, mapColumns, foldColumns #-}