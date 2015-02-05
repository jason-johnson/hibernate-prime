{-# LANGUAGE TypeFamilies #-}
module Database.Hibernate.Meta
(
   ColumnMetaData(..)
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
  toFieldData :: c -> ColType c -> FieldData
  getFieldData :: c -> Table c -> FieldData
  getFieldData c t = toFieldData c $ getConst . lens c Const $ t

class TableMetaData t where
  tableName :: t -> String
  mapColumns :: t -> (String -> FieldData -> a) -> [a]