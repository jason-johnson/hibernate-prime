module Database.Hibernate.Serialization
(
    FieldTypeData(..)
  , KeyTypeData(..)
  , FieldData(..)
  , KeyFieldData(..)
  , RowData(..)
  , Serializable(..)
)
where

import Data.Int (Int16)

type FieldName = String
type TableName = String

--data FieldType =
--    BoolField
--  | Int16Field
--  | IntField 
--  | CharField
--  | StringField
--  | Nullable FieldType
--  deriving (Show)

--data KeyType =
--    NativeSerialKey
--  | GuidKey
--  deriving (Show)

--data Field = Field { fType :: FieldType, fName :: FieldName }
--  deriving (Show)

--data KeyField = KeyField { kfType :: KeyType, kfName :: FieldName }
--  deriving (Show)

data FieldTypeData =
    BoolFieldData Bool
  | Int16FieldData Int16
  | IntFieldData Int
  | CharFieldData Char
  | StringFieldData String
  | NullableData (Maybe FieldData)
  deriving (Show)

data KeyTypeData =
    NativeSerialKeyData (Maybe Int)
  | GuidKeyData (Maybe String)
  deriving (Show)

data FieldData = FieldData { fdType :: FieldTypeData, fdName :: FieldName }
  deriving (Show)

data KeyFieldData = KeyData { kdType :: KeyTypeData, kdName :: FieldName }
  deriving (Show)

data RowData = RowData { rdTableName :: TableName, rdFieldData :: [FieldData], rdKeyData :: [KeyFieldData] }
  deriving (Show)
-- TODO: Ok, so the idea for the moment is that we don't store anything about the tables we know (this would require some sort of "global" storage).  We encode it via the Serializable type class and have the client
-- TODO: simply provide this data for persistence.  We'll see how far we can get with this before we need more sophisticated infrastructure 

--data Table = Table { tName :: TableName, tFields :: [Field], tKeys :: [KeyField]  }

class Serializable a where
  dehydrate :: a -> RowData
  hydrate :: RowData -> a