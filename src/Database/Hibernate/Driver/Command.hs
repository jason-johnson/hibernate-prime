module Database.Hibernate.Driver.Command
(
   FieldData(..)
  ,KeyData(..)
  ,ColumnCommand(..)
  ,TableCommand(..)
  ,RowCommandResponse(..)
  ,TableCommandResponse(..)
)
where

data ColumnData =
    BoolData Bool
  | IntData Int
  | CharData Char
  | StringData String
  | NullableData (Maybe ColumnData)
data TableInfo = TableInfo { tiName :: String, tiSchema :: String }
data FieldInfo = FieldInfo { fiName :: String, fiSchema :: String }

data KeyData =
    NativeSerialKeyData Int
  | GuidKeyData String

data ColumnCommand =
  StoreColumnData String FieldData

--data RowCommand =
--  Insert [ColumnCommand]

data TableCommand =
  StoreTable String [ColumnCommand]

-- Responses

data RowCommandResponse =
  StoredRow KeyData

data TableCommandResponse =
  Stored String [RowCommandResponse]