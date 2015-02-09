module Database.Hibernate.Driver.Command
(
   FieldData(..)
  ,KeyData(..)
  ,ColumnCommand(..)
  ,SaveEntry(..)
  ,UpdateEntry(..)
  ,StoredResponse(..)
  ,UpdatedResponse(..)
  ,TableInfo(..)
  ,FieldInfo(..)
)
where

data TableInfo = TableInfo { tiName :: String, tiSchema :: String }
newtype FieldInfo = FieldInfo String

data FieldData =
    BoolData Bool
  | IntData Int
  | CharData Char
  | StringData String
  | NullableData (Maybe FieldData)

data KeyData =
    NativeSerialKeyData Int
  | GuidKeyData String

data ColumnCommand =
  StoreColumnData FieldInfo FieldData

--data RowCommand =
--  Insert [ColumnCommand]

data SaveEntry = SaveEntry TableInfo [ColumnCommand]

data UpdateEntry = UpdateEntry TableInfo [ColumnCommand]     -- Needs the key to know what to update

-- Responses

data StoredResponse = StoredResponse TableInfo KeyData

data UpdatedResponse = UpdatedResponse TableInfo KeyData