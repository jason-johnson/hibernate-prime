module Database.Hibernate.Driver.Command
(
   FieldData(..)
  ,KeyData(..)
  ,ColumnCommand(..)
  ,SaveTable(..)
  ,UpdateTable(..)
  ,TableCommandResponse(..)
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

data SaveTable = SaveTable TableInfo [ColumnCommand]

data UpdateTable = UpdateTable TableInfo [ColumnCommand]     -- Needs the key to know what to update 

-- Responses

data TableCommandResponse =
    Stored TableInfo KeyData
  | Updated TableInfo KeyData