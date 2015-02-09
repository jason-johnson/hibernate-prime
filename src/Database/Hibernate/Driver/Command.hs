module Database.Hibernate.Driver.Command
(
   FieldData(..)
  ,KeyData(..)
  ,ColumnCommand(..)
  ,ExpressionCommand(..)
  ,SaveEntry(..)
  ,UpdateEntry(..)
  ,FetchEntries(..)
  ,StoredResponse(..)
  ,UpdatedResponse(..)
  ,FetchedResponse(..)
  ,ColumnResponse(..)
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

data ExpressionCommand = ExpressionCommand

data ColumnResponse =
  RetreivedColumnData FieldInfo FieldData

--data RowCommand =
--  Insert [ColumnCommand]

data SaveEntry = SaveEntry TableInfo [ColumnCommand]

data UpdateEntry = UpdateEntry TableInfo [ColumnCommand]     -- Needs the key to know what to update

data FetchEntries = FetchEntries TableInfo [ExpressionCommand]

-- Responses

data StoredResponse = StoredResponse TableInfo KeyData

data UpdatedResponse = UpdatedResponse TableInfo KeyData

data FetchedResponse = FetchedResponse TableInfo [[ColumnResponse]]