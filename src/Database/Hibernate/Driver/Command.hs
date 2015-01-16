module Database.Hibernate.Driver.Command
(
   ColumnData(..)
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

data KeyData =
    NativeSerialKeyData Int
  | GuidKeyData String

data ColumnCommand =
  StoreColumnData String ColumnData

--data RowCommand =
--  Insert [ColumnCommand]

data TableCommand =
  StoreTable String [ColumnCommand]

-- Responses

data RowCommandResponse =
  StoredRow KeyData

data TableCommandResponse =
  Stored String [RowCommandResponse]