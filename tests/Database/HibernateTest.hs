{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TypeFamilies   #-}
module Database.HibernateTest where

import Database.Hibernate
import Database.Hibernate.Driver.Command (FieldData(StringData, IntData))

import Test.Framework
import Data.Functor ((<$>))

data Country = Country { cName :: String } deriving (Show)

data CountryNameField = CountryNameField

cNameCol :: CountryNameField
cNameCol = CountryNameField

instance TableMetaData Country where
  tableName _ = "Country"
  mapColumns t f = [f cn cd]
    where
      cn = columnName CountryNameField
      cd = getFieldData CountryNameField t
  foldColumns [] = error "impossible: no data"
  foldColumns [(cn, StringData n)] | cn == columnName CountryNameField = Country n
  foldColumns _ = error "impossible: wrong data"

instance ColumnMetaData CountryNameField where
  type Table CountryNameField = Country
  type ColType CountryNameField = String
  columnName _ = "name"
  lens _ f c@(Country name) = (\name' -> c { cName = name' }) <$> f name

instance PrimativeColumnMetaData CountryNameField where
  toFieldData _ = StringData

data State = State {
      sName     :: String
    , sCountry  :: Country
    }
    deriving (Show)

data StateNameField = StateNameField

sNameCol :: StateNameField
sNameCol = StateNameField

data StateCountryField = StateCountryField

sCountryCol :: StateCountryField
sCountryCol = StateCountryField

instance TableMetaData State where
  tableName _ = "State"
  mapColumns t f = [f cn cd]
    where
      cn = columnName StateNameField
      cd = getFieldData StateNameField t
  foldColumns [] = error "impossible: no data"
  foldColumns [(cn, StringData n)] | cn == columnName StateNameField = State n (Country "Dummy")
  foldColumns _ = error "impossible: wrong data"

instance ColumnMetaData StateCountryField where
  type Table StateCountryField = State
  type ColType StateCountryField = Country
  columnName _ = "country_id"
  lens _ f s@(State _ country) = (\country' -> s { sCountry = country' }) <$> f country

instance ColumnMetaData StateNameField where
  type Table StateNameField = State
  type ColType StateNameField = String
  columnName _ = "name"
  lens _ f s@(State name _) = (\name' -> s { sName = name' }) <$> f name

instance PrimativeColumnMetaData StateNameField where
  toFieldData _ = StringData

data City = City {
      ccName    :: String
    , cState    :: State
    , cZipCode  :: String
    }
    deriving (Show)

data CityNameField = CityNameField

ccNameCol :: CityNameField
ccNameCol = CityNameField

instance ColumnMetaData CityNameField where
  type Table CityNameField = City
  type ColType CityNameField = String
  columnName _ = "name"
  lens _ f c@(City name _ _) = (\name' -> c { ccName = name' }) <$> f name

instance PrimativeColumnMetaData CityNameField where
  toFieldData _ = StringData

data CityZipCodeField = CityZipCodeField

cZipCodeCol :: CityZipCodeField
cZipCodeCol = CityZipCodeField

instance ColumnMetaData CityZipCodeField where
  type Table CityZipCodeField = City
  type ColType CityZipCodeField = String
  columnName _ = "zip_code"
  lens _ f c@(City _ _ zc) = (\zc' -> c { cZipCode = zc' }) <$> f zc

instance PrimativeColumnMetaData CityZipCodeField where
  toFieldData _ = StringData

data CityStateField = CityStateField

cStateCol :: CityStateField
cStateCol = CityStateField

instance ColumnMetaData CityStateField where
  type Table CityStateField = City
  type ColType CityStateField = State
  columnName _ = "state_id"
  lens _ f c@(City _ state _) = (\state' -> c { cState = state' }) <$> f state

instance TableMetaData City where
  tableName _ = "City"
  mapColumns t f = [f cn cd, f zcn zcd]
    where
      cn = columnName CityNameField
      cd = getFieldData CityNameField t
      zcn = columnName CityZipCodeField
      zcd = getFieldData CityZipCodeField t
  foldColumns = go $ City undefined (State "dummyS" (Country "dummyC")) undefined       -- NOTE: Hack, we plug in placeholders which will be replaced by go
    where
      go city [] = city
      go city ((cn, fd):xs) = go (apf cn fd city) xs
      apf cn (StringData n) city
        | cn == columnName CityNameField    = city { ccName = n }
        | cn == columnName CityZipCodeField = city { cZipCode = n }
      apf _ _ _ = error "impossible: bogus data"

data Address = Address {
      aStreetName  :: String
    , aPOBox       :: Int
    , aCity        :: City
    }
    deriving (Show)

data AddressStreetNameField = AddressStreetNameField

aStreetNameCol :: AddressStreetNameField
aStreetNameCol = AddressStreetNameField

instance ColumnMetaData AddressStreetNameField where
  type Table AddressStreetNameField = Address
  type ColType AddressStreetNameField = String
  columnName _ = "name"
  lens _ f a@(Address name _ _) = (\name' -> a { aStreetName = name' }) <$> f name

instance PrimativeColumnMetaData AddressStreetNameField where
  toFieldData _ = StringData

data AddressPOBoxField = AddressPOBoxField

aPOBoxCol :: AddressPOBoxField
aPOBoxCol = AddressPOBoxField

instance ColumnMetaData AddressPOBoxField where
  type Table AddressPOBoxField = Address
  type ColType AddressPOBoxField = Int
  columnName _ = "zip_code"
  lens _ f a@(Address _ pobox _) = (\pobox' -> a { aPOBox = pobox' }) <$> f pobox

instance PrimativeColumnMetaData AddressPOBoxField where
  toFieldData _ = IntData

instance TableMetaData Address where
  tableName _ = "City"
  mapColumns t f = [f sn sd, f pon pod]
    where
      sn = columnName AddressStreetNameField
      sd = getFieldData AddressStreetNameField t
      pon = columnName AddressPOBoxField
      pod = getFieldData AddressPOBoxField t

saveCountry :: String -> Session Country
saveCountry = save . Country

x :: IO (Country, State, City, Address, [Country], [City])
x = runSession f genericSessionDriver
  where f = do
              c     <- saveCountry "USA"
              s     <- save $ State "NY" c          -- TODO: This will do a lookup in the cache for the id number of this country
              c'    <- update c $ set cNameCol "USA2"
              s'    <- update s $ set sNameCol "NY2"
              city  <- save $ City "NYC" s' "69432"
              city' <- update city $ modify ccNameCol (++ " Rules!") . set cZipCodeCol "69440"
              addr  <- save $ Address "Long" 11 city'
              addr' <- update addr $ set aStreetNameCol "Long2" . set aPOBoxCol 15
              ctys  <- fetchAll
              ctys' <- fetch cStateCol <.> sCountryCol <.> cNameCol ~== "USA"
              sts   <- fetch sNameCol ~== "NYC"         -- NOTE: Purposely not using this variable to show that the return type is inferred anyway
              return (c', s', city', addr', ctys, ctys')

-- TODO: How do we deal with defaultable fields?  Maybe:   data Default a = Default | Specified a

-- NEW STRATEGY

-- saveCountry :: String -> Transaction Country                                      -- NOTE: This is probably wrong, but it should work much like the State monad does (Transaction is a transformer on top of at least IO)
-- saveCountry name = save Country { countryName = name }                             -- NOTE: save is a function that takes an Entity instance and an open transaction and stores the instance (also have update, saveOrUpdate, etc.)

-- saveState :: String -> Country -> Transaction State                               -- NOTE: see above
-- saveState name country = save State { stateName = name, stateCountry = country }

-- saveCity :: String -> State -> String -> Transaction City                         -- NOTE: see above
-- saveCity name state zipCode = save City { cityName = name, cityState = state, cityZipCode = zipCode }

-- saveAddress :: String -> Int -> City -> Transaction Address                       -- NOTE: see above
-- saveAddress street poBox city = save Address { addressStreetName = street, addressPOBox = poBox, addressCity = city }

-- savePerson :: String -> String -> Address -> Transaction Person                   -- NOTE: see above
-- savePerson name lastName address = save Person { personFirstName = name, personLastName = lastName, personAddress = address }

-- saveCouple :: String -> String -> Address -> Transaction Person                   -- NOTE: see above
-- saveCouple husband wife = save MarriedCouple { husband = husband, wife = wife }

-- run = runTransaction (runSession (getCurrentSession configuration)) go
--         where go = do
--                jason <- setup

-- setup = do
--        ch        <- saveCountry "Switzerland"
--        sg        <- saveState "St. Gallen" ch
--        buchs        <- saveCity "Buchs" sg "9470"
--        home        <- saveAddress "Steinweg" 12 buchs
--        jason        <- savePerson "Jason" "Johnson" home
--        kathrin        <- savePerson "Kathrin" "Johnson" home
--        _        <- saveCouple jason kathrin
--        return jason

-- johnsons :: Transaction ([Person], [Person])
-- johnsons = do
--        byName <- fetch (lastName ~== "Johnson")
--        byCountry <- fetch (countryName.stateCountry.cityState.addressCity.personAddress ~== "Switzerland")        -- NOTE: these aren't exactly lenses but a custom lens with additional operations
--        return (byName, byCountry)                                                                                -- NOTE: they can be composed with each other but the (==) will use the generator
--                                                                                                                -- NOTE: portion of them to create a query and join on the required fields

-- namesAndPOBoxes :: Transaction [(String, Int)]
-- namesAndPOBoxes = do
--        (name, poBox) <- view allOf (personFirstName, addressPOBox.personAddress)                                -- NOTE: Again, it's unclear if this can work as is but this shows how to select specific fields should work
--        return (name, poBox)
