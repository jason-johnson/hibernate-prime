{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TypeFamilies   #-}
module Database.HibernateTest where

import Database.Hibernate
import Database.Hibernate.Driver.Command (FieldData(StringData))

import Test.Framework
import Data.Functor ((<$>))
import Control.Applicative (Const(..))

data Country = Country { cName :: String } deriving (Show)

cNameL f c@(Country name) = (\name' -> c { cName = name' }) <$> f name
cNameCol = CountryNameField

instance TableMetaData Country where
  tableName _ = "Country"
  mapColumns t f = f cn cd : []
    where
      cn = columnName CountryNameField
      cd = getFieldData CountryNameField t

data CountryNameField = CountryNameField

instance ColumnMetaData CountryNameField where
  type Table CountryNameField = Country
  type ColType CountryNameField = String
  columnName _ = "name"
  lens _ f c@(Country name) = (\name' -> c { cName = name' }) <$> f name
  toFieldData _ = StringData

data State = State {
      sName     :: String
    , sCountry  :: Country
    }
    deriving (Show)

sNameL f s@(State name _) = (\name' -> s { sName = name' }) <$> f name
sNameCol = StateNameField
sCountryL  f s@(State _ cntry) = (\country' -> s { sCountry = country' }) <$> f cntry

instance TableMetaData State where
  tableName _ = "State"
  mapColumns t f = f cn cd : []
    where
      cn = columnName StateNameField
      cd = getFieldData StateNameField t

data StateNameField = StateNameField

instance ColumnMetaData StateNameField where
  type Table StateNameField = State
  type ColType StateNameField = String
  columnName _ = "name"
  lens _ f c@(State name _) = (\name' -> c { sName = name' }) <$> f name
  toFieldData _ = StringData

data City = City {
      ccName    :: String
    , cState    :: State
    , cZipCode  :: String
    }
    deriving (Show)

ccNameL f c@(City name _ _) = (\name' -> c { ccName = name' }) <$> f name
ccStateL f c@(City _ st _) = (\state' -> c { cState = state' }) <$> f st
ccZipCodeL f c@(City _ _ zc) = (\zc' -> c { cZipCode = zc' }) <$> f zc

data Address = Address {
      aStreetName  :: String
    , aPOBox       :: Int
    , aCity        :: City
    }
    deriving (Show)

aStreetNameL f a@(Address name _ _) = (\name' -> a { aStreetName = name' }) <$> f name
aPOBoxL f a@(Address _ pobox _) = (\pobox' -> a { aPOBox = pobox' }) <$> f pobox
aCityL f a@(Address name _ _) = (\name' -> a { aStreetName = name' }) <$> f name

class NameLens a t | a -> where
  nameL :: Functor f => (t -> f t) -> a -> f a

instance NameLens Country String where nameL = cNameL
instance NameLens State String where nameL = sNameL
instance NameLens City String where nameL = ccNameL

get l = getConst . (l Const)
--set' l v = head . l ((:[]) . const v)                            -- NOTE: We use list as Identity here so we don't have to pull in Identity, but the idea is the same

-- get (ccStateL . sCountryL . cNameL) city                     --> "CH"
-- set (ccStateL . sCountryL . nameL) "EU" city                 -->  Just (City {ccName = "Buchs", cState = State {sName = "SG", sCountry = Country {cName = "EU"}}, cZipCode = "9470"})

country = Country "CH"
state = State "SG" country
city = City "Buchs" state "9470"
address = Address "Steinweg" 12 city

instance Serializable Country where
  dehydrate c = RowData "Country" [FieldData "name" (StringFieldData . get cNameL $ c)] []
  hydrate _ = undefined

instance Serializable State where
  dehydrate s = RowData "State" [FieldData "name" (StringFieldData . get sNameL $ s), FieldData "country_name" (StringFieldData . get (sCountryL . cNameL) $ s)] []
  hydrate _ = undefined

saveCountry :: String -> Session Country
saveCountry = save . Country

x :: IO (Country, State)
x = runSession f genericSessionDriver
  where f = do
              c     <- saveCountry "CH"
              s     <- save $ State "SG" c          -- TODO: This will do a lookup in the cache for the id number of this country
              c'    <- update c $ set cNameCol "CH2"
              s'    <- update s $ set sNameCol "SG2"
--            city  <- save $ City "Buchs" s "9470"
--            city' <-  set <$> ccZipCodeL "9940" <*>  sStateL s $ city
              return (c', s')

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
--        byName <- filter (lastName == "Johnson")
--        byCountry <- filter (countryName.stateCountry.cityState.addressCity.personAddress == "Switzerland")        -- NOTE: these aren't exactly lenses but a custom lens with additional operations
--        return (byName, byCountry)                                                                                -- NOTE: they can be composed with each other but the (==) will use the generator
--                                                                                                                -- NOTE: portion of them to create a query and join on the required fields

-- namesAndPOBoxes :: Transaction [(String, Int)]
-- namesAndPOBoxes = do
--        (name, poBox) <- view allOf (personFirstName, addressPOBox.personAddress)                                -- NOTE: Again, it's unclear if this can work as is but this shows how to select specific fields should work
--        return (name, poBox)
