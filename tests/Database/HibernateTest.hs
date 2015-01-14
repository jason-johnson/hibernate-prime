{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Database.HibernateTest where
import Database.Hibernate


import Test.Framework

data Country = Country { cName :: String } deriving (Show)

data State = State {
      sName     :: String
    , sCountry  :: Country
    }
    deriving (Show)

data City = City {
      ccName    :: String
    , cState    :: State
    , cZipCode  :: String
    }
    deriving (Show)

data Address = Address {
      aStreetName  :: String
    , aPOBox       :: Int
    , aCity        :: City
    }
    deriving (Show)

class NameField a t | a -> t where
  name :: a -> t

instance NameField Country String where name = cName
instance NameField State String where name = sName
instance NameField City String where name = ccName

country = Country "CH"
state = State "SG" country
city = City "Buchs" state "9470"
address = Address "Steinweg" 12 city

instance Serializable Country where
  dehydrate c = RowData "Country" [FieldData (StringFieldData . name $ c) "name"] []
  hydrate _ = undefined

instance Serializable State where
  dehydrate s = RowData "State" [FieldData (StringFieldData . name $ s) "name", FieldData (StringFieldData . name . sCountry $ s) "country_id"] []
  hydrate _ = undefined

saveCountry :: Session Country
saveCountry = save country

x :: IO (Country, State)
x = runSession f genericSessionDriver
  where f = do
              c <- saveCountry
              s <- save state
              return (c, s)

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
