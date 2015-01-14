{-# LANGUAGE RankNTypes #-}
module Database.Hibernate.Driver
(
   Driver(..)
  ,genericSessionDriver
)
where

import qualified Database.Hibernate.Serialization as S

newtype Driver = Driver {
        driverSave :: S.Serializable a => a -> IO a
 }
 
instance Show (Driver) where       -- TODO: Remove this later
  show = const "SESSION-DRIVER"
 
genericSessionDriver :: Driver
genericSessionDriver = Driver $ \x -> do
  putStrLn "Saving..."
  print . S.dehydrate $ x
  putStrLn "Done!"
  return x
