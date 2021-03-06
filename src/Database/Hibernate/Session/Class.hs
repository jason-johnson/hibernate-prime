module Database.Hibernate.Session.Class
(
  MonadSession(..)
)
where

import Database.Hibernate.Session
import Database.Hibernate.Serialization (Serializable)
import qualified Database.Hibernate.Session as HS
import Control.Monad.IO.Class (MonadIO)

class (MonadIO m) => MonadSession m where
  save :: (MonadIO m, Serializable a) => a -> m a

instance (MonadIO m) => MonadSession (SessionT m) where
  save = HS.save