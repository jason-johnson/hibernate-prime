module Database.Hibernate
(
  session
  ,Session
  ,SessionT
  ,genericSessionDriver
  ,runSession
  ,save
  ,HibernateFieldType(..)
  ,HibernateField(..)
  ,HibernateTable(..)
  ,Hibernatable(..)
  ,dummy2
)
where

import Database.Hibernate.Session
import Database.Hibernate.Transaction