{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Database.Hibernate.SessionTest
import {-@ HTF_TESTS @-} Database.HibernateTest
import {-@ HTF_TESTS @-} Database.Hibernate.TransactionTest

main :: IO()
main = htfMain htf_importedTests
