{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}

module Endpoints.Employee where

import Entities.Employee

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack, unpack)
import GHC.Int

import Web.Scotty
import Network.HTTP.Types
import qualified Database.Persist.Sqlite as Db
import qualified Database.Persist as P
import Database.Persist.Types
import Database.Persist.TH
import Data.Pool

processGetEmployee :: Pool Db.SqlBackend -> ActionM ()
processGetEmployee pool = do
  (employeeId :: Int64) <- param "employeeId"
  (employee :: Maybe Employee) <- liftIO $ flip Db.runSqlPool pool $ Db.get $ Db.toSqlKey employeeId
  case employee of
    Just (Employee{..}) -> html $ mconcat ["<h1>Employee ", (pack employeeFirstName), " found</h1>"]
    Nothing ->
      status notFound404

processNotFound :: ActionM ()
processNotFound = do
  status notFound404
  html "<h1>Not found :(</h1>"
