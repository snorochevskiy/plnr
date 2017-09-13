{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import Lib
import qualified Endpoints.Employee as Rest
import Entities.Employee

import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack, unpack)
import GHC.Int

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.HTTP.Types
import qualified Database.Persist.Sqlite as Db
import qualified Database.Persist as P
import Database.Persist.Types
import Database.Persist.TH
import Data.Pool


main :: IO ()
main = do
  -- Db.runSqlite "example.db" $ Db.runMigration migrateAll
  runStderrLoggingT $ Db.withSqlitePool "plnr.db" 10 $ \(pool::Pool Db.SqlBackend) -> liftIO $ do
    runResourceT $ flip Db.runSqlPool pool $ do
      Db.runMigration migrateAll
      johnId <- P.insert $ Employee "John" "Doe" "Street 15" 25
      return ()
    scotty 3000 $ do
      middleware $ logStdoutDev . (staticPolicy noDots) -- >-> addBase "/static/"
      get "/" $ file "index.html"
      get "/employee/:employeeId" (Rest.processGetEmployee pool)
      notFound Rest.processNotFound
