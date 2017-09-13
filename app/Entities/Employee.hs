{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Entities.Employee where

import Database.Persist.Types
import Database.Persist.TH
import Data.Pool


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Employee
    firstName String
    lastName String
    address String
    age Int
    deriving Show
|]
