{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Foundation where
{-
 - This file defines the underlying datastructures the site is built upon.
 - Compile-time settings should end up here
 -}

import Parser
import Runner

-- Additional Pakages
import Yesod
import Yesod.Static
import Database.Persist.Sqlite -- ConnectionPool
import Data.IORef

-- foundataion datatype GameServer declaration
data GameServer = GameServer {
	pool :: ConnectionPool,	
	gamesRef :: IORef [IORef Game],
	currentIdRef :: IORef Int,
	waitIdRef :: IORef Int,
	endIdsRef :: IORef [Int],
	getClient :: Static
	}

-- Database Setting --

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase| -- create entity table & initialise databse
User -- Define User entity
	email 		String
    password 	String
    username 	String
    bestScore	Int
    UniqueEmail email
    deriving 	Show
|]

-- define runDB in YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist GameServer where
    type YesodPersistBackend GameServer = SqlPersistT
    runDB action = do
        GameServer {..} <- getYesod
        runSqlPool action pool

-- Static client files setting --

--Serve static client files from under /client/
staticFiles "client"

--Generate routing map from file
mkYesodData "GameServer" $(parseRoutesFile "routes")

instance Yesod GameServer




