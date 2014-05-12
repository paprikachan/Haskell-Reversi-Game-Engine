{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE FlexibleInstances #-}


module Server where
{-
 - This file defines the entry point to the server application
 -}

import Foundation     --Import the underlying site data structures
import RouteHandlers  --Import the sites route handlers

-- Additional Packages 
import Yesod          --The webserver framework we'll use
import Yesod.Static
import Database.Persist.Sqlite
import Data.IORef
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

--Tie together the site
mkYesodDispatch "GameServer" resourcesGameServer

openConnectionCount = 10

--Entry point
server = withSqlitePool "database.db3" openConnectionCount $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
    gamesRef <- newIORef []
    idRef <- newIORef (-1)
    waitIdRef <- newIORef (-1)
    endRef <- newIORef []
    client@(Static settings) <- static "client"
    warp 3000 $ GameServer pool gamesRef idRef waitIdRef endRef client


